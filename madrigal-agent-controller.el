;;; madrigal-agent-controller.el --- Agent controller for Madrigal  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'madrigal-core)
(require 'madrigal-tool-eval)
(require 'seq)
(require 'subr-x)

(require 'llm nil t)

(cl-defstruct (madrigal-agent-controller-handle
               (:constructor madrigal-agent-controller-handle-create))
  id
  agent
  provider
  model
  llm-request
  prompt
  environment
  notifications
  status)

(defvar madrigal-agent-controller--requests nil
  "Active agent-controller request handles.")

(defun madrigal-agent-controller--safe-notify (function payload)
  "Call FUNCTION with PAYLOAD, ignoring notification failures."
  (when function
    (condition-case err
        (funcall function payload)
      (error
       (message "Madrigal notification failed: %S" err)))))

(defun madrigal-agent-controller--notify (handle event payload)
  "Notify HANDLE's EVENT with PAYLOAD."
  (madrigal-agent-controller--safe-notify
   (plist-get (madrigal-agent-controller-handle-notifications handle) event)
   payload))

(defun madrigal-agent-controller--remember (handle)
  "Remember active HANDLE."
  (setq madrigal-agent-controller--requests
        (cons handle
              (cl-remove (madrigal-agent-controller-handle-id handle)
                         madrigal-agent-controller--requests
                         :key #'madrigal-agent-controller-handle-id
                         :test #'string=)))
  handle)

(defun madrigal-agent-controller--forget (handle)
  "Forget active HANDLE."
  (setq madrigal-agent-controller--requests
        (delq handle madrigal-agent-controller--requests)))

(defun madrigal-agent-controller--response-text (response)
  "Extract assistant text from RESPONSE, or nil when absent."
  (cond
   ((stringp response) response)
   ((and (listp response) (stringp (plist-get response :text)))
    (plist-get response :text))
   (t nil)))

(defun madrigal-agent-controller--response-has-tool-uses-p (response)
  "Return non-nil when RESPONSE includes tool calls to continue from."
  (and (listp response)
       (plist-member response :tool-uses)))

(defun madrigal-agent-controller-provider-use-streaming-p (provider)
  "Return non-nil when PROVIDER should use streaming requests."
  (and (fboundp 'llm-openai-responses-p)
       (llm-openai-responses-p provider)
       (fboundp 'llm-openai-responses-codex-oauth)
       (llm-openai-responses-codex-oauth provider)))

(defun madrigal-agent-controller--utf8-string (value)
  "Return VALUE as a UTF-8 multibyte string when VALUE is a string."
  (if (and (stringp value)
           (not (multibyte-string-p value)))
      (decode-coding-string value 'utf-8 t)
    value))

(defun madrigal-agent-controller--normalize-jsonish (value)
  "Normalize VALUE for JSON serialization."
  (cond
   ((stringp value) (madrigal-agent-controller--utf8-string value))
   ((consp value)
    (cons (madrigal-agent-controller--normalize-jsonish (car value))
          (madrigal-agent-controller--normalize-jsonish (cdr value))))
   ((vectorp value)
    (vconcat (mapcar #'madrigal-agent-controller--normalize-jsonish value)))
   (t value)))

(defun madrigal-agent-controller--normalize-prompt-tool-uses (prompt)
  "Normalize tool-use arguments inside PROMPT before resubmission."
  (dolist (interaction (llm-chat-prompt-interactions prompt))
    (let ((content (llm-chat-prompt-interaction-content interaction)))
      (when (and (consp content)
                 (fboundp 'llm-provider-utils-tool-use-p)
                 (llm-provider-utils-tool-use-p (car content)))
        (dolist (tool-use content)
          (setf (llm-provider-utils-tool-use-args tool-use)
                (madrigal-agent-controller--normalize-jsonish
                 (llm-provider-utils-tool-use-args tool-use)))))))
  prompt)

(defun madrigal-agent-controller--history-to-llm-content (history)
  "Convert typed HISTORY turns to `llm-make-chat-prompt' content."
  (mapcar (lambda (turn)
            (cond
             ((stringp turn) turn)
             ((and (listp turn) (memq (plist-get turn :role) '(user assistant)))
              (or (plist-get turn :content) ""))
             (t (user-error "Invalid Madrigal history turn: %S" turn))))
          history))

(defun madrigal-agent-controller--context-string (agent-name context)
  "Return system/context string for AGENT-NAME and CONTEXT."
  (string-join
   (delq nil
         (list (madrigal--agent-system-prompt agent-name)
               (cond
                ((null context) nil)
                ((stringp context) context)
                ((listp context) (mapconcat #'identity (delq nil context) "\n\n"))
                (t (format "%s" context)))))
   "\n\n"))

(defun madrigal-agent-controller--resolve-provider-and-model (agent-name)
  "Resolve provider/model for AGENT-NAME."
  (let ((model-agent
         (if (alist-get agent-name madrigal-agent-models nil nil #'string=)
             agent-name
           (or (plist-get (madrigal--agent-definition agent-name) :model-agent)
               agent-name))))
    (madrigal--agent-provider-and-model model-agent)))

(defun madrigal-agent-controller--build-tools (agent-name environment request-id)
  "Return llm tools for AGENT-NAME in ENVIRONMENT under REQUEST-ID."
  (let ((buffer (plist-get environment :buffer))
        (event-sink (plist-get environment :event-sink))
        (action-context (plist-get environment :action-context))
        (request-context (plist-get environment :request-context)))
    (mapcar (lambda (tool-name)
              (let ((tool-definition (madrigal--tool-definition tool-name)))
                (unless tool-definition
                  (user-error "No Madrigal tool named %s for agent %s" tool-name agent-name))
                (madrigal--make-tool tool-name tool-definition buffer request-id
                                     event-sink action-context request-context)))
            (or (madrigal--agent-tool-names agent-name) '()))))

(defun madrigal-agent-controller-build-prompt
    (agent-name history context environment request-id &optional response-format)
  "Build an llm prompt for AGENT-NAME from HISTORY and CONTEXT."
  (llm-make-chat-prompt
   (madrigal-agent-controller--history-to-llm-content history)
   :context (madrigal-agent-controller--context-string agent-name context)
   :tools (madrigal-agent-controller--build-tools agent-name environment request-id)
   :response-format response-format))

(defun madrigal-agent-controller--submit-prompt (handle)
  "Submit HANDLE's prompt asynchronously."
  (let* ((provider (madrigal-agent-controller-handle-provider handle))
         (prompt (madrigal-agent-controller-handle-prompt handle))
         (request-id (madrigal-agent-controller-handle-id handle))
         (success-callback
          (lambda (response)
            (let ((text (madrigal-agent-controller--response-text response))
                  (continuep (madrigal-agent-controller--response-has-tool-uses-p response)))
              (when text
                (madrigal-agent-controller--notify
                 handle :response
                 (list :request-id request-id
                       :agent (madrigal-agent-controller-handle-agent handle)
                       :text text
                       :final (not continuep)
                       :raw response)))
              (if continuep
                  (run-at-time
                   0 nil
                   (lambda ()
                     (when (memq handle madrigal-agent-controller--requests)
                       (setf (madrigal-agent-controller-handle-llm-request handle)
                             (madrigal-agent-controller--submit-prompt handle)))))
                (setf (madrigal-agent-controller-handle-status handle) 'finished)
                (madrigal-agent-controller--forget handle)
                (madrigal-agent-controller--notify
                 handle :finished
                 (list :request-id request-id
                       :agent (madrigal-agent-controller-handle-agent handle)
                       :raw response))))))
         (error-callback
          (lambda (err-type err-msg)
            (setf (madrigal-agent-controller-handle-status handle) 'error)
            (madrigal-agent-controller--forget handle)
            (madrigal-agent-controller--notify
             handle :error
             (list :request-id request-id
                   :agent (madrigal-agent-controller-handle-agent handle)
                   :type err-type
                   :message err-msg)))))
    (madrigal-agent-controller--normalize-prompt-tool-uses prompt)
    (if (madrigal-agent-controller-provider-use-streaming-p provider)
        (llm-chat-streaming provider prompt nil success-callback error-callback t)
      (llm-chat-async provider prompt success-callback error-callback t))))

(cl-defun madrigal-agent-controller-submit-async
    (&key agent provider model history context response-format environment
          on-start on-response on-finished on-error on-cancelled)
  "Submit an async Madrigal agent request and return its handle.

Callers provide AGENT, conversation HISTORY, optional CONTEXT, ENVIRONMENT,
and notification functions.  RESPONSE-FORMAT is passed to the provider.
PROVIDER and MODEL default to the named agent's configuration."
  (unless (madrigal-llm-available-p)
    (user-error "The `llm' package is not available"))
  (unless agent
    (user-error "No Madrigal agent supplied"))
  (pcase-let* ((`(,provider . ,model)
                (if provider
                    (cons provider model)
                  (madrigal-agent-controller--resolve-provider-and-model agent)))
               (request-id (or (plist-get environment :request-id)
                               (madrigal--next-request-id)))
               (notifications (list :started on-start :response on-response
                                    :finished on-finished :error on-error
                                    :cancelled on-cancelled))
               (prompt (madrigal-agent-controller-build-prompt
                        agent history context environment request-id
                        response-format))
               (handle (madrigal-agent-controller-handle-create
                        :id request-id :agent agent :provider provider :model model
                        :prompt prompt :environment environment
                        :notifications notifications :status 'pending)))
    (madrigal-agent-controller--remember handle)
    (madrigal-agent-controller--notify
     handle :started
     (list :request-id request-id :agent agent :model model))
    (setf (madrigal-agent-controller-handle-llm-request handle)
          (madrigal-agent-controller--submit-prompt handle))
    handle))

(defun madrigal-agent-controller-cancel (handle)
  "Cancel async agent request HANDLE."
  (when (and handle
             (madrigal-llm-available-p)
             (fboundp 'llm-cancel-request)
             (madrigal-agent-controller-handle-llm-request handle))
    (llm-cancel-request (madrigal-agent-controller-handle-llm-request handle)))
  (when handle
    (setf (madrigal-agent-controller-handle-status handle) 'cancelled)
    (madrigal-agent-controller--forget handle)
    (madrigal-agent-controller--notify
     handle :cancelled
     (list :request-id (madrigal-agent-controller-handle-id handle)
           :agent (madrigal-agent-controller-handle-agent handle)))))

(defun madrigal-agent-controller-cancel-all ()
  "Cancel all active Madrigal agent-controller requests."
  (dolist (handle (copy-sequence madrigal-agent-controller--requests))
    (madrigal-agent-controller-cancel handle)))

(cl-defun madrigal-agent-controller-context-size (&key agent history context provider)
  "Return prompt-size information for AGENT, HISTORY, and CONTEXT.

The result is a plist with =:tokens=, =:limit=, and =:percent= when available."
  (let* ((provider (or provider
                       (car (madrigal-agent-controller--resolve-provider-and-model agent))))
         (texts (delq nil
                      (list (madrigal-agent-controller--context-string agent context)
                            (mapconcat #'identity
                                       (madrigal-agent-controller--history-to-llm-content history)
                                       "\n\n")
                            (mapconcat #'madrigal--tool-description
                                       (or (madrigal--agent-tool-names agent) '())
                                       "\n\n")))))
    (when provider
      (condition-case nil
          (let* ((tokens (apply #'+
                                (mapcar (lambda (text)
                                          (llm-count-tokens provider text))
                                        texts)))
                 (limit (and (fboundp 'llm-chat-token-limit)
                             (llm-chat-token-limit provider)))
                 (percent (and limit (> limit 0)
                               (* 100.0 (/ (float tokens) limit)))))
            (list :tokens tokens :limit limit :percent percent))
        (error nil)))))

(provide 'madrigal-agent-controller)

;;; madrigal-agent-controller.el ends here
