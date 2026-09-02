;;; madrigal-submit.el --- Request submission for Madrigal  -*- lexical-binding: t; -*-

;;; Code:

(require 'madrigal-agent-controller)
(require 'madrigal-org)
(require 'madrigal-org-compact)
(require 'madrigal-context)
(require 'seq)
(require 'subr-x)

(require 'llm nil t)

(defun madrigal--record-request (request-id llm-request)
  "Track LLM-REQUEST under REQUEST-ID in the current buffer."
  (let ((existing (seq-find (lambda (request)
                              (string= request-id (madrigal-request-id request)))
                            madrigal--pending-requests)))
    (if existing
        (setf (madrigal-request-llm-request existing) llm-request)
      (push (madrigal-request-create :id request-id :llm-request llm-request)
            madrigal--pending-requests)))
  (madrigal-update-mode-line-status))

(defun madrigal--forget-request (request-id)
  "Stop tracking REQUEST-ID in the current buffer."
  (setq madrigal--pending-requests
        (cl-remove request-id madrigal--pending-requests
                   :key #'madrigal-request-id
                   :test #'string=))
  (madrigal--forget-request-turn-marker request-id)
  (madrigal-update-mode-line-status))

(defun madrigal-cancel-all-requests ()
  "Cancel all in-flight requests in the current Madrigal buffer."
  (interactive)
  (dolist (request madrigal--pending-requests)
    (let ((handle (madrigal-request-llm-request request)))
      (if (madrigal-agent-controller-handle-p handle)
          (madrigal-agent-controller-cancel handle)
        (when (and (madrigal-llm-available-p)
                   (fboundp 'llm-cancel-request)
                   handle)
          (llm-cancel-request handle)))))
  (setq madrigal--pending-requests nil)
  (madrigal-update-mode-line-status)
  (message "Cancelled Madrigal requests"))

(defun madrigal--ensure-session ()
  "Return the current Madrigal session or signal an error."
  (unless madrigal-session
    (user-error "Current buffer is not a Madrigal session"))
  madrigal-session)

(defun madrigal--ensure-llm ()
  "Ensure the llm package is available."
  (unless (madrigal-llm-available-p)
    (user-error "The `llm' package is not available")))

(defun madrigal-submit (&optional prompt)
  "Append PROMPT as a user turn and submit the Org session asynchronously."
  (interactive)
  (madrigal--ensure-session)
  (madrigal--ensure-llm)
  (unless (madrigal-session-provider madrigal-session)
    (user-error "No Madrigal provider configured"))
  (when madrigal--pending-requests
    (user-error "A Madrigal request is already in flight"))
  (when (and prompt (not (string-empty-p prompt)))
    (goto-char (madrigal--ensure-open-user-turn))
    (insert (string-trim-right prompt))
    (unless (bolp)
      (insert "\n")))
  (let* ((buffer (current-buffer))
         (agent (or (madrigal-session-agent madrigal-session)
                    (madrigal--buffer-agent-name)
                    "assistant"))
         (request-id (madrigal--next-request-id))
         (had-output nil)
         handle)
    (when (madrigal--babel-agent-p agent)
      (madrigal--prepare-request-turn request-id))
    (madrigal--record-request request-id nil)
    (setq
     handle
     (madrigal-agent-controller-submit-async
      :agent agent
      :provider (madrigal-session-provider madrigal-session)
      :model (madrigal-session-model madrigal-session)
      :history (madrigal--session-history-turns)
      :context (madrigal--session-environment-context madrigal-session)
      :environment (list :buffer buffer :request-id request-id)
      :on-response
      (lambda (event)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((text (plist-get event :text))
                  (final (plist-get event :final)))
              (when text
                (setq had-output t)
                (madrigal--append-assistant-text request-id text final))))))
      :on-finished
      (lambda (_event)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (or had-output (madrigal--request-turn-marker request-id))
              (madrigal--finish-assistant-turn request-id))
            (madrigal--forget-request request-id))))
      :on-error
      (lambda (event)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (madrigal--insert-error-response
             request-id (plist-get event :type) (plist-get event :message))
            (madrigal--forget-request request-id))))))
    (when (seq-find (lambda (request)
                      (string= request-id (madrigal-request-id request)))
                    madrigal--pending-requests)
      (madrigal--record-request request-id handle))
    (message "Submitted Madrigal request %s" request-id)
    handle))

(provide 'madrigal-submit)

;;; madrigal-submit.el ends here
