;;; madrigal-conversation-context.el --- Conversation contexts for Madrigal  -*- lexical-binding: t; -*-

;;; Code:

(require 'madrigal-core)
(require 'madrigal-org)
(require 'madrigal-agent-controller)
(require 'org)
(require 'subr-x)

(require 'llm nil t)

(defcustom madrigal-context-update-idle-delay 0.5
  "Idle seconds before recalculating Org-session context status."
  :type 'number
  :group 'madrigal)

(defvar-local madrigal--context-update-timer nil)
(defvar-local madrigal--history-cache nil)
(defvar-local madrigal--history-cache-tick nil)
(defvar-local madrigal--context-size-cache nil)
(defvar-local madrigal--context-size-cache-key nil)

(defun madrigal--invalidate-context-caches ()
  "Invalidate derived context data for the current session buffer."
  (setq madrigal--history-cache nil
        madrigal--history-cache-tick nil
        madrigal--context-size-cache nil
        madrigal--context-size-cache-key nil))

(defun madrigal--buffer-string ()
  "Return the full current buffer text without properties." 
  (buffer-substring-no-properties (point-min) (point-max)))

(defun madrigal--visible-context-string ()
  "Return the current Org buffer text with excluded subtrees removed." 
  (save-excursion
    (save-restriction
      (widen)
      (let ((text (madrigal--buffer-string)))
        (with-temp-buffer
          (insert text)
          (delay-mode-hooks (org-mode))
          (dolist (position (reverse (madrigal--top-level-heading-positions)))
            (goto-char position)
            (when (member madrigal-excluded-context-tag
                          (org-get-tags nil t))
              (delete-region position
                             (save-excursion
                               (goto-char position)
                               (org-end-of-subtree t t)
                               (point)))))
          (buffer-substring-no-properties (point-min) (point-max)))))))

(defun madrigal--session-context ()
  "Return the visible session buffer as Org text." 
  (madrigal--visible-context-string))

(defun madrigal--active-user-turn-bounds-assistant ()
  "Return bounds of the last user turn body in assistant mode." 
  (save-excursion
    (goto-char (point-max))
    (unless (re-search-backward "^\\*+ User\\(?:\\s-.*\\)?$" nil t)
      (user-error "No User heading found in Madrigal buffer"))
    (forward-line 1)
    (let ((start (point))
          (end (or (and (re-search-forward "^\\*+ " nil t)
                        (match-beginning 0))
                   (point-max))))
      (cons start end))))

(defun madrigal--active-request-bounds-babel ()
  "Return bounds of the last Request turn body in Babel mode." 
  (let ((position (madrigal--request-heading-position)))
    (madrigal--top-level-heading-body-bounds-at position)))

(defun madrigal--active-user-turn-bounds ()
  "Return bounds of the current editable user input area." 
  (if (madrigal--babel-agent-p)
      (madrigal--active-request-bounds-babel)
    (madrigal--active-user-turn-bounds-assistant)))

(defun madrigal--point-in-active-user-turn-p ()
  "Return non-nil when point is in the active user turn body." 
  (when-let ((bounds (madrigal--active-user-turn-bounds)))
    (<= (car bounds) (point) (cdr bounds))))

(defun madrigal--plain-submit-context-p ()
  "Return non-nil when point is in ordinary editable user-turn text." 
  (when (and (derived-mode-p 'org-mode)
             (madrigal--point-in-active-user-turn-p))
    (let* ((context (org-element-lineage
                     (org-element-context)
                     '(babel-call dynamic-block inline-babel-call inline-src-block
                                  item keyword paragraph plain-list src-block
                                  table table-cell table-row)
                     t))
           (type (org-element-type context)))
      (and (not (memq type '(src-block inline-src-block babel-call inline-babel-call
                                      table table-row table-cell keyword dynamic-block)))
           (not (org-at-table-p))))))

(defun madrigal-ctrl-c-ctrl-c ()
  "Submit in plain user-turn text when appropriate." 
  (when (and madrigal-mode
             (derived-mode-p 'org-mode)
             (ignore-errors (madrigal--plain-submit-context-p)))
    (madrigal-submit)
    t))

(defun madrigal--current-user-turn-assistant ()
  "Return the text of the active user turn in assistant mode." 
  (pcase-let ((`(,start . ,end) (madrigal--active-user-turn-bounds-assistant)))
    (string-trim-right (buffer-substring-no-properties start end))))

(defun madrigal--skip-initial-blank-lines ()
  "Move point past initial blank lines." 
  (skip-chars-forward " \t\n\r"))

(defun madrigal--quote-block-body-at-point ()
  "Return the quote block body at point, or nil." 
  (let ((element (org-element-at-point)))
    (when (eq (org-element-type element) 'quote-block)
      (buffer-substring-no-properties
       (org-element-property :contents-begin element)
       (org-element-property :contents-end element)))))

(defun madrigal--turn-components-babel (position)
  "Return plist describing the Babel turn at POSITION." 
  (save-excursion
    (goto-char position)
    (org-back-to-heading t)
    (let ((title (org-get-heading t t t t)))
      (forward-line 1)
      (let ((body-start (point))
            (body-end (save-excursion (org-end-of-subtree t t) (point)))
            prompt assistant-start prompt-end)
        (goto-char body-start)
        (madrigal--skip-initial-blank-lines)
        (when-let ((quote-body (madrigal--quote-block-body-at-point)))
          (setq prompt (string-trim-right quote-body)
                prompt-end (org-element-property :end (org-element-at-point)))
          (goto-char prompt-end)
          (madrigal--skip-initial-blank-lines)
          (setq assistant-start (point)))
        (unless prompt
          (setq prompt (string-trim-right
                        (buffer-substring-no-properties body-start body-end))))
        (let ((assistant
               (when assistant-start
                 (string-trim-right
                  (concat "* " title "\n"
                          (buffer-substring-no-properties assistant-start body-end))))))
          (list :title title
                :prompt prompt
                :assistant assistant
                :body (string-trim-right
                       (buffer-substring-no-properties body-start body-end))))))))

(defun madrigal--current-user-turn-babel ()
  "Return the text of the active Request turn in Babel mode." 
  (let* ((position (madrigal--request-heading-position))
         (components (madrigal--turn-components-babel position)))
    (or (plist-get components :prompt) "")))

(defun madrigal--current-user-turn ()
  "Return the text of the active user turn." 
  (if (madrigal--babel-agent-p)
      (madrigal--current-user-turn-babel)
    (madrigal--current-user-turn-assistant)))

(defun madrigal--top-level-heading-bounds ()
  "Return bounds of top-level headings in the current buffer." 
  (mapcar #'madrigal--top-level-heading-body-bounds-at
          (madrigal--top-level-heading-positions)))

(defun madrigal--heading-title-and-body (bounds)
  "Return a plist with title and body for heading BOUNDS." 
  (save-excursion
    (goto-char (car bounds))
    (org-back-to-heading t)
    (let ((title (org-get-heading t t t t)))
      (forward-line 1)
      (list :title title
            :body (string-trim-right
                   (buffer-substring-no-properties (point) (cdr bounds)))))))

(defun madrigal--session-history-turns-assistant ()
  "Return visible session history as alternating chat turns for assistant mode." 
  (let ((visible-context (madrigal--visible-context-string))
        turns)
    (with-temp-buffer
      (insert visible-context)
      (delay-mode-hooks (org-mode))
      (dolist (bounds (madrigal--top-level-heading-bounds))
        (let* ((entry (madrigal--heading-title-and-body bounds))
               (title (plist-get entry :title))
               (body (plist-get entry :body)))
          (unless (string-empty-p body)
            (pcase title
              ((or "AI" "Assistant")
               (push body turns))
              ("User"
               (push body turns))
              ("Context"
               (push body turns)
               (push "Summarize our session so far." turns))))))
      (setq turns (nreverse turns)))
    (if (and turns (eq (mod (length turns) 2) 1))
        turns
      (append turns (list (madrigal--current-user-turn-assistant))))))

(defun madrigal--session-history-turns-babel ()
  "Return visible session history as alternating chat turns for Babel mode." 
  (let ((visible-context (madrigal--visible-context-string))
        turns)
    (with-temp-buffer
      (insert visible-context)
      (delay-mode-hooks (org-mode))
      (dolist (position (madrigal--top-level-heading-positions))
        (let* ((components (madrigal--turn-components-babel position))
               (title (plist-get components :title))
               (prompt (plist-get components :prompt))
               (assistant (plist-get components :assistant))
               (body (plist-get components :body)))
          (pcase title
            ("Context"
             (unless (string-empty-p body)
               (push body turns)
               (push "Summarize our session so far." turns)))
            ("Request"
             (unless (string-empty-p prompt)
               (push prompt turns)))
            (_
             (unless (string-empty-p prompt)
               (push prompt turns))
             (when (and assistant (not (string-empty-p assistant)))
               (push assistant turns))))))
      (setq turns (nreverse turns)))
    turns))

(defun madrigal--session-history-turns ()
  "Return visible session history as alternating chat turns." 
  (let ((tick (buffer-chars-modified-tick)))
    (if (equal tick madrigal--history-cache-tick)
        (copy-tree madrigal--history-cache)
      (let ((history (if (madrigal--babel-agent-p)
                         (madrigal--session-history-turns-babel)
                       (madrigal--session-history-turns-assistant))))
        (setq madrigal--history-cache (copy-tree history)
              madrigal--history-cache-tick tick)
        history))))

(defun madrigal--org-string-demote-headings (string &optional levels)
  "Return STRING with Org headings demoted by LEVELS." 
  (with-temp-buffer
    (insert string)
    (delay-mode-hooks (org-mode))
    (dotimes (_ (or levels 1))
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (beginning-of-line)
        (org-do-demote)
        (forward-line 1)))
    (string-trim-right (buffer-string))))

(defun madrigal--history-debug-string (&optional level)
  "Return an Org rendering of the current chat history turns at LEVEL." 
  (let ((turns (madrigal--session-history-turns))
        (level (or level 1))
        (user-turn t)
        rendered)
    (dolist (turn turns)
      (push (format "%s %s\n%s"
                    (make-string level ?*)
                    (if user-turn "User" "AI")
                    (madrigal--org-string-demote-headings turn level))
            rendered)
      (setq user-turn (not user-turn)))
    (string-join (nreverse rendered) "\n\n")))

(defun madrigal--tool-descriptions ()
  "Return rendered tool descriptions for the active agent." 
  (let ((agent (madrigal--current-agent-name)))
    (mapcar (lambda (tool-name)
              (cons tool-name
                    (madrigal--tool-description tool-name)))
            (or (madrigal--agent-tool-names agent) '()))))

(defun madrigal--prompt-components ()
  "Return the current prompt components as strings." 
  (list (and madrigal-session
             (madrigal--agent-system-prompt
              (madrigal-session-agent madrigal-session)))
        (madrigal--history-debug-string 2)
        (madrigal--tool-descriptions)))

(defun madrigal-context-size (&optional provider)
  "Return prompt token usage information for PROVIDER." 
  (when madrigal-session
    (let ((key (list (buffer-chars-modified-tick)
                     (or provider (madrigal-session-provider madrigal-session))
                     (madrigal--current-agent-name))))
      (if (equal key madrigal--context-size-cache-key)
          madrigal--context-size-cache
        (setq madrigal--context-size-cache-key key
              madrigal--context-size-cache
              (condition-case nil
                  (madrigal-agent-controller-context-size
                   :agent (madrigal--current-agent-name)
                   :history (madrigal--session-history-turns)
                   :provider (or provider
                                 (madrigal-session-provider madrigal-session)))
                (error nil)))))))

(defun madrigal--human-number (n)
  "Format N as a compact human-readable number." 
  (cond
   ((< n 1000) (number-to-string n))
   ((< n 1000000) (format "%sk" (round (/ n 1000.0))))
   (t (format "%sM" (round (/ n 1000000.0))))))

(defun madrigal--percent-glyph (percent)
  "Return a compact glyph representing PERCENT." 
  (cond
   ((null percent) "◌")
   ((< percent 12.5) "○")
   ((< percent 25.0) "◔")
   ((< percent 50.0) "◑")
   ((< percent 75.0) "◕")
   (t "●")))

(defun madrigal--mode-line-icon ()
  "Return the Madrigal mode-line icon for the current buffer." 
  (if madrigal--pending-requests "⌛" "🧠"))

(defun madrigal--format-context-size (info)
  "Format context size INFO for the mode line." 
  (if (not info)
      (format "%s ?" (madrigal--mode-line-icon))
    (format "%s %s %s"
            (madrigal--mode-line-icon)
            (madrigal--human-number (plist-get info :tokens))
            (madrigal--percent-glyph (plist-get info :percent)))))

(defun madrigal-update-mode-line-status ()
  "Update the current buffer's Madrigal mode-line status." 
  (setq madrigal--mode-line-status
        (madrigal--format-context-size (madrigal-context-size)))
  (force-mode-line-update))

(defvar madrigal--suspend-context-updates nil
  "Non-nil suppresses context refreshes during a buffer rewrite.")

(defun madrigal--run-context-update (buffer)
  "Refresh context status for live Madrigal BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq madrigal--context-update-timer nil)
      (when (and madrigal-mode (not madrigal--suspend-context-updates))
        (madrigal-update-mode-line-status)
        (madrigal--maybe-auto-compact-context)))))

(defun madrigal--after-change (&rest _)
  "Schedule a context refresh after buffer changes." 
  (when (and madrigal-mode (not madrigal--suspend-context-updates))
    (madrigal--invalidate-context-caches)
    (when (timerp madrigal--context-update-timer)
      (cancel-timer madrigal--context-update-timer))
    (setq madrigal--context-update-timer
          (run-with-idle-timer madrigal-context-update-idle-delay nil
                               #'madrigal--run-context-update
                               (current-buffer)))))

(defun madrigal--metadata-end ()
  "Return the end position of initial file metadata." 
  (save-excursion
    (goto-char (point-min))
    (if (org-at-heading-p)
        (point)
      (if (ignore-errors (outline-next-heading) t)
          (point)
        (point-max)))))

(defun madrigal--body-string ()
  "Return the current session body, excluding file metadata." 
  (buffer-substring-no-properties (madrigal--metadata-end) (point-max)))

(defun madrigal--compactable-body-string ()
  "Return the non-excluded session body, excluding file metadata." 
  (let ((visible-context (madrigal--visible-context-string)))
    (with-temp-buffer
      (insert visible-context)
      (delay-mode-hooks (org-mode))
      (buffer-substring-no-properties (madrigal--metadata-end) (point-max)))))

(defun madrigal--context-subtree-bounds ()
  "Return bounds of the top-level Context subtree, or nil." 
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (dolist (position (madrigal--top-level-heading-positions))
        (when (equal (madrigal--top-level-heading-title-at position) "Context")
          (throw 'found
                 (cons position
                       (save-excursion
                         (goto-char position)
                         (org-end-of-subtree t t)
                         (point)))))))))

(defun madrigal-view-context ()
  "Show the current Madrigal prompt context in a temporary Org buffer." 
  (interactive)
  (madrigal--ensure-session)
  (let ((buffer (get-buffer-create "*Madrigal Context*"))
        (components (madrigal--prompt-components)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: Madrigal Context\n\n")
        (insert "* System Prompt\n"
                (or (nth 0 components) "")
                "\n\n")
        (insert "* Tools\n")
        (dolist (tool (or (nth 2 components) '()))
          (insert "** " (car tool) "\n"
                  (or (cdr tool) "")
                  "\n\n"))
        (insert "* Chat History\n"
                (or (nth 1 components) "")
                "\n")
        (goto-char (point-min))
        (setq-local buffer-read-only t)))
    (pop-to-buffer buffer)
    buffer))

(provide 'madrigal-conversation-context)

;;; madrigal-conversation-context.el ends here
