;;; madrigal-org-compact.el --- Context compaction for Madrigal  -*- lexical-binding: t; -*-

;;; Code:

(require 'madrigal-context)
(require 'madrigal-org)
(require 'subr-x)

(require 'llm nil t)

(defun madrigal--summary-source-string ()
  "Return the full non-excluded context to summarize." 
  (string-trim (madrigal--compactable-body-string)))

(defun madrigal--archive-body-string ()
  "Return the full non-excluded context to archive during compaction." 
  (madrigal--summary-source-string))

(defun madrigal--previous-contexts-subtree ()
  "Return the existing top-level Previous contexts subtree, or nil." 
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Previous contexts\\(?:\\s-.*\\)?$" nil t)
      (let ((start (line-beginning-position))
            (end (save-excursion (org-end-of-subtree t t))))
        (buffer-substring-no-properties start (min (point-max) (1+ end)))))))

(defun madrigal--next-previous-context-number ()
  "Return the next numbered previous-context heading index." 
  (save-excursion
    (goto-char (point-min))
    (let ((max-n 0))
      (while (re-search-forward "^\\*\\* Context \\([0-9]+\\)$" nil t)
        (setq max-n (max max-n (string-to-number (match-string 1)))))
      (1+ max-n))))

(defun madrigal--demote-org-headings (text)
  "Demote all Org headings in TEXT by two levels." 
  (replace-regexp-in-string "^\\*" "***" text nil nil nil 0))

(defun madrigal--input-heading-name ()
  "Return the trailing input heading name for the active agent." 
  (if (madrigal--babel-agent-p) "Request" "User"))

(defun madrigal--rewrite-compacted-buffer (archived-body placeholder)
  "Rewrite the current buffer with ARCHIVED-BODY and summary PLACEHOLDER." 
  (let* ((metadata (buffer-substring-no-properties (point-min) (madrigal--metadata-end)))
         (previous-contexts-marker
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward "^\\* Previous contexts\\(?:\\s-.*\\)?$" nil t)
              (line-beginning-position))))
         (previous-contexts (madrigal--previous-contexts-subtree))
         (previous-contexts-folded
          (and previous-contexts-marker
               (madrigal--subtree-folded-at-p previous-contexts-marker)))
         (n (madrigal--next-previous-context-number)))
    (let ((madrigal--suspend-context-updates t)
          (inhibit-modification-hooks t))
      (erase-buffer)
      (insert metadata)
      (unless (bolp)
        (insert "\n"))
      (let ((previous-contexts-start (point)))
        (if previous-contexts
            (insert (string-trim-right previous-contexts) "\n")
          (insert "* Previous contexts :exclude:\n"))
        (unless (bolp)
          (insert "\n"))
        (insert (format "** Context %d\n" n))
        (insert (madrigal--demote-org-headings (string-trim-right archived-body)))
        (insert "\n\n* Context\n")
        (insert placeholder)
        (insert "\n\n* " (madrigal--input-heading-name) "\n")
        (when (or (not previous-contexts)
                  previous-contexts-folded)
          (madrigal--fold-subtree-at previous-contexts-start))))
    (goto-char (point-max))))

(defun madrigal--compaction-summary-prompt ()
  "Return the context-summarization prompt for the current buffer." 
  (format
   "%s\nAim for the resulting prompt to use about %.0f%% of the model context window."
   madrigal-context-summary-prompt
   (* 100.0 madrigal-compact-context-target-proportion)))

(defun madrigal--build-compaction-prompt (summary-source)
  "Build a prompt that asks the model to summarize SUMMARY-SOURCE." 
  (llm-make-chat-prompt
   (concat "SESSION CONTEXT TO SUMMARIZE\n"
           "============================\n"
           "Summarize the Org session text below, not the instructions above.\n\n"
           summary-source)
   :context (madrigal--compaction-summary-prompt)))

(defun madrigal--maybe-auto-compact-context ()
  "Compact context automatically when usage exceeds the configured threshold." 
  (when (and madrigal-mode
             (not madrigal--compacting-context)
             madrigal-auto-compact-context-threshold
             madrigal-session)
    (when-let* ((info (madrigal-context-size))
                (percent (plist-get info :percent)))
      (when (>= percent (* 100.0 madrigal-auto-compact-context-threshold))
        (madrigal-compact-context)))))

(defun madrigal--normalize-context-summary (summary)
  "Normalize SUMMARY to the body of a Context heading." 
  (let ((text (string-trim summary)))
    (setq text
          (replace-regexp-in-string
           "\\`\\*+\\s-+\\(?:Context\\|Request\\|Response\\|User\\|AI\\|Assistant\\)\\b\\s-*[: -]*"
           ""
           text))
    (string-trim-left text)))

(defun madrigal--ensure-trailing-user-heading ()
  "Ensure the buffer ends with a blank input heading for the active agent." 
  (goto-char (point-max))
  (unless (madrigal--last-user-heading-empty-p)
    (unless (bolp)
      (insert "\n"))
    (insert "* " (madrigal--input-heading-name) "\n")))

(defun madrigal--replace-context-placeholder (summary)
  "Replace the current Context placeholder with SUMMARY." 
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Context$" nil t)
      (forward-line 1)
      (let ((start (point))
            (end (or (and (re-search-forward
                           (format "^\\* %s$" (regexp-quote (madrigal--input-heading-name)))
                           nil t)
                          (match-beginning 0))
                     (point-max))))
        (delete-region start end)
        (goto-char start)
        (insert (madrigal--normalize-context-summary summary) "\n\n"))))
  (save-excursion
    (madrigal--ensure-trailing-user-heading)))

(defun madrigal-compact-context ()
  "Archive the current session body and replace it with a model summary." 
  (interactive)
  (madrigal--ensure-session)
  (madrigal--ensure-llm)
  (when madrigal--compacting-context
    (user-error "Context compaction already in progress"))
  (let* ((buffer (current-buffer))
         (summary-source (madrigal--summary-source-string))
         (archived-body (madrigal--archive-body-string))
         (provider (madrigal-session-provider madrigal-session))
         (prompt (madrigal--build-compaction-prompt summary-source)))
    (when (string-empty-p archived-body)
      (user-error "No new context to compact"))
    (setq madrigal--compacting-context t)
    (madrigal--rewrite-compacted-buffer archived-body "Summarizing context...")
    (madrigal--record-request
     "compact-context"
     (if (madrigal-agent-controller-provider-use-streaming-p provider)
         (llm-chat-streaming
          provider
          prompt
          nil
          (lambda (response)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (setq madrigal--compacting-context nil)
                (madrigal--forget-request "compact-context")
                (madrigal--replace-context-placeholder
                 (or (madrigal-agent-controller--response-text response)
                     "Summary unavailable"))
                (goto-char (point-max))
                (madrigal-update-mode-line-status))))
          (lambda (err-type err-msg)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (setq madrigal--compacting-context nil)
                (madrigal--forget-request "compact-context")
                (madrigal--replace-context-placeholder
                 (format "Summary failed: %s %s" err-type err-msg))
                (goto-char (point-max))
                (madrigal-update-mode-line-status))))
          t)
       (llm-chat-async
        provider
        prompt
        (lambda (response)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (setq madrigal--compacting-context nil)
              (madrigal--forget-request "compact-context")
              (madrigal--replace-context-placeholder
               (or (madrigal-agent-controller--response-text response)
                   "Summary unavailable"))
              (goto-char (point-max))
              (madrigal-update-mode-line-status))))
        (lambda (err-type err-msg)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (setq madrigal--compacting-context nil)
              (madrigal--forget-request "compact-context")
              (madrigal--replace-context-placeholder
               (format "Summary failed: %s %s" err-type err-msg))
              (goto-char (point-max))
              (madrigal-update-mode-line-status))))
        t)))
    (madrigal-update-mode-line-status)
    (message "Compacting Madrigal context...")))

(provide 'madrigal-org-compact)

;;; madrigal-org-compact.el ends here
