;;; madrigal-org.el --- Org session helpers for Madrigal  -*- lexical-binding: t; -*-

;;; Code:

(require 'madrigal-core)
(require 'org)
(require 'ob-core)
(require 'pp)
(require 'subr-x)

(defvar-local madrigal--request-final-titles nil
  "Alist mapping request ids to finalized turn titles.")

(defun madrigal--current-agent-name ()
  "Return the active Madrigal agent name for the current buffer." 
  (or (and madrigal-session (madrigal-session-agent madrigal-session))
      (madrigal--buffer-agent-name)
      "assistant"))

(defun madrigal--babel-agent-p (&optional agent-name)
  "Return non-nil when AGENT-NAME uses Babel-first Org rendering." 
  (string= (or agent-name (madrigal--current-agent-name))
           "babel-assistant"))

(defun madrigal--insert-src-block (language body)
  "Insert an Org src block in LANGUAGE containing BODY." 
  (insert (format "#+begin_src %s\n%s\n#+end_src\n"
                  language
                  (string-trim-right body))))

(defun madrigal--request-turn-marker (request-id)
  "Return the stored turn marker for REQUEST-ID, or nil." 
  (alist-get request-id madrigal--request-turn-markers nil nil #'string=))

(defun madrigal--store-request-turn-marker (request-id marker)
  "Store MARKER as the turn marker for REQUEST-ID." 
  (setf (alist-get request-id madrigal--request-turn-markers nil nil #'string=)
        marker))

(defun madrigal--forget-request-turn-marker (request-id)
  "Forget the stored turn marker for REQUEST-ID." 
  (setq madrigal--request-turn-markers
        (assoc-delete-all request-id madrigal--request-turn-markers #'string=))
  (setq madrigal--request-final-titles
        (assoc-delete-all request-id madrigal--request-final-titles #'string=)))

(defun madrigal--store-request-final-title (request-id title)
  "Store final TITLE for REQUEST-ID." 
  (setf (alist-get request-id madrigal--request-final-titles nil nil #'string=)
        title))

(defun madrigal--request-final-title (request-id)
  "Return the stored final title for REQUEST-ID, or nil." 
  (alist-get request-id madrigal--request-final-titles nil nil #'string=))

(defun madrigal--fold-subtree-at (position)
  "Fold the Org subtree whose heading starts at POSITION." 
  (save-excursion
    (goto-char position)
    (org-back-to-heading t)
    (org-fold-subtree t)))

(defun madrigal--subtree-folded-at-p (position)
  "Return non-nil when the Org subtree at POSITION is folded." 
  (save-excursion
    (goto-char position)
    (org-back-to-heading t)
    (forward-line 1)
    (and (< (point) (point-max))
         (invisible-p (point)))))

(defun madrigal--show-subtree-at (position)
  "Show the Org subtree whose heading starts at POSITION." 
  (save-excursion
    (goto-char position)
    (org-back-to-heading t)
    (org-fold-show-subtree)))

(defun madrigal--top-level-heading-positions ()
  "Return positions of all top-level headings in the current buffer." 
  (save-excursion
    (goto-char (point-min))
    (let (positions)
      (unless (org-at-heading-p)
        (ignore-errors (outline-next-heading)))
      (while (org-at-heading-p)
        (when (= (org-outline-level) 1)
          (push (point) positions))
        (condition-case nil
            (outline-next-heading)
          (error (goto-char (point-max)))))
      (nreverse positions))))

(defun madrigal--last-top-level-heading-position ()
  "Return the position of the last top-level heading, or nil." 
  (car (last (madrigal--top-level-heading-positions))))

(defun madrigal--top-level-heading-title-at (position)
  "Return the title of the top-level heading at POSITION." 
  (save-excursion
    (goto-char position)
    (org-back-to-heading t)
    (org-get-heading t t t t)))

(defun madrigal--top-level-heading-body-bounds-at (position)
  "Return body bounds for the top-level heading at POSITION." 
  (save-excursion
    (goto-char position)
    (org-back-to-heading t)
    (forward-line 1)
    (cons (point)
          (save-excursion
            (org-end-of-subtree t t)
            (point)))))

(defun madrigal--nest-org-headings (text)
  "Nest Org headings in TEXT under the current top-level entry." 
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (org-mode))
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (beginning-of-line)
      (org-do-demote)
      (forward-line 1))
    (string-trim-right (buffer-string))))

(defun madrigal--nest-org-headings-under-section (text)
  "Nest Org headings in TEXT under a second-level section heading." 
  (replace-regexp-in-string "^\\*" "***" text nil nil nil 0))

(defun madrigal--finalize-tables-in-region (start end)
  "Run Org table finalization across tables between START and END." 
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (if (org-at-table-p)
          (let ((table-start (point)))
            (org-table-align)
            (goto-char table-start)
            (while (and (< (point) end)
                        (org-at-table-p))
              (forward-line 1)))
        (forward-line 1)))))

(defun madrigal--align-tables-in-region (start end)
  "Finalize Org tables between START and END." 
  (madrigal--finalize-tables-in-region start end))

(defun madrigal--schedule-table-finalization (buffer start end)
  "Finalize Org tables in BUFFER between markers START and END." 
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (marker-buffer start)
                 (marker-buffer end)
                 (<= (marker-position start)
                     (marker-position end)))
        (save-excursion
          (madrigal--align-tables-in-region
           (marker-position start)
           (marker-position end)))))))

(defun madrigal--last-user-heading-empty-p-assistant ()
  "Return non-nil when the last heading is an empty User turn." 
  (when-let ((position (madrigal--last-top-level-heading-position)))
    (save-excursion
      (goto-char position)
      (and (equal (org-get-heading t t t t) "User")
           (pcase-let ((`(,start . ,end)
                         (madrigal--top-level-heading-body-bounds-at position)))
             (string-empty-p
              (string-trim
               (buffer-substring-no-properties start end))))))))

(defun madrigal--ensure-open-user-turn-assistant ()
  "Ensure the buffer ends with a blank top-level User heading." 
  (goto-char (point-max))
  (unless (madrigal--last-user-heading-empty-p-assistant)
    (unless (bolp)
      (insert "\n"))
    (insert "* User\n"))
  (goto-char (point-max))
  (point))

(defun madrigal--ensure-request-ai-turn (request-id)
  "Ensure there is an open top-level AI turn for REQUEST-ID." 
  (let ((existing (madrigal--request-turn-marker request-id)))
    (if (and existing (marker-buffer existing))
        existing
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert "* AI\n")
      (let ((marker (copy-marker (point) t)))
        (madrigal--store-request-turn-marker request-id marker)
        marker))))

(defun madrigal--request-ai-heading-position (request-id)
  "Return the top-level AI heading position for REQUEST-ID." 
  (let ((marker (madrigal--ensure-request-ai-turn request-id)))
    (save-excursion
      (goto-char (marker-position marker))
      (or (and (re-search-backward "^\\* AI$" nil t)
               (line-beginning-position))
          (user-error "Could not find AI heading for request %s" request-id)))))

(defun madrigal--ensure-request-tools-section (request-id)
  "Ensure there is a Tools section for REQUEST-ID." 
  (let* ((ai-heading (madrigal--request-ai-heading-position request-id))
         (ai-end (save-excursion
                   (goto-char ai-heading)
                   (forward-line 1)
                   (or (and (re-search-forward "^\\* " nil t)
                            (match-beginning 0))
                       (point-max)))))
    (save-excursion
      (goto-char ai-heading)
      (if (re-search-forward "^\\*\\* Tools$" ai-end t)
          (line-beginning-position)
        (goto-char ai-end)
        (unless (bolp)
          (insert "\n"))
        (let ((tools-heading (point)))
          (insert "** Tools\n")
          (when madrigal-collapse-tool-entries
            (madrigal--fold-subtree-at tools-heading))
          tools-heading)))))

(defun madrigal--insert-tool-entry-assistant (request-id tool-name language source result)
  "Insert a tool entry for REQUEST-ID with TOOL-NAME, LANGUAGE, SOURCE and RESULT." 
  (let* ((tools-heading (madrigal--ensure-request-tools-section request-id))
         (tools-folded (madrigal--subtree-folded-at-p tools-heading)))
    (when tools-folded
      (madrigal--show-subtree-at tools-heading))
    (save-excursion
      (goto-char tools-heading)
      (org-end-of-subtree t t)
      (unless (bolp)
        (insert "\n"))
      (let ((entry-heading (point)))
        (insert (format "*** %s\n" tool-name))
        (madrigal--insert-src-block language source)
        (insert "#+RESULTS:\n")
        (madrigal--insert-src-block language result)
        (insert "\n")
        (when madrigal-collapse-tool-entries
          (madrigal--fold-subtree-at entry-heading))))
    (when tools-folded
      (madrigal--fold-subtree-at tools-heading))))

(defun madrigal--append-assistant-text-assistant (request-id response &optional final)
  "Append RESPONSE text to the open AI turn for REQUEST-ID." 
  (let ((text (string-trim (or response ""))))
    (unless (string-empty-p text)
      (goto-char (marker-position (madrigal--ensure-request-ai-turn request-id)))
      (unless (bolp)
        (insert "\n"))
      (insert (if final "** Response\n" "** Note\n"))
      (let ((start (copy-marker (point)))
            end)
        (set-marker-insertion-type start nil)
        (insert (madrigal--nest-org-headings-under-section text))
        (unless (bolp)
          (insert "\n"))
        (setq end (copy-marker (point) t))
        (madrigal--schedule-table-finalization (current-buffer) start end)))))

(defun madrigal--finish-assistant-turn-assistant (request-id)
  "Finalize the AI turn for REQUEST-ID and reopen the User turn." 
  (madrigal--forget-request-turn-marker request-id)
  (goto-char (point-max))
  (madrigal--ensure-open-user-turn-assistant)
  (goto-char (point-max)))

(defun madrigal--insert-error-response-assistant (_request-id err-type err-msg)
  "Insert an Org error entry for assistant mode." 
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n"))
  (insert "* Error\n")
  (madrigal--insert-src-block
   "emacs-lisp"
   (string-trim-right (pp-to-string (list err-type err-msg))))
  (madrigal--ensure-open-user-turn-assistant)
  (goto-char (point-max)))

(defun madrigal--request-heading-position ()
  "Return the active top-level Request heading position." 
  (let ((position (madrigal--last-top-level-heading-position)))
    (unless position
      (user-error "No Request heading found in Madrigal buffer"))
    (unless (member (madrigal--top-level-heading-title-at position)
                    '("Request" "User"))
      (user-error "Last top-level heading is not Request"))
    position))

(defun madrigal--last-request-heading-empty-p ()
  "Return non-nil when the last heading is an empty Request turn." 
  (when-let ((position (madrigal--last-top-level-heading-position)))
    (and (member (madrigal--top-level-heading-title-at position)
                 '("Request" "User"))
         (pcase-let ((`(,start . ,end)
                       (madrigal--top-level-heading-body-bounds-at position)))
           (string-empty-p
            (string-trim (buffer-substring-no-properties start end)))))))

(defun madrigal--ensure-open-request-turn ()
  "Ensure the buffer ends with a blank top-level Request heading." 
  (goto-char (point-max))
  (unless (madrigal--last-request-heading-empty-p)
    (unless (bolp)
      (insert "\n"))
    (insert "* Request\n"))
  (goto-char (point-max))
  (point))

(defun madrigal--quote-block-string (text)
  "Return TEXT wrapped in an Org quote block." 
  (format "#+begin_quote\n%s\n#+end_quote\n"
          (string-trim-right text)))

(defun madrigal--prepare-request-turn (request-id)
  "Validate and mark the active Request turn for REQUEST-ID." 
  (when (madrigal--babel-agent-p)
    (let ((position (madrigal--request-heading-position)))
      (pcase-let ((`(,start . ,end)
                    (madrigal--top-level-heading-body-bounds-at position)))
        (let ((prompt (string-trim-right
                       (buffer-substring-no-properties start end))))
          (when (string-empty-p (string-trim prompt))
            (user-error "Request is empty"))
          (let ((marker (copy-marker position t)))
            (madrigal--store-request-turn-marker request-id marker)
            marker))))))

(defun madrigal--materialize-request-turn (request-id)
  "Rewrite the raw Request prompt for REQUEST-ID into quoted turn form." 
  (let ((marker (madrigal--ensure-request-turn-marker request-id)))
    (save-excursion
      (goto-char (marker-position marker))
      (org-back-to-heading t)
      (pcase-let ((`(,start . ,end)
                    (madrigal--top-level-heading-body-bounds-at (point))))
        (goto-char start)
        (madrigal--skip-initial-blank-lines)
        (unless (eq (org-element-type (org-element-at-point)) 'quote-block)
          (let ((prompt (string-trim-right
                         (buffer-substring-no-properties start end))))
            (delete-region start end)
            (goto-char start)
            (insert (madrigal--quote-block-string prompt))
            (unless (bolp)
              (insert "\n"))))))))

(defun madrigal--ensure-request-turn-marker (request-id)
  "Return an active Request marker for REQUEST-ID, creating one if needed." 
  (or (let ((marker (madrigal--request-turn-marker request-id)))
        (and marker (marker-buffer marker) marker))
      (let* ((position (or (ignore-errors (madrigal--request-heading-position))
                           (progn
                             (madrigal--ensure-open-request-turn)
                             (madrigal--request-heading-position))))
             (marker (copy-marker position t)))
        (madrigal--store-request-turn-marker request-id marker)
        marker)))

(defun madrigal--request-subtree-end (request-id)
  "Return the insertion point at the end of REQUEST-ID's subtree." 
  (let ((marker (madrigal--ensure-request-turn-marker request-id)))
    (save-excursion
      (goto-char (marker-position marker))
      (org-back-to-heading t)
      (org-end-of-subtree t t)
      (point))))

(defun madrigal--insert-into-request (request-id text)
  "Append TEXT to the subtree for REQUEST-ID and return inserted bounds." 
  (madrigal--materialize-request-turn request-id)
  (let ((start nil)
        (end nil))
    (save-excursion
      (goto-char (madrigal--request-subtree-end request-id))
      (unless (bolp)
        (insert "\n"))
      (unless (or (bobp)
                  (save-excursion
                    (forward-line -1)
                    (looking-at-p "^[[:space:]]*$")))
        (insert "\n"))
      (setq start (copy-marker (point)))
      (set-marker-insertion-type start nil)
      (insert (string-trim-right text))
      (unless (bolp)
        (insert "\n"))
      (setq end (copy-marker (point) t)))
    (cons start end)))

(defun madrigal--parse-final-response (response)
  "Return plist describing final RESPONSE." 
  (with-temp-buffer
    (insert (string-trim-left (or response "")))
    (delay-mode-hooks (org-mode))
    (goto-char (point-min))
    (if (and (org-at-heading-p)
             (= (org-outline-level) 1))
        (let ((title (org-get-heading t t t t)))
          (forward-line 1)
          (let ((body-start (point))
                (body-end (save-excursion
                            (org-end-of-subtree t t)
                            (point))))
            (list :title title
                  :body (string-trim-right
                         (buffer-substring-no-properties body-start body-end)))))
      (list :title nil
            :body (string-trim-right (buffer-string))))))

(defun madrigal--append-assistant-text-babel (request-id response &optional final)
  "Append RESPONSE text to the Request turn for REQUEST-ID." 
  (let* ((parsed (if final
                     (madrigal--parse-final-response response)
                   (list :title nil
                         :body (madrigal--nest-org-headings response))))
         (title (plist-get parsed :title))
         (body (string-trim (or (plist-get parsed :body) ""))))
    (when title
      (madrigal--store-request-final-title request-id title))
    (unless (string-empty-p body)
      (pcase-let ((`(,start . ,end)
                    (madrigal--insert-into-request request-id body)))
        (madrigal--schedule-table-finalization (current-buffer) start end)))))

(defun madrigal--finish-assistant-turn-babel (request-id)
  "Finalize the Request turn for REQUEST-ID and reopen the trailing Request slot." 
  (let ((marker (madrigal--request-turn-marker request-id))
        (title (madrigal--request-final-title request-id)))
    (when (and marker (marker-buffer marker))
      (save-excursion
        (goto-char (marker-position marker))
        (org-back-to-heading t)
        (org-edit-headline (or title "Response")))))
  (madrigal--forget-request-turn-marker request-id)
  (goto-char (point-max))
  (madrigal--ensure-open-request-turn)
  (goto-char (point-max)))

(defun madrigal--tool-fragment-babel (_request-id _tool-name language source result)
  "Return an Org fragment for LANGUAGE SOURCE and RESULT." 
  (concat
   (format "#+begin_src %s\n%s\n#+end_src\n\n#+RESULTS:\n#+begin_src %s\n%s\n#+end_src\n"
           language
           (string-trim-right source)
           language
           (string-trim-right result))))

(defun madrigal--insert-error-response-babel (request-id err-type err-msg)
  "Insert an error response into REQUEST-ID and reopen the Request slot." 
  (let ((message-text
         (string-trim-right (pp-to-string (list err-type err-msg)))))
    (madrigal--store-request-final-title request-id "Error")
    (madrigal--append-assistant-text-babel
     request-id
     (format "#+begin_src emacs-lisp\n%s\n#+end_src"
             message-text))
    (madrigal--finish-assistant-turn-babel request-id)))

(defun madrigal--last-user-heading-empty-p ()
  "Return non-nil when the active agent has an empty input heading." 
  (if (madrigal--babel-agent-p)
      (madrigal--last-request-heading-empty-p)
    (madrigal--last-user-heading-empty-p-assistant)))

(defun madrigal--ensure-open-user-turn ()
  "Ensure the buffer ends with the active agent's input heading." 
  (if (madrigal--babel-agent-p)
      (madrigal--ensure-open-request-turn)
    (madrigal--ensure-open-user-turn-assistant)))

(defun madrigal--append-assistant-text (request-id response &optional final)
  "Append RESPONSE using the current agent's Org rendering mode." 
  (if (madrigal--babel-agent-p)
      (madrigal--append-assistant-text-babel request-id response final)
    (madrigal--append-assistant-text-assistant request-id response final)))

(defun madrigal--finish-assistant-turn (request-id)
  "Finalize REQUEST-ID using the current agent's Org rendering mode." 
  (if (madrigal--babel-agent-p)
      (madrigal--finish-assistant-turn-babel request-id)
    (madrigal--finish-assistant-turn-assistant request-id)))

(defun madrigal--insert-tool-entry (request-id tool-name language source result)
  "Render a tool entry for REQUEST-ID in the current agent mode." 
  (if (madrigal--babel-agent-p)
      (madrigal--tool-fragment-babel request-id tool-name language source result)
    (madrigal--insert-tool-entry-assistant request-id tool-name language source result)))

(defun madrigal--insert-error-response (request-id err-type err-msg)
  "Insert an error response for REQUEST-ID in the current agent mode." 
  (if (madrigal--babel-agent-p)
      (madrigal--insert-error-response-babel request-id err-type err-msg)
    (madrigal--insert-error-response-assistant request-id err-type err-msg)))

(provide 'madrigal-org)

;;; madrigal-org.el ends here
