;;; madrigal-tool-eval.el --- Eval tool support for Madrigal  -*- lexical-binding: t; -*-

;;; Code:

(require 'madrigal-core)
(require 'madrigal-context)
(require 'madrigal-org)
(require 'madrigal-tool-eval-prelude)
(require 'org)
(require 'package)
(require 'pp)
(require 'subr-x)

(require 'llm nil t)

(defun madrigal--read-elisp-forms (source)
  "Read all top-level Emacs Lisp forms from SOURCE." 
  (let ((forms nil)
        (position 0)
        (length (length source)))
    (while (< position length)
      (condition-case err
          (let* ((read-result (read-from-string source position))
                 (form (car read-result))
                 (next-position (cdr read-result)))
            (setq position next-position)
            (push form forms))
        (end-of-file
         (setq position length))
        (error
         (signal (car err) (cdr err)))))
    (nreverse forms)))

(defun madrigal--format-elisp-value (value)
  "Format VALUE as readable Emacs Lisp." 
  (string-trim-right (pp-to-string value)))

(defun madrigal--locate-elisp-syntax-error (source)
  "Return non-nil when SOURCE has a delimiter mismatch." 
  (with-temp-buffer
    (insert source)
    (goto-char (point-min))
    (condition-case _
        (progn
          (check-parens)
          nil)
      (error t))))

(defun madrigal--try-fix-end-of-file (source &optional max-closing-parens)
  "Return a repaired SOURCE for EOF read errors, or nil." 
  (let ((max-closing-parens (or max-closing-parens 8))
        (n 1)
        fixed)
    (while (and (not fixed) (<= n max-closing-parens))
      (let ((candidate (concat source (make-string n ?\)))))
        (when (null (madrigal--locate-elisp-syntax-error candidate))
          (setq fixed candidate)))
      (setq n (1+ n)))
    fixed))

(defconst madrigal--max-preserved-error-string-length 200
  "Maximum length of a string preserved verbatim in a returned error.")

(defun madrigal--summarize-cons (value)
  "Return a compact summary for cons VALUE." 
  (if (proper-list-p value)
      (let ((sample nil)
            (tail value)
            (count 0))
        (while (and tail (< count 3))
          (push (madrigal--summarize-error-arg (car tail)) sample)
          (setq tail (cdr tail))
          (setq count (1+ count)))
        `(:type list
          :length ,(length value)
          :sample ,(nreverse sample)))
    `(:type cons
      :car ,(madrigal--summarize-error-arg (car value))
      :cdr-type ,(type-of (cdr value)))))

(defun madrigal--summarize-error-arg (value)
  "Return a compact, serializable summary of error VALUE." 
  (cond
   ((or (null value) (eq value t)) value)
   ((numberp value) value)
   ((symbolp value) value)
   ((stringp value)
    (if (<= (length value) madrigal--max-preserved-error-string-length)
        value
      `(:type string :length ,(length value))))
   ((vectorp value) `(:type vector :length ,(length value)))
   ((hash-table-p value) `(:type hash-table :count ,(hash-table-count value)))
   ((bufferp value) `(:type buffer :name ,(buffer-name value)))
   ((markerp value) '(:type marker))
   ((consp value) (madrigal--summarize-cons value))
   (t `(:type ,(type-of value)))))

(defun madrigal--sanitize-elisp-error (err)
  "Return a compact Elisp-shaped representation of ERR." 
  (cons (car err)
        (mapcar #'madrigal--summarize-error-arg (cdr err))))

(defun madrigal--next-eval-error-variable ()
  "Return a fresh buffer-local variable symbol for an eval error." 
  (setq madrigal--eval-error-counter (1+ madrigal--eval-error-counter))
  (intern (format "madrigal-eval-error-%d" madrigal--eval-error-counter)))

(defun madrigal--bind-eval-error (err)
  "Bind raw ERR in the current buffer and return the variable symbol." 
  (let ((symbol (madrigal--next-eval-error-variable)))
    (set (make-local-variable symbol) err)
    symbol))

(defun madrigal--eval-elisp (source)
  "Evaluate Emacs Lisp SOURCE and return a structured result plist." 
  (let ((attempt-source source)
        (repaired nil)
        result)
    (while attempt-source
      (condition-case err
          (progn
            (let* ((current-source attempt-source)
                   (value nil)
                   (position 0)
                   (length (length current-source)))
              (while (and (< position length)
                          (string-match-p "\\S-" current-source position))
                (let* ((read-result (read-from-string current-source position))
                       (form (car read-result))
                       (next-position (cdr read-result)))
                  (setq value (eval form))
                  (setq position next-position)))
              (setq result
                    `(:ok t
                      :value ,value
                      :source-used ,current-source))
              (setq attempt-source nil)))
        (end-of-file
         (let ((fixed (and (not repaired)
                           (madrigal--try-fix-end-of-file attempt-source))))
           (if fixed
               (progn
                 (setq repaired t)
                 (setq attempt-source fixed))
             (signal (car err) (cdr err)))))
        (error
         (signal (car err) (cdr err)))))
    result))

(defun madrigal--persistent-elisp-file ()
  "Return the expanded path for Madrigal's persistent Lisp library."
  (expand-file-name madrigal-persistent-elisp-file))

(defun madrigal--persistent-elisp-form-key (form)
  "Return the replacement key for a persistent definition FORM."
  (pcase form
    (`(,(or 'defun 'defmacro 'cl-defmethod) ,name . ,_)
     (and (symbolp name) (cons 'function name)))
    (`(,(or 'defvar 'defconst 'defcustom) ,name . ,_)
     (and (symbolp name) (cons 'variable name)))
    (`(setq ,name ,_)
     (and (symbolp name) (cons 'variable name)))))

(defun madrigal--persistent-elisp-forms ()
  "Read the forms currently stored in the persistent Lisp library."
  (let ((file (madrigal--persistent-elisp-file)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (madrigal--read-elisp-forms (buffer-string)))
      nil)))

(defun madrigal--replace-persistent-elisp-forms (existing replacement)
  "Return EXISTING forms updated by REPLACEMENT forms."
  (let ((replacements (make-hash-table :test #'equal)))
    (dolist (form replacement)
      (when-let* ((key (madrigal--persistent-elisp-form-key form)))
        (puthash key form replacements)))
    (append
     (seq-remove
      (lambda (form)
        (and-let* ((key (madrigal--persistent-elisp-form-key form)))
          (gethash key replacements)))
      existing)
     replacement)))

(defun madrigal--write-persistent-elisp (forms)
  "Write FORMS to Madrigal's persistent Lisp library."
  (let ((file (madrigal--persistent-elisp-file)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert ";;; madrigal-persisted-elisp.el --- Agent-written Emacs Lisp  -*- lexical-binding: t; -*-\n\n")
      (dolist (form forms)
        (insert (pp-to-string form) "\n")))
    file))

(defun madrigal--persistent-elisp-variable-names (forms)
  "Return variables declared by FORMS."
  (delq nil
        (mapcar (lambda (form)
                  (when (eq (car-safe (madrigal--persistent-elisp-form-key form))
                            'variable)
                    (cdr (madrigal--persistent-elisp-form-key form))))
                forms)))

(defun madrigal--persist-elisp (source)
  "Evaluate and persist Emacs Lisp SOURCE as reusable Madrigal code."
  (when (madrigal--locate-elisp-syntax-error source)
    (user-error "Persistent Emacs Lisp source has unmatched delimiters"))
  (let ((replacement (madrigal--read-elisp-forms source)))
    (dolist (variable (madrigal--persistent-elisp-variable-names replacement))
      (when (boundp variable)
        (makunbound variable)))
    (let ((result (madrigal--eval-elisp source)))
      (append result
              (list :file
                    (madrigal--write-persistent-elisp
                     (madrigal--replace-persistent-elisp-forms
                      (madrigal--persistent-elisp-forms) replacement)))))))

(defun madrigal--load-persistent-elisp ()
  "Load the reusable Emacs Lisp written by Madrigal agents."
  (let ((file (madrigal--persistent-elisp-file)))
    (when (file-exists-p file)
      (load file nil nil t))))

(defun madrigal--persisted-elisp-definition (name)
  "Return the persisted definition named NAME, or signal an error."
  (let ((form (seq-find (lambda (item)
                          (equal name
                                 (cdr (madrigal--persistent-elisp-form-key item))))
                        (madrigal--persistent-elisp-forms))))
    (or form (user-error "No persisted Emacs Lisp definition named %S" name))))

(defun madrigal-persisted-elisp-info (&optional offset limit)
  "Return a bounded inventory of persisted Emacs Lisp definitions."
  (let ((definitions nil))
    (dolist (form (madrigal--persistent-elisp-forms))
      (when-let* ((key (madrigal--persistent-elisp-form-key form)))
        (push (list :name (cdr key) :type (car key)) definitions)))
    (list :file (madrigal--persistent-elisp-file)
          :definitions
          (madrigal--paged-items
           (sort definitions
                 (lambda (left right)
                   (string< (symbol-name (plist-get left :name))
                            (symbol-name (plist-get right :name)))))
           offset limit))))

(defun madrigal-persisted-elisp-source (name &optional limit)
  "Return bounded persisted source for definition NAME."
  (let* ((form (madrigal--persisted-elisp-definition name))
         (source (string-trim-right (pp-to-string form)))
         (limit (max 1 (min 10000 (or limit 2000))))
         (length (length source)))
    (list :name name
          :type (car (madrigal--persistent-elisp-form-key form))
          :source (substring source 0 (min length limit))
          :truncated (> length limit)
          :length length)))

(defun madrigal-persisted-elisp-help (name)
  "Return documentation and source metadata for persisted definition NAME."
  (let* ((form (madrigal--persisted-elisp-definition name))
         (type (car (madrigal--persistent-elisp-form-key form))))
    (list :name name
          :type type
          :documentation
          (pcase type
            ('function (documentation name t))
            ('variable (documentation-property name 'variable-documentation)))
          :source (madrigal-persisted-elisp-source name))))

(defun madrigal--format-eval-result (result)
  "Format RESULT for insertion into an Emacs Lisp src block." 
  (let ((filtered nil)
        (plist result))
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (unless (eq key :source-used)
          (setq filtered (append filtered (list key value))))))
    (madrigal--format-elisp-value filtered)))

(defun madrigal--installed-package-names ()
  "Return installed Emacs package names as strings." 
  (sort
   (delete-dups
    (mapcar (lambda (pkg)
              (symbol-name (car pkg)))
            package-alist))
   #'string<))

(defun madrigal--installed-package-names-string ()
  "Return installed Emacs package names as a comma-separated string." 
  (string-join (madrigal--installed-package-names) ", "))

(defun madrigal--emacs-runtime-description (&optional level)
  "Return an Org snippet describing the current Emacs runtime." 
  (let ((heading (make-string (or level 3) ?*)))
    (concat
     heading " Emacs Version\n"
     emacs-version
     "\n"
     heading " Installed Packages\n"
     (madrigal--installed-package-names-string)
     "\n")))

(defun madrigal--eval-prelude-description (&optional levels)
  "Return Org help text for the eval prelude helpers." 
  (madrigal--org-string-demote-headings
   (madrigal--feature-help-org 'madrigal-tool-eval-prelude)
   (or levels 2)))

(defconst madrigal--persist-elisp-tool-guidance
  "- Persist reusable Emacs Lisp in Madrigal's user library and make it available immediately.
- The source is stored in one file under the Emacs configuration directory and loaded whenever Madrigal loads.
- Persisting a function or variable definition replaces its earlier definition in that file.
- Before adding reusable code, inspect =madrigal-persisted-elisp-info= and use =madrigal-persisted-elisp-help= or =madrigal-persisted-elisp-source= to revise or reuse existing definitions.
- Prefer definitions that can be reused for future requests; use the eval tool to call them.
- Supply complete, valid Emacs Lisp source only.\n"
  "Static guidance text for the persistent Elisp tool description.")

(defun madrigal--persist-elisp-tool-description ()
  "Return the description for the persistent Elisp tool."
  madrigal--persist-elisp-tool-guidance)

(defconst madrigal--eval-tool-guidance
  "- Evaluate Emacs Lisp in the current Madrigal buffer.
- Use it to inspect or change buffers, files, project state, variables, packages, and live Emacs data.
- Inspect first using reflection functions when useful; do not guess.
- Start with =madrigal-runtime-info=, =madrigal-project-info=, =madrigal-package-search=, =madrigal-feature-search=, =madrigal-symbol-search=, =madrigal-function-help=, =madrigal-variable-help=, =madrigal-key-binding-help=, or =madrigal-mode-help=.
- Before writing reusable code, inspect =madrigal-persisted-elisp-info=; use =madrigal-persisted-elisp-help= or =madrigal-persisted-elisp-source= to reuse or revise a definition.
- Use =madrigal-context-buffer-text= to retrieve omitted origin-buffer text.
- During =madrigal-do= actions, inspect the request with =madrigal-do-context=, =madrigal-do-turn-history=, =madrigal-do-tool-history=, and =madrigal-do-tool-result-history=.
- Use =madrigal-do-expand-context= with an enclosing candidate id from the selected scope metadata for an explicit read-only expansion.
- Do not add defensive error checks or custom error messages unless you are debugging a previous failure; let errors fail visibly rather than hiding them with =ignore-errors=.
- Keep returned values tiny, since they become model context.
- Use =madrigal-session-state-get= and =madrigal-session-state-put= to persist values across eval calls.
- Inspect the returned error variable in a follow-up call when you need the raw error object.
- Prefer one eval call when practical.
- Use =save-excursion=, =save-window-excursion=, and temporary buffers to avoid disrupting the user's session unless the task requires persistent changes.
"
  "Static guidance text for the eval tool description.")

(defun madrigal--eval-tool-description ()
  "Return the current description string for the eval tool." 
  madrigal--eval-tool-guidance)

(defun madrigal--result-for-callback (result &optional error-variable)
  "Return a tool result derived from RESULT and ERROR-VARIABLE." 
  (let ((value (plist-get result :value)))
    `(:ok ,(plist-get result :ok)
      :value ,value
      ,@(when (plist-get result :error)
          `(:error ,(plist-get result :error)))
      ,@(when error-variable
          `(:error-variable ,error-variable)))))

(defun madrigal--run-elisp-tool (evaluator tool-name buffer request-id callback source
                                           &optional event-sink)
  "Run EVALUATOR for TOOL-NAME in BUFFER and pass its result to CALLBACK."
  (let ((event-id (and event-sink (madrigal--next-request-id)))
        callback-result entry-source formatted-result)
    (when event-sink
      (funcall event-sink
               (list :type 'tool :phase 'started :id event-id
                     :request-id request-id :name tool-name
                     :language "emacs-lisp" :source source)))
    (with-current-buffer buffer
      (let* ((madrigal--eval-origin-buffer buffer)
             (result
              (condition-case err
                  (funcall evaluator source)
                (error
                 (let ((error-variable (madrigal--bind-eval-error err)))
                   `(:ok nil
                     :error ,(madrigal--sanitize-elisp-error err)
                     :error-variable ,error-variable))))))
        (setq callback-result (madrigal--result-for-callback
                               result (plist-get result :error-variable)))
        (setq entry-source (or (plist-get result :source-used) source))
        (setq formatted-result (madrigal--format-eval-result callback-result))))
    (if event-sink
        (funcall event-sink
                 (list :type 'tool :phase 'finished :id event-id
                       :request-id request-id :name tool-name
                       :language "emacs-lisp" :source entry-source
                       :result callback-result
                       :formatted-result formatted-result))
      (with-current-buffer buffer
        (save-excursion
          (if (madrigal--babel-agent-p)
              (madrigal--append-assistant-text
               request-id
               (madrigal--insert-tool-entry request-id tool-name "emacs-lisp" entry-source formatted-result))
            (madrigal--insert-tool-entry request-id tool-name "emacs-lisp" entry-source formatted-result)))))
    (funcall callback formatted-result)))

(defun madrigal--run-eval-tool (tool-name buffer request-id callback source &optional event-sink)
  "Run the transient eval tool in BUFFER for REQUEST-ID."
  (madrigal--run-elisp-tool #'madrigal--eval-elisp tool-name buffer request-id
                            callback source event-sink))

(defun madrigal--run-persist-elisp-tool (tool-name buffer request-id callback source
                                                   &optional event-sink)
  "Run the persistent Elisp tool in BUFFER for REQUEST-ID."
  (madrigal--run-elisp-tool #'madrigal--persist-elisp tool-name buffer request-id
                            callback source event-sink))

(defun madrigal--call-with-action-context (context function)
  "Call FUNCTION in CONTEXT's captured buffer and optional location."
  (let* ((context (madrigal-context-normalize context))
         (origin (plist-get context :origin))
         (buffer (plist-get origin :buffer))
         (buffer-context (plist-get origin :buffer-context))
         (point-marker (plist-get buffer-context :point))
         (mark-position (plist-get buffer-context :mark))
         (region (plist-get buffer-context :region))
         (window (plist-get origin :window))
         (invoke
          (lambda ()
            (unless (buffer-live-p buffer)
              (error "The captured Madrigal origin buffer no longer exists"))
            (with-current-buffer buffer
              (save-mark-and-excursion
                (save-restriction
                  (unless point-marker
                    (widen))
                  (when (and (markerp point-marker)
                             (marker-position point-marker))
                    (goto-char point-marker))
                  (when mark-position
                    (set-mark mark-position))
                  (setq mark-active (not (null region)))
                  (funcall function)))))))
    (if (window-live-p window)
        (let ((display-buffer (window-buffer window))
              (display-point (window-point window))
              (display-start (window-start window)))
          (unwind-protect
              (save-selected-window
                (set-window-buffer window buffer)
                (with-selected-window window
                  (funcall invoke)))
            (when (and (window-live-p window)
                       (eq (window-buffer window) buffer))
              (set-window-buffer window display-buffer)
              (set-window-point window display-point)
              (set-window-start window display-start t))))
      (funcall invoke))))

(defun madrigal--make-tool (tool-name tool-definition buffer request-id
                                      &optional event-sink action-context request-context)
  "Return an llm tool named TOOL-NAME from TOOL-DEFINITION." 
  (let ((runner (plist-get tool-definition :function)))
    (llm-make-tool
     :function (lambda (callback &rest args)
                 (let ((madrigal--eval-request-context request-context)
                       (invoke
                        (lambda ()
                          (apply runner tool-name buffer request-id callback
                                 (if event-sink
                                     (append args (list event-sink))
                                   args)))))
                   (if action-context
                       (madrigal--call-with-action-context action-context invoke)
                     (funcall invoke))))
     :name tool-name
     :description (with-current-buffer buffer
                    (madrigal--tool-description tool-name))
     :args (plist-get tool-definition :args)
     :async (plist-get tool-definition :async))))

(defun madrigal--make-eval-tool (buffer request-id
                                        &optional event-sink action-context request-context)
  "Return the async eval tool for BUFFER and REQUEST-ID." 
  (madrigal--make-tool
   "eval"
   (madrigal--tool-definition "eval")
   buffer
   request-id
   event-sink
   action-context
   request-context))

(provide 'madrigal-tool-eval)

;;; madrigal-tool-eval.el ends here
