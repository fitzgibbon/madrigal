;;; madrigal-tool-eval-prelude.el --- Eval prelude for Madrigal  -*- lexical-binding: t; -*-

;;; Code:

(require 'madrigal-core)
(require 'subr-x)

(defvar madrigal--eval-origin-buffer nil
  "Dynamic origin Madrigal buffer for the current eval tool call.")

(defvar madrigal--eval-request-context nil
  "Dynamic request context for the current eval tool call.")

(defun madrigal--internal-symbol-p (symbol)
  "Return non-nil when SYMBOL names an internal Madrigal binding." 
  (string-match-p "--" (symbol-name symbol)))

(defun madrigal--public-symbol-p (symbol)
  "Return non-nil when SYMBOL should be exposed in Madrigal help." 
  (not (madrigal--internal-symbol-p symbol)))

(defun madrigal--function-symbol (function)
  "Return FUNCTION as a public named function symbol or signal an error." 
  (if (and (symbolp function)
           (fboundp function)
           (madrigal--public-symbol-p function))
      function
    (user-error "Expected a public named function symbol, got %S" function)))

(defun madrigal--variable-symbol (variable)
  "Return VARIABLE as a public named bound symbol or signal an error." 
  (if (and (symbolp variable)
           (boundp variable)
           (madrigal--public-symbol-p variable))
      variable
    (user-error "Expected a public named bound symbol, got %S" variable)))

(defun madrigal-session-state-get (key)
  "Return persisted session state for KEY, or nil when missing." 
  (unless (buffer-live-p madrigal--eval-origin-buffer)
    (error "No active Madrigal eval origin buffer"))
  (with-current-buffer madrigal--eval-origin-buffer
    (when (hash-table-p madrigal--state)
      (gethash key madrigal--state))))

(defun madrigal-session-state-put (key value)
  "Persist VALUE under KEY in the current Madrigal session state." 
  (unless (buffer-live-p madrigal--eval-origin-buffer)
    (error "No active Madrigal eval origin buffer"))
  (with-current-buffer madrigal--eval-origin-buffer
    (unless (hash-table-p madrigal--state)
      (setq madrigal--state (make-hash-table :test #'equal)))
    (puthash key value madrigal--state)
    value))

(defun madrigal-runtime-info ()
  "Return concise information about the running Emacs instance."
  (list :emacs-version emacs-version
        :system-type system-type
        :window-system window-system
        :user-emacs-directory user-emacs-directory
        :features-loaded (length features)))

(defun madrigal-project-info ()
  "Return project information for the eval origin buffer."
  (unless (buffer-live-p madrigal--eval-origin-buffer)
    (error "No active Madrigal eval origin buffer"))
  (with-current-buffer madrigal--eval-origin-buffer
    (let* ((directory (or (and buffer-file-name
                               (file-name-directory buffer-file-name))
                          default-directory))
           (context (madrigal--project-context directory)))
      (if context
          (list :name (madrigal-project-context-name context)
                :root (madrigal-project-context-root context)
                :backend (madrigal-project-context-backend context)
                :file buffer-file-name
                :relative-file (and buffer-file-name
                                    (file-relative-name
                                     buffer-file-name
                                     (madrigal-project-context-root context))))
        (list :project nil :directory directory :file buffer-file-name)))))

(defun madrigal-context-buffer-text (&optional start end)
  "Return text from START to END in the eval origin buffer."
  (unless (buffer-live-p madrigal--eval-origin-buffer)
    (error "No active Madrigal eval origin buffer"))
  (with-current-buffer madrigal--eval-origin-buffer
    (buffer-substring-no-properties (or start (point-min))
                                    (or end (point-max)))))

(defun madrigal--paged-items (items offset limit)
  "Return a bounded page from ITEMS starting at OFFSET."
  (let* ((offset (max 0 (or offset 0)))
         (limit (max 1 (min 100 (or limit 20))))
         (total (length items))
         (page (seq-take (nthcdr offset items) limit))
         (next (+ offset (length page))))
    (list :items page
          :offset offset
          :total total
          :truncated (< next total)
          :next-offset (and (< next total) next))))

(defun madrigal-package-search (regexp &optional offset limit)
  "Search installed packages matching REGEXP and return a bounded page."
  (require 'package)
  (let (items)
    (dolist (entry package-alist)
      (let ((package (car entry)))
        (when (string-match-p regexp (symbol-name package))
          (push (list :name package :summary (madrigal--package-summary package))
                items))))
    (madrigal--paged-items
     (sort items (lambda (left right)
                   (string< (symbol-name (plist-get left :name))
                            (symbol-name (plist-get right :name)))))
     offset limit)))

(defun madrigal-feature-search (regexp &optional offset limit)
  "Search loaded feature names matching REGEXP and return a bounded page."
  (madrigal--paged-items
   (sort (seq-filter (lambda (feature)
                       (string-match-p regexp (symbol-name feature)))
                     (copy-sequence features))
         (lambda (left right)
           (string< (symbol-name left) (symbol-name right))))
   offset limit))

(defun madrigal-symbol-search (regexp kind &optional offset limit)
  "Search symbols matching REGEXP and KIND with a bounded result.

KIND is one of =function=, =variable=, =command=, or =any=."
  (let (items)
    (mapatoms
     (lambda (symbol)
       (when (and (madrigal--public-symbol-p symbol)
                  (string-match-p regexp (symbol-name symbol))
                  (pcase kind
                    ('function (fboundp symbol))
                    ('variable (boundp symbol))
                    ('command (commandp symbol))
                    ('any (or (fboundp symbol) (boundp symbol)))
                    (_ (user-error "Unknown symbol kind %S" kind))))
         (push symbol items))))
    (madrigal--paged-items
     (sort items (lambda (left right)
                   (string< (symbol-name left) (symbol-name right))))
     offset limit)))

(defun madrigal-key-binding-help (keys)
  "Return the command and documentation bound to KEYS."
  (let* ((sequence (if (stringp keys) (kbd keys) keys))
         (command (key-binding sequence t)))
    (list :keys (key-description sequence)
          :command command
          :help (and (symbolp command) (fboundp command)
                     (madrigal--function-help-data command)))))

(defun madrigal-mode-help (mode)
  "Return concise help for major or minor MODE."
  (unless (symbolp mode)
    (user-error "Expected a mode symbol, got %S" mode))
  (list :mode mode
        :function (and (fboundp mode) (madrigal--function-help-data mode))
        :variable (and (boundp mode) (madrigal--variable-help-data mode))))

(defun madrigal--function-signature (function)
  "Return an inline-call prototype string for FUNCTION." 
  (let* ((symbol (madrigal--function-symbol function))
         (arglist (help-function-arglist symbol t)))
    (if (listp arglist)
        (format "(%s %s)"
                symbol
                (string-join
                 (mapcar (lambda (arg)
                           (if (memq arg '(&optional &rest))
                               (symbol-name arg)
                             (upcase (symbol-name arg))))
                         arglist)
                 " "))
      (format "(%s ...)" symbol))))

(defun madrigal--function-doc-summary (function)
  "Return a concise documentation summary for FUNCTION." 
  (let* ((symbol (madrigal--function-symbol function))
         (doc (documentation symbol t)))
    (if (stringp doc)
        (car (split-string (string-trim doc) "\n\n+" t))
      "No documentation available.")))

(defun madrigal--variable-doc-summary (variable)
  "Return a concise documentation summary for VARIABLE." 
  (let* ((symbol (madrigal--variable-symbol variable))
         (doc (documentation-property symbol 'variable-documentation t)))
    (if (stringp doc)
        (car (split-string (string-trim doc) "\n\n+" t))
      "No documentation available.")))

(defun madrigal--function-help-data (function)
  "Return structured help data for FUNCTION." 
  (let ((symbol (madrigal--function-symbol function)))
    (list :type 'function
          :name symbol
          :signature (madrigal--function-signature symbol)
          :summary (madrigal--function-doc-summary symbol))))

(defun madrigal-function-help (function)
  "Return structured help data for FUNCTION." 
  (madrigal--function-help-data function))

(defun madrigal--variable-help-data (variable)
  "Return structured help data for VARIABLE." 
  (let ((symbol (madrigal--variable-symbol variable)))
    (list :type 'variable
          :name symbol
          :summary (madrigal--variable-doc-summary symbol))))

(defun madrigal-variable-help (variable)
  "Return structured help data for VARIABLE." 
  (madrigal--variable-help-data variable))

(defun madrigal--function-search-symbols (regex)
  "Return function symbols whose names match REGEX." 
  (let (matches)
    (mapatoms
     (lambda (symbol)
       (when (and (fboundp symbol)
                  (madrigal--public-symbol-p symbol)
                  (string-match-p regex (symbol-name symbol)))
         (push symbol matches))))
    (sort matches (lambda (left right)
                    (string< (symbol-name left) (symbol-name right))))))

(defun madrigal-function-search-help (regex)
  "Return structured help data for functions whose names match REGEX." 
  (list :type 'function-search
        :regex regex
        :items (mapcar #'madrigal--function-help-data
                       (madrigal--function-search-symbols regex))))

(defun madrigal--variable-search-symbols (regex)
  "Return bound variable symbols whose names match REGEX." 
  (let (matches)
    (mapatoms
     (lambda (symbol)
       (when (and (boundp symbol)
                  (madrigal--public-symbol-p symbol)
                  (string-match-p regex (symbol-name symbol)))
         (push symbol matches))))
    (sort matches (lambda (left right)
                    (string< (symbol-name left) (symbol-name right))))))

(defun madrigal-variable-search-help (regex)
  "Return structured help data for variables whose names match REGEX." 
  (list :type 'variable-search
        :regex regex
        :items (mapcar #'madrigal--variable-help-data
                       (madrigal--variable-search-symbols regex))))

(defun madrigal--feature-library (feature)
  "Return the library file path for FEATURE after loading it." 
  (unless (require feature nil t)
    (user-error "Could not load feature %s" feature))
  (let* ((base (symbol-name feature))
         (source (locate-library (concat base ".el") t))
         (compiled (locate-library base t)))
    (or (and source (file-readable-p source) source)
        (and compiled
             (string-suffix-p ".elc" compiled)
             (let ((candidate (substring compiled 0 -1)))
               (and (file-readable-p candidate) candidate)))
        compiled
        (user-error "Could not locate library for feature %s" feature))))

(defun madrigal--feature-load-history (feature)
  "Return the load-history entry for FEATURE." 
  (let* ((library (madrigal--feature-library feature))
         (base (file-name-sans-extension library))
         (loaded (or (assoc library load-history)
                     (seq-find (lambda (entry)
                                 (string= base
                                          (file-name-sans-extension (car entry))))
                               load-history))))
    (unless loaded
      (user-error "Could not inspect load history for feature %s" feature))
    loaded))

(defun madrigal--feature-functions (feature)
  "Return function symbols defined in FEATURE." 
  (let (functions)
    (dolist (entry (cdr (madrigal--feature-load-history feature)))
      (when (and (consp entry)
                 (eq (car entry) 'defun)
                 (symbolp (cdr entry))
                 (fboundp (cdr entry)))
        (when (madrigal--public-symbol-p (cdr entry))
          (push (cdr entry) functions))))
    (nreverse functions)))

(defun madrigal--feature-variables (feature)
  "Return variable symbols defined in FEATURE." 
  (let (variables)
    (dolist (entry (cdr (madrigal--feature-load-history feature)))
      (cond
       ((and (symbolp entry)
             (boundp entry))
        (when (madrigal--public-symbol-p entry)
          (push entry variables)))
       ((let ((kind (car-safe entry))
              (symbol (cdr-safe entry)))
          (and (memq kind '(defvar defconst custom-declare-variable))
               (symbolp symbol)
               (boundp symbol)))
        (let ((symbol (cdr-safe entry)))
          (when (madrigal--public-symbol-p symbol)
            (push symbol variables))))))
    (nreverse variables)))

(defun madrigal--feature-help-data (feature)
  "Return structured help data for FEATURE." 
  (list :type 'feature
        :name feature
        :functions (mapcar #'madrigal--function-help-data
                           (madrigal--feature-functions feature))
        :variables (mapcar #'madrigal--variable-help-data
                           (madrigal--feature-variables feature))))

(defun madrigal-feature-help (feature)
  "Return structured help data for FEATURE." 
  (madrigal--feature-help-data feature))

(defun madrigal--package-desc (package)
  "Return the package descriptor for PACKAGE, or nil when unavailable." 
  (require 'package)
  (cond
   ((fboundp 'package-get-descriptor)
    (ignore-errors (package-get-descriptor package)))
   ((alist-get package package-alist nil nil #'eq)
    (car (alist-get package package-alist nil nil #'eq)))
   (t nil)))

(defun madrigal--package-summary (package)
  "Return the top-level summary for PACKAGE, or nil when unavailable." 
  (when-let ((desc (madrigal--package-desc package)))
    (let ((summary (ignore-errors (package-desc-summary desc))))
      (and (stringp summary)
           (not (string-empty-p summary))
           summary))))

(defun madrigal--package-directory (package)
  "Return the installed package directory for PACKAGE, or nil when unavailable." 
  (when-let ((desc (madrigal--package-desc package)))
    (ignore-errors (package-desc-dir desc))))

(defun madrigal--path-within-directory-p (path directory)
  "Return non-nil when PATH is located inside DIRECTORY." 
  (let ((path (file-truename path))
        (directory (file-name-as-directory (file-truename directory))))
    (string-prefix-p directory path)))

(defun madrigal--library-provides (file)
  "Return feature symbols provided by Emacs Lisp FILE." 
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (features form)
      (condition-case nil
          (while t
            (setq form (read (current-buffer)))
            (when (and (listp form)
                       (eq (car form) 'provide)
                       (= (length form) 2))
              (let ((feature (cadr form)))
                (when (and (listp feature)
                           (eq (car feature) 'quote)
                           (= (length feature) 2))
                  (setq feature (cadr feature)))
                (when (symbolp feature)
                  (push feature features)))))
        (end-of-file nil))
      (nreverse features))))

(defun madrigal--package-el-files (package)
  "Return Emacs Lisp source files belonging to PACKAGE." 
  (when-let ((directory (madrigal--package-directory package)))
    (sort
     (seq-filter
      (lambda (file)
        (and (string-suffix-p ".el" file)
             (not (string-suffix-p "-autoloads.el" file))
             (not (string-suffix-p "-pkg.el" file))))
      (directory-files-recursively directory "\\.el\\'" nil nil t))
     #'string<)))

(defun madrigal--package-features (package)
  "Return feature symbols defined by PACKAGE source files." 
  (delete-dups
   (apply #'append
          (mapcar #'madrigal--library-provides
                  (madrigal--package-el-files package)))))

(defun madrigal--package-help-data (package)
  "Return structured help data for PACKAGE." 
  (list :type 'package
        :name package
        :summary (madrigal--package-summary package)
        :features (mapcar #'madrigal--feature-help-data
                          (madrigal--package-features package))))

(defun madrigal-package-help (package)
  "Return structured help data for PACKAGE." 
  (madrigal--package-help-data package))

(defun madrigal--feature-help-org (feature)
  "Return Org-formatted help for FEATURE." 
  (let* ((help (madrigal-feature-help feature))
         (functions (plist-get help :functions))
         (variables (plist-get help :variables)))
    (string-join
     (delq nil
           (append
            (list (format "* Feature =%s=" (plist-get help :name)))
            (when functions
              (list
               (string-join
                (mapcar (lambda (item)
                          (format "*** =%s=\n%s"
                                  (plist-get item :signature)
                                  (plist-get item :summary)))
                        functions)
                "\n\n")))
            (when variables
              (list
               (string-join
                (mapcar (lambda (item)
                          (format "*** =%s=\n%s"
                                  (plist-get item :name)
                                  (plist-get item :summary)))
                        variables)
                "\n\n")))))
     "\n\n")))

(provide 'madrigal-tool-eval-prelude)

;;; madrigal-tool-eval-prelude.el ends here
