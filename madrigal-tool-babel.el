;;; madrigal-tool-babel.el --- Org Babel tool support for Madrigal  -*- lexical-binding: t; -*-

;;; Code:

(require 'madrigal-core)
(require 'madrigal-org)
(require 'madrigal-tool-eval)
(require 'cl-lib)
(require 'flymake)
(require 'ob-core)
(require 'ob-tangle)
(require 'org)
(require 'org-src)
(require 'project)
(require 'subr-x)

(require 'eglot nil t)

(defvar mcp-hub-servers)

(defun madrigal--python-executable ()
  "Return the preferred Python executable, or nil when unavailable." 
  (or (executable-find "python3")
      (executable-find "python")))

(defun madrigal--python-version-string ()
  "Return the available Python version string, or nil." 
  (when-let ((python (madrigal--python-executable)))
    (with-temp-buffer
      (when (eq 0 (call-process python nil (current-buffer) nil
                                "-c" "import sys; print(sys.version.split()[0])"))
        (string-trim (buffer-string))))))

(defun madrigal--python-package-string ()
  "Return installed Python package names as a comma-separated string, or nil." 
  (when-let ((python (madrigal--python-executable)))
    (with-temp-buffer
      (when (eq 0 (call-process
                   python nil (current-buffer) nil
                   "-c"
                   (concat
                    "import importlib.metadata\n"
                    "names = sorted({dist.metadata.get('Name') or dist.metadata.get('Summary') or dist.name "
                    "for dist in importlib.metadata.distributions()})\n"
                    "print(', '.join(str(name) for name in names if name))")))
        (string-trim (buffer-string))))))

(defun madrigal--python-runtime-description ()
  "Return an Org snippet describing the available Python runtime, if any." 
  (when-let ((version (madrigal--python-version-string)))
    (concat
     "*** Python\n"
     "**** Python Version\n"
     version "\n"
     "**** Installed Packages\n"
     (or (madrigal--python-package-string) "none") "\n"
     "Only Python standard-library modules and the listed installed packages are available. Do not assume any other third-party packages exist.\n"
     "Use :results output only for printed stdout, and use :results value only when intentionally returning a value.\n"
     "For :results graphics file, either return a matplotlib Figure object or create the plot and let Org save the current figure. Do not print or return a filename, and do not mix graphics-file mode with manual save-and-return patterns.\n")))

(defun madrigal--shell-runtime-description ()
  "Return an Org snippet describing shell-specific Babel guidance." 
  (concat
   "*** Shell\n"
   "Use shell only for genuine shell tasks such as invoking command-line tools, pipelines, or process orchestration.\n"))

(defun madrigal--mcp-hub-servers-bound-p ()
  "Return non-nil when `mcp-hub-servers' can be inspected."
  (or (boundp 'mcp-hub-servers)
      (progn
        (condition-case nil
            (require 'mcp-hub nil t)
          (error nil))
        (boundp 'mcp-hub-servers))))

(defun madrigal--mcp-server-description (entry)
  "Return ENTRY's configured MCP server description, if present."
  (when-let ((description (plist-get (cdr entry) :description)))
    (when (stringp description)
      (let ((trimmed (string-trim description)))
        (unless (string-empty-p trimmed)
          (string-join (split-string trimmed "[[:space:]\n\r]+" t) " "))))))

(defun madrigal--mcp-server-list-description ()
  "Return an Org list of configured MCP servers."
  (if (not (madrigal--mcp-hub-servers-bound-p))
      "unavailable\n"
    (let ((servers (symbol-value 'mcp-hub-servers)))
      (if servers
          (mapconcat
           (lambda (entry)
             (let ((description (madrigal--mcp-server-description entry)))
               (if description
                   (format "- =%s= :: %s\n" (car entry) description)
                 (format "- =%s=\n" (car entry)))))
           servers
           "")
        "none\n"))))

(defun madrigal--mcp-runtime-description ()
  "Return an Org snippet describing the available MCP runtime."
  (concat
   "*** MCP\n"
   "Use =#+begin_src mcp :server SERVER= blocks through ob-mcp for configured MCP servers; ob-mcp starts the selected server lazily when needed.\n"
   "Use =servers= to inspect configured servers and connection state, then =tools=, =resources=, or =prompts= for a selected server before calling unknown capabilities.\n"
   "**** Available Servers\n"
   (madrigal--mcp-server-list-description)))

(defcustom madrigal-babel-allowed-languages nil
  "Languages allowed in the Madrigal Babel tool.

When nil, allow any language enabled in `org-babel-load-languages'."
  :type '(choice (const :tag "Use Org Babel enabled languages" nil)
                 (repeat symbol))
  :group 'madrigal)

(defcustom madrigal-babel-preflight-timeout 2.0
  "Maximum seconds to wait for Babel syntax or LSP preflight diagnostics."
  :type 'number
  :group 'madrigal)


(defun madrigal--babel-enabled-languages ()
  "Return enabled Org Babel languages as symbols." 
  (sort
   (mapcar #'car
           (seq-filter (lambda (entry) (cdr entry))
                       org-babel-load-languages))
   (lambda (left right)
     (string< (symbol-name left) (symbol-name right)))))

(defun madrigal--babel-allowed-language-symbols ()
  "Return Babel languages currently allowed for the tool." 
  (or madrigal-babel-allowed-languages
      (madrigal--babel-enabled-languages)))

(defun madrigal--babel-language-summary ()
  "Return a human-readable summary of currently allowed Babel languages." 
  (let ((languages (madrigal--babel-allowed-language-symbols)))
    (if languages
        (string-join (mapcar #'symbol-name languages) ", ")
      "none")))

(defun madrigal--babel-tool-description ()
  "Return the current description string for the Babel tool." 
  (concat
   (string-join
    (list
     "Insert one complete Org Babel source block verbatim into the current Request turn."
     "Do not invoke supported interpreters from shell blocks, and do not invoke shell commands from other language blocks when a direct Babel block for the needed language or interface is available; treat cross-language indirection as the wrong tool choice unless the task explicitly requires it."
     "The argument must be literal Org containing an optional #+name: line, a #+begin_src line with any headers, the block body, and #+end_src."
     "Use the most direct runtime or interface for each stage of the task. When a task calls for a particular language or environment, use that language directly in a src block unless the task explicitly requires indirection."
     "The tool syntax-checks the block before execution when possible: Lisp blocks use delimiter checking, and languages with working Eglot support may report LSP diagnostics first. Fix preflight errors and resubmit instead of executing broken code."
     "If execution fails, revise the src block and call the babel tool again."
     "The tool executes exactly that inserted block as Org Babel and returns only the resulting Org fragment: the source block plus its #+RESULTS:. File results are included inline, so do not repeat them unless extra explanation is needed."
     "Use normal Babel linkage such as named blocks, :var, :session, and :noweb."
     "For live or environment-specific information, inspect first, keep results small, and use direct interfaces instead of guessing or scraping underlying storage heuristically."
     "For multi-stage tasks, validate each intermediate result before continuing. Prove extracted data is present and shaped as expected before rendering, and if a result is empty or implausible, inspect and correct before moving on."
     "Avoid affecting the user's session unless the task requires persistent changes. Prefer temporary buffers, temporary files, and disposable sessions; use persistent Babel sessions only when needed."
     (format "Allowed languages right now: %s." (madrigal--babel-language-summary)))
    "\n")
   (when (seq-intersection '(shell sh bash) (madrigal--babel-allowed-language-symbols))
     (concat "\n\n" (madrigal--shell-runtime-description)))
   (when (member 'emacs-lisp (madrigal--babel-allowed-language-symbols))
     (concat
      "\n\n*** Emacs Lisp\n"
      (madrigal--emacs-runtime-description 4)))
   (when (member 'python (madrigal--babel-allowed-language-symbols))
     (concat "\n\n" (or (madrigal--python-runtime-description)
                           "*** Python\n**** Python Version\nunavailable\n**** Installed Packages\nunavailable\n")))
   (when (member 'mcp (madrigal--babel-allowed-language-symbols))
     (concat "\n\n" (madrigal--mcp-runtime-description)))))

(defun madrigal--babel-src-block-info (src-block)
  "Return Org Babel info for SRC-BLOCK or signal an error." 
  (with-temp-buffer
    (insert (string-trim src-block) "\n")
    (delay-mode-hooks (org-mode))
    (goto-char (point-min))
    (unless (re-search-forward "^[[:space:]]*#\\+begin_src\\b" nil t)
      (user-error "Tool input must be one complete Org src block"))
    (beginning-of-line)
    (let ((element (org-element-at-point)))
      (unless (eq (org-element-type element) 'src-block)
        (user-error "Tool input must be one complete Org src block"))
      (org-babel-get-src-block-info nil element))))

(defun madrigal--babel-language-allowed-p (language)
  "Return non-nil when LANGUAGE is allowed for the Babel tool." 
  (member (intern language) (madrigal--babel-allowed-language-symbols)))

(defun madrigal--babel-fragment-bounds ()
  "Return bounds of the src block at point plus its following results, if any." 
  (let* ((element (org-element-at-point))
         (start (or (org-element-property :begin element) (point)))
         (end (or (org-element-property :end element) (point))))
    (save-excursion
      (goto-char start)
      (let ((result-start (org-babel-where-is-src-block-result nil)))
        (when result-start
          (goto-char result-start)
          (setq end (org-babel-result-end))))
      (cons start end))))

(defun madrigal--delete-babel-fragment (start end)
  "Delete the Babel fragment between START and END, including any results." 
  (save-excursion
    (goto-char (marker-position start))
    (let* ((bounds (if (eq (org-element-type (org-element-at-point)) 'src-block)
                       (madrigal--babel-fragment-bounds)
                     (cons (marker-position start) (marker-position end))))
           (delete-start (car bounds))
           (delete-end (cdr bounds)))
      (delete-region delete-start delete-end)
      (when (and (< (point-min) (point-max))
                 (save-excursion
                   (goto-char delete-start)
                   (looking-at-p "\n\{3,\}")))
        (replace-match "\n\n")))))

(defun madrigal--babel-error-output-buffers ()
  "Return Org Babel error buffers that may contain execution failures." 
  (delq nil
        (list (get-buffer org-babel-error-buffer-name)
              (get-buffer " *Org-Babel Error*"))))

(defun madrigal--babel-clear-error-output ()
  "Clear Org Babel execution error buffers." 
  (when (fboundp 'org-babel-eval-wipe-error-buffer)
    (org-babel-eval-wipe-error-buffer))
  (when-let ((buffer (get-buffer " *Org-Babel Error*")))
    (with-current-buffer buffer
      (erase-buffer))))

(defun madrigal--babel-error-output-string ()
  "Return combined Org Babel execution error output, or nil when empty." 
  (let ((parts
         (delq nil
               (mapcar (lambda (buffer)
                         (with-current-buffer buffer
                           (let ((text (string-trim (buffer-substring-no-properties
                                                     (point-min) (point-max)))))
                             (unless (string-empty-p text)
                               text))))
                       (madrigal--babel-error-output-buffers)))))
    (when parts
      (string-join parts "\n\n"))))

(defun madrigal--format-babel-execution-error (language err)
  "Return a retry-oriented tool result for LANGUAGE execution error ERR." 
  (format
   "Execution failed for %s. Revise the src block and call the babel tool again. Error: %s"
   language
   (error-message-string err)))

(defun madrigal--format-babel-error-output (language output)
  "Return a retry-oriented tool result for LANGUAGE execution OUTPUT." 
  (format
   "Execution failed for %s. Revise the src block and call the babel tool again. Error output: %s"
   language
   output))

(defun madrigal--babel-lisp-language-p (language)
  "Return non-nil when LANGUAGE should use Lisp delimiter preflight." 
  (member (intern language)
          '(emacs-lisp elisp lisp scheme racket clojure common-lisp)))

(defun madrigal--babel-require-lang-backend (language)
  "Try to load Org Babel support for LANGUAGE." 
  (ignore-errors
    (require (intern (format "ob-%s" language)) nil t)))

(defun madrigal--babel-language-extension (language)
  "Return a likely file extension for LANGUAGE, or nil." 
  (madrigal--babel-require-lang-backend language)
  (cdr (assoc language org-babel-tangle-lang-exts)))

(defun madrigal--babel-lsp-major-mode (language)
  "Return the major mode symbol for LANGUAGE, or nil." 
  (when-let ((mode (org-src-get-lang-mode language)))
    (unless (fboundp mode)
      (ignore-errors
        (require mode nil t))
      (unless (fboundp mode)
        (ignore-errors
          (require (intern (string-remove-suffix "-mode" (symbol-name mode))) nil t))))
    (and (fboundp mode) mode)))

(defun madrigal--babel-preflight-root (buffer)
  "Return the project root used for Babel preflight checks in BUFFER." 
  (with-current-buffer buffer
    (or (and madrigal-session
             (madrigal-session-root madrigal-session))
        default-directory
        temporary-file-directory)))

(defun madrigal--babel-format-diagnostic-at-point (type message position)
  "Return a diagnostic plist for TYPE MESSAGE at POSITION." 
  (save-excursion
    (goto-char position)
    (list :type type
          :line (line-number-at-pos position)
          :column (current-column)
          :message message)))

(defun madrigal--babel-check-parens-diagnostics (source)
  "Return Lisp delimiter diagnostics for SOURCE, or nil." 
  (with-temp-buffer
    (insert source)
    (goto-char (point-min))
    (condition-case err
        (progn
          (check-parens)
          nil)
      (error
       (list
        (madrigal--babel-format-diagnostic-at-point
         'error
         (error-message-string err)
         (point)))))))

(defun madrigal--babel-error-diagnostic-p (diagnostic)
  "Return non-nil when DIAGNOSTIC should block Babel execution." 
  (memq (flymake-diagnostic-type diagnostic)
        '(eglot-error flymake-error error)))

(defun madrigal--babel-flymake-diagnostics ()
  "Return blocking Flymake diagnostics for the current buffer." 
  (mapcar
   (lambda (diagnostic)
     (list :type 'error
           :line (line-number-at-pos (flymake-diagnostic-beg diagnostic))
           :column (save-excursion
                     (goto-char (flymake-diagnostic-beg diagnostic))
                     (current-column))
           :message (flymake-diagnostic-text diagnostic)))
   (seq-filter #'madrigal--babel-error-diagnostic-p
               (flymake-diagnostics))))

(defun madrigal--babel-wait-until (predicate deadline)
  "Wait until PREDICATE returns non-nil or DEADLINE elapses." 
  (while (and (< (float-time) deadline)
              (not (funcall predicate)))
    (accept-process-output nil 0.1))
  (funcall predicate))

(defun madrigal--babel-lsp-file (language root source)
  "Create and return an LSP preflight file for LANGUAGE under ROOT with SOURCE." 
  (let* ((extension (madrigal--babel-language-extension language))
         (name (if extension
                   (format "main.%s" extension)
                 "main"))
         (file (expand-file-name name root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert source))
    file))

(defun madrigal--babel-eglot-guess (mode project)
  "Return Eglot connection parameters for MODE in PROJECT, or nil." 
  (when-let* ((languages-and-contact (ignore-errors (eglot--lookup-mode mode)))
              (managed-modes (mapcar #'car (car languages-and-contact)))
              (language-ids (mapcar #'cdr (car languages-and-contact)))
              (guess (cdr languages-and-contact))
              (guess (if (functionp guess)
                         (pcase (cdr (func-arity guess))
                           (1 (funcall guess nil))
                           (_ (funcall guess nil project)))
                       guess))
              (class (or (and (consp guess) (symbolp (car guess))
                              (prog1 (car guess)
                                (setq guess (cdr guess))))
                         'eglot-lsp-server))
              (contact (and (listp guess) (stringp (car guess)) guess)))
    (list managed-modes class contact language-ids)))

(defun madrigal--babel-ghc-diagnostic-message ()
  "Return the message body for a GHC diagnostic at point."
  (let ((raw (or (match-string 4) "")))
    (setq raw (string-trim
               (replace-regexp-in-string "\\`\\[[^]]+\\][[:space:]]*" "" raw)))
    (if (not (string-empty-p raw))
        raw
      (save-excursion
        (forward-line 1)
        (catch 'message
          (while (and (not (eobp))
                      (not (looking-at-p "^[^:\n]+:[0-9]+:[0-9]+: ")))
            (let ((line (string-trim
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))))
              (unless (or (string-empty-p line)
                          (string-prefix-p "|" line)
                          (string-match-p "\\`[0-9]+ |" line))
                (throw 'message line)))
            (forward-line 1))
          raw)))))

(defun madrigal--babel-ghc-diagnostics (file)
  "Return GHC syntax diagnostics for FILE, or nil."
  (when-let ((ghc (executable-find "ghc")))
    (with-temp-buffer
      (let ((status (let ((default-directory (file-name-directory file)))
                      (call-process ghc nil (current-buffer) nil
                                    "-fno-code" file))))
        (unless (eq status 0)
          (goto-char (point-min))
          (let (diagnostics)
            (while (re-search-forward
                    "^[^:\n]+:\\([0-9]+\\):\\([0-9]+\\): \\(error\\|warning\\):\\(?: \\(.*\\)\\)?$"
                    nil t)
              (push (list :type (if (equal (match-string 3) "error")
                                    'error
                                  'warning)
                          :line (string-to-number (match-string 1))
                          :column (1- (string-to-number (match-string 2)))
                          :message (madrigal--babel-ghc-diagnostic-message))
                    diagnostics))
            (nreverse diagnostics)))))))

(defun madrigal--babel-command-diagnostics (language file)
  "Return command-line diagnostics for LANGUAGE FILE, or nil."
  (pcase language
    ("haskell" (madrigal--babel-ghc-diagnostics file))
    (_ nil)))

(defun madrigal--babel-lsp-diagnostics (language source root)
  "Return LSP diagnostics for LANGUAGE SOURCE under ROOT, or nil." 
  (when (featurep 'eglot)
    (when-let* ((mode (madrigal--babel-lsp-major-mode language))
                (root (file-name-as-directory (expand-file-name root)))
                (file (madrigal--babel-lsp-file language root source))
                (project `(transient . ,root))
                (params (madrigal--babel-eglot-guess mode project)))
      (pcase-let ((`(,managed-modes ,class ,contact ,language-ids) params))
        (let ((diagnostics nil)
              (check-buffer (generate-new-buffer (format " *madrigal-%s-lsp*" language))))
          (unwind-protect
              (cl-letf (((symbol-function 'project-current)
                         (lambda (&optional _maybe-prompt _directory)
                           project)))
                (with-current-buffer check-buffer
                  (setq default-directory root)
                  (erase-buffer)
                  (insert source)
                  (write-region nil nil file nil 'silent)
                  (setq-local buffer-file-name file)
                  (delay-mode-hooks
                    (funcall mode))
                  (condition-case nil
                      (eglot managed-modes project class contact language-ids)
                    (error nil))
                  (when (fboundp 'eglot--maybe-activate-editing-mode)
                    (ignore-errors (eglot--maybe-activate-editing-mode)))
                  (let ((deadline (+ (float-time) madrigal-babel-preflight-timeout)))
                    (when (madrigal--babel-wait-until
                           (lambda () (ignore-errors (eglot-managed-p)))
                           deadline)
                      (flymake-mode 1)
                      (flymake-start)
                      (madrigal--babel-wait-until
                       (lambda () (flymake-diagnostics))
                       deadline)
                      (setq diagnostics (madrigal--babel-flymake-diagnostics))))))
            (unless diagnostics
              (setq diagnostics
                    (madrigal--babel-command-diagnostics language file)))
            (when (buffer-live-p check-buffer)
              (with-current-buffer check-buffer
                (when (ignore-errors (eglot-managed-p))
                  (ignore-errors (eglot-shutdown (eglot-current-server))))
                (set-buffer-modified-p nil))
              (kill-buffer check-buffer))
            (ignore-errors (delete-file file)))
          diagnostics)))))

(defun madrigal--format-babel-preflight-error (language diagnostics)
  "Return a user-facing error string for LANGUAGE DIAGNOSTICS." 
  (concat
   (format "Pre-execution check failed for %s. Fix these errors and resubmit a corrected src block:\n" language)
   (string-join
    (mapcar (lambda (diagnostic)
              (format "- line %s, column %s: %s"
                      (plist-get diagnostic :line)
                      (plist-get diagnostic :column)
                      (plist-get diagnostic :message)))
            diagnostics)
    "\n")))

(defun madrigal--babel-preflight-check (language source buffer)
  "Return nil when LANGUAGE SOURCE passes preflight in BUFFER, else an error string." 
  (when-let ((diagnostics
              (or (and (madrigal--babel-lisp-language-p language)
                       (madrigal--babel-check-parens-diagnostics source))
                  (madrigal--babel-lsp-diagnostics
                   language source (madrigal--babel-preflight-root buffer)))))
    (madrigal--format-babel-preflight-error language diagnostics)))

(defun madrigal--run-babel-tool (_tool-name buffer request-id callback src_block)
  "Run the Babel tool in BUFFER for REQUEST-ID and pass its fragment to CALLBACK." 
  (let (fragment)
    (with-current-buffer buffer
      (unless (madrigal--babel-agent-p)
        (user-error "The babel tool is intended for babel-assistant sessions"))
      (let* ((info (madrigal--babel-src-block-info src_block))
             (language (car info))
             (source (nth 1 info)))
        (unless (madrigal--babel-language-allowed-p language)
          (user-error "Babel language %s is not allowed; enabled languages: %s"
                      language
                      (madrigal--babel-language-summary)))
        (when-let ((preflight-error
                    (madrigal--babel-preflight-check language source buffer)))
          (setq fragment preflight-error))
        (unless fragment
          (pcase-let ((`(,start . ,end) (madrigal--insert-into-request request-id (string-trim src_block))))
            (save-excursion
              (goto-char (marker-position start))
              (madrigal--babel-clear-error-output)
              (condition-case err
                  (progn
                    (let ((org-confirm-babel-evaluate nil))
                      (org-babel-execute-src-block))
                    (if-let ((error-output (madrigal--babel-error-output-string)))
                        (progn
                          (madrigal--delete-babel-fragment start end)
                          (setq fragment
                                (madrigal--format-babel-error-output language error-output)))
                      (pcase-let ((`(,fragment-start . ,fragment-end)
                                    (madrigal--babel-fragment-bounds)))
                        (setq fragment
                              (string-trim-right
                               (buffer-substring-no-properties fragment-start fragment-end))))))
                (error
                 (madrigal--delete-babel-fragment start end)
                 (setq fragment
                       (madrigal--format-babel-execution-error language err)))))))))
    (funcall callback fragment)))

(setf (alist-get "babel" madrigal-tools nil nil #'string=)
      '(:description madrigal--babel-tool-description
        :args ((:name "src_block"
                :description
                "One complete Org Babel src block, including headers and #+begin_src/#+end_src lines."
                :type string))
        :function madrigal--run-babel-tool
        :async t))

(setf (alist-get "babel-assistant" madrigal-agents nil nil #'string=)
      '(:system-prompt
        "Reply in Org mode only.
Do NOT invoke python or other babel languages from a shell script.
Final natural-language responses must begin with a top-level Org heading whose title summarizes the completed turn."
        :tools ("babel")))

(provide 'madrigal-tool-babel)

;;; madrigal-tool-babel.el ends here
