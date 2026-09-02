;;; madrigal-test.el --- Tests for Madrigal  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'madrigal)

(defun madrigal-test--write-file (path content)
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content)))

(ert-deftest madrigal-make-session-requires-configured-agent ()
  (let ((default-directory "/tmp/madrigal/"))
    (should-error (madrigal-make-session nil) :type 'user-error)))

(ert-deftest madrigal-make-session-uses-explicit-agent-name ()
  (let ((madrigal-providers '(("Agent Provider" :factory (lambda (model)
                                                            (concat "provider:" model)))))
        (madrigal-agent-models '(("developer" . ("Agent Provider" . "dev-model")))))
    (let ((session (madrigal-make-session "developer" "/worktree/")))
      (should (equal (madrigal-session-agent session) "developer"))
      (should (equal (madrigal-session-provider session) "provider:dev-model"))
      (should (equal (madrigal-session-model session) "dev-model"))
      (should (equal (madrigal-session-root session) "/worktree/")))))

(ert-deftest madrigal-make-session-fails-when-agent-selection-missing ()
  (let ((madrigal-providers nil)
        (madrigal-agent-models '(("assistant" . ("Missing" . "agent-model")))))
    (should-error (madrigal-make-session "assistant") :type 'user-error)))

(ert-deftest madrigal-agent-provider-and-model-prompts-when-missing ()
  (let ((madrigal-providers '(("Provider A" :factory (lambda (model)
                                                        (concat "provider:" model))
                               :models ("alpha" "beta"))))
        (madrigal-agent-models nil))
    (cl-letf (((symbol-function 'madrigal--can-prompt-for-provider-model-p)
               (lambda () t))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "Provider A / beta")))
      (let ((resolved (madrigal--agent-provider-and-model "assistant")))
        (should (equal (car resolved) "provider:beta"))
        (should (equal (cdr resolved) "beta"))
        (should (equal (alist-get "assistant" madrigal-agent-models nil nil #'string=)
                       '("Provider A" . "beta")))))))

(ert-deftest madrigal-select-agent-provider-and-model-overrides-existing-selection ()
  (let ((madrigal-providers '(("Provider A" :factory (lambda (model)
                                                        (concat "provider:" model))
                               :models ("alpha" "beta"))))
        (madrigal-agent-models '(("assistant" . ("Provider A" . "alpha")))))
    (cl-letf (((symbol-function 'madrigal--can-prompt-for-provider-model-p)
               (lambda () t))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "Provider A / beta")))
      (should (equal (madrigal-select-agent-provider-and-model "assistant")
                     '("Provider A" . "beta")))
      (should (equal (alist-get "assistant" madrigal-agent-models nil nil #'string=)
                     '("Provider A" . "beta"))))))

(ert-deftest madrigal-llm-available-p-returns-a-booleanish-value ()
  (should (memq (madrigal-llm-available-p) '(nil t))))

(ert-deftest madrigal-agent-controller-resolves-agent-and-builds-prompt ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (let ((madrigal-providers '(("Provider" :provider fake-provider)))
          (madrigal-agent-models '(("assistant" . ("Provider" . "model"))))
          (madrigal-agents '(("assistant" :system-prompt "System" :tools ("eval"))))
          captured-provider
          captured-prompt
          started)
      (cl-letf (((symbol-function 'llm-chat-async)
                 (lambda (provider prompt _response _error &optional _multi)
                   (setq captured-provider provider)
                   (setq captured-prompt prompt)
                   'fake-request)))
        (let ((handle (madrigal-agent-controller-submit-async
                       :agent "assistant"
                       :history '((:role user :content "Hello"))
                       :context "Extra context"
                       :environment (list :buffer (current-buffer))
                       :on-start (lambda (event) (setq started event)))))
          (should (madrigal-agent-controller-handle-p handle))
          (should (eq captured-provider 'fake-provider))
          (should (string-match-p "System" (llm-chat-prompt-context captured-prompt)))
          (should (string-match-p "Extra context" (llm-chat-prompt-context captured-prompt)))
          (should (equal "Hello"
                         (llm-chat-prompt-interaction-content
                          (car (llm-chat-prompt-interactions captured-prompt)))))
          (should (equal '("eval")
                         (mapcar #'llm-tool-name
                                 (llm-chat-prompt-tools captured-prompt))))
          (should (plist-get started :request-id)))))))

(ert-deftest madrigal-agent-controller-context-size-reports-limit-and-percent ()
  (let ((madrigal-agents '(("assistant" :system-prompt "System" :tools nil))))
    (cl-letf (((symbol-function 'llm-count-tokens)
               (lambda (_provider text) (length text)))
              ((symbol-function 'llm-chat-token-limit)
               (lambda (_provider) 100)))
      (let ((info (madrigal-agent-controller-context-size
                   :agent "assistant"
                   :history '((:role user :content "Hi"))
                   :context "Ctx"
                   :provider 'fake-provider)))
        (should (> (plist-get info :tokens) 0))
        (should (= (plist-get info :limit) 100))
        (should (> (plist-get info :percent) 0))))))

(ert-deftest madrigal-project-todos-collects-tracked-work-items ()
  (let* ((root (make-temp-file "madrigal-project-" t))
         (org-file (expand-file-name "notes.org" root))
         (md-file (expand-file-name "plan.md" root))
         (el-file (expand-file-name "src/code.el" root))
         (ignored-file (expand-file-name "scratch.txt" root))
         (project 'fake-project))
    (unwind-protect
        (progn
          (madrigal-test--write-file
           org-file
           "* TODO First heading\n* DONE Finished heading\n- [ ] Org checkbox\n")
          (madrigal-test--write-file
           md-file
           "TODO: Write release notes\n- [ ] Markdown checkbox\n")
          (madrigal-test--write-file
           el-file
           ";; TODO: Refactor agent loop\n(message \"ok\")\n")
          (madrigal-test--write-file
           ignored-file
           "# TODO: ignored\n")
          (cl-letf (((symbol-function 'project-root)
                     (lambda (_project) root))
                    ((symbol-function 'project-files)
                     (lambda (_project)
                       (list "notes.org" "plan.md" "src/code.el"))))
            (let ((todos (madrigal-project-todos project)))
              (should (= (length todos) 5))
              (should (equal (mapcar #'madrigal-todo-kind todos)
                             '(org-heading org-checkbox markdown-todo markdown-checkbox comment)))
              (should (equal (mapcar #'madrigal-todo-keyword todos)
                             '("TODO" "CHECKBOX" "TODO" "CHECKBOX" "TODO")))
              (should (equal (mapcar #'madrigal-todo-text todos)
                             '("First heading"
                               "Org checkbox"
                               "Write release notes"
                               "Markdown checkbox"
                               "Refactor agent loop")))
              (should-not (seq-some (lambda (todo)
                                      (string= (madrigal-todo-file todo) ignored-file))
                                    todos)))))
      (delete-directory root t))))

(ert-deftest madrigal-project-todos-uses-current-project-by-default ()
  (let* ((root (make-temp-file "madrigal-project-" t))
         (file (expand-file-name "todo.org" root))
         (project 'current-project))
    (unwind-protect
        (progn
          (madrigal-test--write-file file "#+TODO: NEXT | DONE\n* NEXT Ship it\n")
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&optional _maybe-prompt) project))
                    ((symbol-function 'project-root)
                     (lambda (_project) root))
                    ((symbol-function 'project-files)
                     (lambda (_project) (list "todo.org"))))
            (let ((todos (madrigal-project-todos)))
              (should (= (length todos) 1))
              (should (equal (madrigal-todo-keyword (car todos)) "NEXT"))
              (should (equal (madrigal-todo-text (car todos)) "Ship it")))))
      (delete-directory root t))))

(ert-deftest madrigal-eval-elisp-returns-structured-result ()
  (let ((result (madrigal--eval-elisp "(+ 1 2)")))
    (should (eq (plist-get result :ok) t))
    (should (= (plist-get result :value) 3))))

(ert-deftest madrigal-eval-elisp-auto-closes-trailing-delimiters ()
  (let ((result (madrigal--eval-elisp "(list 1 2")))
    (should (eq (plist-get result :ok) t))
    (should (equal (plist-get result :value) '(1 2)))
    (should (equal (plist-get result :source-used) "(list 1 2)"))))

(ert-deftest madrigal-persist-elisp-persists-and-evaluates-reusable-code ()
  (let* ((directory (make-temp-file "madrigal-elisp-" t))
         (madrigal-persistent-elisp-file
          (expand-file-name "madrigal-persisted-elisp.el" directory))
         (function 'madrigal-test-persisted-function))
    (unwind-protect
        (progn
          (fmakunbound function)
          (let ((result (madrigal--persist-elisp
                         "(defun madrigal-test-persisted-function (value) (+ value 1))")))
            (should (eq (plist-get result :ok) t))
            (should (equal (plist-get result :file) madrigal-persistent-elisp-file))
            (should (= (funcall function 41) 42))
            (should (string-match-p (regexp-quote "madrigal-test-persisted-function")
                                    (with-temp-buffer
                                      (insert-file-contents madrigal-persistent-elisp-file)
                                      (buffer-string)))))
          (fmakunbound function)
          (madrigal--load-persistent-elisp)
          (should (= (funcall function 41) 42)))
      (when (fboundp function) (fmakunbound function))
      (delete-directory directory t))))

(ert-deftest madrigal-persist-elisp-replaces-existing-function-and-variable-definitions ()
  (let* ((directory (make-temp-file "madrigal-elisp-" t))
         (madrigal-persistent-elisp-file
          (expand-file-name "madrigal-persisted-elisp.el" directory))
         (function 'madrigal-test-persisted-redefinition)
         (variable 'madrigal-test-persisted-variable))
    (unwind-protect
        (progn
          (madrigal--persist-elisp
           "(defun madrigal-test-persisted-redefinition () 1)\n(defvar madrigal-test-persisted-variable 1)")
          (madrigal--persist-elisp
           "(defun madrigal-test-persisted-redefinition () 2)\n(defvar madrigal-test-persisted-variable 2)")
          (should (= (funcall function) 2))
          (should (= (symbol-value variable) 2))
          (should (= 2 (length (madrigal--persistent-elisp-forms))))
          (fmakunbound function)
          (makunbound variable)
          (madrigal--load-persistent-elisp)
          (should (= (funcall function) 2))
          (should (= (symbol-value variable) 2)))
      (when (fboundp function) (fmakunbound function))
      (when (boundp variable) (makunbound variable))
      (delete-directory directory t))))

(ert-deftest madrigal-persisted-elisp-introspection-lists-documents-and-bounds-source ()
  (let* ((directory (make-temp-file "madrigal-elisp-" t))
         (madrigal-persistent-elisp-file
          (expand-file-name "madrigal-persisted-elisp.el" directory))
         (function 'madrigal-test-persisted-introspection)
         (variable 'madrigal-test-persisted-introspection-variable))
    (unwind-protect
        (progn
          (madrigal--persist-elisp
           "(defun madrigal-test-persisted-introspection () \"Return an answer.\" 42)\n(defvar madrigal-test-persisted-introspection-variable 42 \"An answer.\")")
          (let* ((info (madrigal-persisted-elisp-info))
                 (definitions (plist-get (plist-get info :definitions) :items))
                 (source (madrigal-persisted-elisp-source function 20))
                 (help (madrigal-persisted-elisp-help function)))
            (should (equal (plist-get info :file) madrigal-persistent-elisp-file))
            (should (member '(:name madrigal-test-persisted-introspection :type function)
                            definitions))
            (should (member '(:name madrigal-test-persisted-introspection-variable :type variable)
                            definitions))
            (should (plist-get source :truncated))
            (should (string-match-p (regexp-quote "Return an answer.")
                                    (plist-get help :documentation)))))
      (when (fboundp function) (fmakunbound function))
      (when (boundp variable) (makunbound variable))
      (delete-directory directory t))))

(ert-deftest madrigal-persist-elisp-rejects-invalid-source-without-persisting ()
  (let* ((directory (make-temp-file "madrigal-elisp-" t))
         (madrigal-persistent-elisp-file
          (expand-file-name "madrigal-persisted-elisp.el" directory)))
    (unwind-protect
        (progn
          (should-error (madrigal--persist-elisp "(defun incomplete ()"))
          (should-not (file-exists-p madrigal-persistent-elisp-file)))
      (delete-directory directory t))))

(ert-deftest madrigal-maybe-activate-for-madrigal-file ()
  (with-temp-buffer
    (let ((madrigal-providers '(("Agent Provider" :provider 'provider)))
          (madrigal-agent-models '(("assistant" . ("Agent Provider" . "model")))))
      (setq buffer-file-name "/tmp/demo.madrigal.org")
      (org-mode)
      (madrigal-maybe-activate)
      (should madrigal-mode)
      (should (equal (madrigal-session-agent madrigal-session) "assistant"))
      (should (equal (madrigal-session-file madrigal-session)
                     "/tmp/demo.madrigal.org")))))

(ert-deftest madrigal-maybe-activate-for-keyword-marked-buffer ()
  (with-temp-buffer
    (let ((madrigal-providers '(("Agent Provider" :provider 'provider)))
          (madrigal-agent-models '(("assistant" . ("Agent Provider" . "model")))))
      (insert "#+TITLE: Demo\n#+MADRIGAL-AGENT: assistant\n")
      (setq default-directory "/tmp/project/")
      (org-mode)
      (madrigal-maybe-activate)
      (should madrigal-mode)
      (should (equal (madrigal-session-agent madrigal-session) "assistant"))
      (should (equal (madrigal-session-root madrigal-session) "/tmp/project/")))))

(ert-deftest madrigal-activate-babel-session-uses-request-heading ()
  (with-temp-buffer
    (let ((madrigal-providers '(("Agent Provider" :provider 'provider)))
          (madrigal-agent-models '(("babel-assistant" . ("Agent Provider" . "model")))))
      (org-mode)
      (madrigal--activate-session "babel-assistant" "/tmp/project/" nil)
      (should (string-match-p (regexp-quote "* Request\n") (buffer-string))))))

(ert-deftest madrigal-make-eval-tool-inserts-highlighted-src-blocks ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (org-mode)
    (let* ((tool (madrigal--make-eval-tool (current-buffer) "req-1"))
           (callback-result nil))
      (funcall (llm-tool-function tool)
               (lambda (value) (setq callback-result value))
               "(+ 1 2)")
      (should (string-match-p "\\* AI" (buffer-string)))
      (should (string-match-p "\\*\\* Tools" (buffer-string)))
      (should (string-match-p "\\*\\*\\* eval" (buffer-string)))
      (should (string-match-p "#\\+begin_src emacs-lisp" (buffer-string)))
      (should (string-match-p "#\\+RESULTS:" (buffer-string)))
      (should (string-match-p "(:ok t" callback-result))
      (goto-char (point-max))
      (org-back-to-heading t)
      (should (equal (org-get-heading t t t t) "eval"))
      (forward-line 1)
      (should (invisible-p (point)))
      (should (string-match-p "Tools"
                              (madrigal--visible-context-string))))))

(ert-deftest madrigal-make-eval-tool-renders-in-origin-buffer-after-buffer-switch ()
  (skip-unless (madrigal-llm-available-p))
  (let ((other (generate-new-buffer " *madrigal-test-other*")))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (let ((origin (current-buffer))
                (tool (madrigal--make-eval-tool (current-buffer) "req-1"))
                callback-result)
            (funcall (llm-tool-function tool)
                     (lambda (value) (setq callback-result value))
                     (format "(set-buffer %S) (+ 2 3)" (buffer-name other)))
            (should (eq (current-buffer) origin))
            (should (string-match-p "\\*\\*\\* eval" (buffer-string)))
            (should (string-match-p "(:ok t" callback-result))
            (with-current-buffer other
              (should-not (string-match-p "\\*\\*\\* eval" (buffer-string))))))
      (when (buffer-live-p other)
        (kill-buffer other)))))

(ert-deftest madrigal-make-eval-tool-respects-expanded-tool-entry-setting ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (org-mode)
    (let ((madrigal-collapse-tool-entries nil)
          (tool (madrigal--make-eval-tool (current-buffer) "req-1"))
          (callback-result nil))
      (funcall (llm-tool-function tool)
               (lambda (value) (setq callback-result value))
               "(+ 1 2)")
      (should (string-match-p "(:ok t" callback-result))
      (goto-char (point-min))
      (re-search-forward "^\\*\\* Tools$")
      (forward-line 1)
      (should-not (invisible-p (point))))))

(ert-deftest madrigal-make-eval-tool-groups-tool-entries-under-tools-heading ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (org-mode)
    (let ((tool (madrigal--make-eval-tool (current-buffer) "req-1")))
      (funcall (llm-tool-function tool) #'ignore "(+ 1 2)")
      (funcall (llm-tool-function tool) #'ignore "(+ 3 4)")
      (should (= 1 (how-many "^\\*\\* Tools$" (point-min) (point-max))))
      (should (= 2 (how-many "^\\*\\*\\* eval$" (point-min) (point-max)))))))

(ert-deftest madrigal-make-eval-tool-preserves-tools-heading-fold-state ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (org-mode)
    (let ((tool (madrigal--make-eval-tool (current-buffer) "req-1")))
      (funcall (llm-tool-function tool) #'ignore "(+ 1 2)")
      (goto-char (point-min))
      (re-search-forward "^\\*\\* Tools$")
      (let ((tools-pos (line-beginning-position)))
        (madrigal--fold-subtree-at tools-pos)
        (should (madrigal--subtree-folded-at-p tools-pos))
        (funcall (llm-tool-function tool) #'ignore "(+ 3 4)")
        (should (madrigal--subtree-folded-at-p tools-pos))))))

(ert-deftest madrigal-make-eval-tool-repairs-incomplete-source ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (org-mode)
    (let* ((tool (madrigal--make-eval-tool (current-buffer) "req-1"))
           (callback-result nil))
      (funcall (llm-tool-function tool)
               (lambda (value) (setq callback-result value))
               "(require 'json)\n(let ((x 1)) x")
      (should (string-match-p "(:ok t" callback-result))
      (should (string-match-p ":value 1" callback-result))
      (should (string-match-p (regexp-quote "(let ((x 1)) x)")
                              (buffer-string))))))

(ert-deftest madrigal-babel-tool-inserts-block-and-returns-fragment ()
  (with-temp-buffer
    (org-mode)
    (insert "#+MADRIGAL-AGENT: babel-assistant\n\n* Request\n#+begin_quote\nRun a block\n#+end_quote\n")
    (setq-local madrigal-session (madrigal-session-create :agent "babel-assistant"))
    (goto-char (point-min))
    (re-search-forward "^\\* Request$")
    (madrigal--store-request-turn-marker "req-1" (copy-marker (line-beginning-position) t))
    (let ((fragment nil)
          (madrigal-babel-allowed-languages '(emacs-lisp)))
      (madrigal--run-babel-tool
       "babel"
       (current-buffer)
       "req-1"
       (lambda (value) (setq fragment value))
       "#+name: add-two\n#+begin_src emacs-lisp :results value pp\n(+ 1 2)\n#+end_src")
      (should (string-match-p (regexp-quote "#+name: add-two") fragment))
      (should (string-match-p (regexp-quote "#+RESULTS: add-two") fragment))
      (should (string-match-p (regexp-quote ": 3") fragment))
      (should (string-match-p (regexp-quote fragment) (buffer-string))))))

(ert-deftest madrigal-babel-tool-rejects-disallowed-language ()
  (with-temp-buffer
    (org-mode)
    (insert "#+MADRIGAL-AGENT: babel-assistant\n\n* Request\n#+begin_quote\nRun a block\n#+end_quote\n")
    (setq-local madrigal-session (madrigal-session-create :agent "babel-assistant"))
    (goto-char (point-min))
    (re-search-forward "^\\* Request$")
    (madrigal--store-request-turn-marker "req-1" (copy-marker (line-beginning-position) t))
    (let ((madrigal-babel-allowed-languages '(emacs-lisp)))
      (should-error
       (madrigal--run-babel-tool
        "babel"
        (current-buffer)
        "req-1"
        #'ignore
        "#+begin_src shell\necho hi\n#+end_src")
       :type 'user-error))))

(ert-deftest madrigal-babel-preflight-check-reports-unbalanced-elisp ()
  (with-temp-buffer
    (let ((message (madrigal--babel-preflight-check "emacs-lisp" "(+ 1 2" (current-buffer))))
      (should (stringp message))
      (should (string-match-p
               (regexp-quote "Pre-execution check failed for emacs-lisp")
               message))
      (should (string-match-p
               (regexp-quote "line 1")
               message)))))

(ert-deftest madrigal-babel-lsp-diagnostics-python-finds-errors ()
  (skip-unless (and (featurep 'eglot)
                    (madrigal--babel-lsp-major-mode "python")))
  (let ((madrigal-babel-preflight-timeout 10.0)
        (diagnostics (madrigal--babel-lsp-diagnostics
                      "python"
                      "def f(:\n    pass\n"
                      default-directory)))
    (should diagnostics)
    (should (seq-some (lambda (diagnostic)
                        (plist-get diagnostic :message))
                      diagnostics))))

(ert-deftest madrigal-babel-lsp-diagnostics-rust-finds-errors ()
  (skip-unless (and (featurep 'eglot)
                    (madrigal--babel-lsp-major-mode "rust")))
  (let ((madrigal-babel-preflight-timeout 12.0)
        (tmpdir (make-temp-file "madrigal-rust-eglot-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "Cargo.toml" tmpdir)
            (insert "[package]\nname = \"madtest\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[[bin]]\nname = \"madtest\"\npath = \"main.rs\"\n"))
          (let ((diagnostics (madrigal--babel-lsp-diagnostics
                              "rust"
                              "fn main( {\n}\n"
                              tmpdir)))
            (should diagnostics)
            (should (seq-some (lambda (diagnostic)
                                (string-match-p
                                 (regexp-quote "unclosed delimiter")
                                 (plist-get diagnostic :message)))
                              diagnostics))))
      (delete-directory tmpdir t))))

(ert-deftest madrigal-babel-lsp-diagnostics-haskell-finds-errors ()
  (skip-unless (and (featurep 'eglot)
                    (require 'haskell-mode nil t)
                    (madrigal--babel-lsp-major-mode "haskell")))
  (let ((madrigal-babel-preflight-timeout 12.0)
        (tmpdir (make-temp-file "madrigal-haskell-eglot-" t)))
    (unwind-protect
        (let ((diagnostics (madrigal--babel-lsp-diagnostics
                            "haskell"
                            "main = do\n  where\n"
                            tmpdir)))
          (should diagnostics)
          (should (seq-some (lambda (diagnostic)
                              (string-match-p
                               (regexp-quote "Empty 'do' block")
                               (plist-get diagnostic :message)))
                            diagnostics)))
      (delete-directory tmpdir t))))

(ert-deftest madrigal-babel-tool-reports-preflight-errors-before-execution ()
  (with-temp-buffer
    (org-mode)
    (insert "#+MADRIGAL-AGENT: babel-assistant\n\n* Request\n#+begin_quote\nRun a block\n#+end_quote\n")
    (setq-local madrigal-session (madrigal-session-create :agent "babel-assistant"))
    (goto-char (point-min))
    (re-search-forward "^\\* Request$")
    (madrigal--store-request-turn-marker "req-1" (copy-marker (line-beginning-position) t))
    (let ((madrigal-babel-allowed-languages '(python))
          (result nil))
      (cl-letf (((symbol-function 'madrigal--babel-preflight-check)
                 (lambda (_language _source _buffer)
                   "Pre-execution check failed for python."))
                ((symbol-function 'org-babel-execute-src-block)
                 (lambda ()
                   (ert-fail "Should not execute a block that fails preflight"))))
        (madrigal--run-babel-tool
         "babel"
         (current-buffer)
         "req-1"
         (lambda (value) (setq result value))
         "#+begin_src python\nprint(1)\n#+end_src")
        (should (string-match-p
                 (regexp-quote "Pre-execution check failed for python.")
                 result))
        (should-not (string-match-p
                     (regexp-quote "#+begin_src python")
                     (buffer-string)))))))

(ert-deftest madrigal-babel-tool-removes-failed-block-and-returns-error ()
  (with-temp-buffer
    (org-mode)
    (insert "#+MADRIGAL-AGENT: babel-assistant\n\n* Request\n#+begin_quote\nRun a block\n#+end_quote\n")
    (setq-local madrigal-session (madrigal-session-create :agent "babel-assistant"))
    (goto-char (point-min))
    (re-search-forward "^\\* Request$")
    (madrigal--store-request-turn-marker "req-1" (copy-marker (line-beginning-position) t))
    (let ((madrigal-babel-allowed-languages '(python))
          (result nil))
      (cl-letf (((symbol-function 'org-babel-execute-src-block)
                 (lambda ()
                   (error "boom"))))
        (madrigal--run-babel-tool
         "babel"
         (current-buffer)
         "req-1"
         (lambda (value) (setq result value))
         "#+begin_src python\nprint(1)\n#+end_src")
        (should (string-match-p
                 (regexp-quote "Execution failed for python")
                 result))
        (should (string-match-p
                 (regexp-quote "Revise the src block and call the babel tool again")
                 result))
        (should-not (string-match-p
                     (regexp-quote "#+begin_src python")
                     (buffer-string)))
        (should-not (string-match-p
                     (regexp-quote "#+RESULTS:")
                     (buffer-string)))))))

(ert-deftest madrigal-babel-tool-removes-block-when-babel-error-buffer-has-output ()
  (with-temp-buffer
    (org-mode)
    (insert "#+MADRIGAL-AGENT: babel-assistant\n\n* Request\n#+begin_quote\nRun a block\n#+end_quote\n")
    (setq-local madrigal-session (madrigal-session-create :agent "babel-assistant"))
    (goto-char (point-min))
    (re-search-forward "^\\* Request$")
    (madrigal--store-request-turn-marker "req-1" (copy-marker (line-beginning-position) t))
    (let ((madrigal-babel-allowed-languages '(python))
          (result nil))
      (cl-letf (((symbol-function 'org-babel-execute-src-block)
                 (lambda ()
                   (insert "\n#+RESULTS:\n: /tmp/out.png\n")
                   (with-current-buffer (get-buffer-create org-babel-error-buffer-name)
                     (erase-buffer)
                     (insert "Traceback: plotting failed")))))
        (madrigal--run-babel-tool
         "babel"
         (current-buffer)
         "req-1"
         (lambda (value) (setq result value))
         "#+begin_src python :results file graphics :file /tmp/out.png\nprint(1)\n#+end_src")
        (should (string-match-p
                 (regexp-quote "Execution failed for python")
                 result))
        (should (string-match-p
                 (regexp-quote "Traceback: plotting failed")
                 result))
        (should-not (string-match-p
                     (regexp-quote "#+begin_src python")
                     (buffer-string)))
        (should-not (string-match-p
                     (regexp-quote "#+RESULTS:")
                     (buffer-string)))))))


(ert-deftest madrigal-babel-language-allowed-p-defaults-to-enabled-languages ()
  (let ((madrigal-babel-allowed-languages nil)
        (org-babel-load-languages '((emacs-lisp . t)
                                    (python . t)
                                    (shell . nil))))
    (should (madrigal--babel-language-allowed-p "python"))
    (should (madrigal--babel-language-allowed-p "emacs-lisp"))
    (should-not (madrigal--babel-language-allowed-p "shell"))))

(ert-deftest madrigal-sanitize-elisp-error-summarizes-large-values ()
  (let* ((raw '(wrong-type-argument ement-session
                                    ("@niall" . huge-session-object)))
         (sanitized (madrigal--sanitize-elisp-error raw)))
    (should (equal (car sanitized) 'wrong-type-argument))
    (should (equal (cadr sanitized) 'ement-session))
    (should (equal (caaddr sanitized) :type))
    (should (equal (plist-get (caddr sanitized) :type) 'cons))))

(ert-deftest madrigal-function-help-includes-signature-and-doc-summary ()
  (let ((help (madrigal-function-help 'madrigal-session-state-put)))
    (should (eq 'function (plist-get help :type)))
    (should (equal 'madrigal-session-state-put (plist-get help :name)))
    (should (equal "(madrigal-session-state-put KEY VALUE)"
                   (plist-get help :signature)))
    (should (equal "Persist VALUE under KEY in the current Madrigal session state."
                   (plist-get help :summary)))))

(ert-deftest madrigal-function-search-help-returns-help-text ()
  (let* ((help (madrigal-function-search-help "^madrigal-function-help$"))
         (items (plist-get help :items)))
    (should (eq 'function-search (plist-get help :type)))
    (should (equal "^madrigal-function-help$" (plist-get help :regex)))
    (should (= 1 (length items)))
    (should (equal 'madrigal-function-help
                   (plist-get (car items) :name)))))

(ert-deftest madrigal-variable-help-includes-doc-summary ()
  (let ((help (madrigal-variable-help 'madrigal-excluded-context-tag)))
    (should (eq 'variable (plist-get help :type)))
    (should (equal 'madrigal-excluded-context-tag (plist-get help :name)))
    (should (equal "Tag marking Org subtrees that should be omitted from LLM context."
                   (plist-get help :summary)))))

(ert-deftest madrigal-variable-search-help-returns-help-text ()
  (let* ((help (madrigal-variable-search-help "^madrigal-excluded-context-tag$"))
         (items (plist-get help :items)))
    (should (eq 'variable-search (plist-get help :type)))
    (should (= 1 (length items)))
    (should (equal 'madrigal-excluded-context-tag
                   (plist-get (car items) :name)))))

(ert-deftest madrigal-public-help-ignores-internal-bindings ()
  (should-error (madrigal-function-help 'madrigal--function-symbol))
  (should-error (madrigal-variable-help 'madrigal--eval-origin-buffer))
  (should (equal nil
                 (plist-get (madrigal-function-search-help "^madrigal--function-symbol$")
                            :items)))
  (should (equal nil
                 (plist-get (madrigal-variable-search-help "^madrigal--eval-origin-buffer$")
                            :items))))

(ert-deftest madrigal-feature-help-includes-function-and-variable-help ()
  (let* ((help (madrigal-feature-help 'madrigal-tool-eval-prelude))
         (functions (plist-get help :functions))
         (variables (plist-get help :variables))
         (names (mapcar (lambda (item) (plist-get item :name)) functions)))
    (should (eq 'feature (plist-get help :type)))
    (should (equal 'madrigal-tool-eval-prelude (plist-get help :name)))
    (should (member 'madrigal-function-help names))
    (should (member 'madrigal-function-search-help names))
    (should (member 'madrigal-feature-help names))
    (should (member 'madrigal-session-state-put names))
    (should-not (member 'madrigal--eval-origin-buffer
                        (mapcar (lambda (item) (plist-get item :name)) variables)))))

(ert-deftest madrigal-package-help-includes-summary-and-feature-help ()
  (cl-letf (((symbol-function 'madrigal--package-summary)
             (lambda (_package) "Demo package"))
            ((symbol-function 'madrigal--package-features)
             (lambda (_package) '(madrigal-tool-eval-prelude))))
    (let* ((help (madrigal-package-help 'demo))
           (features (plist-get help :features)))
      (should (eq 'package (plist-get help :type)))
      (should (equal 'demo (plist-get help :name)))
      (should (equal "Demo package" (plist-get help :summary)))
      (should (= 1 (length features)))
      (should (equal 'madrigal-tool-eval-prelude
                     (plist-get (car features) :name))))))

(ert-deftest madrigal-library-provides-reads-provided-features ()
  (let ((file (make-temp-file "madrigal-provides-" nil ".el"
                              ";;; demo.el --- Demo\n(provide 'demo-one)\n(provide 'demo-two)\n")))
    (unwind-protect
        (should (equal (madrigal--library-provides file)
                       '(demo-one demo-two)))
      (delete-file file))))

(ert-deftest madrigal-package-el-files-finds-source-files ()
  (let ((dir (make-temp-file "madrigal-package-" t)))
    (unwind-protect
        (progn
          (madrigal-test--write-file (expand-file-name "demo.el" dir) ";;; demo.el --- Demo\n")
          (madrigal-test--write-file (expand-file-name "demo-autoloads.el" dir) "")
          (madrigal-test--write-file (expand-file-name "demo-pkg.el" dir) "")
          (cl-letf (((symbol-function 'madrigal--package-directory)
                     (lambda (_package) dir)))
            (should (equal (madrigal--package-el-files 'demo)
                           (list (expand-file-name "demo.el" dir))))))
      (delete-directory dir t))))

(ert-deftest madrigal-bind-eval-error-uses-unique-generated-name-without-bind ()
  (with-temp-buffer
    (let ((first (madrigal--bind-eval-error '(error one)))
          (second (madrigal--bind-eval-error '(error two))))
      (should (not (eq first second)))
      (should (local-variable-p first))
      (should (local-variable-p second))
      (should (equal (symbol-value first) '(error one)))
      (should (equal (symbol-value second) '(error two))))))

(ert-deftest madrigal-session-state-persists-across-buffers-within-one-eval ()
  (with-temp-buffer
    (let ((origin (current-buffer)))
      (with-temp-buffer
        (let ((madrigal--eval-origin-buffer origin))
          (madrigal-session-state-put :answer 42)
          (should (= 42 (madrigal-session-state-get :answer)))))
      (with-current-buffer origin
        (should (hash-table-p madrigal--state))
        (should (= 42 (gethash :answer madrigal--state)))))))

(ert-deftest madrigal-eval-tool-session-state-persists-across-calls ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (org-mode)
    (let ((tool (madrigal--make-eval-tool (current-buffer) "req-1"))
          (first-result nil)
          (second-result nil))
      (funcall (llm-tool-function tool)
               (lambda (value) (setq first-result value))
               "(with-temp-buffer (madrigal-session-state-put :saved 42))")
      (funcall (llm-tool-function tool)
               (lambda (value) (setq second-result value))
               "(with-temp-buffer (madrigal-session-state-get :saved))")
      (should (string-match-p ":value 42" first-result))
      (should (string-match-p ":value 42" second-result))
      (should (= 42 (gethash :saved madrigal--state))))))

(ert-deftest madrigal-eval-tool-description-uses-reflection-entry-points ()
  (let ((description (madrigal--eval-tool-description)))
    (should-not (string-match-p (regexp-quote "Installed Packages") description))
    (should-not (string-match-p
                 (regexp-quote "Feature =madrigal-tool-eval-prelude=")
                 description))
    (dolist (function '(madrigal-runtime-info
                        madrigal-project-info
                        madrigal-package-search
                        madrigal-feature-search
                        madrigal-symbol-search
                        madrigal-function-help
                        madrigal-variable-help
                        madrigal-key-binding-help
                        madrigal-mode-help
                        madrigal-focus-buffer-text
                        madrigal-do-context
                        madrigal-do-turn-history
                        madrigal-do-tool-history
                        madrigal-do-tool-result-history))
      (should (string-match-p (regexp-quote (format "=%s=" function))
                              description)))))

(ert-deftest madrigal-babel-tool-description-includes-emacs-lisp-runtime ()
  (let ((madrigal-babel-allowed-languages '(emacs-lisp))
        (description (madrigal--babel-tool-description)))
    (should (string-match-p
             (regexp-quote "*** Emacs Lisp")
             description))
    (should-not (string-match-p
                 (regexp-quote "*** Emacs Lisp via Org Babel")
                 description))
    (should (string-match-p
             (regexp-quote "**** Emacs Version")
             description))
    (should (string-match-p
             (regexp-quote "**** Installed Packages")
             description))
    (should-not (string-match-p
                 (regexp-quote "current Madrigal session buffer through Org Babel")
                 description))
    (should-not (string-match-p
                 (regexp-quote "madrigal-tool-eval-prelude")
                 description))
    (should-not (string-match-p
                 (regexp-quote "madrigal-session-state-put")
                 description))))

(ert-deftest madrigal-babel-tool-description-includes-python-runtime-headings ()
  (let* ((madrigal-babel-allowed-languages '(python))
         (description (madrigal--babel-tool-description)))
    (should (string-match-p
             (regexp-quote "Do not invoke supported interpreters from shell blocks, and do not invoke shell commands from other language blocks")
             description))
    (should (string-match-p
             (regexp-quote "*** Python")
             description))
    (should (string-match-p
             (regexp-quote "**** Python Version")
             description))
    (should (string-match-p
             (regexp-quote "**** Installed Packages")
             description))
    (should (string-match-p
             (regexp-quote "Use :results output only for printed stdout")
             description))
    (should (string-match-p
             (regexp-quote "For :results graphics file, either return a matplotlib Figure object or create the plot and let Org save the current figure")
             description))
    (should (string-match-p
             (regexp-quote "Use the most direct runtime or interface for each stage of the task")
             description))
    (should (string-match-p
             (regexp-quote "keep results small")
             description))
    (should (string-match-p
             (regexp-quote "validate each intermediate result before continuing")
             description))
    (should (string-match-p
             (regexp-quote "Avoid affecting the user's session unless the task requires persistent changes")
             description))))

(ert-deftest madrigal-babel-tool-description-includes-mcp-servers-when-allowed ()
  (cl-progv
      '(mcp-hub-servers)
      '((("filesystem"
          :command "npx"
          :args ("-y" "@modelcontextprotocol/server-filesystem" "/tmp")
          :description "Read and write files.\nScoped to /tmp.")
         ("remote"
          :url "https://api.example.invalid/mcp")))
    (let* ((madrigal-babel-allowed-languages '(mcp))
           (description (madrigal--babel-tool-description)))
      (should (string-match-p
               (regexp-quote "*** MCP")
               description))
      (should (string-match-p
               (regexp-quote "**** Available Servers")
               description))
      (should (string-match-p
               (regexp-quote "- =filesystem= :: Read and write files. Scoped to /tmp.")
               description))
      (should (string-match-p
               (regexp-quote "- =remote=")
               description))
      (should (string-match-p
               (regexp-quote "starts the selected server lazily")
               description))
      (should-not (string-match-p
                   (regexp-quote "@modelcontextprotocol/server-filesystem")
                   description))
      (should-not (string-match-p
                   (regexp-quote "https://api.example.invalid/mcp")
                   description)))))

(ert-deftest madrigal-babel-tool-description-includes-shell-guidance-when-allowed ()
  (let* ((madrigal-babel-allowed-languages '(shell python))
         (description (madrigal--babel-tool-description)))
    (should (string-match-p
             (regexp-quote "*** Shell")
             description))
    (should (string-match-p
             (regexp-quote "Use shell only for genuine shell tasks")
             description))
    (should (< (string-match-p
                (regexp-quote "Do not invoke supported interpreters from shell blocks, and do not invoke shell commands from other language blocks")
                description)
               (string-match-p (regexp-quote "The argument must be literal Org") description)))
    (should (< (string-match-p (regexp-quote "*** Shell") description)
               (string-match-p (regexp-quote "*** Python") description)))))

(ert-deftest madrigal-babel-assistant-system-prompt-is-minimal ()
  (let ((prompt (madrigal--agent-system-prompt "babel-assistant")))
    (should (string-match-p
             (regexp-quote "Reply in Org mode only")
             prompt))
    (should (string-match-p
             (regexp-quote "Do NOT invoke python or other babel languages from a shell script")
             prompt))
    (should (string-match-p
             (regexp-quote "Final natural-language responses must begin with a top-level Org heading")
             prompt))
    (should-not (string-match-p
                 (regexp-quote "src block")
                 prompt))))

(ert-deftest madrigal-submit-sends-full-buffer-and-appends-response ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* User\nHello\n")
    (let ((captured-prompt nil)
          (madrigal-session (madrigal-session-create
                             :agent "assistant"
                             :provider 'fake-provider
                             :model "fake-model"
                             :root "/tmp/project/"
                             :buffer (current-buffer))))
      (madrigal-mode 1)
      (cl-letf (((symbol-function 'llm-chat-async)
                 (lambda (_provider prompt response-callback _error-callback &optional _multi-output)
                   (setq captured-prompt prompt)
                   (funcall response-callback '(:text "* Assistant\nHi from the model"))
                   'fake-request)))
        (madrigal-submit)
        (should (string-match-p "\\* AI" (buffer-string)))
        (should (string-match-p "Hi from the model" (buffer-string)))
        (should (madrigal--last-user-heading-empty-p))
        (should (= (point) (point-max)))
        (should (string-match-p "You are Madrigal"
                                (llm-chat-prompt-context captured-prompt)))
        (should (equal "Hello"
                       (llm-chat-prompt-interaction-content
                        (car (last (llm-chat-prompt-interactions captured-prompt))))))))))

(ert-deftest madrigal-submit-continues-after-tool-use ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* User\nCompute something\n")
    (let ((call-count 0)
          (madrigal-session (madrigal-session-create
                             :provider 'fake-provider
                             :model "fake-model"
                             :root "/tmp/project/"
                             :buffer (current-buffer))))
      (madrigal-mode 1)
      (cl-letf (((symbol-function 'llm-chat-async)
                 (lambda (_provider _prompt response-callback _error-callback &optional _multi-output)
                   (setq call-count (1+ call-count))
                   (pcase call-count
                     (1 (funcall response-callback
                                 '(:text "Checking..."
                                   :tool-uses ((:name "eval" :args ((source . "(+ 20 22)"))))
                                   :tool-results (("eval" . "(:ok t :value 42)")))))
                     (2 (funcall response-callback '(:text "* Assistant\n42"))))
                   (format "fake-request-%d" call-count))))
        (madrigal-submit)
        (sleep-for 0.01)
        (should (= call-count 2))
        (should (string-match-p "\\* AI" (buffer-string)))
        (should (string-match-p "\\*\\* Note" (buffer-string)))
        (should (string-match-p "Checking\.\.\." (buffer-string)))
        (should (string-match-p "\\*\\* Response" (buffer-string)))
        (should (string-match-p "42" (buffer-string)))
        (should (madrigal--last-user-heading-empty-p))
        (should (= (point) (point-max)))
        (should-not madrigal--pending-requests)))))

(ert-deftest madrigal-submit-finishes-when-response-has-no-tool-uses ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* User\nCompute something\n")
    (let ((call-count 0)
          (madrigal-session (madrigal-session-create
                             :provider 'fake-provider
                             :model "fake-model"
                             :root "/tmp/project/"
                             :buffer (current-buffer))))
      (madrigal-mode 1)
      (cl-letf (((symbol-function 'llm-chat-async)
                 (lambda (_provider _prompt response-callback _error-callback &optional _multi-output)
                   (setq call-count (1+ call-count))
                   (funcall response-callback
                            '(:text "Done"
                              :tool-results (("eval" . "(:ok t :value 42)"))))
                   (format "fake-request-%d" call-count))))
        (madrigal-submit)
        (should (= call-count 1))
        (should (string-match-p "\\*\\* Response" (buffer-string)))
        (should (madrigal--last-user-heading-empty-p))
        (should-not madrigal--pending-requests)))))

(ert-deftest madrigal-submit-babel-rewrites-request-heading ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: babel-assistant\n\n* Request\nHello from /user/\n")
    (let ((madrigal-session (madrigal-session-create
                             :agent "babel-assistant"
                             :provider 'fake-provider
                             :model "fake-model"
                             :root "/tmp/project/"
                             :buffer (current-buffer))))
      (madrigal-mode 1)
      (cl-letf (((symbol-function 'llm-chat-async)
                 (lambda (_provider _prompt response-callback _error-callback &optional _multi-output)
                   (funcall response-callback '(:text "* Summary heading\nFinal reply"))
                   'fake-request)))
        (madrigal-submit)
        (should (string-match-p (regexp-quote "* Summary heading") (buffer-string)))
        (should (string-match-p (regexp-quote "#+begin_quote\nHello from /user/")
                                (buffer-string)))
        (should (string-match-p (regexp-quote "Final reply") (buffer-string)))
        (should (string-match-p (regexp-quote "* Request\n") (buffer-string)))))))

(ert-deftest madrigal-submit-babel-leaves-raw-request-until-response ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: babel-assistant\n\n* Request\nHello from /user/\n")
    (let ((madrigal-session (madrigal-session-create
                             :agent "babel-assistant"
                             :provider 'fake-provider
                             :model "fake-model"
                             :root "/tmp/project/"
                             :buffer (current-buffer))))
      (madrigal-mode 1)
      (cl-letf (((symbol-function 'madrigal-agent-controller-submit-async)
                 (lambda (&rest _args) 'fake-handle)))
        (madrigal-submit)
        (should-not (string-match-p (regexp-quote "#+begin_quote") (buffer-string)))
        (should (string-match-p (regexp-quote "* Request\nHello from /user/")
                                (buffer-string)))))))

(ert-deftest madrigal-current-user-turn-uses-last-user-heading ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n\n* User\nFirst\n* Assistant\nReply\n* User\nSecond\n")
    (should (equal (madrigal--current-user-turn) "Second"))))

(ert-deftest madrigal-visible-context-string-omits-excluded-subtrees ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n\n* User\nKeep this\n* Secret :exclude:\nHide this\n** Nested\nHide this too\n* Assistant\nKeep this too\n")
    (let ((context (madrigal--visible-context-string)))
      (should (string-match-p "Keep this" context))
      (should (string-match-p "Keep this too" context))
      (should-not (string-match-p "Hide this" context)))))

(ert-deftest madrigal-visible-context-string-preserves-heading-after-exclusion ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* Previous contexts :exclude:\n** Context 1\nArchived\n* Context\nSummary\n* User\n")
    (let ((context (madrigal--visible-context-string)))
      (should (string-match-p (regexp-quote "* Context\nSummary") context))
      (should (string-match-p (regexp-quote "* User") context)))))

(ert-deftest madrigal-view-context-renders-prompt-components-in-org-buffer ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* User\nHello\n")
    (let ((madrigal-session (madrigal-session-create
                             :agent "assistant"
                             :provider 'fake-provider
                             :model "fake-model"
                             :root "/tmp/project/"
                             :buffer (current-buffer)))
          (shown-buffer nil))
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer-or-name &rest _args)
                   (setq shown-buffer (get-buffer buffer-or-name)))))
        (let ((buffer (madrigal-view-context)))
          (should (buffer-live-p buffer))
          (should (eq buffer shown-buffer))
          (with-current-buffer buffer
            (should (derived-mode-p 'org-mode))
            (should buffer-read-only)
            (should (string-match-p (regexp-quote "#+TITLE: Madrigal Context")
                                    (buffer-string)))
            (should (string-match-p (regexp-quote "* System Prompt")
                                    (buffer-string)))
            (should (string-match-p (regexp-quote "* Chat History")
                                    (buffer-string)))
            (should (string-match-p (regexp-quote "** User")
                                    (buffer-string)))
            (should (string-match-p (regexp-quote "* Tools")
                                    (buffer-string)))
            (should (string-match-p (regexp-quote "** eval")
                                    (buffer-string)))
            (should (< (string-match-p (regexp-quote "* Tools") (buffer-string))
                       (string-match-p (regexp-quote "* Chat History") (buffer-string))))
            (should (string-match-p (regexp-quote "Hello")
                                    (buffer-string)))))))))

(ert-deftest madrigal-context-size-reports-tokens-and-percent ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* User\nHello\n")
    (let ((madrigal-session (madrigal-session-create
                             :provider 'fake-provider
                             :model "fake-model"
                             :root "/tmp/project/"
                             :buffer (current-buffer))))
      (cl-letf (((symbol-function 'llm-count-tokens)
                 (lambda (_provider text)
                   (length text)))
                ((symbol-function 'llm-chat-token-limit)
                 (lambda (_provider) 1000)))
        (let ((info (madrigal-context-size 'fake-provider)))
          (should (integerp (plist-get info :tokens)))
          (should (= 1000 (plist-get info :limit)))
          (should (numberp (plist-get info :percent))))))))

(ert-deftest madrigal-compactable-body-string-uses-current-buffer-context ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* User\nHello\n* Secret :exclude:\nHide me\n")
    (let ((body (madrigal--compactable-body-string)))
      (should (string-match-p "Hello" body))
      (should-not (string-match-p "Hide me" body)))))

(ert-deftest madrigal-summary-source-string-includes-existing-context-subtree ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* Context\nSummary\n\n* User\nHello\n* AI\nReply\n* User\n")
    (let ((body (madrigal--summary-source-string)))
      (should (string-match-p "Summary" body))
      (should (string-match-p "Hello" body)))))

(ert-deftest madrigal-archive-body-string-includes-existing-context-subtree ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* Context\nSummary\n\n* User\nHello\n* AI\nReply\n* User\n")
    (let ((body (madrigal--archive-body-string)))
      (should (string-match-p "Summary" body))
      (should (string-match-p "\\* User" body))
      (should (string-match-p "Hello" body)))))

(ert-deftest madrigal-update-mode-line-status-formats-context-usage ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* User\nHello\n")
    (let ((madrigal-session (madrigal-session-create
                             :provider 'fake-provider
                             :model "fake-model"
                             :root "/tmp/project/"
                             :buffer (current-buffer))))
      (cl-letf (((symbol-function 'madrigal-context-size)
                 (lambda (&optional _provider)
                   '(:tokens 250000 :limit 1000000 :percent 25.0))))
        (madrigal-update-mode-line-status)
        (should (equal madrigal--mode-line-status "🧠 250k ◑"))
        (setq madrigal--pending-requests
              (list (madrigal-request-create :id "req-1" :llm-request nil)))
        (madrigal-update-mode-line-status)
        (should (equal madrigal--mode-line-status "⌛ 250k ◑"))))))

(ert-deftest madrigal-mode-adds-context-status-to-mode-line-misc-info ()
  (with-temp-buffer
    (org-mode)
    (let ((madrigal-session (madrigal-session-create
                             :provider 'fake-provider
                             :model "fake-model"
                             :root "/tmp/project/"
                             :buffer (current-buffer)))
          (mode-line-misc-info '(foo bar)))
      (madrigal-mode 1)
      (should (equal madrigal--mode-line-construct (car mode-line-misc-info)))
      (should (member madrigal--mode-line-construct mode-line-misc-info))
      (madrigal-mode -1)
      (should-not (member madrigal--mode-line-construct mode-line-misc-info)))))

(ert-deftest madrigal-compaction-summary-prompt-includes-target-proportion ()
  (let ((madrigal-compact-context-target-proportion 0.3)
        (madrigal-context-summary-prompt "Base prompt"))
    (should (equal (madrigal--compaction-summary-prompt)
                   "Base prompt\nAim for the resulting prompt to use about 30% of the model context window."))))

(ert-deftest madrigal-build-compaction-prompt-separates-instructions-from-source ()
  (let* ((madrigal-context-summary-prompt "Base prompt")
         (prompt (madrigal--build-compaction-prompt "* Context\nHello")))
    (should (equal (llm-chat-prompt-context prompt)
                   "Base prompt\nAim for the resulting prompt to use about 25% of the model context window."))
    (should (string-match-p "SESSION CONTEXT TO SUMMARIZE"
                            (llm-chat-prompt-interaction-content
                             (car (llm-chat-prompt-interactions prompt)))))
    (should (string-match-p (regexp-quote "* Context\nHello")
                            (llm-chat-prompt-interaction-content
                             (car (llm-chat-prompt-interactions prompt)))))))

(ert-deftest madrigal-human-number-formats-compactly ()
  (should (equal (madrigal--human-number 1) "1"))
  (should (equal (madrigal--human-number 10) "10"))
  (should (equal (madrigal--human-number 150) "150"))
  (should (equal (madrigal--human-number 1000) "1k"))
  (should (equal (madrigal--human-number 250000) "250k")))

(ert-deftest madrigal-maybe-auto-compact-context-triggers-at-threshold ()
  (with-temp-buffer
    (org-mode)
    (let ((madrigal-mode t)
          (madrigal-session (madrigal-session-create :provider 'fake-provider))
          (madrigal-auto-compact-context-threshold 0.8)
          (called nil))
      (cl-letf (((symbol-function 'madrigal-context-size)
                 (lambda (&optional _provider)
                   '(:tokens 800 :limit 1000 :percent 80.0)))
                ((symbol-function 'madrigal-compact-context)
                 (lambda () (setq called t))))
        (madrigal--maybe-auto-compact-context)
        (should called)))))

(ert-deftest madrigal-rewrite-compacted-buffer-archives-and-resets-context ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* User\nHello\n* Assistant\nHi\n")
    (madrigal--rewrite-compacted-buffer "* User\nHello\n* Assistant\nHi" "Summary")
    (should (string-match-p "\\* Previous contexts :exclude:" (buffer-string)))
    (should (string-match-p "\\*\\* Context 1" (buffer-string)))
    (should (string-match-p "\\*\\*\\* User" (buffer-string)))
    (should (string-match-p "\\*\\*\\* Assistant" (buffer-string)))
    (should (string-match-p (regexp-quote "* Context\nSummary") (buffer-string)))
    (goto-char (point-min))
    (re-search-forward "^\\* Previous contexts")
    (forward-line 1)
    (should (invisible-p (point)))
    (should (madrigal--last-user-heading-empty-p))))

(ert-deftest madrigal-rewrite-compacted-babel-buffer-keeps-input-outside-archive ()
  (with-temp-buffer
    (org-mode)
    (insert "#+MADRIGAL-AGENT: babel-assistant\n\n* Response\n#+begin_quote\nOld request\n#+end_quote\nOld response\n")
    (let ((madrigal-session (madrigal-session-create :agent "babel-assistant")))
      (madrigal--rewrite-compacted-buffer
       "* Response\n#+begin_quote\nOld request\n#+end_quote\nOld response"
       "Summary")
      (goto-char (point-max))
      (insert "New request")
      (should (equal (madrigal--current-user-turn-babel) "New request"))
      (should-not (string-match-p "Old request"
                                  (madrigal--current-user-turn-babel))))))

(ert-deftest madrigal-rewrite-compacted-buffer-suppresses-context-updates ()
  (with-temp-buffer
    (org-mode)
    (insert "* User\nHello\n")
    (let ((madrigal-mode t)
          (updates 0))
      (cl-letf (((symbol-function 'madrigal-update-mode-line-status)
                 (lambda () (setq updates (1+ updates)))))
        (add-hook 'after-change-functions #'madrigal--after-change nil t)
        (unwind-protect
            (madrigal--rewrite-compacted-buffer "* User\nHello" "Summary")
          (remove-hook 'after-change-functions #'madrigal--after-change t)))
      (should (zerop updates)))))

(ert-deftest madrigal-fold-subtree-at-folds-only-target-subtree ()
  (with-temp-buffer
    (org-mode)
    (insert "* Keep\nVisible\n* Hide :exclude:\nHidden\n* Also hide :exclude:\nHidden too\n")
    (goto-char (point-min))
    (re-search-forward "^\\* Hide")
    (let ((hide-pos (line-beginning-position)))
      (madrigal--fold-subtree-at hide-pos))
    (forward-line 1)
    (should (invisible-p (point)))
    (re-search-forward "^\\* Also hide")
    (forward-line 1)
    (should-not (invisible-p (point)))))

(ert-deftest madrigal-rewrite-compacted-buffer-refolds-previous-contexts-when-needed ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* Previous contexts :exclude:\n** Context 1\n*** User\nOld\n\n* Context\nSummary\n\n* User\nNew\n")
    (goto-char (point-min))
    (re-search-forward "^\\* Previous contexts")
    (let ((start (line-beginning-position)))
      (madrigal--fold-subtree-at start))
    (madrigal--rewrite-compacted-buffer "* User\nNew" "Summary 2")
    (goto-char (point-min))
    (re-search-forward "^\\* Previous contexts")
    (forward-line 1)
    (should (invisible-p (point)))))

(ert-deftest madrigal-rewrite-compacted-buffer-keeps-previous-contexts-open-when-open ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* Previous contexts :exclude:\n** Context 1\n*** User\nOld\n\n* Context\nSummary\n\n* User\nNew\n")
    (madrigal--rewrite-compacted-buffer "* User\nNew" "Summary 2")
    (goto-char (point-min))
    (re-search-forward "^\\* Previous contexts")
    (forward-line 1)
    (should-not (invisible-p (point)))))

(ert-deftest madrigal-replace-context-placeholder-normalizes-summary-and-keeps-user ()
  (with-temp-buffer
    (org-mode)
    (insert "* Context\nSummarizing context...\n\n* User\n")
    (madrigal--replace-context-placeholder "* User- Summary line\n- Detail")
    (should (equal (buffer-string)
                   "* Context\nSummary line\n- Detail\n\n* User\n"))))

(ert-deftest madrigal-append-assistant-text-nests-model-headings ()
  (with-temp-buffer
    (org-mode)
    (madrigal--append-assistant-text "req-1" "* Response\nHello\n** Detail")
    (should (string-match-p "\\* AI" (buffer-string)))
    (should (string-match-p "\\*\\* Note" (buffer-string)))
    (should (string-match-p "\\*\\*\\* Response" (buffer-string)))
    (should (string-match-p "\\*\\*\\*\\* Detail" (buffer-string)))))

(ert-deftest madrigal-append-assistant-text-aligns-org-tables ()
  (with-temp-buffer
    (org-mode)
    (madrigal--append-assistant-text
     "req-1"
     "| a | bb|\n|1| 2 |")
    (sleep-for 0.01)
    (should (string-match-p (regexp-quote "| a | bb |") (buffer-string)))
    (should (string-match-p (regexp-quote "| 1 |  2 |") (buffer-string)))))

(ert-deftest madrigal-submit-does-not-prompt-and-reuses-open-user-turn ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* User\nHello there\n")
    (let ((madrigal-session (madrigal-session-create
                             :provider 'fake-provider
                             :model "fake-model"
                             :root "/tmp/project/"
                             :buffer (current-buffer)))
          (read-string-called nil))
      (madrigal-mode 1)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _args)
                   (setq read-string-called t)
                   "should not be used"))
                ((symbol-function 'llm-chat-async)
                 (lambda (_provider _prompt response-callback _error-callback &optional _multi-output)
                   (funcall response-callback '(:text "Done"))
                   'fake-request)))
        (call-interactively #'madrigal-submit)
        (should-not read-string-called)
        (should (string-match-p "Hello there" (buffer-string)))))))

(ert-deftest madrigal-ctrl-c-ctrl-c-submits-in-active-user-turn ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* User\nHello there\n")
    (let ((madrigal-session (madrigal-session-create
                             :provider 'fake-provider
                             :model "fake-model"
                             :root "/tmp/project/"
                             :buffer (current-buffer)))
          (submitted nil))
      (madrigal-mode 1)
      (goto-char (point-max))
      (insert "x")
      (backward-char)
      (cl-letf (((symbol-function 'madrigal-submit)
                 (lambda (&optional _prompt)
                   (setq submitted t))))
        (should (madrigal-ctrl-c-ctrl-c))
        (should submitted)))))

(ert-deftest madrigal-ctrl-c-ctrl-c-ignores-outside-user-turn ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* User\nHello\n* Assistant\nHi\n")
    (let ((madrigal-session (madrigal-session-create
                             :provider 'fake-provider
                             :model "fake-model"
                             :root "/tmp/project/"
                             :buffer (current-buffer))))
      (madrigal-mode 1)
      (goto-char (point-min))
      (should-not (madrigal-ctrl-c-ctrl-c)))))

(ert-deftest madrigal-ctrl-c-ctrl-c-ignores-user-turn-table ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Session\n#+MADRIGAL-AGENT: assistant\n\n* User\n| a | b |\n| 1 | 2 |\n")
    (let ((madrigal-session (madrigal-session-create
                             :provider 'fake-provider
                             :model "fake-model"
                             :root "/tmp/project/"
                             :buffer (current-buffer)))
          (submitted nil))
      (madrigal-mode 1)
      (goto-char (point-min))
      (search-forward "1")
      (cl-letf (((symbol-function 'madrigal-submit)
                 (lambda (&optional _prompt)
                   (setq submitted t))))
        (should-not (madrigal-ctrl-c-ctrl-c))
        (should-not submitted)))))

(ert-deftest madrigal-ctrl-c-ctrl-c-ignores-non-org-buffers ()
  (with-temp-buffer
    (fundamental-mode)
    (let ((madrigal-mode t))
      (should-not (madrigal-ctrl-c-ctrl-c)))))

(ert-deftest madrigal-project-root-is-optional ()
  (let ((default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (should-not (madrigal--project-root default-directory))
      (should (equal (madrigal--working-directory)
                     (file-name-as-directory
                      (expand-file-name temporary-file-directory)))))))

(ert-deftest madrigal-session-environment-context-reports-no-project ()
  (let ((session (madrigal-session-create :root "/tmp/")))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (let ((context (madrigal--session-environment-context session)))
        (should (string-match-p "Working directory: /tmp/" context))
        (should (string-match-p "Project: none" context))))))

(defun madrigal-test--captured-text (&optional limit)
  (plist-get
   (madrigal-focus-context-buffer-context
    (madrigal-focus-capture (current-buffer) nil limit))
   :text))

(ert-deftest madrigal-focus-capture-includes-buffer-and-point ()
  (with-temp-buffer
    (insert "alpha beta gamma")
    (goto-char 7)
    (let ((default-directory temporary-file-directory))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (let* ((context (madrigal-focus-capture (current-buffer)))
               (buffer-context (madrigal-focus-context-buffer-context context)))
          (should (= 7 (marker-position (plist-get buffer-context :point))))
          (should (equal "alpha beta gamma" (plist-get buffer-context :text)))
          (should-not (plist-get context :project))
          (let ((buffer-context
                 (plist-get (plist-get context :origin) :buffer-context)))
            (should-not (plist-member context :working-directory))
            (should (eq 'fundamental-mode
                        (plist-get buffer-context :major-mode)))
            (should (listp (plist-get buffer-context :minor-modes)))
            (should (markerp (plist-get buffer-context :point)))))))))

(ert-deftest madrigal-focus-defaults-to-four-kibibyte-contexts ()
  (should (= 4096 (default-value 'madrigal-do-buffer-context-limit)))
  (should (= 4096 (default-value 'madrigal-do-dwim-context-limit))))

(ert-deftest madrigal-focus-plist-discovers-project-and-nests-buffer-metadata ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((project 'fake-project)
          (root (file-name-as-directory temporary-file-directory)))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&rest _) project))
                ((symbol-function 'project-root)
                 (lambda (_) root))
                ((symbol-function 'project-name)
                 (lambda (_) "fake")))
        (let* ((context (madrigal-focus-context (current-buffer)))
               (origin (plist-get context :origin))
               (buffer-context (plist-get origin :buffer-context)))
          (should (eq (current-buffer) (plist-get origin :buffer)))
          (should (eq 'emacs-lisp-mode
                      (plist-get buffer-context :major-mode)))
          (should (listp (plist-get buffer-context :minor-modes)))
          (should (markerp (plist-get buffer-context :point)))
          (should (equal "fake" (plist-get (plist-get context :project) :name)))
          (should-not (plist-member context :working-directory)))))))

(ert-deftest madrigal-project-action-context-has-project-granularity ()
  (let ((project 'fake-project)
        (root (file-name-as-directory temporary-file-directory)))
    (cl-letf (((symbol-function 'project-root) (lambda (_) root))
              ((symbol-function 'project-name) (lambda (_) "fake")))
      (let* ((context (madrigal-project-action-context project))
             (model-context (madrigal-focus-model-context context)))
        (should-not (plist-member context :origin))
        (should-not (plist-member context :working-directory))
        (should (equal "fake" (plist-get (plist-get context :project) :name)))
        (should-not (plist-member (plist-get model-context :project) :object))))))

(ert-deftest madrigal-custom-origin-point-is-optional-and-enables-highlighting ()
  (with-temp-buffer
    (insert "first\nsecond")
    (let* ((whole (madrigal-focus-normalize-context
                   (list :origin (list :buffer (current-buffer)))))
           (focused (madrigal-focus-normalize-context
                     (list :origin
                           (list :buffer (current-buffer)
                                 :buffer-context (list :point 2)))))
           (indicator (madrigal-do--make-request-indicator focused)))
      (unwind-protect
          (progn
            (should-not (madrigal-focus-context-point whole))
            (should (markerp (madrigal-focus-context-point focused)))
            (should (eq 'fundamental-mode
                        (plist-get
                         (plist-get (plist-get focused :origin) :buffer-context)
                         :major-mode)))
            (should (= 2 (length indicator))))
        (madrigal-do--delete-request-indicator indicator)))))

(ert-deftest madrigal-whole-buffer-context-widens-eval-execution ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (insert "abcdef")
    (narrow-to-region 3 5)
    (let ((context (list :origin (list :buffer (current-buffer))))
          result)
      (let ((tool (madrigal--make-eval-tool
                   (current-buffer) "whole" #'ignore context)))
        (funcall (llm-tool-function tool)
                 (lambda (value) (setq result value))
                 "(cons (point-min) (point-max))"))
      (should (string-match-p (regexp-quote ":value (1 . 7)") result)))))

(ert-deftest madrigal-focus-capture-bounds-large-buffer-context ()
  (with-temp-buffer
    (insert (make-string 200 ?x))
    (goto-char 100)
    (let ((default-directory temporary-file-directory)
          (madrigal-do-buffer-context-limit 40))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (let ((buffer-context
               (madrigal-focus-context-buffer-context
                (madrigal-focus-capture (current-buffer)))))
          (should (= 40 (length (plist-get buffer-context :text))))
          (should (= 200 (plist-get buffer-context :buffer-size))))))))

(ert-deftest madrigal-focus-fallback-context-selects-complete-lines ()
  (with-temp-buffer
    (insert "row-1\nrow-2 target\nrow-3\nrow-4\n")
    (search-backward "target")
    (let ((default-directory temporary-file-directory))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (let ((text (madrigal-test--captured-text 25)))
          (should (string-prefix-p "row-1\n" text))
          (should (string-match-p "row-2 target" text))
          (should (string-suffix-p "\n" text))
          (should (<= (length text) 25)))))))

(ert-deftest madrigal-focus-programming-context-selects-current-defun ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun first ()\n  1)\n\n(defun second ()\n  2)\n")
    (search-backward "2")
    (let ((default-directory temporary-file-directory))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'treesit-parser-list) (lambda () nil)))
        (let ((text (madrigal-test--captured-text 100)))
          (should (string-match-p "defun second" text))
          (should-not (string-match-p "defun first" text)))))))

(ert-deftest madrigal-focus-outline-context-widens-to-fitting-parent ()
  (with-temp-buffer
    (insert "* Parent\nintro\n** Child\nbody\n** Sibling\nother\n")
    (outline-mode)
    (goto-char (point-min))
    (search-forward "body")
    (let ((default-directory temporary-file-directory))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (let ((text (madrigal-test--captured-text 100)))
          (should (string-match-p "\\* Parent" text))
          (should (string-match-p "\\*\\* Sibling" text)))
        (let ((text (madrigal-test--captured-text 20)))
          (should (string-match-p "\\*\\* Child" text))
          (should-not (string-match-p "\\* Parent" text)))))))

(ert-deftest madrigal-focus-magit-context-widens-to-fitting-parent-section ()
  (skip-unless (require 'magit-section nil t))
  (with-temp-buffer
    (insert (make-string 100 ?x))
    (let ((parent (make-instance 'magit-section))
          (child (make-instance 'magit-section)))
      (eieio-oset parent 'start 10)
      (eieio-oset parent 'end 80)
      (eieio-oset child 'start 30)
      (eieio-oset child 'end 50)
      (eieio-oset child 'parent parent)
      (cl-letf (((symbol-function 'magit-current-section)
                 (lambda () child)))
        (should (equal '(10 . 80)
                       (madrigal-focus--magit-section-context-range 40 80)))
        (should (equal '(30 . 50)
                       (madrigal-focus--magit-section-context-range 40 40)))))))

(ert-deftest madrigal-focus-tabulated-context-selects-complete-nearby-lines ()
  (with-temp-buffer
    (tabulated-list-mode)
    (let ((inhibit-read-only t))
      (insert "row-1\nrow-2 target\nrow-3\nrow-4\n"))
    (search-backward "target")
    (let ((default-directory temporary-file-directory))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (let ((text (madrigal-test--captured-text 25)))
          (should (string-match-p "row-2 target" text))
          (should (string-suffix-p "\n" text))
          (should (<= (length text) 25)))))))

(ert-deftest madrigal-eval-tool-event-sink-avoids-org-rendering ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (insert "source buffer")
    (let (event callback-result)
      (let ((tool (madrigal--make-eval-tool
                   (current-buffer) "do-1"
                   (lambda (value) (setq event value)))))
        (funcall (llm-tool-function tool)
                 (lambda (value) (setq callback-result value))
                 "(+ 20 22)"))
      (should (equal "source buffer" (buffer-string)))
      (should (equal 'tool (plist-get event :type)))
      (should (= 42 (plist-get (plist-get event :result) :value)))
      (should (string-match-p ":value 42" callback-result)))))

(ert-deftest madrigal-do-and-dwim-use-independent-context-limits ()
  (let ((madrigal-do-buffer-context-limit 123)
        (madrigal-do-dwim-context-limit 234)
        do-limit
        dwim-limit)
    (cl-letf (((symbol-function 'madrigal-focus-context)
               (lambda (&optional _buffer _window limit)
                 (if do-limit
                     (setq dwim-limit limit)
                   (setq do-limit limit))
                 (list :project (list :name "test" :root "/tmp/"))))
              ((symbol-function 'read-string) (lambda (&rest _) "Inspect"))
              ((symbol-function 'madrigal-do--execute) #'ignore)
              ((symbol-function 'madrigal-llm-available-p) (lambda () nil)))
      (call-interactively #'madrigal-do)
      (should-error (call-interactively #'madrigal-do-dwim) :type 'user-error)
      (should (= 123 do-limit))
      (should (= 234 dwim-limit)))))

(ert-deftest madrigal-do-interactive-highlights-before-prompt-and-cleans-up-on-quit ()
  (with-temp-buffer
    (insert "focused text")
    (goto-char 5)
    (let ((default-directory temporary-file-directory)
          seen-indicator)
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'redisplay) #'ignore)
                ((symbol-function 'read-string)
                 (lambda (&rest _)
                   (setq seen-indicator
                         (seq-find
                          (lambda (overlay) (overlay-get overlay 'before-string))
                          (append (car (overlay-lists))
                                  (cdr (overlay-lists)))))
                   (signal 'quit nil))))
        (should (condition-case nil
                    (progn (call-interactively #'madrigal-do) nil)
                  (quit t)))
        (should (overlayp seen-indicator))
        (should-not (overlay-buffer seen-indicator))))))

(ert-deftest madrigal-do-interactive-discovers-optional-project-context ()
  (with-temp-buffer
    (let ((project 'fake-project)
          (root (file-name-as-directory temporary-file-directory))
          captured)
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) project))
                ((symbol-function 'project-root) (lambda (_) root))
                ((symbol-function 'project-name) (lambda (_) "fake"))
                ((symbol-function 'read-string) (lambda (&rest _) "Inspect"))
                ((symbol-function 'madrigal-do--execute)
                 (lambda (context instruction &rest _)
                   (setq captured (list context instruction)))))
        (call-interactively #'madrigal-do)
        (should (equal "Inspect" (cadr captured)))
        (should (equal "fake"
                       (plist-get (plist-get (car captured) :project) :name)))))))

(ert-deftest madrigal-do-executes-through-shared-controller-path ()
  (with-temp-buffer
    (insert "Focused text")
    (let ((default-directory temporary-file-directory)
          (madrigal-do--active-actions nil)
          (madrigal-do--recent-actions nil)
          captured-history captured-context captured-environment)
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'madrigal-do--show-result) #'ignore)
                ((symbol-function 'madrigal-agent-controller-submit-async)
                 (lambda (&rest args)
                   (setq captured-history (plist-get args :history)
                         captured-context (plist-get args :context)
                         captured-environment (plist-get args :environment))
                   (funcall (plist-get args :on-start)
                            (list :model "model"))
                   (funcall (plist-get args :on-response)
                            (list :text "Done" :final t))
                   (funcall (plist-get args :on-finished) nil)
                   (madrigal-agent-controller-handle-create
                    :provider 'provider :model "model"))))
        (let* ((context (madrigal-focus-context (current-buffer)))
               (action (madrigal-do "Do the thing" context)))
          (should (equal 'finished (madrigal-action-status action)))
          (should (equal "Done" (madrigal-action-response action)))
          (should (equal "Do the thing"
                         (plist-get (car captured-history) :content)))
          (should (string-match-p "Focused text" captured-context))
          (should (functionp (plist-get captured-environment :event-sink)))
          (should (= 1 (length madrigal-do--recent-actions))))))))

(ert-deftest madrigal-do-project-executes-with-project-only-context ()
  (let* ((root (make-temp-file "madrigal-do-project-" t))
         (project 'fake-project)
         (madrigal-do--active-actions nil)
         (madrigal-do--recent-actions nil)
         execution-buffer execution-directory rendered-context)
    (unwind-protect
        (cl-letf (((symbol-function 'project-root) (lambda (_) root))
                  ((symbol-function 'project-name) (lambda (_) "fake"))
                  ((symbol-function 'madrigal-do--show-result) #'ignore)
                  ((symbol-function 'madrigal-agent-controller-submit-async)
                   (lambda (&rest args)
                     (setq rendered-context (plist-get args :context)
                           execution-buffer
                           (plist-get (plist-get args :environment) :buffer)
                           execution-directory
                           (buffer-local-value 'default-directory execution-buffer))
                     (funcall (plist-get args :on-finished) nil)
                     (madrigal-agent-controller-handle-create
                      :provider 'provider :model "model"))))
          (let* ((context (madrigal-project-action-context project))
                 (action (madrigal-do "Inspect project" context)))
            (should (equal (file-name-as-directory root) execution-directory))
            (should-not (buffer-live-p execution-buffer))
            (should-not (string-match-p ":origin" rendered-context))
            (should-not (string-match-p ":major-mode" rendered-context))
            (should-not (madrigal-action-indicator action))))
      (delete-directory root t))))

(ert-deftest madrigal-do-request-highlights-mix-rainbow-and-theme-colours ()
  (cl-letf (((symbol-function 'madrigal-do--face-rgb)
             (lambda (face attribute)
               (pcase (list face attribute)
                 (`(default :background) '(0.05 0.05 0.05))
                 (`(default :foreground) '(0.9 0.9 0.9))
                 (_ '(0.8 0.2 0.3))))))
    (let* ((faces (madrigal-do--request-highlight-faces 'theme-accent 0.5))
           (other-faces
            (madrigal-do--request-highlight-faces 'theme-accent 0.0))
           (context (car faces))
           (point (cdr faces))
           (box (plist-get point :box)))
      (should (stringp (plist-get context :background)))
      (should (stringp (plist-get point :background)))
      (should (= 7 (length (plist-get box :color))))
      (should-not (equal (plist-get box :color)
                         (plist-get (plist-get (cdr other-faces) :box) :color)))
      (should-not (equal (plist-get context :background)
                         (plist-get point :background)))
      (should-not (equal "#cccc33334ccc" (plist-get box :color))))))

(ert-deftest madrigal-do-request-indicators-survive-edits-until-cleanup ()
  (with-temp-buffer
    (insert "first line\nsecond line")
    (goto-char 5)
    (let ((default-directory temporary-file-directory))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (let ((indicator
               (madrigal-do--make-request-indicator
                (madrigal-focus-context (current-buffer)))))
          (delete-region (point-min) (point-max))
          (insert "replacement")
          (should (overlay-buffer (car indicator)))
          (should (overlay-buffer (cadr indicator)))
          (should (= (point-min) (overlay-start (car indicator))))
          (should (= (point-max) (overlay-end (car indicator))))
          (madrigal-do--remember-completed
           (madrigal-action-create :indicator indicator))
          (should-not (overlay-buffer (car indicator)))
          (should-not (overlay-buffer (cadr indicator))))))))

(ert-deftest madrigal-do-request-indicators-track-and-clean-up-origins ()
  (with-temp-buffer
    (insert "first line\nsecond line")
    (let ((default-directory temporary-file-directory)
          (madrigal-do--request-face-index 0))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (goto-char (point-min))
        (let* ((first (madrigal-focus-context (current-buffer)))
               (first-indicator (madrigal-do--make-request-indicator first)))
          (goto-char (point-max))
          (let ((second-indicator
                 (madrigal-do--make-request-indicator
                  (madrigal-focus-context (current-buffer)))))
            (should (= 2 (length first-indicator)))
            (should (equal "🧠"
                           (substring-no-properties
                            (overlay-get (cadr first-indicator) 'before-string))))
            (let ((context-face (overlay-get (car first-indicator) 'face))
                  (point-face
                   (get-text-property
                    0 'face
                    (overlay-get (cadr first-indicator) 'before-string))))
              (should (eq 'bold (plist-get point-face :weight)))
              (should (plist-get point-face :box))
              (should-not (equal context-face point-face)))
            (should (equal (plist-get
                            (plist-get (plist-get first :origin) :buffer-context)
                            :range)
                           (cons (overlay-start (car first-indicator))
                                 (overlay-end (car first-indicator)))))
            (should-not (eq (overlay-get (car first-indicator) 'face)
                            (overlay-get (car second-indicator) 'face)))
            (madrigal-do--remember-completed
             (madrigal-action-create :indicator first-indicator))
            (should-not (overlay-buffer (car first-indicator)))
            (madrigal-do--delete-request-indicator second-indicator)))))))

(ert-deftest madrigal-eval-tool-uses-captured-point-after-focus-moves ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (insert "abcdef")
    (goto-char 3)
    (let* ((default-directory temporary-file-directory)
           (context (cl-letf (((symbol-function 'project-current)
                               (lambda (&rest _) nil)))
                      (madrigal-focus-context (current-buffer))))
           callback-result)
      (goto-char 6)
      (let ((tool (madrigal--make-eval-tool
                   (current-buffer) "do-focus" #'ignore context)))
        (funcall (llm-tool-function tool)
                 (lambda (value) (setq callback-result value))
                 "(point)"))
      (should (string-match-p ":value 3" callback-result)))))

(ert-deftest madrigal-eval-tool-captured-point-tracks-buffer-edits ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (insert "abcdef")
    (goto-char 3)
    (let* ((default-directory temporary-file-directory)
           (context (cl-letf (((symbol-function 'project-current)
                               (lambda (&rest _) nil)))
                      (madrigal-focus-context (current-buffer))))
           callback-result)
      (goto-char (point-min))
      (insert "X")
      (goto-char (point-max))
      (let ((tool (madrigal--make-eval-tool
                   (current-buffer) "do-focus" #'ignore context)))
        (funcall (llm-tool-function tool)
                 (lambda (value) (setq callback-result value))
                 "(point)"))
      (should (string-match-p ":value 4" callback-result)))))

(ert-deftest madrigal-do-keeps-only-final-text-as-summary ()
  (with-temp-buffer
    (let ((default-directory temporary-file-directory)
          (madrigal-do--active-actions nil)
          (madrigal-do--recent-actions nil))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'madrigal-do--show-result) #'ignore)
                ((symbol-function 'madrigal-agent-controller-submit-async)
                 (lambda (&rest args)
                   (funcall (plist-get args :on-start) '(:model "model"))
                   (funcall (plist-get args :on-response)
                            '(:text "I will inspect first." :final nil))
                   (funcall (plist-get args :on-response)
                            '(:text "  Updated   the buffer.\n" :final t))
                   (funcall (plist-get args :on-finished) nil)
                   (madrigal-agent-controller-handle-create
                    :provider 'provider :model "model"))))
        (let ((action (madrigal-do
                       "Update it"
                       (madrigal-focus-context (current-buffer)))))
          (should (equal "Updated the buffer."
                         (madrigal-action-response action)))
          (should (= 3 (length (madrigal-action-turns action))))
          (should (equal 'intermediate
                         (madrigal-action-turn-kind
                          (nth 1 (madrigal-action-turns action)))))
          (should (equal 'summary
                         (madrigal-action-turn-kind
                          (nth 2 (madrigal-action-turns action))))))))))

(ert-deftest madrigal-do-displays-summary-only-in-minibuffer ()
  (let ((action (madrigal-action-create :response "Updated the buffer."))
        displayed)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq displayed (apply #'format format-string args)))))
      (madrigal-do--show-result action))
    (should (equal "Madrigal: Updated the buffer." displayed))))

(ert-deftest madrigal-do-history-default-retains-one-thousand-actions ()
  (should (= 1000 (default-value 'madrigal-do-history-length))))

(ert-deftest madrigal-do-history-selects-the-most-recent-action-by-default ()
  (let* ((newest (madrigal-action-create :id "new" :instruction "Newest"
                                         :started-at (current-time)))
         (oldest (madrigal-action-create :id "old" :instruction "Oldest"
                                         :started-at (time-subtract (current-time) 1)))
         (madrigal-do--active-actions (list newest))
         (madrigal-do--recent-actions (list oldest))
         captured-default)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args)
                 (setq captured-default (nth 6 args))
                 captured-default)))
      (should (eq newest (madrigal-do--read-history-action)))
      (should (equal (list newest oldest) (madrigal-do--history-actions)))
      (should (equal (madrigal-do--history-candidate newest) captured-default)))))

(ert-deftest madrigal-do-history-completion-exposes-time-and-status-metadata ()
  (let* ((action (madrigal-action-create
                  :id "recent" :instruction "Inspect"
                  :status 'finished
                  :started-at (time-subtract (current-time) 90)))
         (candidate (madrigal-do--history-candidate action))
         (table (madrigal-do--history-completion-table
                 (list (cons candidate action)) 'madrigal-do-history
                 #'madrigal-action-started-at #'madrigal-action-status))
         (metadata (funcall table "" nil 'metadata))
         (annotation-function (alist-get 'annotation-function metadata))
         (annotation (funcall annotation-function candidate)))
    (should (eq 'madrigal-do-history (alist-get 'category metadata)))
    (should (string-match-p "1m ago" annotation))
    (should (string-match-p "finished" annotation))
    (should (get-text-property 0 'face candidate))))

(ert-deftest madrigal-do-history-renders-turns-and-tools-in-read-only-org-buffer ()
  (let* ((action
          (madrigal-action-create
           :id "history-1" :instruction "Inspect this"
           :turns (list (madrigal-action-turn-create
                         :role 'user :text "Inspect this")
                        (madrigal-action-turn-create
                         :role 'assistant :kind 'summary :final t :text "Done."))
           :tool-events (list (madrigal-tool-event-create
                               :name "eval" :language "emacs-lisp"
                               :source "(+ 1 1)" :result '(:ok t :value 2)))))
         buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (value &rest _) (setq buffer value))))
          (madrigal-do-history action)
          (with-current-buffer buffer
            (should (derived-mode-p 'org-mode))
            (should buffer-read-only)
            (should (eq (key-binding (kbd "q")) #'quit-window))
            (should (string-match-p (regexp-quote "* User\nInspect this")
                                    (buffer-string)))
            (should (string-match-p (regexp-quote "** Response\nDone.")
                                    (buffer-string)))
            (should (string-match-p (regexp-quote "** Tools\n*** eval")
                                    (buffer-string)))
            (should (string-match-p (regexp-quote "(+ 1 1)")
                                    (buffer-string)))
            (should (string-match-p (regexp-quote ":value 2")
                                    (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest madrigal-do-history-renders-the-captured-focus-context ()
  (with-temp-buffer
    (insert "Context sent to the agent")
    (let ((default-directory temporary-file-directory)
          buffer)
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'display-buffer)
                 (lambda (value &rest _) (setq buffer value))))
        (madrigal-do-history
         (madrigal-action-create
          :id "context-1" :instruction "Inspect"
          :context (madrigal-focus-context (current-buffer))))
        (unwind-protect
            (with-current-buffer buffer
              (should (string-match-p
                       (regexp-quote
                        "* Context\n#+begin_src emacs-lisp\n'(")
                       (buffer-string)))
              (should (string-match-p "Context sent to the agent" (buffer-string)))
              (should (string-match-p ":buffer-context" (buffer-string)))
              (goto-char (point-min))
              (search-forward "#+begin_src emacs-lisp\n")
              (let ((start (point)))
                (search-forward "\n#+end_src")
                (let ((form (read (buffer-substring-no-properties
                                   start (match-beginning 0)))))
                  (should (eq 'quote (car form)))
                  (should (plist-get (cadr form) :origin)))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest madrigal-do-history-candidates-use-small-context-excerpts ()
  (with-temp-buffer
    (insert (make-string 200 ?a) "TEXT-AT-POINT" (make-string 200 ?z))
    (search-backward "TEXT-AT-POINT")
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (let* ((context (madrigal-focus-context (current-buffer)))
             (action (madrigal-action-create
                      :id "small-action" :instruction "Inspect this"
                      :context context :started-at (current-time)))
             (request (madrigal-dwim-suggestion-request-create
                       :id "small-request" :action-context context
                       :status 'success :started-at (current-time)))
             (action-candidate (madrigal-do--history-candidate action))
             (dwim-candidate (madrigal-do--dwim-history-candidate request)))
        (should (string-prefix-p "Inspect this  " action-candidate))
        (should (string-match-p "TEXT-AT-POINT" action-candidate))
        (should (string-match-p "TEXT-AT-POINT" dwim-candidate))
        (should (< (length action-candidate) 220))
        (should (< (length dwim-candidate) 160))))))

(ert-deftest madrigal-do-dwim-history-is-newest-first-across-request-states ()
  (let* ((now (current-time))
         (oldest (madrigal-dwim-suggestion-request-create
                  :id "oldest" :status 'running
                  :started-at (time-subtract now 2)))
         (newest (madrigal-dwim-suggestion-request-create
                  :id "newest" :status 'success :started-at now))
         (madrigal-do--active-dwim-suggestions (list oldest))
         (madrigal-do--recent-dwim-suggestions (list newest)))
    (should (equal '("newest" "oldest")
                   (mapcar #'madrigal-dwim-suggestion-request-id
                           (madrigal-do--dwim-history-requests))))))

(ert-deftest madrigal-do-dwim-history-allows-stale-origin-buffers ()
  (let* ((buffer (generate-new-buffer " *madrigal-stale-origin*"))
         (context (with-current-buffer buffer
                    (madrigal-focus-normalize-context
                     (list :origin (list :buffer buffer)))))
         (request (madrigal-dwim-suggestion-request-create
                   :id "stale-dwim" :action-context context
                   :context "(:origin (:buffer (:name \"old\")))"
                   :status 'cancelled :started-at (current-time)))
         (madrigal-do--recent-dwim-suggestions (list request)))
    (kill-buffer buffer)
    (should (string-match-p "stale-dwim"
                            (madrigal-do--dwim-history-candidate request)))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args) (nth 6 args))))
      (should (eq request (madrigal-do--read-dwim-history-request))))))

(ert-deftest madrigal-do-dwim-history-renders-request-response-and-timing ()
  (with-temp-buffer
    (insert "Focused suggestion context")
    (let ((default-directory temporary-file-directory)
          buffer)
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'display-buffer)
                 (lambda (value &rest _) (setq buffer value))))
        (let* ((context (madrigal-focus-context (current-buffer)))
               (request
                (madrigal-dwim-suggestion-request-create
                 :id "dwim-1" :action-context context
                 :context (madrigal-do--suggestion-context context)
                 :prompt (madrigal-do--suggestion-prompt context)
                 :response "{\"suggestions\":[]}" :status 'success
                 :started-at (current-time))))
          (unwind-protect
              (progn
                (madrigal-do-dwim-history request)
                (with-current-buffer buffer
                  (should (derived-mode-p 'org-mode))
                  (should buffer-read-only)
                  (should (eq (key-binding (kbd "q")) #'quit-window))
                  (should (string-match-p
                           (regexp-quote
                            "* Request\n** Context\n#+begin_src emacs-lisp\n'(")
                           (buffer-string)))
                  (should (string-match-p "Focused suggestion context"
                                          (buffer-string)))
                  (should (string-match-p (regexp-quote "* Response")
                                          (buffer-string)))
                  (goto-char (point-min))
                  (search-forward "#+begin_src emacs-lisp\n")
                  (let ((start (point)))
                    (search-forward "\n#+end_src")
                    (let ((form (read (buffer-substring-no-properties
                                       start (match-beginning 0)))))
                      (should (eq 'quote (car form)))
                      (should (plist-get (cadr form) :instructions)))))
            (when (buffer-live-p buffer)
              (kill-buffer buffer)))))))))

(ert-deftest madrigal-do-introspection-exposes-current-action-history ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (insert "focused")
    (let* ((default-directory temporary-file-directory)
           (context
            (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
              (madrigal-focus-context (current-buffer))))
           (action
            (madrigal-action-create
             :id "action-1" :instruction "Inspect"
             :context context :status 'running
             :turns (list (madrigal-action-turn-create
                           :role 'user :kind 'instruction :text "Inspect"))))
           (sink (lambda (event)
                   (madrigal-do--record-tool-event action event)))
           callback-result)
      (let ((tool (madrigal--make-eval-tool
                   (current-buffer) "action-1" sink context action)))
        (funcall
         (llm-tool-function tool)
         (lambda (value) (setq callback-result value))
         "(list (plist-get (madrigal-do-context) :id) (length (madrigal-do-turn-history)) (length (madrigal-do-tool-history)) (length (madrigal-do-tool-result-history)))"))
      (should (string-match-p
               (regexp-quote "(\"action-1\" 1 1 0)") callback-result))
      (should (= 1 (length (madrigal-do-tool-history action))))
      (should (= 1 (length (madrigal-do-tool-result-history action)))))))

(ert-deftest madrigal-do-parses-and-sorts-structured-json-suggestions ()
  (let ((suggestions
         (madrigal-do--parse-suggestions
          "{\"suggestions\":[{\"relevance\":0.7,\"action\":\"Rename the symbol.\"},{\"relevance\":0.9,\"action\":\"Run the test at point.\"}]}")))
    (should (= 2 (length suggestions)))
    (should (= 0.9
               (madrigal-action-suggestion-relevance (car suggestions))))
    (should (equal "Run the test at point."
                   (madrigal-action-suggestion-action (car suggestions))))))

(ert-deftest madrigal-do-parses-fenced-json-from-prompt-only-providers ()
  (should-not
   (madrigal-do--parse-suggestions
    "```json\n{\"suggestions\":[]}\n```")))

(ert-deftest madrigal-do-rejects-non-json-suggestions ()
  (should-error
   (madrigal-do--parse-suggestions
    "Run test\tRun the test at point.\tPoint is in a test.")))

(ert-deftest madrigal-do-rejects-json-suggestions-with-extra-fields ()
  (should-error
   (madrigal-do--parse-suggestions
    "{\"suggestions\":[],\"commentary\":\"none\"}")))

(ert-deftest madrigal-do-rejects-invalid-suggestion-relevance-and-length ()
  (should-error
   (madrigal-do--parse-suggestions
    "{\"suggestions\":[{\"relevance\":1.1,\"action\":\"Act\"}]}"))
  (should-error
   (madrigal-do--parse-suggestions
    (format "{\"suggestions\":[{\"relevance\":0.5,\"action\":\"%s\"}]}"
            (make-string 81 ?a)))))

(ert-deftest madrigal-do-suggestion-schema-requires-structured-fields ()
  (let* ((properties (plist-get madrigal-do--suggestion-response-schema
                                :properties))
         (array (plist-get properties :suggestions))
         (item (plist-get array :items)))
    (should (equal ["relevance" "action"]
                   (plist-get item :required)))
    (should (= 80 (plist-get (plist-get (plist-get item :properties) :action)
                              :maxLength)))
    (should-not (plist-member array :maxItems))))

(ert-deftest madrigal-do-suggestions-have-no-count-limit ()
  (let ((entries
         (mapconcat
          (lambda (_)
            "{\"relevance\":0.5,\"action\":\"Act at point.\"}")
          (number-sequence 1 6) ",")))
    (should (= 6 (length (madrigal-do--parse-suggestions
                          (format "{\"suggestions\":[%s]}" entries)))))))

(ert-deftest madrigal-do-suggestion-context-is-local-to-point ()
  (with-temp-buffer
    (insert "REMOTE-TEXT" (make-string 13000 ?a) "LOCAL-TEXT"
            (make-string 13000 ?z))
    (search-backward "LOCAL-TEXT")
    (let ((default-directory temporary-file-directory))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (let ((context (madrigal-do--suggestion-context
                        (madrigal-focus-context (current-buffer)))))
          (should (string-match-p "LOCAL-TEXT" context))
          (should (string-match-p ":range" context))
          (should-not (string-match-p "REMOTE-TEXT" context)))))))

(ert-deftest madrigal-do-suggestion-context-preserves-source-at-point ()
  (with-temp-buffer
    (insert "(message \"unbroken string\")")
    (search-backward "string")
    (forward-char 3)
    (let ((default-directory temporary-file-directory))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (let ((context (madrigal-do--suggestion-context
                        (madrigal-focus-context (current-buffer)))))
          (should (string-match-p (regexp-quote "\\\"unbroken string\\\"") context))
          (should-not (string-match-p "<<POINT>>" context)))))))

(ert-deftest madrigal-do-dwim-removes-indicator-when-suggestion-read-quits ()
  (with-temp-buffer
    (insert "focused")
    (let* ((context
            (madrigal-focus-normalize-context
             (list :origin
                   (list :buffer (current-buffer)
                         :buffer-context (list :point 1 :text "focused"
                                               :range '(1 . 8))))))
           (madrigal-do--active-dwim-suggestions nil)
           (madrigal-do--recent-dwim-suggestions nil)
           success request indicator)
      (cl-letf (((symbol-function 'madrigal-llm-available-p) (lambda () t))
                ((symbol-function 'madrigal-agent-controller--resolve-provider-and-model)
                 (lambda (&rest _) '(provider . "model")))
                ((symbol-function 'madrigal-do--submit-suggestion-prompt)
                 (lambda (_provider _prompt on-success _error)
                   (setq success on-success)
                   'handle))
                ((symbol-function 'madrigal-do--visit-context) #'ignore)
                ((symbol-function 'madrigal-do--read-suggestion)
                 (lambda (&rest _) (signal 'quit nil))))
        (madrigal-do-dwim context)
        (setq request (car madrigal-do--active-dwim-suggestions)
              indicator (madrigal-dwim-suggestion-request-indicator request))
        (should (overlay-buffer (car indicator)))
        (condition-case nil
            (funcall success
                     "{\"suggestions\":[{\"relevance\":1,\"action\":\"Act\"}]}")
          (quit nil))
        (should (eq 'cancelled
                    (madrigal-dwim-suggestion-request-status request)))
        (should-not madrigal-do--active-dwim-suggestions)
        (should-not (overlay-buffer (car indicator)))))))

(ert-deftest madrigal-do-dwim-completion-accepts-a-custom-action ()
  (with-temp-buffer
    (let* ((context (madrigal-focus-normalize-context
                     (list :origin (list :buffer (current-buffer)))))
           (suggestions
            (list (madrigal-action-suggestion-create
                   :relevance 0.9 :action "Use the suggestion.")))
           require-match)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _collection _predicate required &rest _)
                   (setq require-match required)
                   "Run my own action.")))
        (should (equal "Run my own action."
                       (madrigal-do--read-suggestion context suggestions)))
        (should-not require-match)))))

(ert-deftest madrigal-do-formats-suggestions-for-completion ()
  (let ((suggestion
         (madrigal-action-suggestion-create
          :relevance 0.9 :action "Run the test.")))
    (let ((display (madrigal-do--suggestion-display suggestion)))
      (should (equal "● Run the test." display))
      (should (get-text-property 0 'face display))
      (should-not (get-text-property 2 'face display)))
    (let* ((formatted (madrigal-do--org-fontify-string "Run =make test= now"))
           (code-position (string-match "make" formatted)))
      (should (eq t (get-text-property (1- code-position) 'invisible formatted)))
      (should (eq 'org-verbatim
                  (car (get-text-property code-position 'face formatted)))))
    (should (equal '("●" "◕" "◑" "◔" "○")
                   (mapcar #'madrigal-do--relevance-indicator
                           '(1.0 0.7 0.5 0.2 0.0))))
    (should (equal '(metadata
                     (category . madrigal-dwim-suggestion)
                     (display-sort-function . identity)
                     (cycle-sort-function . identity))
                   (funcall (madrigal-do--suggestion-completion-table nil)
                            "" nil 'metadata)))))

(ert-deftest madrigal-do-suggestions-stream-for-codex-oauth-providers ()
  (let (streamed)
    (cl-letf (((symbol-function 'madrigal-agent-controller-provider-use-streaming-p)
               (lambda (_provider) t))
              ((symbol-function 'llm-chat-streaming)
               (lambda (&rest args) (setq streamed args) 'stream-request))
              ((symbol-function 'llm-chat-async)
               (lambda (&rest _) (ert-fail "Unexpected non-streaming request"))))
      (should (eq 'stream-request
                  (madrigal-do--submit-suggestion-prompt
                   'provider 'prompt 'success 'error)))
      (should (equal '(provider prompt nil success error t) streamed)))))

(ert-deftest madrigal-do-suggestions-use-async-for-other-providers ()
  (let (submitted)
    (cl-letf (((symbol-function 'madrigal-agent-controller-provider-use-streaming-p)
               (lambda (_provider) nil))
              ((symbol-function 'llm-chat-async)
               (lambda (&rest args) (setq submitted args) 'async-request))
              ((symbol-function 'llm-chat-streaming)
               (lambda (&rest _) (ert-fail "Unexpected streaming request"))))
      (should (eq 'async-request
                  (madrigal-do--submit-suggestion-prompt
                   'provider 'prompt 'success 'error)))
      (should (equal '(provider prompt success error t) submitted)))))

(ert-deftest madrigal-do-agent-definition-survives-custom-agent-lists ()
  (let ((madrigal-agents '(("assistant" :system-prompt "Custom" :tools nil))))
    (should (equal '("eval" "persist-elisp")
                   (plist-get (madrigal--agent-definition "do") :tools)))
    (should (equal madrigal--do-system-prompt
                   (plist-get (madrigal--agent-definition "do") :system-prompt)))
    (should (equal '("do" "do-dwim")
                   (seq-filter (lambda (name) (member name '("do" "do-dwim")))
                               (madrigal--selectable-agent-names))))))

(ert-deftest madrigal-default-assistant-can-persist-reusable-elisp ()
  (let ((definition (madrigal--agent-definition "assistant")))
    (should (equal '("eval" "persist-elisp") (plist-get definition :tools)))
    (should (string-match-p (regexp-quote "Use the persist-elisp tool")
                            (plist-get definition :system-prompt)))))

(ert-deftest madrigal-do-explicit-model-selection-overrides-model-fallback ()
  (let ((madrigal-agent-models
         '(("do" . ("Provider" . "do-model"))
           ("assistant" . ("Provider" . "assistant-model"))))
        (madrigal-providers '(("Provider" :factory (lambda (model) model)))))
    (should (equal '("do-model" . "do-model")
                   (madrigal-agent-controller--resolve-provider-and-model
                    "do")))))

(ert-deftest madrigal-do-dwim-uses-its-configured-model-or-the-do-model ()
  (let ((madrigal-do-agent "do")
        (madrigal-agent-models '(("do" . ("Provider" . "do-model")))))
    (should (equal "do" (madrigal-do--dwim-model-agent))))
  (let ((madrigal-do-agent "do")
        (madrigal-agent-models
         '(("do" . ("Provider" . "do-model"))
           ("do-dwim" . ("Provider" . "fast-model")))))
    (should (equal "do-dwim" (madrigal-do--dwim-model-agent)))))

(ert-deftest madrigal-do-suggestion-prompt-always-requests-json-schema ()
  (with-temp-buffer
    (let ((default-directory temporary-file-directory))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (let* ((context (madrigal-focus-context (current-buffer)))
               (prompt (madrigal-do--suggestion-prompt context)))
          (should (eq madrigal-do--suggestion-response-schema
                      (llm-chat-prompt-response-format prompt)))
          (should-not (llm-chat-prompt-temperature prompt))
          (should (eq 'none (llm-chat-prompt-reasoning prompt)))
          (should (string-match-p
                   (regexp-quote "Return only JSON")
                   (llm-chat-prompt-context prompt)))
          (should (string-match-p
                   (regexp-quote "Use Org inline markup")
                   (llm-chat-prompt-context prompt))))))))

(ert-deftest madrigal-open-session-works-outside-projects ()
  (let* ((directory (make-temp-file "madrigal-no-project-" t))
         (default-directory directory)
         (madrigal-providers '(("Provider" :provider provider)))
         (madrigal-agent-models '(("assistant" . ("Provider" . "model"))))
         opened)
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) (setq opened buffer))))
          (madrigal-open-session nil "assistant")
          (should (buffer-live-p opened))
          (with-current-buffer opened
            (should (equal (madrigal-session-root madrigal-session)
                           (file-name-as-directory directory)))
            (should-not (madrigal-session-project-context madrigal-session))))
      (when (buffer-live-p opened) (kill-buffer opened))
      (delete-directory directory t))))

(ert-deftest madrigal-bounded-reflection-pages-symbol-results ()
  (let ((page (madrigal-symbol-search "^madrigal-" 'function 0 3)))
    (should (= 3 (length (plist-get page :items))))
    (should (plist-get page :truncated))
    (should (= 3 (plist-get page :next-offset)))))

(ert-deftest madrigal-context-size-caches-unchanged-buffer ()
  (with-temp-buffer
    (org-mode)
    (let ((madrigal-session
           (madrigal-session-create :agent "assistant" :provider 'provider))
          (calls 0))
      (insert "* User\nHello")
      (cl-letf (((symbol-function 'madrigal-agent-controller-context-size)
                 (lambda (&rest _)
                   (setq calls (1+ calls))
                   '(:tokens 1))))
        (madrigal-context-size)
        (madrigal-context-size)
        (should (= 1 calls))
        (insert "!")
        (madrigal-context-size)
        (should (= 2 calls))))))

;;; madrigal-test.el ends here
