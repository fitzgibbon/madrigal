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

(ert-deftest madrigal-agent-controller-mcp-context-is-optional ()
  (let ((mcp-hub-servers '(("playwright" :command "secret-command"))))
    (cl-letf (((symbol-function 'madrigal-agent-controller--mcp-available-p)
               (lambda () nil)))
      (should-not (madrigal-agent-controller--mcp-context "assistant"))))
  (let ((mcp-hub-servers nil))
    (cl-letf (((symbol-function 'madrigal-agent-controller--mcp-available-p)
               (lambda () t)))
      (should-not (madrigal-agent-controller--mcp-context "assistant")))))

(ert-deftest madrigal-agent-controller-mcp-context-lists-only-server-names ()
  (let ((mcp-hub-servers
         '(("playwright" :command "secret-command" :env (:token "secret"))
           ("github" :url "https://secret.invalid" :headers (:authorization "secret")))))
    (cl-letf (((symbol-function 'madrigal-agent-controller--mcp-available-p)
               (lambda () t)))
      (let ((context (madrigal-agent-controller--mcp-context "assistant")))
        (should (string-prefix-p
                 "MCP is available through eval via mcp.el. Configured mcp-hub servers: "
                 context))
        (should (string-match-p "playwright, github" context))
        (should (string-match-p "use the playwright MCP server" context))
        (dolist (secret '("secret-command" "secret.invalid" "authorization" "token"))
          (should-not (string-match-p secret context)))))))

(ert-deftest madrigal-agent-controller-mcp-context-is-agent-specific-and-live ()
  (cl-letf (((symbol-function 'madrigal-agent-controller--mcp-available-p)
             (lambda () t)))
    (let ((mcp-hub-servers '(("github"))))
      (should (string-match-p "github"
                              (madrigal-agent-controller--mcp-context "assistant")))
      (should (string-match-p "github"
                              (madrigal-agent-controller--mcp-context "do")))
      (should-not (madrigal-agent-controller--mcp-context "do-dwim"))
      (should-not (madrigal-agent-controller--mcp-context "developer")))
    (let ((mcp-hub-servers '(("playwright"))))
      (should (string-match-p "playwright"
                              (madrigal-agent-controller--mcp-context "assistant"))))))

(ert-deftest madrigal-agent-controller-mcp-context-failure-is-isolated ()
  (cl-letf (((symbol-function 'madrigal-agent-controller--mcp-available-p)
             (lambda () (error "broken MCP installation"))))
    (should-not (madrigal-agent-controller--mcp-context "assistant"))))

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
          (madrigal--agents '(("assistant" :system-prompt "System" :tools ("eval"))))
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

(ert-deftest madrigal-agent-controller-extracts-multi-output-reasoning ()
  (should (equal "Consider the buffer first."
                 (madrigal-agent-controller--response-reasoning
                  '(:text "Done" :reasoning "Consider the buffer first."))))
  (should-not (madrigal-agent-controller--response-reasoning "Done")))

(ert-deftest madrigal-agent-controller-notifies-tool-only-responses ()
  (skip-unless (madrigal-llm-available-p))
  (let ((madrigal-providers '(("Provider" :provider fake-provider)))
        (madrigal-agent-models '(("assistant" . ("Provider" . "model"))))
        (madrigal--agents '(("assistant" :system-prompt "System" :tools nil)))
        event)
    (cl-letf (((symbol-function 'llm-chat-async)
               (lambda (_provider _prompt response-callback _error &optional _multi)
                 (funcall response-callback
                          '(:tool-uses ((:name "eval" :args nil))))
                 'request))
              ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
      (madrigal-agent-controller-submit-async
       :agent "assistant" :history '((:role user :content "Act"))
       :on-response (lambda (value) (setq event value))))
    (should event)
    (should-not (plist-get event :text))
    (should-not (plist-get event :final))))

(ert-deftest madrigal-agent-controller-context-size-reports-limit-and-percent ()
  (let ((madrigal--agents '(("assistant" :system-prompt "System" :tools nil))))
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

(ert-deftest madrigal-babel-lsp-diagnostics-does-not-leave-file-without-server ()
  (skip-unless (featurep 'eglot))
  (let ((directory (make-temp-file "madrigal-no-eglot-server-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'madrigal--babel-lsp-major-mode)
                   (lambda (_language) 'fundamental-mode))
                  ((symbol-function 'madrigal--babel-eglot-guess)
                   (lambda (_mode _project) nil)))
          (should-not
           (madrigal--babel-lsp-diagnostics
            "emacs-lisp" "(+ 1 2)" directory))
          (should-not (file-exists-p (expand-file-name "main.el" directory))))
      (delete-directory directory t))))

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
                        madrigal-context-buffer-text
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

(ert-deftest madrigal-append-assistant-text-omits-blank-notes ()
  (with-temp-buffer
    (org-mode)
    (madrigal--append-assistant-text "req-blank" " \n\t")
    (should-not (string-match-p "\\*\\* Note" (buffer-string)))))

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


(ert-deftest madrigal-context-normalize-discards-changed-origin-window ()
  (save-window-excursion
    (let ((origin (generate-new-buffer " *madrigal-origin*"))
          (other (generate-new-buffer " *madrigal-other*")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) origin)
            (with-current-buffer origin
              (insert "origin")
              (goto-char (point-min)))
            (let ((context (madrigal-context origin (selected-window))))
              (set-window-buffer (selected-window) other)
              (setq context (madrigal-context-normalize context))
              (should-not (madrigal-context-window context))
              (let (executed-in)
                (madrigal--call-with-action-context
                 context
                 (lambda () (setq executed-in (current-buffer))))
                (should (eq origin executed-in))
                (should (eq other (window-buffer (selected-window)))))))
        (kill-buffer origin)
        (kill-buffer other)))))





















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



(ert-deftest madrigal-do-interactive-highlights-before-prompt-and-cleans-up-on-quit ()
  (with-temp-buffer
    (insert "context text")
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
      (should-not (equal "#cccc33334ccc" (plist-get box :color)))
      (should (equal (plist-get context :background)
                     (plist-get
                      (madrigal-do--mode-line-face '(theme-accent . 0.5))
                      :foreground))))))

(ert-deftest madrigal-do-mode-line-cache-is-safe-in-nested-formatting ()
  (let ((madrigal-do--active-actions nil)
        (madrigal-do--mode-line-feedback nil))
    (madrigal-do--refresh-mode-line)
    (should (stringp (car global-mode-string)))
    (should (member madrigal-do--mode-line-entry global-mode-string))
    (should-not
     (string-match-p
      "invalid"
      (format-mode-line
       `((:eval (format-mode-line ',global-mode-string))))))))

(ert-deftest madrigal-do-mode-line-shows-coloured-request-spinners ()
  (let* ((first-face '(:foreground "#ff0000" :weight bold))
         (second-face '(:foreground "#00ff00" :weight bold))
         (first (madrigal-action-create
                 :instruction "First" :ui-face first-face))
         (second (madrigal-action-create
                  :instruction "Second" :ui-face second-face))
         (madrigal-do--active-actions (list second first))
         (madrigal-do--mode-line-feedback nil)
         (madrigal-do--spinner-index 0)
         (text (madrigal-do--mode-line-string))
         (first-spinner (string-match "⠋" text))
         (second-spinner (string-match "⠋" text (1+ first-spinner))))
    (should (string-prefix-p " 🧠 " text))
    (should (equal first-face (get-text-property first-spinner 'face text)))
    (should (equal second-face (get-text-property second-spinner 'face text)))))

(ert-deftest madrigal-do-mode-line-action-spinner-visits-and-cancels-request ()
  (let* ((context '(:scope (:target document)))
         (action (madrigal-action-create
                  :instruction "First" :context context :handle 'handle
                  :ui-face '(:foreground "#ff0000")))
         (madrigal-do--active-actions (list action))
         (madrigal-do--active-dwim-suggestions nil)
         (madrigal-do--mode-line-feedback nil)
         (madrigal-do--spinner-index 0)
         visited cancelled)
    (cl-letf (((symbol-function 'madrigal-do--visit-context)
               (lambda (value) (setq visited value)))
              ((symbol-function 'madrigal-agent-controller-cancel)
               (lambda (handle) (setq cancelled handle))))
      (let* ((text (madrigal-do--mode-line-string))
             (position (string-match "⠋" text))
             (map (get-text-property position 'local-map text)))
        (should (eq 'mode-line-highlight
                    (get-text-property position 'mouse-face text)))
        (funcall (lookup-key map [mode-line mouse-1]) nil)
        (should (eq context visited))
        (funcall (lookup-key map [mode-line mouse-3]) nil)
        (should (eq 'handle cancelled))))))

(ert-deftest madrigal-do-mode-line-suggestion-spinner-visits-and-cancels-request ()
  (let* ((context '(:scope (:target document)))
         (request (madrigal-dwim-suggestion-request-create
                   :action-context context :handle 'request-handle
                   :ui-face '(:foreground "#00ff00")))
         (madrigal-do--active-actions nil)
         (madrigal-do--active-dwim-suggestions (list request))
         (madrigal-do--recent-dwim-suggestions nil)
         (madrigal-do--mode-line-feedback nil)
         (madrigal-do--spinner-index 0)
         visited cancelled)
    (cl-letf (((symbol-function 'madrigal-do--visit-context)
               (lambda (value) (setq visited value)))
              ((symbol-function 'llm-cancel-request)
               (lambda (handle) (setq cancelled handle)))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (let* ((text (madrigal-do--mode-line-string))
             (position (string-match "⠋" text))
             (map (get-text-property position 'local-map text)))
        (funcall (lookup-key map [mode-line mouse-1]) nil)
        (should (eq context visited))
        (funcall (lookup-key map [mode-line mouse-3]) nil)
        (should (eq 'request-handle cancelled))
        (should (eq 'cancelled
                    (madrigal-dwim-suggestion-request-status request)))))))

(ert-deftest madrigal-do-mode-line-face-matches-point-highlight ()
  (with-temp-buffer
    (insert "alpha")
    (goto-char 3)
    (let* ((indicator (madrigal-do--make-request-indicator (madrigal-context)))
           (point-face (get-text-property
                        0 'face (overlay-get (cadr indicator) 'before-string)))
           (mode-line-face (madrigal-do--indicator-mode-line-face indicator)))
      (unwind-protect
          (should (equal (plist-get (plist-get point-face :box) :color)
                         (plist-get mode-line-face :foreground)))
        (madrigal-do--delete-request-indicator indicator)))))

(ert-deftest madrigal-do-mode-line-shows-suggestion-spinner ()
  (let* ((face '(:foreground "#ff0000" :weight bold))
         (request (madrigal-dwim-suggestion-request-create :ui-face face))
         (madrigal-do--active-actions nil)
         (madrigal-do--active-dwim-suggestions (list request))
         (madrigal-do--mode-line-feedback nil)
         (madrigal-do--spinner-index 0)
         (text (madrigal-do--mode-line-string))
         (spinner (string-match "⠋" text)))
    (should spinner)
    (should (equal face (get-text-property spinner 'face text)))))

(ert-deftest madrigal-do-minibuffer-indicator-hides-suggestion-spinner-and-cleans-up ()
  (let* ((face '(:foreground "#ff0000" :weight bold))
         (request (madrigal-dwim-suggestion-request-create :ui-face face))
         (madrigal-do--active-actions nil)
         (madrigal-do--active-dwim-suggestions (list request))
         (madrigal-do--mode-line-feedback nil)
         (madrigal-do--spinner-index 0)
         seen)
    (condition-case nil
        (madrigal-do--call-with-minibuffer-indicator
         face (lambda ()
                (setq seen (madrigal-do--mode-line-string))
                (signal 'quit nil)))
      (quit nil))
    (should (string-match-p "?" seen))
    (should-not (string-match-p "⠋" seen))
    (should-not madrigal-do--minibuffer-face)
    (should-not (string-match-p "?" madrigal-do--mode-line-construct))))

(ert-deftest madrigal-do-mode-line-completion-feedback-lasts-two-seconds ()
  (let* ((action (madrigal-action-create
                  :instruction "Done" :ui-face '(:foreground "#ff0000")))
         (madrigal-do--active-actions nil)
         (madrigal-do--mode-line-feedback nil)
         scheduled-delay scheduled-callback)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (delay _repeat callback &rest _)
                 (setq scheduled-delay delay
                       scheduled-callback callback)
                 'timer))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (madrigal-do--add-mode-line-feedback action "✓")
      (should (= 2 scheduled-delay))
      (should (string-match-p "✓" (madrigal-do--mode-line-string)))
      (funcall scheduled-callback)
      (should-not (madrigal-do--mode-line-string)))))

(ert-deftest madrigal-do-request-indicators-survive-edits-until-cleanup ()
  (with-temp-buffer
    (insert "first line\nsecond line")
    (goto-char 5)
    (let ((default-directory temporary-file-directory))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (let ((indicator
               (madrigal-do--make-request-indicator
                (madrigal-context (current-buffer)))))
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
        (let* ((first (madrigal-context (current-buffer)))
               (first-indicator (madrigal-do--make-request-indicator first)))
          (goto-char (point-max))
          (let ((second-indicator
                 (madrigal-do--make-request-indicator
                  (madrigal-context (current-buffer)))))
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

(ert-deftest madrigal-eval-tool-uses-captured-point-after-point-moves ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (insert "abcdef")
    (goto-char 3)
    (let* ((default-directory temporary-file-directory)
           (context (cl-letf (((symbol-function 'project-current)
                               (lambda (&rest _) nil)))
                      (madrigal-context (current-buffer))))
           callback-result)
      (goto-char 6)
      (let ((tool (madrigal--make-eval-tool
                   (current-buffer) "do-context" #'ignore context)))
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
                      (madrigal-context (current-buffer))))
           callback-result)
      (goto-char (point-min))
      (insert "X")
      (goto-char (point-max))
      (let ((tool (madrigal--make-eval-tool
                   (current-buffer) "do-context" #'ignore context)))
        (funcall (llm-tool-function tool)
                 (lambda (value) (setq callback-result value))
                 "(point)"))
      (should (string-match-p ":value 4" callback-result)))))

(ert-deftest madrigal-do-keeps-only-final-org-response ()
  (with-temp-buffer
    (let ((default-directory temporary-file-directory)
          (madrigal-do--active-actions nil)
          (madrigal-do--recent-actions nil))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'madrigal-do--show-result) #'ignore)
                ((symbol-function 'madrigal-agent-controller-submit-async)
                 (lambda (&rest args)
                   (funcall (plist-get args :on-start) '(:model "model"))
                   (let ((sink (plist-get (plist-get args :environment) :event-sink)))
                     (funcall sink '(:phase started :id "tool-1" :name "eval"
                                     :source "(buffer-name)"))
                     (funcall sink '(:phase finished :id "tool-1" :name "eval"
                                     :source "(buffer-name)" :result "buffer")))
                   (funcall (plist-get args :on-response) '(:final nil))
                   (funcall (plist-get args :on-response)
                            '(:text "```org\n* Updated\nThe buffer is current.\n```"
                              :reasoning "Checked the selected buffer."
                              :final t))
                   (funcall (plist-get args :on-finished) nil)
                   (should-not (plist-member args :response-format))
                   (madrigal-agent-controller-handle-create
                    :provider 'provider :model "model"))))
        (let ((action (madrigal-do
                       "Update it"
                       (madrigal-context (current-buffer)))))
          (should (equal "* Updated\nThe buffer is current."
                         (madrigal-action-response action)))
          (should (= 3 (length (madrigal-action-turns action))))
          (should (equal "Checked the selected buffer."
                         (madrigal-action-turn-reasoning
                          (nth 2 (madrigal-action-turns action)))))
          (should (equal '("tool-1")
                         (mapcar #'madrigal-tool-event-id
                                 (madrigal-action-turn-tool-events
                                  (nth 1 (madrigal-action-turns action))))))
          (should-not (madrigal-action-turn-tool-events
                       (nth 2 (madrigal-action-turns action))))
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

(ert-deftest madrigal-do-routes-responses-longer-than-three-lines-to-a-buffer ()
  (should-not (madrigal-do--response-more-than-three-lines-p "one\ntwo\nthree"))
  (should-not (madrigal-do--response-more-than-three-lines-p "one\ntwo\nthree\n"))
  (should (madrigal-do--response-more-than-three-lines-p
           "one\ntwo\nthree\nfour"))
  (let ((action (madrigal-action-create :response "one\ntwo\nthree\nfour"))
        displayed)
    (cl-letf (((symbol-function 'madrigal-do--show-document)
               (lambda (value) (setq displayed value))))
      (madrigal-do--show-result action))
    (should (eq action displayed))))

(ert-deftest madrigal-do-pops-read-only-org-document-with-quit-binding ()
  (let* ((action (madrigal-action-create
                  :id "document-1"
                  :response "#+title: Result\n\n* Details\nComplete"))
         displayed)
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _) (setq displayed buffer))))
      (madrigal-do--show-result action))
    (unwind-protect
        (with-current-buffer displayed
          (should (equal "*Madrigal response: Result*" (buffer-name)))
          (should (derived-mode-p 'org-mode))
          (should buffer-read-only)
          (should (eq #'quit-window (key-binding (kbd "q"))))
          (should (equal "#+title: Result\n\n* Details\nComplete"
                         (buffer-string))))
      (kill-buffer displayed))))

(ert-deftest madrigal-do-promotes-a-fully-fenced-org-response ()
  (should (equal "#+title: Result\n\n* Details\nComplete"
                 (madrigal-do--promote-org-response
                  "```org\n#+title: Result\n\n* Details\nComplete\n```\n")))
  (should (equal "Text before\n```org\n* Details\n```"
                 (madrigal-do--promote-org-response
                  "Text before\n```org\n* Details\n```"))))

(ert-deftest madrigal-do-response-title-prefers-title-heading-then-prompt ()
  (should (equal "Named report"
                 (madrigal-do--response-title
                  (madrigal-action-create
                   :instruction "Fallback"
                   :response "#+TITLE: Named report\n\n* Heading"))))
  (should (equal "First heading"
                 (madrigal-do--response-title
                  (madrigal-action-create
                   :instruction "Fallback"
                   :response "Intro\n* First heading\nBody"))))
  (should (equal "Explain the selection"
                 (madrigal-do--response-title
                  (madrigal-action-create
                   :instruction "Explain the selection"
                   :response "A response without a title.")))))

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

(ert-deftest madrigal-do-history-reads-system-context-from-prompt-interactions ()
  (let ((prompt (llm-make-chat-prompt "Inspect" :context "System instructions")))
    (cl-letf (((symbol-function 'llm-chat-prompt-context) (lambda (_) nil)))
      (should (equal "System instructions"
                     (madrigal-do--prompt-system-context prompt))))))

(ert-deftest madrigal-do-history-renders-turns-and-tools-in-read-only-org-buffer ()
  (let* ((prompt (llm-make-chat-prompt
                  "Inspect this"
                  :context
                  "* Instructions\nSystem instructions\n* Context\n#+begin_src emacs-lisp\n'(:text \"Selected context\")\n#+end_src"))
         (action
          (madrigal-action-create
           :id "history-1" :instruction "Inspect this"
           :handle (madrigal-agent-controller-handle-create :prompt prompt)
           :turns (list (madrigal-action-turn-create
                         :role 'user :text "Inspect this")
                        (madrigal-action-turn-create
                         :role 'assistant :kind 'intermediate
                         :reasoning "Check the live value."
                         :tool-events (list (madrigal-tool-event-create
                                             :name "eval" :language "emacs-lisp"
                                             :source "(+ 1 1)"
                                             :result '(:ok t :value 2))))
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
            (should (eq (key-binding (kbd "g"))
                        #'madrigal-do--refresh-history-buffer))
            (should (string-match-p
                     (regexp-quote
                      "* System prompt\n** Instructions\nSystem instructions\n** Context\n#+begin_src emacs-lisp")
                     (buffer-string)))
            (should-not (string-match-p "System prompt and model context"
                                        (buffer-string)))
            (should (string-match-p (regexp-quote "* User\nInspect this")
                                    (buffer-string)))
            (should (string-match-p
                     (regexp-quote "** Reasoning\nCheck the live value.")
                     (buffer-string)))
            (should-not (string-match-p (regexp-quote "** Note")
                                        (buffer-string)))
            (should (string-match-p (regexp-quote "** Response\nDone.")
                                    (buffer-string)))
            (should (string-match-p (regexp-quote "** Tools\n*** eval")
                                    (buffer-string)))
            (let ((reasoning-position
                   (string-match (regexp-quote "** Reasoning") (buffer-string)))
                  (tool-position
                   (string-match (regexp-quote "** Tools") (buffer-string)))
                  (response-position
                   (string-match (regexp-quote "** Response") (buffer-string))))
              (should (< reasoning-position tool-position response-position)))
            (should (string-match-p (regexp-quote "(+ 1 1)")
                                    (buffer-string)))
            (should (string-match-p (regexp-quote ":value 2")
                                    (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))



(ert-deftest madrigal-do-history-refresh-renders-current-action-state ()
  (let* ((action (madrigal-action-create
                  :id "refresh-1" :instruction "Inspect"
                  :turns (list (madrigal-action-turn-create
                                :role 'user :text "Inspect"))))
         buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (value &rest _) (setq buffer value))))
          (madrigal-do--render-history action)
          (setf (madrigal-action-turns action)
                (append (madrigal-action-turns action)
                        (list (madrigal-action-turn-create
                               :role 'assistant :final t :text "Finished"))))
          (with-current-buffer buffer
            (call-interactively (key-binding (kbd "g")))
            (should (string-match-p "Finished" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest madrigal-do-dwim-history-binds-refresh ()
  (let* ((prompt (llm-make-chat-prompt
                  "Suggest"
                  :context "* Instructions and context\nDWIM system instructions"))
         (request (madrigal-dwim-suggestion-request-create
                   :id "dwim-refresh-1" :context '(:scope (:target session))
                   :prompt prompt :response "response"))
         buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (value &rest _) (setq buffer value))))
          (madrigal-do--render-dwim-history request)
          (with-current-buffer buffer
            (should (eq (key-binding (kbd "g"))
                        #'madrigal-do--refresh-history-buffer))
            (should (string-match-p "DWIM system instructions"
                                    (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest madrigal-do-history-candidates-use-small-context-excerpts ()
  (with-temp-buffer
    (insert (make-string 200 ?a) "TEXT-AT-POINT" (make-string 200 ?z))
    (search-backward "TEXT-AT-POINT")
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (let* ((context (madrigal-context (current-buffer)))
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
                    (madrigal-context buffer)))
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



(ert-deftest madrigal-do-introspection-exposes-current-action-history ()
  (skip-unless (madrigal-llm-available-p))
  (with-temp-buffer
    (insert "context")
    (let* ((default-directory temporary-file-directory)
           (context
            (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
              (madrigal-context (current-buffer))))
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

(ert-deftest madrigal-do-parses-and-sorts-action-and-prompt-suggestions ()
  (let ((suggestions
         (madrigal-do--parse-suggestions
          "{\"suggestions\":[{\"relevance\":0.7,\"do-prompt\":\"Explain the symbol at point.\"},{\"relevance\":0.9,\"action-description\":\"Run the test\",\"action-source\":\"(ert-run-test-at-point)\"}]}")))
    (should (= 2 (length suggestions)))
    (should (madrigal-do--suggestion-action-p (car suggestions)))
    (should (equal "Run the test"
                   (madrigal-do--suggestion-label (car suggestions))))))

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

(ert-deftest madrigal-do-discards-invalid-suggestions-independently ()
  (let ((suggestions
         (madrigal-do--parse-suggestions
          "{\"suggestions\":[{\"relevance\":1.1,\"do-prompt\":\"Act\"},{\"relevance\":0.5,\"do-prompt\":\"Explain this.\"}]}")))
    (should (= 1 (length suggestions)))
    (should (= 1 (length madrigal-do--last-suggestion-diagnostics)))))

(ert-deftest madrigal-do-suggestion-schema-is-an-unbounded-sum-of-products ()
  (let* ((properties (plist-get madrigal-do--suggestion-response-schema
                                :properties))
         (array (plist-get properties :suggestions))
         (alternatives (append (plist-get (plist-get array :items) :anyOf)
                               nil)))
    (should (equal '(:suggestions) (madrigal-do--plist-keys properties)))
    (should-not (plist-member array :maxItems))
    (should (= 2 (length alternatives)))
    (dolist (alternative alternatives)
      (dolist (field '(:do-prompt :action-description :action-source))
        (when-let* ((property
                     (plist-get (plist-get alternative :properties) field)))
          (should-not (plist-member property :maxLength)))))))

(ert-deftest madrigal-do-accepts-unbounded-prompt-and-source-suggestions ()
  (let* ((prompt (make-string 501 ?p))
         (source (make-string 16001 ?s))
         (suggestions
          (madrigal-do--parse-suggestions
           (json-serialize
            `(:suggestions
              [(:relevance 0.5 :do-prompt ,prompt)
               (:relevance 0.5 :action-description "Run it"
                :action-source ,source)])))))
    (let ((prompt-suggestion
           (seq-find (lambda (suggestion)
                       (madrigal-action-suggestion-prompt suggestion))
                     suggestions))
          (action-suggestion
           (seq-find #'madrigal-do--suggestion-action-p suggestions)))
      (should (equal prompt
                     (madrigal-action-suggestion-prompt prompt-suggestion)))
      (should (equal source
                     (plist-get
                      (plist-get
                       (plist-get (madrigal-action-suggestion-action action-suggestion)
                                  :tool-call)
                       :arguments)
                      :source))))))

(ert-deftest madrigal-do-completion-keeps-multiline-prompts-and-omits-source ()
  (let* ((prompt "Explain this.\nInclude an example.")
         (source "(message \"never display this\")")
         (suggestions
          (madrigal-do--parse-suggestions
           (json-serialize
            `(:suggestions
              [(:relevance 0.9 :do-prompt ,prompt)
               (:relevance 0.8 :action-description "Run tests"
                :action-source ,source)]))))
         (candidates (mapcar (lambda (suggestion)
                               (cons (madrigal-do--suggestion-label suggestion)
                                     suggestion))
                             suggestions))
         (completions
          (all-completions ""
                           (madrigal-do--suggestion-completion-table candidates))))
    (should (member prompt completions))
    (should-not (seq-some (lambda (completion) (string-match-p source completion))
                          completions))))

(ert-deftest madrigal-do-completion-affixes-match-suggestion-kinds ()
  (let* ((prompt (madrigal-action-suggestion-create
                  :relevance 0.5 :prompt "Explain this"))
         (action (madrigal-action-suggestion-create
                  :relevance 0.5
                  :action '(:description "Run tests" :tool-call (:name "eval"))))
         (candidates `(("Explain this" . ,prompt) ("Run tests" . ,action)))
         (table (madrigal-do--suggestion-completion-table candidates))
         (metadata (funcall table "" nil 'metadata))
         (affixation (cdr (assq 'affixation-function (cdr metadata))))
         (affixes (funcall affixation '("Explain this" "Run tests"))))
    (should (string-match-p "🧠" (nth 1 (car affixes))))
    (should (string-match-p "⚡" (nth 1 (cadr affixes))))))

(ert-deftest madrigal-do-accepts-more-than-eight-suggestions ()
  (let ((entries
         (mapconcat
          (lambda (index)
            (format "{\"relevance\":0.5,\"do-prompt\":\"Explain item %d.\"}" index))
          (number-sequence 1 9) ",")))
    (should (= 9
               (length
                (madrigal-do--parse-suggestions
                 (format "{\"suggestions\":[%s]}" entries)))))))





(ert-deftest madrigal-do-dwim-removes-indicator-when-suggestion-read-quits ()
  (with-temp-buffer
    (insert "context")
    (let* ((context (madrigal-context (current-buffer)))
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
                     "{\"suggestions\":[{\"relevance\":1,\"do-prompt\":\"Act\"}]}")
          (quit nil))
        (should (eq 'cancelled
                    (madrigal-dwim-suggestion-request-status request)))
        (should-not madrigal-do--active-dwim-suggestions)
        (should-not (overlay-buffer (car indicator)))))))

(ert-deftest madrigal-do-dwim-completion-accepts-a-custom-action ()
  (with-temp-buffer
    (let* ((context (madrigal-context (current-buffer)))
           (suggestions
            (list (madrigal-action-suggestion-create
                   :relevance 0.9 :prompt "Use the suggestion.")))
           require-match)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _collection _predicate required &rest _)
                   (setq require-match required)
                   "Run my own action.")))
        (should (equal "Run my own action."
                       (madrigal-do--read-suggestion context suggestions)))
        (should-not require-match)))))



(ert-deftest madrigal-do-discards-malformed-invocations-and-duplicate-labels ()
  (let ((suggestions
         (madrigal-do--parse-suggestions
          "{\"suggestions\":[{\"relevance\":1,\"action-description\":\"Change\"},{\"relevance\":0.8,\"do-prompt\":\"Explain this.\"},{\"relevance\":0.7,\"do-prompt\":\"Explain this.\"}]}")))
    (should (= 1 (length suggestions)))
    (should (= 2 (length madrigal-do--last-suggestion-diagnostics)))))

(ert-deftest madrigal-do-immediate-selection-invokes-eval-once-without-controller ()
  (with-temp-buffer
    (let* ((context (madrigal-context (current-buffer)))
           (suggestion
            (madrigal-action-suggestion-create
             :relevance 1
             :action '(:description "Insert text"
                       :tool-call (:name "eval"
                                   :arguments (:source "(insert \"done\")")))))
           (madrigal-do--recent-immediate-actions nil)
           (eval-count 0)
           operation displayed)
      (cl-letf (((symbol-function 'madrigal-agent-controller-submit-async)
                 (lambda (&rest _) (ert-fail "Unexpected controller request")))
                ((symbol-function 'madrigal--run-eval-tool)
                 (lambda (_tool _buffer _id callback _source sink)
                   (setq eval-count (1+ eval-count))
                   (funcall sink '(:phase started :id "tool" :name "eval"))
                   (funcall sink '(:phase finished :id "tool" :name "eval"
                                   :result (:ok t :value "ok")
                                   :formatted-result "(:ok t :value ok)"))
                   (funcall callback "(:ok t :value ok)")))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq displayed (apply #'format format-string args)))))
        (setq operation
              (madrigal-do--dispatch-suggestion context suggestion nil)))
      (should (= 1 eval-count))
      (should (equal "ok" displayed))
      (should (equal "ok" (madrigal-immediate-action-result operation)))
      (should (equal "ok"
                     (madrigal-tool-event-result
                      (car (madrigal-immediate-action-tool-events operation)))))
      (should (eq 'finished (madrigal-immediate-action-status operation)))
      (should-not madrigal-do--active-actions)
      (should (= 1 (length madrigal-do--recent-immediate-actions))))))

(ert-deftest madrigal-do-immediate-failure-shows-cross-and-error-text ()
  (let ((text (concat "❌ "
                      (madrigal-do--immediate-error-text
                       '(void-variable missing)))))
    (should (string-prefix-p "❌ " text))
    (should (string-match-p "missing" text))))

(ert-deftest madrigal-do-immediate-history-selects-and-renders-operations ()
  (let* ((suggestion
          (madrigal-action-suggestion-create
           :relevance 1
           :action '(:description "Insert text"
                     :tool-call (:name "eval" :arguments (:source "(insert \"ok\")")))))
         (event (madrigal-tool-event-create
                 :id "tool-1" :name "eval" :language "emacs-lisp"
                 :source "(insert \"ok\")" :result "ok"
                 :started-at (current-time) :finished-at (current-time)))
         (operation
          (madrigal-immediate-action-create
           :id "immediate-1" :suggestion suggestion :status 'finished
           :result "ok" :tool-events (list event) :started-at (current-time)
           :finished-at (current-time)))
         (madrigal-do--recent-immediate-actions (list operation))
         displayed)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (car (all-completions "" collection))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) (setq displayed buffer))))
      (should (eq operation (madrigal-do--read-immediate-history-operation)))
      (madrigal-do-immediate-history operation))
    (unwind-protect
        (with-current-buffer displayed
          (should (derived-mode-p 'org-mode))
          (should buffer-read-only)
          (should (eq #'quit-window (key-binding (kbd "q"))))
          (should (eq #'madrigal-do--refresh-history-buffer
                      (key-binding (kbd "g"))))
          (should (string-match-p "Insert text" (buffer-string)))
          (should (string-match-p "(insert \\\"ok\\\")" (buffer-string)))
          (should (string-match-p "ok" (buffer-string))))
      (kill-buffer displayed))))

(ert-deftest madrigal-do-prompt-selection-uses-ordinary-executor ()
  (let ((suggestion
         (madrigal-action-suggestion-create :prompt "Explain this."))
        called)
    (cl-letf (((symbol-function 'madrigal-do--execute)
               (lambda (context instruction kind indicator)
                 (setq called (list context instruction kind indicator)))))
      (madrigal-do--dispatch-suggestion 'context suggestion 'indicator))
    (should (equal '(context "Explain this." dwim indicator) called))))

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

(ert-deftest madrigal-agent-definitions-are-internal ()
  (should-not (custom-variable-p 'madrigal--agents))
  (should (equal '("eval" "persist-elisp")
                 (plist-get (madrigal--agent-definition "do") :tools)))
  (should (equal madrigal--do-system-prompt
                 (plist-get (madrigal--agent-definition "do") :system-prompt)))
  (should (equal '("do" "do-dwim")
                 (seq-filter (lambda (name) (member name '("do" "do-dwim")))
                             (madrigal--selectable-agent-names)))))

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

(ert-deftest madrigal-do-system-prompt-requires-an-org-response ()
  (should (string-match-p
           (regexp-quote "When finished, return your response as org-mode formatted text.")
           madrigal--do-system-prompt))
  (should (string-match-p
           (regexp-quote "Emacs chooses and names any response buffer")
           madrigal--do-system-prompt)))

(ert-deftest madrigal-do-suggestion-prompt-always-requests-json-schema ()
  (with-temp-buffer
    (let ((default-directory temporary-file-directory))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (let* ((context (madrigal-context (current-buffer)))
               (prompt (madrigal-do--suggestion-prompt context)))
          (should (eq madrigal-do--suggestion-response-schema
                      (llm-chat-prompt-response-format prompt)))
          (should-not (llm-chat-prompt-temperature prompt))
          (should (eq 'none (llm-chat-prompt-reasoning prompt)))
          (should (string-match-p
                   (regexp-quote "Use action-description and action-source")
                   (llm-chat-prompt-context prompt)))
          (should (string-match-p
                   (regexp-quote "Return action-description as org-mode formatted text.")
                   (llm-chat-prompt-context prompt)))
          (should (string-match-p
                   (regexp-quote "Do not perform an action or emit tool calls")
                   (llm-chat-prompt-context prompt)))
          (should-not (string-match-p
                       (regexp-quote ":response-schema")
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

(ert-deftest madrigal-context-source-freezes-document-text ()
  (with-temp-buffer
    (insert "alpha beta")
    (goto-char 3)
    (let ((source (madrigal-context-capture)))
      (erase-buffer)
      (should (equal "alpha beta" (madrigal-context-source-text source))))))

(ert-deftest madrigal-context-active-region-always-wins-default-selection ()
  (with-temp-buffer
    (insert "alpha beta gamma")
    (goto-char 11)
    (push-mark 7 t t)
    (activate-mark)
    (let* ((source (madrigal-context-capture))
           (candidates (madrigal-context-discover source))
           (selected (madrigal-context-select-default source candidates)))
      (should (equal "region" (madrigal-context-candidate-label selected)))
      (should (equal '(:active-region t :exact t)
                     (madrigal-context-candidate-signals selected)))
      (should (equal "beta"
                     (plist-get
                      (madrigal-context-document-metadata
                       (madrigal-context-materialize
                        (madrigal-context-selection-create
                         :source source :candidate selected)))
                      :text))))))

(ert-deftest madrigal-context-discovery-preserves-same-range-labels ()
  (with-temp-buffer
    (insert "alpha")
    (goto-char 3)
    (let* ((source (madrigal-context-capture))
           (candidates (madrigal-context-discover source))
           (same-range
            (seq-filter
             (lambda (candidate)
               (and (eq (madrigal-context-candidate-target candidate) 'document)
                    (= 1 (madrigal-context-candidate-start candidate))
                    (= 6 (madrigal-context-candidate-end candidate))))
             candidates)))
      (should (> (length same-range) 1))
      (should (> (length (delete-dups
                          (mapcar #'madrigal-context-candidate-label same-range)))
                 1)))))

(ert-deftest madrigal-context-provider-relevance-must-be-bounded ()
  (with-temp-buffer
    (insert "alpha")
    (let ((source (madrigal-context-capture)))
      (should-error
       (madrigal-context--validate-relevance
        (madrigal-context-document-candidate
         source 'test 'invalid "invalid" 1 6 2.0)))
      (should (= 1.0
                 (madrigal-context-candidate-score
                  (madrigal-context--validate-relevance
                   (madrigal-context-document-candidate
                    source 'test 'maximum "maximum" 1 6 1.0))))))))

(ert-deftest madrigal-context-default-order-is-stable ()
  (with-temp-buffer
    (insert "alpha beta")
    (goto-char 3)
    (let* ((source (madrigal-context-capture))
           (first-candidates (madrigal-context-discover source))
           (first (mapcar #'madrigal-context-candidate-id first-candidates))
           (second (mapcar #'madrigal-context-candidate-id
                           (madrigal-context-discover source))))
      (should (equal first second))
      (dolist (candidate first-candidates)
        (should (<= 0.0 (madrigal-context-candidate-score candidate) 1.0)))
      (should (string-match-p
               "\\`[●◕◑◔○] [^ ]+ +[^ ]+ +\\["
               (madrigal-context--candidate-display
                (car first-candidates)))))))

(ert-deftest madrigal-context-provider-priority-multiplies-scope-priority ()
  (with-temp-buffer
    (insert "alpha")
    (let* ((madrigal-context-providers
            (list
             (madrigal-context-provider-create
              :name 'generic :priority 0.5 :applicable (lambda (_) t)
              :discover
              (lambda (source)
                (list (madrigal-context-document-candidate
                       source 'generic 'whole "generic scope" 1 6 1.0))))
             (madrigal-context-provider-create
              :name 'specific :priority 1.0 :applicable (lambda (_) t)
              :discover
              (lambda (source)
                (list (madrigal-context-document-candidate
                       source 'specific 'whole "specific scope" 1 6 0.6))))))
           (candidates (madrigal-context-discover (madrigal-context-capture))))
      (should (equal 'specific
                     (madrigal-context--candidate-provider (car candidates))))
      (should (= 0.6 (madrigal-context-candidate-score (car candidates))))
      (should (= 0.5 (madrigal-context-candidate-score (cadr candidates)))))))

(ert-deftest madrigal-context-provider-limit-is-the-only-truncation ()
  (with-temp-buffer
    (insert "abcdefghij")
    (let* ((source (madrigal-context-capture))
           (candidate (madrigal-context-document-candidate
                       source 'test 'all "all" 1 11 0.5 :confidence 1.0))
           (selection (madrigal-context-selection-create
                       :source source :candidate candidate))
           (complete (madrigal-context-materialize selection))
           (limited (madrigal-context-materialize selection 4)))
      (should (equal "abcdefghij"
                     (plist-get (madrigal-context-document-metadata complete)
                                :text)))
      (should (equal "abcd"
                     (plist-get (madrigal-context-document-metadata limited)
                                :text)))
      (should (eq 'truncated
                  (plist-get (plist-get limited :scope)
                             :provider-context-limit-status))))))

(ert-deftest madrigal-context-session-target-is-private ()
  (with-temp-buffer
    (insert "private document")
    (let* ((source (madrigal-context-capture))
           (candidate (seq-find
                       (lambda (item)
                         (eq 'session
                             (madrigal-context-candidate-target item)))
                       (madrigal-context-discover source)))
           (context (madrigal-context-materialize
                     (madrigal-context-selection-create
                      :source source :candidate candidate)))
           (model-context (madrigal-context-model-data context)))
      (should (plist-get model-context :session))
      (should-not (plist-member model-context :origin))
      (should-not (plist-member model-context :project))
      (should-not (string-match-p "private document"
                                  (prin1-to-string model-context))))))

(ert-deftest madrigal-context-project-target-has-metadata-only ()
  (with-temp-buffer
    (insert "private document")
    (let* ((root (file-name-as-directory temporary-file-directory))
           (project 'fake-project))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) project))
                ((symbol-function 'project-root) (lambda (_) root))
                ((symbol-function 'project-name) (lambda (_) "fake")))
        (let* ((source (madrigal-context-capture))
               (candidate (seq-find
                           (lambda (item)
                             (eq 'project
                                 (madrigal-context-candidate-target item)))
                           (madrigal-context-discover source)))
               (context (madrigal-context-materialize
                         (madrigal-context-selection-create
                          :source source :candidate candidate))))
          (should (equal "fake" (plist-get (plist-get context :project) :name)))
          (should-not (plist-member context :origin))
          (should-not (string-match-p "private document"
                                      (madrigal-context-render context))))))))

(ert-deftest madrigal-context-render-uses-an-org-elisp-block ()
  (with-temp-buffer
    (insert "selected text")
    (let ((rendered (madrigal-context-render (madrigal-context (current-buffer)))))
      (should (string-prefix-p "* Context\n" rendered))
      (should (string-match-p (regexp-quote "#+begin_src emacs-lisp\n'(")
                              rendered))
      (should (string-suffix-p "#+end_src" rendered)))))

(ert-deftest madrigal-context-selects-project-scope-without-prompting ()
  (with-temp-buffer
    (let* ((root (file-name-as-directory temporary-file-directory))
           (project 'fake-project))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) project))
                ((symbol-function 'project-root) (lambda (_) root))
                ((symbol-function 'project-name) (lambda (_) "fake"))
                ((symbol-function 'madrigal-context-read-candidate)
                 (lambda (&rest _)
                   (ert-fail "Scope completion should not be opened"))))
        (let ((context (madrigal-context-choose nil nil nil nil nil 'project)))
          (should (eq 'project (plist-get (plist-get context :scope) :target)))
          (should (equal "fake" (plist-get (plist-get context :project) :name)))
          (should-not (plist-member context :origin)))))))

(ert-deftest madrigal-context-project-and-session-have-zero-relevance ()
  (with-temp-buffer
    (let* ((source (madrigal-context-capture))
           (candidates (madrigal-context-discover source)))
      (dolist (candidate candidates)
        (when (memq (madrigal-context-candidate-target candidate)
                    '(project session))
          (should (= 0.0 (madrigal-context-candidate-score candidate))))))))

(ert-deftest madrigal-dwim-suggestion-display-uses-relevance-and-kind ()
  (let* ((suggestion
          (madrigal-action-suggestion-create
           :relevance 0.9 :prompt "Inspect it"))
         (display (madrigal-do--suggestion-display suggestion)))
    (should (string-match-p "🧠" display))
    (should (string-match-p "Inspect it" display))))

(ert-deftest madrigal-context-completion-uses-request-preview-face ()
  (with-temp-buffer
    (insert "alpha")
    (goto-char 3)
    (let* ((source (madrigal-context-capture))
           (candidates (madrigal-context-discover source))
           seen-face seen-completion-properties)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt entries &rest _)
                   (setq seen-face
                         (overlay-get (car (overlays-at (point))) 'face)
                         seen-completion-properties completion-extra-properties)
                   (caar entries))))
        (madrigal-context-read-candidate source candidates 'madrigal-test-face))
      (should (eq 'madrigal-test-face seen-face))
      (should (eq 'identity
                  (plist-get seen-completion-properties :display-sort-function)))
      (should-not (overlays-at (point))))))

(defun madrigal-test--context-candidate-by-provider (provider candidates)
  "Return PROVIDER's first member of CANDIDATES."
  (seq-find (lambda (candidate)
              (eq provider (madrigal-context--candidate-provider candidate)))
            candidates))

(ert-deftest madrigal-context-outline-provider-walks-org-ancestors ()
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n** Child\nBody\n")
    (goto-char (point-max))
    (let* ((source (madrigal-context-capture))
           (candidates (madrigal-context--outline-discover source)))
      (should (= 2 (length candidates)))
      (should (seq-every-p
               (lambda (candidate)
                 (equal "Org subtree"
                        (madrigal-context-candidate-label candidate)))
               candidates)))))

(ert-deftest madrigal-context-diff-provider-selects-hunk-and-file ()
  (require 'diff-mode)
  (with-temp-buffer
    (insert "--- a/demo\n+++ b/demo\n@@ -1 +1 @@\n-old\n+new\n")
    (diff-mode)
    (goto-char (point-min))
    (search-forward "+new")
    (let* ((source (madrigal-context-capture))
           (candidates (madrigal-context--diff-discover source)))
      (should (seq-find (lambda (candidate)
                          (equal "diff hunk"
                                 (madrigal-context-candidate-label candidate)))
                        candidates))
      (should (seq-find (lambda (candidate)
                          (equal "diff file"
                                 (madrigal-context-candidate-label candidate)))
                        candidates)))))

(ert-deftest madrigal-context-compilation-provider-selects-diagnostic ()
  (require 'compile)
  (with-temp-buffer
    (compilation-mode)
    (let ((inhibit-read-only t))
      (insert "demo.el:3:2: Something failed\n")
      (put-text-property (point-min) (point-max)
                         'compilation-message 'diagnostic))
    (goto-char (+ (point-min) 5))
    (let* ((source (madrigal-context-capture))
           (candidate (car (madrigal-context--compilation-discover source))))
      (should (equal "compilation diagnostic"
                     (madrigal-context-candidate-label candidate)))
      (should (equal (cons (point-min) (point-max))
                     (cons (madrigal-context-candidate-start candidate)
                           (madrigal-context-candidate-end candidate)))))))

(ert-deftest madrigal-context-dired-provider-selects-entry ()
  (require 'dired)
  (let* ((directory (make-temp-file "madrigal-dired-" t))
         (file (expand-file-name "entry.txt" directory))
         buffer)
    (unwind-protect
        (progn
          (write-region "hello" nil file nil 'silent)
          (setq buffer (dired-noselect directory))
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "entry.txt")
            (let* ((source (madrigal-context-capture))
                   (candidate (car (madrigal-context--dired-discover source))))
              (should (equal "Dired entry"
                             (madrigal-context-candidate-label candidate)))
              (should (string-match-p
                       "entry.txt"
                       (madrigal-context--source-text
                        source
                        (madrigal-context-candidate-start candidate)
                        (madrigal-context-candidate-end candidate)))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest madrigal-context-tabulated-list-provider-selects-entry ()
  (require 'tabulated-list)
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 20 t)]
          tabulated-list-entries '((demo ["Demo row"])))
    (tabulated-list-init-header)
    (tabulated-list-print)
    (goto-char (point-min))
    (search-forward "Demo row")
    (let* ((source (madrigal-context-capture))
           (candidate (car (madrigal-context--tabulated-list-discover source))))
      (should (equal "tabulated-list entry"
                     (madrigal-context-candidate-label candidate))))))

(ert-deftest madrigal-context-message-provider-distinguishes-header-and-body ()
  (require 'message)
  (with-temp-buffer
    (insert "To: person@example.test\nSubject: Demo\n"
            mail-header-separator "\nBody text\n")
    (message-mode)
    (goto-char (point-min))
    (let* ((source (madrigal-context-capture))
           (candidate (seq-find
                       (lambda (item)
                         (equal "message header"
                                (madrigal-context-candidate-label item)))
                       (madrigal-context--message-discover source))))
      (should candidate))
    (goto-char (point-max))
    (let* ((source (madrigal-context-capture))
           (candidate (seq-find
                       (lambda (item)
                         (equal "message body"
                                (madrigal-context-candidate-label item)))
                       (madrigal-context--message-discover source))))
      (should candidate)
      (should (string-match-p
               "Body text"
               (madrigal-context--source-text
                source (madrigal-context-candidate-start candidate)
                (madrigal-context-candidate-end candidate)))))))

(ert-deftest madrigal-context-shell-provider-selects-comint-interaction ()
  (require 'comint)
  (with-temp-buffer
    (comint-mode)
    (setq-local comint-prompt-regexp "^\\$ "
                comint-use-prompt-regexp t)
    (let ((inhibit-read-only t))
      (insert "$ first\nfirst output\n$ second\nsecond output\n"))
    (goto-char (point-min))
    (search-forward "first output")
    (let* ((source (madrigal-context-capture))
           (candidates (madrigal-context--shell-discover source))
           (candidate (seq-find
                       (lambda (item)
                         (equal "Terminal prompt interaction"
                                (madrigal-context-candidate-label item)))
                       candidates))
           (text (madrigal-context--source-text
                  source (madrigal-context-candidate-start candidate)
                  (madrigal-context-candidate-end candidate))))
      (should (string-prefix-p "first" text))
      (should (string-suffix-p "$ " text))
      (should-not (string-match-p "second output" text)))))

(ert-deftest madrigal-context-shell-provider-uses-terminal-prompt-navigation ()
  (with-temp-buffer
    (setq major-mode 'eat-mode)
    (insert "λ first\nfirst output\nλ second\nsecond output\n")
    (goto-char (point-min))
    (search-forward "first output")
    (cl-letf (((symbol-function 'eat-previous-shell-prompt)
               (lambda (_)
                 (re-search-backward "^λ " nil t)
                 (goto-char (match-end 0))))
              ((symbol-function 'eat-next-shell-prompt)
               (lambda (_)
                 (re-search-forward "^λ " nil t)
                 (goto-char (match-end 0)))))
      (let* ((source (madrigal-context-capture))
             (candidates (madrigal-context--shell-discover source))
             (prompt (car candidates))
             (scroll (cadr candidates))
             (text (madrigal-context--source-text
                    source (madrigal-context-candidate-start prompt)
                    (madrigal-context-candidate-end prompt))))
        (should (string-prefix-p "first" text))
        (should (string-suffix-p "λ " text))
        (should-not (string-match-p "second output" text))
        (should (> (madrigal-context-candidate-score prompt)
                   (madrigal-context-candidate-score scroll)))))))

(ert-deftest madrigal-context-shell-provider-spans-prompt-after-point ()
  (with-temp-buffer
    (insert (make-string 40 ?x))
    (let ((previous-prompt-end 10)
          (origin 20)
          (next-prompt-end 30)
          (functions '(madrigal-test-previous-prompt
                       . madrigal-test-next-prompt)))
      (cl-letf (((symbol-function 'madrigal-test-previous-prompt)
                 (lambda (_) (goto-char previous-prompt-end)))
                ((symbol-function 'madrigal-test-next-prompt)
                 (lambda (_)
                   (goto-char (if (>= (point) origin)
                                  next-prompt-end
                                origin)))))
        (should (equal (cons previous-prompt-end next-prompt-end)
                       (madrigal-context--shell-navigation-bounds
                        origin functions)))))))

(ert-deftest madrigal-context-shell-provider-clamps-missing-next-prompt ()
  (with-temp-buffer
    (insert (make-string 30 ?x))
    (let ((previous-prompt-end 10)
          (origin 20)
          (functions '(madrigal-test-previous-prompt
                       . madrigal-test-next-prompt)))
      (cl-letf (((symbol-function 'madrigal-test-previous-prompt)
                 (lambda (_) (goto-char previous-prompt-end)))
                ((symbol-function 'madrigal-test-next-prompt)
                 (lambda (_)
                   (if (= (point) origin)
                       (user-error "No later prompt")
                     (goto-char origin)))))
        (should (equal (cons previous-prompt-end (point-max))
                       (madrigal-context--shell-navigation-bounds
                        origin functions)))))))

(ert-deftest madrigal-context-shell-provider-uses-uniform-shell-navigation ()
  (require 'shell)
  (with-temp-buffer
    (shell-mode)
    (insert "$ first\noutput\n$ second\n")
    (let ((previous-prompt-end 4)
          (origin 10)
          (next-prompt-end 20))
      (goto-char origin)
      (cl-letf (((symbol-function 'comint-previous-prompt)
                 (lambda (_) (goto-char previous-prompt-end)))
                ((symbol-function 'comint-next-prompt)
                 (lambda (_) (goto-char next-prompt-end))))
        (let* ((source (madrigal-context-capture))
               (prompt (car (madrigal-context--shell-discover source))))
          (should (= previous-prompt-end
                     (madrigal-context-candidate-start prompt)))
          (should (= next-prompt-end
                     (madrigal-context-candidate-end prompt))))))))

(ert-deftest madrigal-context-shell-provider-adds-scroll-without-prompt-data ()
  (with-temp-buffer
    (setq major-mode 'eat-mode)
    (insert (make-string 20000 ?x))
    (goto-char 10000)
    (cl-letf (((symbol-function 'eat-previous-shell-prompt)
               (lambda (&optional _) (user-error "No previous prompt")))
              ((symbol-function 'eat-next-shell-prompt)
               (lambda (&optional _) (user-error "No next prompt"))))
      (let* ((source (madrigal-context-capture))
             (candidates (madrigal-context--shell-discover source))
             (prompt (car candidates))
             (scroll (cadr candidates)))
        (should (= 2 (length candidates)))
        (should (equal (cons (point-min) (point-max))
                       (cons (madrigal-context-candidate-start prompt)
                             (madrigal-context-candidate-end prompt))))
        (should (equal "Terminal scrollback/forward"
                       (madrigal-context-candidate-label scroll)))
        (should (= 16384 (madrigal-context-candidate-size scroll)))
        (should (madrigal-context-candidate-contains-point scroll))))))

(ert-deftest madrigal-context-notmuch-show-selects-mail-and-thread ()
  (with-temp-buffer
    (insert "Mail one\n\nMail two\n")
    (setq major-mode 'notmuch-show-mode)
    (goto-char 3)
    (cl-letf (((symbol-function 'notmuch-show-message-extent)
               (lambda () (cons (point-min) 10)))
              ((symbol-function 'notmuch-show-get-message-properties)
               (lambda () '(:headers (:Subject "A useful subject")))))
      (let* ((source (madrigal-context-capture))
             (candidates (madrigal-context--notmuch-discover source))
             (mail (seq-find
                    (lambda (candidate)
                      (string-prefix-p
                       "Notmuch mail"
                       (madrigal-context-candidate-label candidate)))
                    candidates))
             (thread (seq-find
                      (lambda (candidate)
                        (string-prefix-p
                         "Notmuch thread"
                         (madrigal-context-candidate-label candidate)))
                      candidates)))
        (should mail)
        (should thread)
        (should (> (madrigal-context-candidate-score thread)
                   (madrigal-context-candidate-score mail)))
        (should (< (madrigal-context-candidate-size mail)
                   (madrigal-context-candidate-size thread)))))))

(ert-deftest madrigal-context-notmuch-search-selects-thread-result ()
  (with-temp-buffer
    (insert "Thread result\n")
    (setq major-mode 'notmuch-search-mode)
    (goto-char 3)
    (cl-letf (((symbol-function 'notmuch-search-get-result)
               (lambda (&optional _) '(:thread "id" :subject "Subject")))
              ((symbol-function 'notmuch-search-result-beginning)
               (lambda (&optional _) (point-min)))
              ((symbol-function 'notmuch-search-result-end)
               (lambda (&optional _) (point-max))))
      (let* ((source (madrigal-context-capture))
             (candidate (car (madrigal-context--notmuch-discover source))))
        (should (equal "Notmuch thread: Subject"
                       (madrigal-context-candidate-label candidate)))))))

(ert-deftest madrigal-context-ement-room-list-selects-room ()
  (with-temp-buffer
    (insert "Matrix room\n")
    (setq major-mode 'ement-tabulated-room-list-mode)
    (goto-char 3)
    (cl-letf (((symbol-function 'tabulated-list-get-id)
               (lambda (&optional _) (vector 'room 'session)))
              ((symbol-function 'ement-room-display-name)
               (lambda (_) "Madrigal")))
      (let* ((source (madrigal-context-capture))
             (candidate
              (car (madrigal-context--ement-room-list-discover source))))
        (should (equal "Ement room: Madrigal"
                       (madrigal-context-candidate-label candidate)))))))

(ert-deftest madrigal-context-ement-room-selects-event-and-timeline ()
  (require 'ewoc)
  (with-temp-buffer
    (let ((ewoc (ewoc-create (lambda (event) (insert (plist-get event :body) "\n")))))
      (ewoc-enter-last ewoc '(:body "First event"))
      (ewoc-enter-last ewoc '(:body "Second event"))
      (setq major-mode 'ement-room-mode)
      (set (make-local-variable 'ement-ewoc) ewoc)
      (goto-char (point-min))
      (search-forward "First")
      (cl-letf (((symbol-function 'ement-event-p) (lambda (_) t))
                ((symbol-function 'ement-event-content)
                 (lambda (event) `((body . ,(plist-get event :body))))))
        (let* ((source (madrigal-context-capture))
               (candidates (madrigal-context--ement-room-discover source))
               (event
                (seq-find
                 (lambda (candidate)
                   (string-prefix-p
                    "Ement event"
                    (madrigal-context-candidate-label candidate)))
                 candidates)))
          (should (equal "Ement event: First event"
                         (madrigal-context-candidate-label event)))
          (should (seq-find
                   (lambda (candidate)
                     (string-prefix-p
                      "Ement room timeline"
                      (madrigal-context-candidate-label candidate)))
                   candidates)))))))

(ert-deftest madrigal-dwim-schema-has-only-requested-product-fields ()
  (let* ((suggestions
          (plist-get (plist-get madrigal-do--suggestion-response-schema
                                :properties)
                     :suggestions))
         (alternatives (append (plist-get (plist-get suggestions :items)
                                          :anyOf)
                               nil)))
    (should (equal '(:relevance :do-prompt)
                   (madrigal-do--plist-keys
                    (plist-get (car alternatives) :properties))))
    (should (equal '(:relevance :action-description :action-source)
                   (madrigal-do--plist-keys
                    (plist-get (cadr alternatives) :properties))))))

;;; madrigal-test.el ends here
