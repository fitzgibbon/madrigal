;;; madrigal-todo.el --- TODO discovery for Madrigal  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'project)
(require 'subr-x)

(defgroup madrigal nil
  "Agentic coding with LLM providers."
  :group 'tools
  :prefix "madrigal-")

(defcustom madrigal-comment-todo-keywords
  '("TODO" "FIXME" "XXX" "HACK" "BUG" "NOTE")
  "Comment keywords treated as work items."
  :type '(repeat string)
  :group 'madrigal)

(cl-defstruct (madrigal-todo
               (:constructor madrigal-todo-create))
  file
  line
  column
  kind
  keyword
  text)

(defconst madrigal--checklist-item-regexp
  "^[[:space:]]*[-+*]\\s-+\\[ \\]\\s-*\\(.*\\)$")

(defconst madrigal--markdown-todo-regexp
  "^[[:space:]]*\\(?:[-+*]\\s-+\\)?\\([[:upper:]][-[:upper:]]*\\)\\b[: ]*\\(.*\\)$")

(defun madrigal--comment-todo-regexp ()
  "Return the regexp used to match comment TODOs."
  (concat
   "^[[:space:]]*\\(?:;+\\|#+\\|//+\\|/\\*+\\|\\*+\\|--+\\|%+\\)\\s-*"
   "\\("
   (regexp-opt madrigal-comment-todo-keywords)
   "\\)\\b[: ]*\\(.*\\)$"))

(defun madrigal--todo-from-match (file line-number column kind keyword text)
  "Build a todo item for FILE at LINE-NUMBER and COLUMN."
  (madrigal-todo-create
   :file file
   :line line-number
   :column column
   :kind kind
   :keyword keyword
   :text (string-trim (or text ""))))

(defun madrigal--todo-at-pos (file pos kind keyword text)
  "Build a todo item for FILE at POS."
  (save-excursion
    (goto-char pos)
    (madrigal--todo-from-match
     file
     (line-number-at-pos pos)
     (1+ (current-column))
     kind keyword text)))

(defun madrigal--org-item-text (item)
  "Return a readable text label for Org ITEM."
  (save-excursion
    (goto-char (org-element-property :begin item))
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
      (if (string-match madrigal--checklist-item-regexp line)
          (match-string 1 line)
        (string-trim line)))))

(defun madrigal--org-open-todo-keywords ()
  "Return open Org TODO keywords for the current buffer."
  (seq-difference org-todo-keywords-1 org-done-keywords #'string=))

(defun madrigal--collect-org-file-todos (file)
  "Return todos found in Org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (delay-mode-hooks (org-mode))
    (let* ((ast (org-element-parse-buffer 'element))
           (open-keywords (madrigal--org-open-todo-keywords))
           todos)
      (org-element-map ast 'headline
        (lambda (headline)
          (let ((keyword (org-element-property :todo-keyword headline)))
            (when (member keyword open-keywords)
              (push (madrigal--todo-at-pos
                     file
                     (org-element-property :begin headline)
                     'org-heading
                     keyword
                     (org-element-property :raw-value headline))
                    todos)))))
      (org-element-map ast 'item
        (lambda (item)
          (let ((checkbox (org-element-property :checkbox item)))
            (when (memq checkbox '(off trans))
              (push (madrigal--todo-at-pos
                     file
                     (org-element-property :begin item)
                     'org-checkbox
                     "CHECKBOX"
                     (madrigal--org-item-text item))
                    todos)))))
      (sort todos
            (lambda (left right)
              (< (madrigal-todo-line left)
                 (madrigal-todo-line right)))))))

(defun madrigal--markdown-todo-from-line (file line-number line)
  "Return a Markdown todo item from LINE, if any."
  (cond
   ((string-match madrigal--checklist-item-regexp line)
    (madrigal--todo-from-match
     file line-number (1+ (match-beginning 1))
     'markdown-checkbox "CHECKBOX" (match-string 1 line)))
   ((string-match madrigal--markdown-todo-regexp line)
    (let ((keyword (match-string 1 line)))
      (when (member keyword madrigal-comment-todo-keywords)
        (madrigal--todo-from-match
         file line-number (1+ (match-beginning 1))
         'markdown-todo keyword (match-string 2 line)))))))

(defun madrigal--comment-todo-from-line (file line-number line)
  "Return a comment todo item from LINE, if any."
  (when (string-match (madrigal--comment-todo-regexp) line)
    (madrigal--todo-from-match
     file line-number (1+ (match-beginning 1))
     'comment (match-string 1 line) (match-string 2 line))))

(defun madrigal--collect-file-todos (file)
  "Return todos found in FILE."
  (let ((extension (downcase (or (file-name-extension file) ""))))
    (if (string= extension "org")
        (madrigal--collect-org-file-todos file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((line-number 1)
              todos)
          (while (not (eobp))
            (let* ((line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
                   (todo
                    (if (member extension '("md" "markdown" "mdown" "mkd"))
                        (madrigal--markdown-todo-from-line file line-number line)
                      (madrigal--comment-todo-from-line file line-number line))))
              (when todo
                (push todo todos)))
            (forward-line 1)
            (setq line-number (1+ line-number)))
          (nreverse todos))))))

(defun madrigal--project-files (project)
  "Return absolute tracked files for PROJECT."
  (let ((root (project-root project)))
    (mapcar (lambda (file)
              (if (file-name-absolute-p file)
                  file
                (expand-file-name file root)))
            (project-files project))))

(defun madrigal-project-todos (&optional project)
  "Return work items for PROJECT.

PROJECT defaults to the current Emacs project."
  (let* ((project (or project (project-current nil)))
         (files (and project (madrigal--project-files project))))
    (unless project
      (user-error "Not in a project"))
    (sort
     (cl-mapcan #'madrigal--collect-file-todos files)
     (lambda (left right)
       (or (string< (madrigal-todo-file left) (madrigal-todo-file right))
           (and (string= (madrigal-todo-file left) (madrigal-todo-file right))
                (< (madrigal-todo-line left) (madrigal-todo-line right))))))))

(provide 'madrigal-todo)

;;; madrigal-todo.el ends here
