;;; madrigal.el --- Agentic coding with LLM providers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Niall
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (llm "0"))
;; Keywords: tools, ai
;; URL: https://example.com/madrigal

;;; Commentary:

;; Package wrapper for Madrigal.

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (when dir
    (add-to-list 'load-path dir)))

(require 'madrigal-core)
(require 'madrigal-org)
(require 'madrigal-conversation-context)
(require 'madrigal-tool-eval-prelude)
(require 'madrigal-tool-eval)
(require 'madrigal-tool-babel)
(require 'madrigal-agent-controller)
(require 'madrigal-org-compact)
(require 'madrigal-submit)
(require 'madrigal-context)
(require 'madrigal-do)

(madrigal--load-persistent-elisp)

(provide 'madrigal)

;;; madrigal.el ends here
