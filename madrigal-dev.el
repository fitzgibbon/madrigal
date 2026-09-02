;;; madrigal-dev.el --- Reload Madrigal from source  -*- lexical-binding: t; -*-

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (when dir
    (add-to-list 'load-path dir)))

(defun madrigal-dev-reload ()
  "Reload all Madrigal source modules from this directory."
  (interactive)
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name default-directory)))
         (self (file-name-nondirectory (or load-file-name buffer-file-name "madrigal-dev.el")))
         (files (sort (directory-files dir nil "\\`madrigal.*\\.el\\'" nil)
                      #'string<)))
    (add-to-list 'load-path dir)
    (dolist (file files)
      (unless (string= file self)
        (load (expand-file-name file dir) nil 'nomessage)))
    (message "Reloaded Madrigal from %s" dir)))

(madrigal-dev-reload)

(provide 'madrigal-dev)

;;; madrigal-dev.el ends here
