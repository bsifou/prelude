;;; package --- My own config

;;; Commentary:
;;; My config

;;; Code:
(prelude-require-packages '(column-enforce-mode
                            markdown-mode
                            refheap))

;; highlight characters beyond 80 columns
(column-enforce-n 80)
(add-hook 'prog-mode-hook 'column-enforce-mode)

;; window size on startup
(if (window-system) (set-frame-size (selected-frame) 200 56))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun iwb ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key (kbd "s-<f2>") 'iwb)

(global-set-key (kbd "C-M-SPC") 'mark-sexp)

(provide 'michiel-config)
;;; init.el ends here
