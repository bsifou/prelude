;;; package --- My own config

;;; Commentary:
;;; My config

;;; Code:
(prelude-require-packages '(markdown-mode
                            refheap
                            multi-term
                            slim-mode
                            sass-mode
                            rvm))

;; (defun fci-hook ()
;;   (setq-default fci-rule-column 80)
;;   (fci-mode 1))

;; (add-hook 'prog-mode-hook 'fci-hook)

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

;;(global-set-key (kbd "C-M-SPC") 'mark-sexp)

;; see http://rawsyntax.com/blog/learn-emacs-zsh-and-multi-term/
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (define-key term-raw-map (kbd "s-v") 'term-paste)))

(visual-line-mode) ;; probably I have hook this up to some modes

(provide 'michiel-config)
;;; init.el ends here

;; disable automatic scss compilation on save
(custom-set-variables '(scss-compile-at-save nil))

(setq prelude-whitespace nil)

;; coffeescript-mode
(custom-set-variables '(coffee-tab-width 2))

;; slim template mode
(custom-set-variables '(slim-backspace-backdents-nesting nil))

;; js2mode
(setq-default js2-basic-offset 2)

;; magit
(setq magit-last-seen-setup-instructions "1.4.0")
