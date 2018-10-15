;;; package --- My own config

;;; Commentary:
;;; My config

;;; Code:
(prelude-require-packages '(markdown-mode
                            multi-term
                            slim-mode
                            sass-mode
                            rvm
                            inf-clojure
                            ag
                            projectile-rails
                            clj-refactor
                            ;; flycheck-clojure
                            flycheck-pos-tip
                            jedi
                            use-package
                            neotree
                            auto-highlight-symbol
                            haskell-mode
                            intero
                            flymake-hlint
                            ;; hlint-refactor uses https://github.com/mpickering/apply-refact
                            ;; install by running the following outside a project dir: stack --resolver=nightly install apply-refact
                            hlint-refactor

                            ;; install joker first
                            ;; https://github.com/candid82/joker#installation
                            flycheck-joker
                            adoc-mode
                            ido-vertical-mode
                            purescript-mode
                            ))

(require 'flycheck-joker)

;; (defun fci-hook ()
;;   (setq-default fci-rule-column 80)
;;   (fci-mode 1))

;; (add-hook 'prog-mode-hook 'fci-hook)

;; window size on startup
;; (if (window-system) (set-frame-size (selected-frame) 200 56))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; obsolete, use crux-rename-file-and-buffer
;; (defun rename-file-and-buffer (new-name)
;;   "Renames both current buffer and file it's visiting to NEW-NAME."
;;   (interactive "sNew name: ")
;;   (let ((name (buffer-name))
;;         (filename (buffer-file-name)))
;;     (if (not filename)
;;         (message "Buffer '%s' is not visiting a file!" name)
;;       (if (get-buffer new-name)
;;           (message "A buffer named '%s' already exists!" new-name)
;;         (progn
;;           (rename-file name new-name 1)
;;           (rename-buffer new-name)
;;           (set-visited-file-name new-name)
;;           (set-buffer-modified-p nil))))))

(defun iwb ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key (kbd "<f7>") 'iwb)

;; (add-hook 'mouse-leave-buffer-hook
;;           (lambda ()
;;             (let ((mode (message "%s" major-mode)))
;;               (if (string= "clojure-mode" mode)
;;                   (iwb)
;;                 (progn (print "mode is not clojure mode") (print mode))
;;                 ))))

;; TODO: try this:
;; (defvar auto-indent-modes
;;   '(clojure-mode emacs-lisp-mode))

;; (defun indent-maybe ()
;;   (when (member major-mode auto-indent-modes)
;;     (indent-region (point-min) (point-max))))

;; (add-hook 'before-save-hook #'indent-maybe)



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

;; Don't show whitespace
(setq prelude-whitespace nil)
;; Do not warn about arrow keys
(setq prelude-guru nil)

;; coffeescript-mode
(custom-set-variables '(coffee-tab-width 2))

;; slim template mode
(custom-set-variables '(slim-backspace-backdents-nesting nil))

;; js2mode
(setq-default js2-basic-offset 2)

;; magit
(setq magit-last-seen-setup-instructions "1.4.0")

;; projectile
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; emacs server

(require 'server)
(unless (server-running-p)
  (server-start))

;; clj-refactor

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(setq cljr-favor-prefix-notation nil)

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; cider cljs repl setting - start with cider-create-sibling-cljs-repl in project
(setq cider-cljs-boot-repl "(do (in-ns boot.user) (start-repl))")

;; neotree
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)

;; highlighting
(setq global-auto-highlight-symbol-mode t)

;; flyspell

(setq prelude-flyspell nil)
;; source: https://www.emacswiki.org/emacs/FlySpell
;; (defun fd-switch-dictionary()
;;   (interactive)
;;   (let* ((dic ispell-current-dictionary)
;;          (change (if (string= dic "nl") "en" "nl")))
;;     (ispell-change-dictionary change)
;;     (message "Dictionary switched from %s to %s" dic change)
;;     ))

;; (global-set-key (kbd "<f8>") 'fd-switch-dictionary)

;;
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; CIDER choose REPL to evaluate against
(defun set-local-connection (conn)
  (interactive "bChoose connection: ")
  (if (with-current-buffer conn
        (derived-mode-p 'cider-repl-mode))
      (setq-local cider-connections (list conn))
    (message "not a connection buffer")))

;; Haskell
(add-hook 'haskell-mode-hook 'intero-mode)
;; the non-nil optional argument puts it after intero-mode
(add-hook 'haskell-mode-hook 'flymake-hlint-load  t)
(global-auto-complete-mode t)
(require 'haskell-align-imports)

;; Highlighting
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)
(mapc (lambda (mode)
        (add-to-list 'ahs-modes mode))
      '(clojure-mode clojurescript-mode cider-repl-mode haskell-mode javascript-mode))

(setq-default fill-column 80)

;; cljfmt
(defun cljfmt ()
  (when (or (eq major-mode 'clojure-mode)
            (eq major-mode 'clojurescript-mode))
    (shell-command-to-string (format "cljfmt %s" buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm)))

;; (add-hook 'after-save-hook #'cljfmt)

(define-key global-map (kbd "s-`") 'other-frame)

;; (setq debug-on-error t)

;; Common Lisp

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; ;; Replace "sbcl" with the path to your implementation
;; (setq inferior-lisp-program "sbcl")

(ido-vertical-mode 1)


;; https://queertypes.com/posts/34-purescript-emacs.html
(add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
(add-hook 'purescript-mode-hook 'flycheck-mode)

;; https://twitter.com/unlog1c/status/1051877170874904578
(setq-default ffap-machine-p-known 'reject)
