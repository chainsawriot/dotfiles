(require 'package) ;; You might already have this line
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(setq inhibit-startup-message t)
(show-paren-mode 1)
(global-visual-line-mode t)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (eval-in-repl racket-mode ebib vterm poly-R stan-mode dockerfile-mode docker rg polymode paredit markdown-mode magit inf-ruby flymake-ruby cider))))

(load-theme 'tango-dark)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(require 'rg)
(rg-enable-default-bindings)

(require 'ess-r-mode)
(define-key ess-r-mode-map "_" 'ess-insert-assign)
(define-key inferior-ess-r-mode-map "_" 'ess-insert-assign)
(setq ess-r-package-auto-set-evaluation-env nil)

(setq ess-ask-for-ess-directory nil)
(fset 'yes-or-no-p 'y-or-n-p)

(defun then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (just-one-space 1))
  
(define-key ess-mode-map (kbd "M-`") 'then_R_operator)
(define-key inferior-ess-mode-map (kbd "M-`") 'then_R_operator)
(define-key ess-mode-map (kbd "C-q") 'ess-eval-region-or-line-and-step)
(defalias 'lp 'ess-r-devtools-load-package)

(load-file "~/dev/ess_rproj/ess_rproj.el")
(add-hook 'ess-mode-hook #'ess_rproj)

(add-to-list 'load-path "/Users/chainsaw/tools/polymode")
(add-to-list 'load-path "/Users/chainsaw/tools/poly-markdown")
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.rmd" . poly-markdown-mode))

(global-set-key (kbd "C-c m") 'recompile)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c r") 'inf-ruby)
;; (global-set-key (kbd "C-c d") 'ess-r-devtools-load-package)

(setq org-log-done 'time)
