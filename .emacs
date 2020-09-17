(load-theme 'tango-dark)

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(defun refresh-emacs ()
   (interactive)
   (org-babel-tangle-file "~/dev/dotfiles/emacs.org")
   ;;(byte-compile-file "~/dev/dotfiles/emacs")
   (load-file "~/dev/dotfiles/.emacs")
)
(global-set-key (kbd "C-c e") #'refresh-emacs)

(defun pbs ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "pbcopy")
)

(setq inhibit-startup-message t)
(show-paren-mode 1)
(global-visual-line-mode t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini))
  :config
  (helm-mode 1))

(set-register ?q '(file . "~/dev/dotfiles/emacs.org"))
(set-register ?w '(file . "~/dev/braindump/deutsch.org"))
(set-register ?e '(file . "~/dev/braindump/brain/brain.org"))

;; (autoload;;  'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;;     (eval-in-repl racket-mode ebib vterm poly-R stan-mode dockerfile-mode docker rg polymode paredit markdown-mode magit inf-ruby flymake-ruby cider))))

;;(setq inferior-lisp-program "clisp")

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

(use-package rg
  :config
  (rg-enable-default-bindings))

(defun then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (just-one-space 1))

(use-package ess
  :bind (
  :map ess-mode-map 
  ("_" . 'ess-insert-assign)
  ("M-`" . 'then_R_operator)
  ("C-q" . 'ess-eval-region-or-line-and-step)
  :map inferior-ess-mode-map 
  ("_" . 'ess-insert-assign)
  ("M-`" . 'then_R_operator))
  :config
  (require 'ess-r-mode)
  (setq ess-r-package-auto-enable-namespaced-evaluation nil)
  (setq ess-ask-for-ess-directory nil)
  (defalias 'lp 'ess-r-devtools-load-package)
)

;;(load-file "~/dev/ess_rproj/ess_rproj.el")
;;(add-hook 'ess-mode-hook #'ess-rproj)

(use-package rainbow-delimiters
   :init
   (add-hook 'ess-mode-hook #'rainbow-delimiters-mode)
   (add-hook 'ess-mode-hook 'hs-minor-mode)
)
(use-package rainbow-mode
   :init
   (dolist (hook '(ess-mode-hook inferior-ess-mode-hook))
   (add-hook hook 'rainbow-turn-on))   
)

(use-package poly-markdown
  :init
  (add-to-list 'auto-mode-alist '("\\.rmd" . poly-markdown-mode))
)

(use-package magit
  :init
  (global-set-key (kbd "C-c g") 'magit-status))

(global-set-key (kbd "C-c m") 'recompile)

;;(global-set-key (kbd "C-c r") 'inf-ruby)

;; (require 'helm-bibtex)
;; (autoload 'helm-bibtex "helm-bibtex" "" t)
;; (setq bibtex-completion-bibliography
;;       '("~/dev/chcbibtex/bib.bib"))
;; (setq bibtex-completion-notes-path "~/dev/chcbibtex/notes.org")
;; (setq bibtex-completion-format-citation-functions
;;   '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
;;     (latex-mode    . bibtex-completion-format-citation-cite)
;;     (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
;;     (default       . bibtex-completion-format-citation-pandoc-citeproc)))

;; ;; make bibtex-completion-insert-citation the default action

;; (helm-delete-action-from-source "Insert citation" helm-source-bibtex)
;; (helm-add-action-to-source "Insert citation" 'helm-bibtex-insert-citation helm-source-bibtex 0)

;; (global-set-key (kbd "C-c x") 'helm-bibtex)

;; (setq-default biblio-bibtex-use-autokey t)

;; (setq-default
;; bibtex-autokey-name-year-separator ":"
;; bibtex-autokey-year-title-separator ":"
;; bibtex-autokey-year-length 4
;; bibtex-autokey-titlewords 3
;; bibtex-autokey-titleword-length -1 ;; -1 means exactly one
;; bibtex-autokey-titlewords-stretch 0
;; bibtex-autokey-titleword-separator ""
;; bibtex-autokey-titleword-case-convert 'upcase)

(use-package eval-in-repl
  :bind (
  :map emacs-lisp-mode-map
  ("C-q" . 'eir-eval-in-ielm)
  :map lisp-interaction-mode-map
  ("C-q" . 'eir-eval-in-ielm)
  :map Info-mode-map
  ("C-q" . 'eir-eval-in-ielm))
  :config
  (require 'eval-in-repl-ielm)
  :init
  (setq eir-ielm-eval-in-current-buffer t)
)

(setq org-log-done 'time)

(require 'ox-md)


(org-babel-do-load-languages
'org-babel-load-languages
'((emacs-lisp . t)
(lisp . t)))

(setq org-default-notes-file "~/dev/braindump/brain/brain.org")
(setq org-agenda-files '("~/dev/braindump/brain/brain.org"))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-capture-templates
'(("t" "todo" entry (file org-default-notes-file)
"* TODO %?\n%u\n%a\n")
("m" "Meeting" entry (file org-default-notes-file)
"* MEETING with %? :MEETING:\n%t")
("i" "Idea" entry (file org-default-notes-file)
"* %? :IDEA: \n%t")
))

(use-package yasnippet
  :init
  (yas-global-mode 1)
)

(use-package yasnippet-snippets
  :after yasnippet
)

(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/dev/dotfiles/my-snippets")))

(use-package deft
  :init
  (setq deft-extensions '("txt" "markdown" "md" "org"))
  (setq deft-directory "~/dev/braindump")
  (setq deft-recursive t)
  (setq deft-extensions '("org"))
  (setq deft-default-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 10)
  (global-set-key (kbd "C-c d") 'deft)  
)

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
;; (require 'mu4e)

;; (setq
;;  mue4e-headers-skip-duplicates  t
;;  mu4e-view-show-images t
;;  mu4e-view-show-addresses t
;;  mu4e-compose-format-flowed nil
;;  mu4e-date-format "%d/%m/%Y"
;;  mu4e-headers-date-format "%d/%m/%Y"
;;  mu4e-change-filenames-when-moving t
;;  mu4e-attachments-dir "~/Downloads"
;;  mu4e-maildir       "~/maildir"
;;  mu4e-refile-folder "/Archive"
;;  mu4e-sent-folder   "/Sent"
;;  mu4e-drafts-folder "/Drafts"
;;  mu4e-trash-folder  "/Trash")

;; ;; check email
;; (setq mu4e-get-mail-command  "mbsync -a"
;;       mu4e-update-interval 600)

;; ;; smtp
;; (setq message-send-mail-function 'smtpmail-send-it
;;      smtpmail-stream-type 'starttls
;;      smtpmail-default-smtp-server "smtp.mail.uni-mannheim.de"
;;      smtpmail-smtp-server "smtp.mail.uni-mannheim.de"
;;      smtpmail-smtp-service 587)

;; ;; about myself

;; (setq user-mail-address "chung-hong.chan@mzes.uni-mannheim.de"
;;       mu4e-compose-reply-to-address "chung-hong.chan@mzes.uni-mannheim.de"
;;       user-full-name "Chung-hong Chan")

;; (setq mu4e-compose-signature
;;    "Dr. Chung-hong Chan\nFellow\nMannheimer Zentrum für Europäische Sozialforschung (MZES)\nUniversität Mannheim\ntwitter / github: @chainsawriot")

;; (global-set-key (kbd "C-c 4") 'mu4e)
;; ;; No confirm
;; (setq mu4e-confirm-quit nil)
;; ;; short cuts
;; (setq mu4e-maildir-shortcuts
;;   '( (:maildir "/unimannheim/inbox"     :key  ?i)
;;      (:maildir "/sent"      :key  ?s)))

;; ;; mu4e-alert
;; (require 'mu4e-alert)
;; (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
