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
;;(global-set-key (kbd "C-c v") 'ansi-term)

(setq inhibit-startup-message t)
(setq master-font-family "Fira Code")
(show-paren-mode 1)
(global-visual-line-mode t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode 0)

;; Make <F12> set-mark-command
(global-set-key (kbd "<f12>") 'set-mark-command)
;; (global-set-key (kbd "<f9>") 'clipboard-yank)
(global-set-key (kbd "<S-delete>") 'clipboard-yank)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-set-key (kbd "M-i") 'ispell-word)
;; Change this from 10MB to 100MB
(setq large-file-warning-threshold 100000000)
(defun dired-dev ()
    (interactive)
  (dired "~/dev"))
(global-set-key (kbd "C-c w") 'dired-dev)

(use-package helm
  :bind (("M-x" . helm-M-x)
	 ("C-x r b" . helm-filtered-bookmarks)
	 ("C-x C-f" . helm-find-files)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini)
	 ("C-x r e" . helm-register))
  :config
  (helm-mode 1))

(use-package all-the-icons)
;; (use-package doom-themes
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;       doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-one t)
;;   (doom-themes-visual-bell-config)
;;   )
(use-package kaolin-themes
  :config
  (load-theme 'kaolin-dark t)
  (kaolin-treemacs-theme))

;; (use-package nord-theme
;;   :ensure t
;;   :init (load-theme 'nord))
;; (use-package ayu-theme
;;   :config (load-theme 'ayu-grey t))

(set-face-attribute 'default nil :family master-font-family :height 140)
;; (use-package mood-line
;;   :config
;;   (mood-line-mode))

;; (use-package fira-code-mode
;;   :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
;;   (add-hook 'prog-mode-hook 'fira-code-mode)
;;   (add-hook 'ess-mode-hook 'fira-code-mode)
;;   )

(when (window-system)
  (set-frame-font master-font-family))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
	       (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
	       (36 . ".\\(?:>\\)")
	       (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
	       (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
	       (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
	       (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
	       ;; (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
	       ;; (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
	       (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
	       (48 . ".\\(?:x[a-zA-Z]\\)")
	       (58 . ".\\(?:::\\|[:=]\\)")
	       (59 . ".\\(?:;;\\|;\\)")
	       (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
	       (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
	       (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
	       (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
	       (91 . ".\\(?:]\\)")
	       (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
	       (94 . ".\\(?:=\\)")
	       (119 . ".\\(?:ww\\)")
	       (123 . ".\\(?:-\\)")
	       (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
	       (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
	       )
	     ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
			  `([,(cdr char-regexp) 0 font-shape-gstring]))))

(set-register ?e '(file . "~/dev/dotfiles/emacs.org"))
(set-register ?w '(file . "~/dev/braindump/deutsch.org"))
(set-register ?d '(file . "~/dev/braindump/brain/brain.org"))
(set-register ?b '(file . "~/dev/dotfiles/bib.bib"))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(use-package rg
  :config
  (rg-enable-default-bindings))

(global-set-key (kbd "C-c m") 'recompile)

;; (defun dired-open-file ()
;;   "In dired, open the file named on this line."
;;   (interactive)
;;   (let* ((file (dired-get-filename nil t)))
;;     (call-process "xdg-open" nil 0 nil file)))
;; (define-key dired-mode-map (kbd "C-q") 'dired-open-file)

(use-package yaml-mode)

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(use-package ess
  :bind (
	 :map ess-r-mode-map 
	 ("_" . 'ess-insert-assign)
	 ("C-q" . 'ess-eval-region-or-line-and-step)
	 ("C-c C-k" . 'ess-request-a-process)
	 :map inferior-ess-r-mode-map 
	 ("_" . 'ess-insert-assign))
  :config
  (require 'ess-r-mode)
  (require 'ess-r-package)
  (setq ess-r-package-auto-enable-namespaced-evaluation nil)
  (setq ess-ask-for-ess-directory nil)
  (defalias 'lp 'ess-r-devtools-load-package)
  (defalias 'lt 'ess-r-devtools-test-package)
  (defalias 'lc 'ess-r-devtools-check-package)
  )

(use-package key-chord
  :init
  (key-chord-mode 1)
  (key-chord-define ess-r-mode-map ">>" " %>% ")
  (key-chord-define ess-r-mode-map "++" " -> ")
  (key-chord-define inferior-ess-r-mode-map ">>" " %>% ")
  (key-chord-define inferior-ess-r-mode-map "++" " -> ")
  )

(load-file "~/dev/ess_rproj/ess_rproj.el")
(add-hook 'ess-mode-hook #'ess-rproj)

(defun render-readme ()
  "A elisp function to quickly render README.Rmd in a package directory"
  (interactive)
  (setq-local readmepath (car (directory-files (expand-file-name (plist-get (ess-r-package-info default-directory) :root)) t "README\\.[Rr][Mm][Dd]")))
  (if (stringp readmepath)
      (ess-eval-linewise (format "rmarkdown::render('%s', output_format = 'all')" readmepath))
    (message "No README.RMD found.")))
(defalias 'rmd #'render-readme)

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

(use-package poly-markdown)
(use-package poly-R
  :init
  (add-to-list 'auto-mode-alist '("\\.rmd" . poly-markdown+r-mode)))

;; highlighting citations
(defvar markdown-mode-keywords nil)
(setq markdown-mode-keywords
      '(("@[^] ;\\.]+" . font-lock-keyword-face)
	)
      )

(font-lock-add-keywords
 'markdown-mode
 markdown-mode-keywords
 )

(use-package magit
  :init
  (global-set-key (kbd "C-c g") 'magit-status)
  ;; stole from here: https://github.com/y-tsutsu/dotfiles/blob/master/.emacs.d/config/local.el
  ;; (set-face-foreground 'magit-diff-added "#40ff40")
  ;; (set-face-background 'magit-diff-added "gray20")
  ;; (set-face-foreground 'magit-diff-added-highlight "#40ff40")
  ;; (set-face-background 'magit-diff-added-highlight "gray20")
  ;; (set-face-foreground 'magit-diff-removed "#d54e53")
  ;; (set-face-background 'magit-diff-removed "gray20")
  ;; (set-face-foreground 'magit-diff-removed-highlight "#d54e53")
  ;; (set-face-background 'magit-diff-removed-highlight "gray20")
  ;; (set-face-background 'magit-diff-lines-boundary "blue")
  )

(use-package helm-bibtex
  :config
  (autoload 'helm-bibtex "helm-bibtex" "" t)
  (setq bibtex-completion-bibliography '("~/dev/dotfiles/bib.bib"))
  (setq bibtex-completion-notes-path "~/dev/dotfiles/bib_notes.org")
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
  (setq bibtex-completion-format-citation-functions
	'((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
	  (latex-mode    . bibtex-completion-format-citation-cite)
	  (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
	  (default       . bibtex-completion-format-citation-pandoc-citeproc)))

  ;; make bibtex-completion-insert-citation the default action

  (helm-delete-action-from-source "Insert citation" helm-source-bibtex)
  (helm-add-action-to-source "Insert citation" 'helm-bibtex-insert-citation helm-source-bibtex 0)
  (global-set-key (kbd "C-c x") 'helm-bibtex)
  )

(use-package biblio
  :config
  (setq-default
   biblio-bibtex-use-autokey t
   bibtex-autokey-name-year-separator ":"
   bibtex-autokey-year-title-separator ":"
   bibtex-autokey-year-length 4
   bibtex-autokey-titlewords 3
   bibtex-autokey-titleword-length -1 ;; -1 means exactly one
   bibtex-autokey-titlewords-stretch 0
   bibtex-autokey-titleword-separator ""
   bibtex-autokey-titleword-case-convert 'upcase
   biblio-crossref-user-email-address "chung-hong.chan@mzes.uni-mannheim.de")
  )

(use-package org-ref
  :config
  (setq org-ref-completion-library 'org-ref-helm-bibtex
	org-ref-bibliography-notes "~/dev/dotfiles/bib_notes.org"
	org-ref-default-bibliography "~/dev/dotfiles/bib.bib")
  )

(defun ins-doi ()
    (interactive)
    (progn
      (setq doi-to-query (read-string "DOI "))
      (find-file "~/dev/dotfiles/bib.bib")
      (end-of-buffer)
      (doi-insert-bibtex doi-to-query)
      )
    )

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
(setq org-support-shift-select 'always)
(setq org-confirm-babel-evaluate nil)

(require 'ox-md)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (lisp . t)
   (C . t)
   (R . t)))

(setq org-default-notes-file "~/dev/braindump/brain/brain.org")
(setq org-agenda-files '("~/dev/braindump/brain/brain.org"))
(setq micro-journal-file "~/dev/braindump/brain/micro.org")
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-capture-templates
      '(("t" "todo" entry (file org-default-notes-file)
	 "* TODO %?\n%u\n%a\n")
	("m" "Meeting" entry (file org-default-notes-file)
	 "* MEETING with %? :MEETING:\n %t")
	("i" "Idea" entry (file micro-journal-file)
	 "* %? :IDEA: \n%t")
	))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(setq org-startup-with-inline-images t)

(use-package yasnippet
  :init
  (yas-global-mode 1)
  (setq yas-snippet-dirs (append yas-snippet-dirs
				 '("~/dev/dotfiles/my-snippets")))			       
  (yas-reload-all)
  )

(use-package yasnippet-snippets
  :after yasnippet
  )

(use-package deft
  :init
  (setq deft-extensions '("rmd" "markdown" "md" "org"))
  (setq deft-directory "~/dev/braindump")
  (setq deft-recursive t)
  ;;  (setq deft-extensions '("org"))
  ;;  (setq deft-default-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 10)
  (global-set-key (kbd "C-c d") 'deft)  
  )

(setq-default c-basic-offset 4)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(setq
 mue4e-headers-skip-duplicates  t
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-compose-format-flowed nil
 mu4e-date-format "%d/%m/%Y"
 mu4e-headers-date-format "%d/%m/%Y"
 mu4e-change-filenames-when-moving t
 mu4e-attachments-dir "~/Downloads"
 mu4e-maildir       "~/maildir"
 mu4e-refile-folder "/Archive"
 mu4e-sent-folder   "/Sent"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder  "/Trash"
 mu4e-use-fancy-chars t
 message-kill-buffer-on-exit t
 )

;; check email
(setq mu4e-get-mail-command  "mbsync -a"
      mu4e-update-interval 2400)

;; smtp
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "exchange.uni-mannheim.de"
      smtpmail-smtp-server "exchange.uni-mannheim.de"
      smtpmail-smtp-service 587)

;; about myself

(setq user-mail-address "chung-hong.chan@uni-mannheim.de"
      mu4e-compose-reply-to-address "chung-hong.chan@uni-mannheim.de"
      user-full-name "Chung-hong Chan")

(setq mu4e-compose-signature
      "Dr. Chung-hong Chan\nFellow\nMannheimer Zentrum für Europäische Sozialforschung (MZES)\nUniversität Mannheim\ntwitter / github: @chainsawriot")

(global-set-key (kbd "C-c 4") 'mu4e)
;; No confirm
(setq mu4e-confirm-quit nil)
;; short cuts
(setq mu4e-maildir-shortcuts
      '( ("/unimannheim/inbox" .  ?i)))

;;	mu4e-alert
(use-package mu4e-alert
  :init
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  )

(use-package xclip
  :config
  (xclip-mode 1)
  )

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

(defun knit ()
  (interactive)
  (save-buffer)
  (async-shell-command (concat "Rscript -e \"rmarkdown::render('" buffer-file-name "', output_format = 'all')\"")))

(global-set-key (kbd "C-c t") (lambda() (interactive) (find-file "~/dev")))

(setq python-shell-interpreter "python3")

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

(use-package dockerfile-mode)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;; (setq dashboard-match-agenda-entry
  ;;   "TODO=\"TODO\"|TODO=\"MEETING\"")
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-items '((recents  . 5)
			  (registers . 5)
			  (agenda . 5)))
  (setq dashboard-week-agenda t)
  (setq dashboard-filter-agenda-entry "MEETING|TODO")
  )

(setq shr-color-visible-luminance-min 100)

(use-package elfeed
  :config
  (setq elfeed-feeds '(
		       ("http://feeds.feedburner.com/thisweekinlinuxnew" linux)
		       ("http://fullcirclemagazine.org/feed/" linux)
		       ("http://www.raspberrypi.org/feed" linux)
		       ("http://www.greghendershott.com/feeds/all.rss.xml" emacs)
		       ("http://www.chainsawriot.com/feed.xml" blog)
		       ("http://mysterophilia.blogspot.com/feeds/posts/default" blog)
		       ("http://tiney.com/?feed=rss2" blog)
		       ("http://blog.liyiwei.org/?feed=rss2" research)
		       ;;("http://gabefung.wordpress.com/feed/" blog)
		       ("https://qbgabe12.wordpress.com/feed/" blog)
		       ("http://feeds.feedburner.com/JackysBlog" blog)
		       ("http://yccmcb.blogspot.com/feeds/posts/default" blog)
		       ("http://www.jstatsoft.org/rss" journal)
		       ("http://kbotjammer.blogspot.hk/feeds/posts/default" blog)
		       ("https://www.tagesschau.de/xml/rss2/" news)
		       ("https://www.tandfonline.com/feed/rss/hcms20" journal)
		       ("https://computationalcommunication.org/ccr/gateway/plugin/WebFeedGatewayPlugin/atom" journal)
		       ("https://ijoc.org/index.php/ijoc/gateway/plugin/WebFeedGatewayPlugin/atom" journal)
		       ("https://journals.sagepub.com/action/showFeed?ui=0&mi=ehikzz&ai=2b4&jc=hijb&type=axatoc&feed=rss" journal)
		       ("https://www.tandfonline.com/feed/rss/upcp20" journal)
		       ("https://journals.sagepub.com/action/showFeed?ui=0&mi=ehikzz&ai=2b4&jc=crxa&type=axatoc&feed=rss" journal)
		       ("https://bymiachang.com/feed/" blog)
		       ("https://martin.leyrer.priv.at/index.completerss20" blog)
		       ))
  )
;; ("http://chowching.wordpress.com/feed/" blog)
;; ("http://uingusu.blogspot.hk/feeds/posts/default" blog)
;; ("http://joechungvschina.blogspot.com/feeds/posts/default" blog)

;;"http://feeds.feedburner.com/hkscience"
;;"http://rayneyuenblog.wordpress.com/feed/"

;; "http://feeds.feedburner.com/cosine-inn"
;; "http://fishandhappiness.blogspot.com/feeds/posts/default"
;; "http://feeds.feedburner.com/naitik"
;; "http://emacs-fu.blogspot.com/feeds/posts/default?alt=rss"
;;"http://latexforhumans.wordpress.com/feed/"
;; "http://simplystatistics.org/feed/"

;; "http://feeds.feedburner.com/RBloggers"
;; "http://pragmaticemacs.com/feed/"
;; "http://www.stat.columbia.edu/~cook/movabletype/mlm/atom.xml"
;;"http://api.flickr.com/services/feeds/photos_public.gne?id=46738700@N00&format=atom"
;; "http://www.google.com/alerts/feeds/02150599014854607054/4889200315958358584"
;;"http://laosaomaster.com/laosao/

;;"http://laosaomaster.studium-sinicum.com/?feed=rss2"
;;"http://blog.age.com.hk/feed/"
;;"http://tungpakfool.wordpress.com/feed/"
;;"http://qb280.tumblr.com/rss"
;; ("http://linerak.wordpress.com/feed/" blog)
;;"http://laosaomaster.com/laosaomaster/?feed=rss2"
;;"http://feeds.feedburner.com/hoiking"
;;"http://pcheung25.wordpress.com/feed/"
;;"http://fongpik.wordpress.com/feed/"
;;"http://hk.myblog.yahoo.com/isle-wong/rss"
;;"http://comebacktolove.blogspot.com/feeds/posts/default"
;; "http://aukalun.blogspot.com/feeds/posts/default"
;; "http://bigantclimbing.blogspot.com/feeds/posts/default"
;; "http://feeds.feedburner.com/libertines/qHZz"
;; "http://feeds.feedburner.com/darkman"
;; "http://milkteamonster.blogspot.com/feeds/posts/default"
;; "http://feeds.feedburner.com/Room2046"
;; "http://feeds.feedburner.com/chiunam"
;; "http://aloneinthefart.blogspot.com/feeds/posts/default"
;; "http://badtastesmellgood.blogspot.com/feeds/posts/default"
;; "http://laosao.wordpress.com/feed/"
;; "http://point.south.hk/feed/"
;; "http://landofnocheese.blogspot.com/feeds/posts/default"
;; "http://feeds.feedburner.com/mildbutcalmless"
;; "http://stone.age.com.hk/feed"
;; "http://kaichileung.blogspot.com/feeds/posts/default"
;; "http://hongkonghell.blogspot.com/atom.xml"

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  )

;; (add-to-list 'load-path "/home/chainsawriot/dev/elisp/arduino-mode")
;; (setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . ) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))
;; (autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

(defun eir-eval-in-indium ()
  "Reinventing"
  (interactive)
    (if (and transient-mark-mode mark-active)
	(indium-eval-region (point) (mark))
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)
      (if (not (equal (point) (mark)))
	(indium-eval-region (point) (mark))
      ;; If empty, deselect region
      (setq mark-active nil))
      (eir-next-code-line)
      (setq mark-active nil)
  ))

      (use-package indium
	;; :bind (
	;; 	 :map javascript-mode-map
	;; 	("C-c C-r" . 'indium-eval-region))
	;; :config
	;; (add-hook 'js-mode-hook #'indium-interaction-mode)
	)

	(use-package js2-mode
	  :bind (
:map js2-mode-map
     ("C-c C-r" . 'indium-eval-region)
     ("C-q" . eir-eval-in-indium)
		 )
	:config
	(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))


(use-package js-comint)

;; Manual install
  (use-package vterm
    :load-path "/home/chainsawriot/dev/emacs-libvterm"
    :bind (
	   :map vterm-mode-map
	   ("C-y" . vterm-yank))
    )
  (global-set-key (kbd "C-c v") 'vterm)

(defun my-nov-font-setup ()
(face-remap-add-relative 'variable-pitch :family "Liberation Serif"
					 :height 1.5))
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (add-hook 'nov-mode-hook 'my-nov-font-setup)
  )

(use-package rust-mode
      :config
      (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
)

(use-package tide)

(use-package ts-comint
  :config
  (setq ts-comint-program-command "/home/chainsawriot/dev/fodira/twitter/node_modules/.bin/ts-node")
  (add-hook 'typescript-mode-hook
      (lambda ()
	(local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
	(local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
	(local-set-key (kbd "C-c C-r") 'ts-send-region)
	(local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
	(local-set-key (kbd "C-c l") 'ts-load-file-and-go))))

;; (defun nvm-which ()
;;   (let ((output (shell-command-to-string "source ~/.nvm/nvm.sh; nvm which")))
;;     (cadr (split-string output "[\n]+" t))))
;;   (setq nodejs-repl-command #'nvm-which)

;; (use-package elfeed-goodies
;;   :init
;;   (elfeed-goodies/setup)
;;   :config
;;   (setq elfeed-goodies/entry-pane-size 0.6)
;;   )

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

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

;; (use-package edit-server
;;   :ensure t
;;   :commands edit-server-start
;;   :init (if after-init-time
;; 	    (edit-server-start)
;; 	  (add-hook 'after-init-hook
;; 		    #'(lambda() (edit-server-start))))
;;   :config (setq edit-server-new-frame-alist
;; 		'((name . "Edit with Emacs FRAME")
;; 		  (top . 200)
;; 		  (left . 200)
;; 		  (width . 80)
;; 		  (height . 25)
;; 		  (minibuffer . t)
;; 		  (menu-bar-lines . t)
;; 		  (window-system . x))))

;; (use-package sudo-edit)

;; (use-package disable-mouse
;;   :config
;;   (global-disable-mouse-mode)
;;   )

;;(global-set-key (kbd "C-c r") 'inf-ruby)

;; (use-package openwith
;; :config
;; (openwith-mode t)
;; (setq openwith-associations '(("\\.pdf\\'" "evince" (file)))))

;; (add-to-list 'load-path "/home/chainsawriot/dev/sunrise-commander")
;; (require 'sunrise)
;; (require 'sunrise-buttons)
;; (require 'sunrise-modeline)

;; (defun open-spotify ()
;;   (vterm t)
;;   (rename-buffer "spotify" nil)
;;   (vterm-send-string "ncspot")
;;   (vterm-send-return))

;; (defun spotify ()
;;   (interactive)
;;   (if (get-buffer "spotify")
;;       (switch-to-buffer "spotify")
;;     (open-spotify)))

;; (defun spotify-play/pause ()
;;   (interactive)
;;   (if (get-buffer "spotify")
;;       (progn (set-buffer "spotify")
;; 	     (vterm-send-string "P"))))

;; (use-package emojify
;;   :hook (after-init . global-emojify-mode))
