(in-package :stumpwm)

(setf (getenv "XIM") "ibus")
(setf (getenv "XIM_PROGRAM") "ibus")
(setf (getenv "XMODIFIERS") "@im=ibus")
(setf (getenv "GTK_IM_MODULE") "ibus")
(setf (getenv "QT_IM_MODULE") "ibus")

(stumpwm:set-prefix-key (stumpwm:kbd "C-."))

(define-key *root-map* (kbd "0") "remove")
(define-key *root-map* (kbd "1") "only")
(define-key *root-map* (kbd "2") "vsplit")
(define-key *root-map* (kbd "3") "hsplit")

(defcommand kitty () ()
  "Start kitty unless it is already running, in which case focus it."
  (run-or-raise "kitty tmux" '(:class "kitty")))

(define-key *root-map* (kbd "c") "kitty")
(define-key *root-map* (kbd "C-c") "kitty")

(defcommand switch-edp () (:rest)
  "Switch on eDP-1 display"
  (run-shell-command "xrandr --output eDP-1 --auto")
  (message "eDP-1: On"))

(defcommand emacs () ()
  "Start emacs unless it is already running, in which case focus it."
  (run-or-raise "emacsclient -c" '(:class "Emacs")))

 (defcommand firefox () ()
    (run-or-raise "firefox" '(:class "Firefox")))
(define-key *root-map* (kbd "b") "firefox")

(defcommand spotify () ()
  (run-or-raise "spotify" '(:class "Spotify")))
(define-key *root-map* (kbd "s") "spotify")


(defcommand spell-check (word)
  ((:string "Enter word to check: "))
  (setf *timeout-wait* 120)
  (setq x (concatenate 'string "echo '" word "' | aspell -a"))
  (run-shell-command x t ))

(defcommand wttr () ()
  (setf *timeout-wait* 6)
  (run-shell-command " curl -s 'wttr.in?0TF'" t))
(define-key *root-map* (kbd "w") "wttr")
(define-key *root-map* (kbd "t") "loadrc")

(defcommand screenshot () ()
  "Take screenshot"
  (run-shell-command "scrot ~/Pictures/%Y-%m-%d-%T-screenshot.png -e 'xclip -selection clipboard -target image/png -i $f'"))
;; (global-set-key (kbd "Print") "screenshot")
(define-key *top-map* (kbd "Print") "screenshot")

;; mode-line
(setq *window-format* "%m%s%n%5t")
(setf stumpwm:*screen-mode-line-format*
      (list "%w | "
            '(:eval (stumpwm:run-shell-command "date" t))))

(stumpwm:toggle-mode-line (stumpwm:current-screen)
 (stumpwm:current-head))
(if (not (head-mode-line (current-head)))
    (toggle-mode-line (current-screen) (current-head)))


;; input-window

(setf *input-window-gravity* :center)
(setf *message-window-gravity* :center)

(setq font-size 30)
(set-font (format nil "-*-*-bold-r-*-*-~D-240-*-*-*-*-*-*"
		      font-size))

;; (ql:quickload "clx-truetype")

;; (load-module "ttf-fonts")

;; (xft:cache-fonts)
;; (set-font (make-instance 'xft:font :family "Fira Code" :subfamily "Regular" :size 15))

;; init
;; (spotify)
;; (kitty)

(run-shell-command "ibus-daemon -d -x -r -n stump")
(run-shell-command "xrandr --output HDMI-1 --same-as eDP-1")
(run-shell-command "xrandr --output eDP-1 --off")
(emacs)

(run-shell-command "/usr/libexec/deja-dup/deja-dup-monitor &")
(run-shell-command "redshift &")

