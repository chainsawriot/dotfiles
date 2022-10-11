(in-package :stumpwm)

(setf (getenv "XIM") "ibus")
(setf (getenv "XIM_PROGRAM") "ibus")
(setf (getenv "XMODIFIERS") "@im=ibus")
(setf (getenv "GTK_IM_MODULE") "ibus")
(setf (getenv "QT_IM_MODULE") "ibus")

;; theming


(set-fg-color "#61afef")
(set-bg-color "#21252b")
(set-border-color "#21252b")
(set-win-bg-color "#21252b")
(set-focus-color "#61afef")
(set-unfocus-color "#21252b")

(setf *maxsize-border-width* 6)
(setf *transient-border-width* 6)
(setf *normal-border-width* 6)

(stumpwm:set-prefix-key (stumpwm:kbd "C-."))

(define-key *root-map* (kbd "0") "remove")
(define-key *root-map* (kbd "1") "only")
(define-key *root-map* (kbd "2") "vsplit")
(define-key *root-map* (kbd "3") "hsplit")

(defcommand kitty () ()
	    "Start kitty unless it is already running, in which case focus it."
	    (run-or-raise "kitty" '(:class "kitty")))

(define-key *root-map* (kbd "c") "kitty")
(define-key *root-map* (kbd "C-c") "kitty")

(defcommand switch-edp () (:rest)
	    "Switch on eDP-1 display"
	    (run-shell-command "xrandr --output eDP-1 --auto")
	    (message "eDP-1: On"))

(define-key *top-map* (kbd "F12") "switch-edp")

(defcommand lock () ()
	    "lock"
	    (run-shell-command "xss-lock -- i3lock -c 000000 &"))

(defcommand emacs () ()
	    "Start emacs unless it is already running, in which case focus it."
	    (run-or-raise "emacsclient -c" '(:class "Emacs")))

(defcommand firefox () ()
	    (run-or-raise "firefox" '(:class "firefox")))
(define-key *root-map* (kbd "b") "firefox")

(defcommand thunderbird () ()
	    (run-or-raise "thunderbird" '(:class "thunderbird")))
(define-key *root-map* (kbd "t") "thunderbird")

(defcommand spotify () ()
	    (run-or-raise "spotify" '(:class "Spotify")))
(define-key *root-map* (kbd "s") "spotify")

(defcommand nautilus () ()
	    (run-or-raise "nautilus" '(:class "Nautilus")))
(define-key *root-map* (kbd "[") "nautilus")

(defcommand drun () ()
	    "drun"
	    (run-shell-command "rofi -modi drun -show drun"))

(define-key *root-map* (kbd "]") "drun")

(defcommand spell-check (word)
  ((:string "Enter word to check: "))
  (setf *timeout-wait* 120)
  (setq x (concatenate 'string "echo '" word "' | aspell -a"))
  (run-shell-command x t ))

(defcommand wttr () ()
	    (setf *timeout-wait* 6)
	    (run-shell-command " curl -s 'wttr.in?0TF'" t))
(define-key *root-map* (kbd "w") "wttr")

(define-key *root-map* (kbd "z") "loadrc")

;; (defcommand screenshot () ()
;; 	    "Take screenshot"
;; 	    (run-shell-command "scrot ~/Pictures/%Y-%m-%d-%T-screenshot.png -e 'xclip -selection clipboard -target image/png -i $f'"))
;; ;; (global-set-key (kbd "Print") "screenshot")
;; (define-key *top-map* (kbd "Print") "screenshot")

(defcommand screenshotarea () ()
	    "Take screenshot area"
	    (run-shell-command "gnome-screenshot --area"))
(define-key *top-map* (kbd "Print") "screenshotarea")
	    

(defcommand amixer () () 
	    (setq vol (read-one-line (current-screen) "Volume: "))
	    (cond ((> (parse-integer vol) 100) (setq vol 100))
		  ((< (parse-integer vol) 0) (setq vol 0)))
	    (setq cmd (concatenate 'string "amixer -D pulse sset Master " vol "%"))
	    (run-shell-command cmd t)
	    )

(define-key *root-map* (kbd "a") "amixer")

;; mode-line
(setq *window-format* "%m%s%n%5t")
;; (setf stumpwm:*screen-mode-line-format*
;;       (list '(:eval (stumpwm:run-shell-command "cat /sys/class/power_supply/CMB1/capacity" t))))

;; TODO: Should be a function to do tr automatically

(defun cmd-tr (cmd)
  "run a command, but do tr automatically"
  (stumpwm:run-shell-command (concatenate 'string cmd " | tr '\\n' ' '") t)
  )

;; (setf stumpwm:*screen-mode-line-format*
;;       (list '(:eval (cmd-tr "date")) "Batt: " '(:eval (cmd-tr "cat /sys/class/power_supply/CMB1/capacity")) "%"))

(setf stumpwm:*screen-mode-line-format*
      (list '(:eval (cmd-tr "date")) "Batt: " '(:eval (cmd-tr "cat /sys/class/power_supply/BAT1/capacity")) "%"))


(stumpwm:toggle-mode-line (stumpwm:current-screen)
			  (stumpwm:current-head))
(if (not (head-mode-line (current-head)))
    (toggle-mode-line (current-screen) (current-head)))


;; input-window

(setf *input-window-gravity* :center)
(setf *message-window-gravity* :center)

(setq font-size 20)
(set-font (format nil "-*-*-bold-r-*-*-~D-240-*-*-*-*-*-*"
		  font-size))

;; (ql:quickload "clx-truetype")

;; (load-module "ttf-fonts")

;; (xft:cache-fonts)
;; (set-font (make-instance 'xft:font :family "Fira Code" :subfamily "Regular" :size 15))

;; init
;; (spotify)
;; (kitty)

;; (ql:quickload :swank)
;; (defcommand swank () ()
;;             (swank:create-server :port 4005
;;                                  :style swank:*communication-style*
;;                                  :dont-close t)
;;             (echo-string (current-screen)
;;                          "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))

;; (run-shell-command "ibus-daemon -d -x -r -n stump")
(run-shell-command "xrandr --output HDMI-1 --same-as eDP-1")
(run-shell-command "xrandr --output eDP-1 --off")
;; (emacs)

;; (run-shell-command "/usr/libexec/deja-dup/deja-dup-monitor &")
;; (run-shell-command "killall redshift")
;; (run-shell-command "redshift &")
(lock)					;; (run-shell-command "xss-lock -- i3lock -c 000000 &")


