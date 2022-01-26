(in-package :stumpwm)

(setf (getenv "XIM") "ibus")
(setf (getenv "XIM_PROGRAM") "ibus")
(setf (getenv "XMODIFIERS") "@im=ibus")
(setf (getenv "GTK_IM_MODULE") "ibus")
(setf (getenv "QT_IM_MODULE") "ibus")

(run-shell-command "ibus-daemon -d -x -r -n stump")

(stumpwm:set-prefix-key (stumpwm:kbd "C-."))

(define-key *root-map* (kbd "0") "remove")
(define-key *root-map* (kbd "1") "only")
(define-key *root-map* (kbd "2") "vsplit")
(define-key *root-map* (kbd "3") "hsplit")

(run-shell-command "xrandr --output HDMI-1 --same-as eDP-1")
(run-shell-command "xrandr --output eDP-1 --off")

(defcommand switch-edp () (:rest)
  "Switch on eDP-1 display"
  (run-shell-command "xrandr --output eDP-1 --auto")
  (message "eDP-1: On"))

(defcommand emacs () ()
  "Start emacs unless it is already running, in which case focus it."
  (run-or-raise "emacsclient -c" '(:class "Emacs")))

(mode-line)
