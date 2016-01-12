(ql:quickload "cl-arrows")
(ql:quickload "cl-op")
(ql:quickload "split-sequence")
(ql:quickload "xembed")
(ql:quickload "alexandria")

(use-package :cl-arrows)
(use-package :cl-op)


;; Enable SLIME/Emacs integration
(load "/home/cji/.stumpwm.d/swank.lisp")


(set-prefix-key (kbd "C-z"))
(set-module-dir "/home/cji/portless/stumpwm/contrib")

(setf *input-window-gravity*    :center)
(setf *message-window-gravity*  :top-right)
(setf *mouse-focus-policy*      :click)
(setf *print-circle*            t)
(setf *timeout-wait*            3)

(setf *startup-message*
      (format nil "Welcome to ~a!" (machine-instance)))


;; Damn, I forgot what these (fp+ and company) do. Note to self: don't put
;; commenting the code off too far into the future...
(run-shell-command "sudo mkfontscale /usr/share/fonts/bitstream-vera/" t)
(run-shell-command "sudo mkfontdir /usr/share/fonts/bitstream-vera/" t)
(run-shell-command "xset fp+ /usr/share/fonts/bitstream-vera/" t)
(let
    ((fonts-list (xlib:list-font-names *display* "*vera*"
                                       :max-fonts 1)))
  (when (> (length fonts-list) 0)
    (set-font "-*-bitstream vera sans mono-*-r-*-*-15-*-*-*-*-*-iso8859-*")))





;; NOTE:
;; *root-map* - the one under C-z
;; *top-map* - global one

(define-key *root-map* (kbd "=")   "place-existing-windows")
(define-key *root-map* (kbd "C-r") "place-existing-windows")
(define-key *root-map* (kbd "C-o") "only")
(define-key *root-map* (kbd "C-s") "my-vsplit")
(define-key *root-map* (kbd "C-v") "my-hsplit")
(define-key *root-map* (kbd "C-z") "send-escape")
(define-key *root-map* (kbd "C-c") "exec urxvt")
(define-key *root-map* (kbd "C-d") "my-remove")
(define-key *root-map* (kbd "I")   "show-window-properties")
(define-key *root-map* (kbd "c")   "my-gnew")
(define-key *root-map* (kbd "x")   "colon")
(define-key *root-map* (kbd "d")   "remove")

;; Screen brightness & Volume adjustment
(define-key *top-map* (kbd "XF86AudioLowerVolume")  "exec amixer set Master 5%-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume")  "exec amixer set Master 5%+")
(define-key *top-map* (kbd "XF86AudioMute")         "exec amixer set Master toggle")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "backlight-down")
(define-key *top-map* (kbd "XF86MonBrightnessUp")   "backlight-up")

;; Window switching
;; TODO: make this behave like a queue
(define-key *top-map* (kbd "M-TAB")                 "next")
(define-key *top-map* (kbd "M-ISO_Left_Tab")        "prev")

;; Moving between groups
(define-key *top-map* (kbd "C-M-Left")    "gprev")
(define-key *top-map* (kbd "C-M-Right")   "gnext")
(define-key *top-map* (kbd "C-M-Down")    "gnext")
(define-key *top-map* (kbd "C-M-Up")      "gprev")
(define-key *top-map* (kbd "C-M-\"")      "grouplist")


;; Resizing frames
(define-key *top-map* (kbd "C-KP_Divide")     "decrease-step-size")
(define-key *top-map* (kbd "C-KP_Multiply")   "increase-step-size")
(define-key *top-map* (kbd "C-KP_Add")        "widen-frame")
(define-key *top-map* (kbd "C-KP_Subtract")   "narrow-frame")
(define-key *top-map* (kbd "M-KP_Add")        "grow-frame")
(define-key *top-map* (kbd "M-KP_Subtract")   "shrink-frame")

(define-key *top-map* (kbd "C-`")    "my-screen-shot")
(define-key *top-map* (kbd "C-M-l")  "exec slock") ; Locks current session
(define-key *top-map* (kbd "M-F4")   "delete-window")
(define-key *top-map* (kbd "C-M-F4") "kill-window")
(define-key *top-map* (kbd "C-Z")    "pop-console") ; more precisely, pop urxvt


(define-key *top-map* (kbd "XF86Sleep") "my-suspend")


;; INIT: create the three usual workspaces
(run-commands "grename CHROME"
              "gnewbg EMACS"
              "gnewbg SHELL")


;; MODELINE WIDGETS DEFINITIONS
(load "/home/cji/.stumpwm.d/my-modeline.lisp")


;; SCREENSHOTS
(load "/home/cji/.stumpwm.d/my-screenshots.lisp")


;; AUTO WINDOW PLACEMENT
(load "/home/cji/.stumpwm.d/my-window-rules.lisp")


;; SPLITTING AND RESIZING WINDOWS INTERACTIVELY
(load "/home/cji/.stumpwm.d/my-windows.lisp")


;; GROUPS CREATION AND SWITCHING
(load "/home/cji/.stumpwm.d/my-groups.lisp")


;; MISC COMMANDS

(defcommand my-suspend () ()
  (run-shell-command "sudo pm-suspend" t))

;; TODO: debounce these to make them call ext. command less often. It sometimes
;; breaks when the command is issued too often/too many times.
(defcommand backlight-down () ()
  (run-shell-command "xbacklight -time 0.1 -dec 10" t))

(defcommand backlight-up () ()
  (run-shell-command "xbacklight -time 0.1 -inc 10" t))

(defcommand pop-console () ()
  (run-commands "vsplit"
                "move-focus down"
                "resize 0 -175"
                "exec urxvt"))
