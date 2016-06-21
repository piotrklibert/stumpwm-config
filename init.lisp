(ql:quickload "cl-arrows")
(ql:quickload "cl-op")
(ql:quickload "split-sequence")
(ql:quickload "xembed")
(ql:quickload "alexandria")
(ql:quickload "clx-truetype")


(use-package :cl-arrows)
(use-package :cl-op)


;; Enable SLIME/Emacs integration
(load "/home/cji/.stumpwm.d/swank.lisp")

(load "/home/cji/.stumpwm.d/utils.lisp")


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
(shell$ "sudo mkfontscale /usr/share/fonts/bitstream-vera/"
        "sudo mkfontdir /usr/share/fonts/bitstream-vera/"
        "xset fp+ /usr/share/fonts/bitstream-vera/")


(load-module "ttf-fonts")

(defun my-set-font ()
  (let
      ((font (make-instance 'clx-truetype:font
                            :family "Bitstream Vera Sans Mono"
                            :subfamily "Roman" :size 13
                            :antialiased t)))
    (message "Setting TrueType font for mode-line")
    (set-font font)))

(my-set-font)

;; for some reason SET-FONT stopped working on startup, but it works if you wait
;; a bit... No idea WTF.
;; And now it stopped working completely and X server says there is no such
;; font...
;; (run-with-timer 10 nil #'my-set-font)

;; NOTE:
;; *top-map* - GLOBAL one
;; *root-map* - the one under C-z

;; (define-key *root-map* (kbd "=")     "place-existing-windows")
;; (define-key *root-map* (kbd "C-r")   "place-existing-windows")
(define-key *root-map* (kbd "C-o")   "only")
(define-key *root-map* (kbd "C-s")   "my-vsplit")
(define-key *root-map* (kbd "C-M-s") "my-vsplit")
(define-key *root-map* (kbd "C-v")   "my-hsplit")
(define-key *root-map* (kbd "C-M-v") "my-hsplit")
(define-key *root-map* (kbd "C-z")   "send-escape")
(define-key *root-map* (kbd "C-c")   "exec urxvt")
(define-key *root-map* (kbd "d")     "remove")
(define-key *root-map* (kbd "C-d")   "remove-split")
(define-key *root-map* (kbd "C-M-d") "my-remove")
(define-key *root-map* (kbd "I")     "show-window-properties")
(define-key *root-map* (kbd "c")     "my-gnew")
(define-key *root-map* (kbd "x")     "colon")
(define-key *root-map* (kbd "C-w")   "chrome")
(define-key *root-map* (kbd "M-s")   "control-center")


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

(define-key *root-map* (kbd "C-Right") "move-focus right")
(define-key *root-map* (kbd "C-Left")  "move-focus left")
(define-key *root-map* (kbd "C-Up")    "move-focus up")
(define-key *root-map* (kbd "C-Down")  "move-focus down")

;; Moving between groups
(define-key *top-map* (kbd "C-M-Left")    "gprev")
(define-key *top-map* (kbd "C-M-Right")   "gnext")
(define-key *top-map* (kbd "C-M-Down")    "gnext")
(define-key *top-map* (kbd "C-M-Up")      "gprev")
(define-key *top-map* (kbd "C-M-\"")      "grouplist")


;; Resizing frames
;; (define-key *top-map* (kbd "C-KP_Divide")     "decrease-step-size")
;; (define-key *top-map* (kbd "C-KP_Multiply")   "increase-step-size")

(define-key *top-map* (kbd "C-KP_Add")        "widen-frame")
(define-key *top-map* (kbd "C-KP_Subtract")   "narrow-frame")
(define-key *top-map* (kbd "M-KP_Add")        "grow-frame")
(define-key *top-map* (kbd "M-KP_Subtract")   "shrink-frame")

(define-key *top-map* (kbd "C-`")    "make-screen-shot")
(define-key *top-map* (kbd "C-M-`")  "pop-last-screen-shot")
(define-key *top-map* (kbd "C-M-l")  "exec /home/cji/poligon/slock/slock") ; Locks current session
;; (define-key *top-map* (kbd "C-M-l")  "exec slock") ; Locks current session
(define-key *top-map* (kbd "M-F4")   "delete-window")
(define-key *top-map* (kbd "C-M-F4") "kill-window")
(define-key *top-map* (kbd "C-Z")    "pop-console") ; more precisely, pop urxvt


(define-key *top-map* (kbd "XF86Sleep") "suspend")


;; INIT: create the three usual workspaces
(run-commands "grename CHROME"
              "gnewbg EMACS"
              "gnewbg SHELL")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SCREENSHOTS
(load "/home/cji/.stumpwm.d/my-screenshots.lisp")


;; AUTO WINDOW PLACEMENT
(load "/home/cji/.stumpwm.d/my-window-rules.lisp")


;; SPLITTING AND RESIZING WINDOWS INTERACTIVELY
(load "/home/cji/.stumpwm.d/my-windows.lisp")


;; GROUPS CREATION AND SWITCHING
(load "/home/cji/.stumpwm.d/my-groups.lisp")

;; MODELINE WIDGETS DEFINITIONS
(load "/home/cji/.stumpwm.d/my-modeline.lisp")


;; MISC COMMANDS
(defcommand control-center () ()
  (run-shell-command "gnome-control-center"))

(defcommand chrome () ()
  (run-shell-command "google-chrome"))

(defcommand suspend () ()
  (run-shell-command "sudo systemctl suspend" t))


(defcommand pop-console () ()
  (run-commands "vsplit"
                "move-focus down"
                "resize 0 -175"
                "exec urxvt"))
(defcommand rpi () ()
  (run-shell-command "urxvt -e ssh pi"))
