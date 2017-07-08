;; NOTE:
;; *top-map* - global, prefixless one
;; *root-map* - the one under C-z

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
(define-key *root-map* (kbd "C-p")   "skype")


;; Screen brightness & Volume adjustment
(define-key *top-map* (kbd "XF86AudioLowerVolume")  "exec amixer set Master 5%-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume")  "exec amixer set Master 5%+")
(define-key *top-map* (kbd "XF86AudioMute")         "exec amixer set Master toggle")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "backlight-down")
(define-key *top-map* (kbd "XF86MonBrightnessUp")   "backlight-up")

;; Window switching. TODO: make this behave like a queue
(define-key *top-map* (kbd "M-TAB")                 "my-next")
(define-key *top-map* (kbd "M-ISO_Left_Tab")        "my-prev")

(define-key *root-map* (kbd "C-Right") "move-focus right")
(define-key *root-map* (kbd "C-Left")  "move-focus left")
(define-key *root-map* (kbd "C-Up")    "move-focus up")
(define-key *root-map* (kbd "C-Down")  "move-focus down")

(define-key *root-map* (kbd "C-SunPageUp")  "gprev")
(define-key *root-map* (kbd "C-SunPageDown")  "gnext")

(define-key *root-map* (kbd "C-M-SunPageUp") "move-group-backward-cmd")
(define-key *root-map* (kbd "C-M-SunPageDown") "move-group-forward-cmd")

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
(define-key *top-map* (kbd "C-M-l")  "exec /home/cji/priv/slock/slock") ; Locks current session
;; (define-key *top-map* (kbd "C-M-l")  "exec slock") ; Locks current session
(define-key *top-map* (kbd "M-F4")   "delete-window")
(define-key *top-map* (kbd "C-M-F4") "kill-window")
(define-key *top-map* (kbd "C-Z")    "pop-console") ; more precisely, pop urxvt


(define-key *top-map* (kbd "XF86Sleep") "suspend")
