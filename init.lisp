(ql:quickload "cl-arrows")
(ql:quickload "split-sequence")

(use-package :cl-arrows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load SWANK.
(load "/home/cji/.emacs.d/forked-plugins/slime/swank-loader.lisp")
(swank-loader:init)
(defcommand swank () ()
  (swank:create-server
   :port 4005
   :style swank:*communication-style*
   :dont-close t)
  (echo-string (current-screen)
               (concat "Starting swank. M-x slime-connect RET RET, "
                       "then (in-package stumpwm).")))
(swank)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FONTS
(run-shell-command "sudo mkfontscale /usr/share/fonts/bitstream-vera/" t)
(run-shell-command "sudo mkfontdir /usr/share/fonts/bitstream-vera/" t)
(run-shell-command "xset fp+ /usr/share/fonts/bitstream-vera/" t)

(when (< 0 (length (xlib:list-font-names *display* "*vera*" :max-fonts 1)))
 (set-font "-*-bitstream vera sans mono-*-r-*-*-15-*-*-*-*-*-iso8859-*"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(set-prefix-key (kbd "C-z"))
(set-module-dir "/home/cji/portless/stumpwm/contrib")


(setf *input-window-gravity*         :center)
(setf *message-window-gravity*       :top-right)
(setf *mouse-focus-policy*           :click)
(setf *print-circle*                 t)
(setf *timeout-wait*                 3)

(setf *startup-message*              (format nil "Welcome to ~a!" (machine-instance)))


(load-module "cpu")
(setf *window-format* "%m%n%s%c")
(setf *mode-line-timeout* 0.7)

(defun get-volume ()
  (string-trim
   (string #\newline)
   (run-shell-command "amixer sget Master | awk '/^ +Front L/{print $5}'" t)))

(defun get-backlight ()
  (car (split-string (run-shell-command "xbacklight" t) ".")))

(defun get-signal-strength ()
  (string-trim
   (concat "." (string #\newline))
   (run-shell-command "awk 'NR==3{print $3}' /proc/net/wireless" t)))


(defvar *current-battery-status* 100)

(defun low-battery-alert (&optional bat_percentage)
  ;; use a global value if the caller didn't provide its own value
  ;; (for ease of testing in the REPL)
  (when (not bat_percentage) (setf bat_percentage *current-battery-status*))
  (let
      ((*message-window-gravity* :center) ; make alert appear at the at the center of screen
       (*timeout-wait* 6))              ; wait 6 sec before making alert disappear
    (message "Your battery is running low!~%Only ~s% remaining..." bat_percentage)))


(defun get-battery-status ()
  "Needs the `upower' utility to be installed in the system, i.e. on Fedora:

$ sudo dnf install -y upower

  Also, try running:

$ upower --enumerate

  to learn what your battery is called. "

  (let* ((command (concat
                   "upower -i /org/freedesktop/UPower/devices/battery_BAT0 "
                   "| grep perc "
                   "| awk '{print $2}'"))
         ;; the command above returns a string looking like this: "11%\n"
         ;; we need to get rid of a percent sign and a newline char then convert
         ;; it into a number, which READ-FROM-STRING does for us
         (new-battery-status (->> (run-shell-command command t)
                                  (string-trim (string #\newline))
                                  (string-trim "%")
                                  read-from-string)))

    (when (and (<= *current-battery-status* 15)
               ;; show alert only when battery status changed since last check,
               ;; but don't do this if the new value is greater than the last
               ;; (this means we're probably plugged in and charging already)
               (> *current-battery-status* new-battery-status))
      (low-battery-alert new-battery-status))
    (setf *current-battery-status* new-battery-status)
    (format nil "~s%" new-battery-status)))


(setf stumpwm:*screen-mode-line-format*
      (list "^2%d^]    ^1[|>^] %g    ^1[|>^] %W "
            " ^> "                      ; remaining elements become left-aligned

            "^2%c %t^]  "               ; CPU usage indicators (from load-module
                                        ; call earlier)
            ;; my own indicators:
            "   Vol:"   '(:eval (get-volume))
            "   Disp:[" '(:eval (get-backlight))
            "]  WiFi:[" '(:eval (get-signal-strength))
            "]  BAT:["  '(:eval (get-battery-status)) "]"))

(setf *mode-line-position* :top)
(mode-line)



;;==============================================================================
;;; TODO:
;; automatyczne przerzucanie okienek miedzy frames
;; kilka roznych wariantow predefiniowanych
;; dzielenie z przeniesieniemum focusu lub bez
;; quick-peek (szybkie pokazanie i schowanie)


(define-key *root-map* (kbd "I") "show-window-properties")

(define-key *root-map* (kbd "C-o")  "only")
(define-key *root-map* (kbd "C-s")  "my-vsplit")
(define-key *root-map* (kbd "C-v")  "my-hsplit")
(define-key *root-map* (kbd "C-z")  "send-escape")
(define-key *root-map* (kbd "c")    "my-gnew")
(define-key *root-map* (kbd "x")    "colon")
(define-key *root-map* (kbd "C-c")  "exec urxvt")


(defcommand my-remove () ()
  (run-commands "delete" "remove"))

(define-key *root-map* (kbd "C-d")  "my-remove")
(define-key *root-map* (kbd "d")    "remove")

(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec amixer set Master 5%-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec amixer set Master 5%+")
(define-key *top-map* (kbd "XF86AudioMute") "exec amixer set Master toggle")

;; TODO: debounce these to make them call ext. command less often. It sometimes
;; breaks when the command is issued too often/too many times.
(defcommand backlight-down () ()
  (echo "ok!")
  (run-shell-command "xbacklight -time 1 -dec 10"))

(defcommand backlight-up () ()
  (echo "ok!")
  (run-shell-command "xbacklight -time 1 -inc 10"))


(define-key *top-map* (kbd "XF86MonBrightnessDown") "backlight-down")
(define-key *top-map* (kbd "XF86MonBrightnessUp") "backlight-up")


(define-key *top-map* (kbd "M-F4") "delete-window")
(define-key *top-map* (kbd "C-M-F4") "kill-window")



(define-key *top-map* (kbd "M-TAB") "next")
(define-key *top-map* (kbd "M-ISO_Left_Tab") "prev")


(defcommand pop-console () ()
  (run-commands "vsplit"
                "move-focus down"
                "resize 0 -175"
                "exec urxvt"))

(define-key *top-map* (kbd "C-Z")  "pop-console")

(define-key *top-map* (kbd "C-M-Left")    "gprev")
(define-key *top-map* (kbd "C-M-Down")    "gnext")
(define-key *top-map* (kbd "C-M-Right")   "gnext")
(define-key *top-map* (kbd "C-M-Up")      "gprev")
(define-key *top-map* (kbd "C-M-\"")      "grouplist")

;; (define-key *top-map* (kbd "C-M-l") "exec xscreensaver-command -activate")
(define-key *top-map* (kbd "C-M-l")    "exec slock")

(defcommand my-hsplit () ()
  (hsplit)
  (move-focus :right))

(defcommand my-vsplit () ()
  (vsplit)
  (move-focus :down))

(defcommand my-gnew (&optional name) ((:string "Group name? "))
  (let*
      ((prev (current-group))
       (name (if (or (not name) (= 0 (length name))) (random-name 7) name))
       (new (gnew name)))
    (gselect prev)
    (gmove new)
    (gselect new)))


(defun random-name (length)
  (with-output-to-string (stream)
    (let ((*print-base* 36))
      (loop repeat length do (princ (random 36) stream)))))



;; (let ((l (copy-list steps)))
;;   (setf (cdr (last steps)) l)
;;   (loop for i from 0 to 15
;;      do (print (nth (+ 200 i) l))))

(defvar steps (list 50 120 200 300 500))
(defvar current-step-choice 120)

(defun get-next-step (step lst)
  (if (= (car lst) step)
      (if (not (cdr lst))
          (car steps)
          (cadr lst))
      (get-next-step step (cdr lst))))

(defun get-prev-step (step lst)
  (if (= step (car lst))
      step
      (labels ((find-prev (step lst)
                 (if (= (cadr lst) step)
                     (car lst)
                     (get-prev-step step (cdr lst))) ))
        (find-prev step lst))))

(defcommand increase-step-size () ()
  (setf current-step-choice (get-next-step current-step-choice steps))
  (message (format nil "~a" current-step-choice)))

(defcommand decrease-step-size () ()
  (setf current-step-choice (get-prev-step current-step-choice steps))
  (message (format nil "~a" current-step-choice)))

(defun widen/narrow-frame (&key x y)
  (let ((x (or x 0))
        (y (or y 0)))
    (run-commands (format nil "resize ~a ~a" x y))))

(defcommand widen-frame  () () (widen/narrow-frame :x current-step-choice))
(defcommand narrow-frame () () (widen/narrow-frame :x (- current-step-choice)))
(defcommand grow-frame   () () (widen/narrow-frame :y current-step-choice))
(defcommand shrink-frame () () (widen/narrow-frame :y (- current-step-choice)))



(define-key *top-map* (kbd "C-KP_Divide")     "decrease-step-size")
(define-key *top-map* (kbd "C-KP_Multiply")   "increase-step-size")
(define-key *top-map* (kbd "C-KP_Add")        "widen-frame")
(define-key *top-map* (kbd "C-KP_Subtract")   "narrow-frame")
(define-key *top-map* (kbd "M-KP_Add")        "grow-frame")
(define-key *top-map* (kbd "M-KP_Subtract")   "shrink-frame")


(defcommand emacsclient () ()
  (run-shell-command "emacsclient -c" t)
  )

(defcommand exec-other-window (command) ((:string "Command? "))
  (run-commands "hsplit" "resize 400 0")
  (fnext)
  (run-shell-command "urxvt" t))


(defun mywm ()
  (run-commands "grename CHROME")
  (run-commands "exec google-chrome")

  (run-commands "gnewbg EMACS")
  (run-commands "exec emacs")
  (gmove 2)

  (run-commands "exec urxvt")
  (window-send-string "tmux "))

(mywm)

(defun my-close-and-kill () ()
  (kill-window)
  (remove-split))



(defparameter *my-img-count* 0)
(defun my-reset-count ()
  (setf *my-img-count* 0))

(defun my-get-fname (num)
  (format nil "~4,'0D_screen.png" num))

(defcommand my-screen-shot () ()
  (setf *my-img-count* (1+ *my-img-count*))
  (let
      ((target-filename (my-get-fname *my-img-count*)))
    (run-commands
     (format nil "exec scrot '~a' -e 'mv $f ~~/shots/'" target-filename)
     "echo OK!")))


(define-key *top-map* (kbd "C-`") "my-screen-shot")



;; TODO: make this suspend things...
(defcommand my-suspend () ()
  (message (run-shell-command "echo `echo as` Asdasd" t)))
(define-key *top-map* (kbd "XF86Sleep") "my-suspend")
