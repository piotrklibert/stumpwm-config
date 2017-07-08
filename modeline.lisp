(load-module "stumptray")
(load-module "cpu")

(setf *mode-line-timeout* 0.7)
(stumptray:add-mode-line-hooks)

;; Indicators running in background threads:
(define-modeline-thread *vol*
    "amixer sget Master | awk '/^ +Mono/{print $4}'")

(defun get-volume ()
  (string-trim (string #\newline) *vol*))


(define-modeline-thread *bklight*
    "/usr/local/bin/xbacklight -get")

(defun get-backlight ()
  (car (split-string *bklight* ".")))


(define-modeline-thread *sig*
    "awk 'NR==3{print $3}' /proc/net/wireless")

(defun get-signal-strength ()
  (string-trim (concat "." (string #\newline)) *sig*))


;; Needs the `upower' utility to be installed in the system.
;; Run: `upower --enumerate` to learn what your battery is called.
(define-modeline-thread *bat*
    (concat "upower -i /org/freedesktop/UPower/devices/battery_BAT0 "
            "| grep perc "
            "| awk '{print $2}'"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *modeline-thread* nil)
(defparameter *modeline-async-delay* 0.6
  "How long to wait between calls to external commands")

(defun modeline-thread ()
  (loop                                 ; NOTE: loops forever!
     (let
         ((threads (mapcar 'apply0 modeline-external-threads)))
       (loop                            ; wait for all commands to finish
          do (sleep 0.1)
          while (some-thread-still-runs-p threads))
       ;; finalize the threads to make sure memory is freed
       (destroy-threads threads)
       (sleep *modeline-async-delay*))))

(defun destroy-threads (threads)
  (ignore-errors
    (mapcar #'sb-thread:destroy-thread threads)))


(defun make-modeline-thread ()
  (setf *modeline-thread* (sb-thread:make-thread #'modeline-thread)))

(add-hook *start-hook* #'make-modeline-thread)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *current-battery-status* 100)

(defun low-battery-alert (&optional bat_percentage)
  ;; use a global value if the caller didn't provide its own value
  ;; (for ease of testing in the REPL)
  (when (not bat_percentage)
    (setf bat_percentage *current-battery-status*))
  (let
      ((*message-window-gravity* :center) ; make alert appear at the at the center of screen
       (*timeout-wait* 6))              ; wait 6 sec before making alert disappear
    (message "Your battery is running low!~%Only ~s% remaining..." bat_percentage)))


(defun get-battery-status ()
  (let
      ((new-battery-status (parse-integer *bat* :junk-allowed t)))
    (when (and (<= *current-battery-status* 15)
               ;; show alert only when battery status changed since last check,
               ;; but don't do this if the new value is greater than the last
               ;; (this means we're probably plugged in and charging already)
               (> *current-battery-status* new-battery-status))
      (low-battery-alert new-battery-status))
    (setf *current-battery-status* new-battery-status)
    (format nil "~s%" new-battery-status)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-uptime ()
  "Example uptime output:
    20:56:14 up  2:50,  1 user,  load average: 0,28, 0,38, 0,2"
  (let
      ((line (string-trim '(#\newline #\space) (run-shell-command "uptime" t))))
    (multiple-value-bind (match groups)
        (cl-ppcre:scan-to-strings "up *([^,]*)" line)
      (concat "[" (elt groups 0) "]"))))


;;
;;                              MODELINE FORMAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf stumpwm:*screen-mode-line-format*
      (list
       ;; "Up:"
       ;; '(:eval (get-uptime))
       " ^2%d^]    ^1[|>^] %g    ^1[|>^] %W "
       " ^> "                           ; remaining elements become left-aligned

       "^2%c %t^]  "                    ; CPU usage indicators (from load-module
                                        ; call earlier)

            ;; my own indicators:
            "   Vol:"   '(:eval (get-volume))
            "   Disp:[" '(:eval (get-backlight))
            "]  WiFi:[" '(:eval (get-signal-strength))
            "]  BAT:["  '(:eval (get-battery-status)) "]  "
            ))



(setf *mode-line-position* :top)
(add-hook *start-hook* #'mode-line)

;; TODO: debounce these to make them call ext. command less often. It sometimes
;; breaks when the command is issued too often/too many times.
(defcommand backlight-down () ()
  (run-shell-command "sudo /usr/local/bin/xbacklight -time 0.1 -dec 10" t))

(defcommand backlight-up () ()
  (run-shell-command "sudo /usr/local/bin/xbacklight -time 0.1 -inc 10" t))
