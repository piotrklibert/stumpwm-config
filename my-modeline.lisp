(load-module "stumptray")
(load-module "cpu")

;; (setf *window-format* "%m%n%s")
(setf *window-format* "%50t")
(setf *mode-line-timeout* 0.7)



(define-modeline-thread *vol* (concat "amixer sget Master "
                                      "| awk '/^ +Front L/{print $5}'"))
(define-modeline-thread *bklight* "/usr/bin/xbacklight")
(define-modeline-thread *sig* "awk 'NR==3{print $3}' /proc/net/wireless")
(define-modeline-thread *bat* (concat "upower -i /org/freedesktop/UPower/devices/battery_BAT0 "
                                      "| grep perc "
                                      "| awk '{print $2}'"))
(defun get-volume ()
  (string-trim (string #\newline) *vol*))

(defun get-backlight ()
  (car (split-string *bklight* ".")))

(defun get-signal-strength ()
  (string-trim
   (concat "." (string #\newline))
   *sig*))


(defvar *modeline-async-delay* 1.2
  "How long to wait between calls to external commands")

(defun some-thread-still-runs-p (threads)
  "This HAS to be possible to write much more elegantly..."
  (not (= 0 (count-if #'identity (mapcar #'sb-thread:thread-alive-p threads)))))

(defun apply0 (f) (apply f '()))
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

(defparameter modeline-thread nil)

(defun make-modeline-thread ()
  (setf modeline-thread (sb-thread:make-thread #'modeline-thread)))

(add-hook *start-hook* #'make-modeline-thread)





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
  "Needs the `upower' utility to be installed in the system, i.e. on Fedora:
`sudo dnf install -y upower`.
  Also, try running: `upower --enumerate` to learn what your battery is called. "
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
;;
(setf stumpwm:*screen-mode-line-format*
      (list
       "Up:"
       '(:eval (get-uptime))
       " ^2%d^]    ^1[|>^] %g    ^1[|>^] %W "
       " ^> "                      ; remaining elements become left-aligned

       "^2%c %t^]  "               ; CPU usage indicators (from load-module
                                        ; call earlier)

            ;; my own indicators:
            "   Vol:"   '(:eval (get-volume))
            "   Disp:[" '(:eval (get-backlight))
            "]  WiFi:[" '(:eval (get-signal-strength))
            "]  BAT:["  '(:eval (get-battery-status)) "]"
            ))



(setf *mode-line-position* :top)
(mode-line)

;; TODO: debounce these to make them call ext. command less often. It sometimes
;; breaks when the command is issued too often/too many times.
(defcommand backlight-down () ()
  (run-shell-command "xbacklight -time 0.1 -dec 10" t))

(defcommand backlight-up () ()
  (run-shell-command "xbacklight -time 0.1 -inc 10" t))



(defun run-prog (prog &rest opts &key args output (wait t) &allow-other-keys)
  "Common interface to shell. Does not return anything useful."
  (setq opts (remove-plist opts :args :output :wait))
  (let ((env (sb-ext:posix-environ)))
    (when (current-screen)
      (setf env (cons (screen-display-string (current-screen) t)
                      (remove-if (lambda (str)
                                   (string= "DISPLAY=" str
                                            :end2 (min 8 (length str))))
                                 env))))
    (apply #'sb-ext:run-program prog args :output (if output output t)
           :error t :wait wait :environment env opts)))

(defun run-prog-collect-output (prog &rest args)
  "run a command and read its output."
  (with-output-to-string (s)
    (run-prog prog :args args :output s :wait t)))

(defun remove-plist (plist &rest keys)
  "Remove the keys from the plist.
Useful for re-using the &REST arg after removing some options."
  (do (copy rest)
      ((null (setq rest (nth-value 2 (get-properties plist keys))))
       (nreconc copy plist))
    (do () ((eq plist rest))
      (push (pop plist) copy)
      (push (pop plist) copy))
    (setq plist (cddr plist))))

(defun screen-display-string (screen &optional (assign t))
  (format nil
          (if assign "DISPLAY=~a:~d.~d" "~a:~d.~d")
          "" ;(screen-host screen)
          (xlib:display-display *display*)
          "" ;(screen-id screen)
          ))
