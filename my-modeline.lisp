(load-module "stumptray")
(load-module "cpu")

;; (message (format nil "Argh: ~s ~s" out output))
;; (setf *window-format* "%m%n%s")
(setf *window-format* "%50t")
(setf *mode-line-timeout* 0.7)

;; TODO: decide whether it's needed to call run-prog directly, or if just
;; run-shell-command is going to work
(defun runner-thread (cmd output)
  (let
      ((process (run-prog *shell-program*
                          :args (list "-c" cmd)
                          :wait nil
                          :output :stream)))
    (loop
       do (sleep 0.1)
       while (sb-ext:process-alive-p process)
       finally
         (progn
           (let ((out (sb-ext:process-output process)))
             (setf (symbol-value output)
                   ;; TODO: CLEAN THIS!
                   (handler-case
                       (format nil "~s" (read out))
                     (error (x)
                       (run-shell-command cmd t))))
             (sb-ext:process-close process))))))

(defun run-prog-async (cmd output)
  (sb-thread:make-thread #'runner-thread :arguments (list cmd output)))

(defvar *vol* nil)
(defvar bklight nil)
(defvar sig 1)

(defvar *modeline-async-delay* 1.2)
(defun some-thread-still-runs-p (threads)
  "This HAS to be possible to write much more elegantly..."
  (not (= 0 (count-if #'identity (mapcar #'sb-thread:thread-alive-p threads)))))

(defun modeline-thread ()
  (loop
     (let
         ((threads (list (run-prog-async "amixer sget Master | awk '/^ +Front L/{print $5}'" '*vol*)
                         (run-prog-async "/usr/bin/xbacklight" 'bklight)
                         (run-prog-async "awk 'NR==3{print $3}' /proc/net/wireless" 'sig))))
       (loop
          do (sleep 0.1)
          while (some-thread-still-runs-p threads)
          finally (ignore-errors (mapcar #'sb-thread:destroy-thread threads)))
       (sleep *modeline-async-delay*))))

(defparameter modeline-thread nil)
(defun make-modeline-thread ()
  (setf modeline-thread (sb-thread:make-thread #'modeline-thread)))

(add-hook *start-hook* #'make-modeline-thread)

(defun get-volume ()
  (string-trim (string #\newline) *vol*))

(defun get-backlight ()
  (car (split-string bklight ".")))

(defun get-signal-strength ()
  (string-trim
   (concat "." (string #\newline))
   sig))


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


;;
;;                              MODELINE FORMAT
;;
(setf stumpwm:*screen-mode-line-format*
      (list "^2%d^]    ^1[|>^] %g    ^1[|>^] %W "
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
