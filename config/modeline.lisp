(in-package :my-modeline)

;; (load-module "stumptray")
;; (load-module "cpu")

;; (setf stumpwm:*mode-line-timeout* 0.3)
;; (stumptray:add-mode-line-hooks)

(defvar *modeline-subthreads-processing* nil)
(defvar *modeline-subthreads-timer*      nil)
(defvar *modeline-subthreads-timeout*    5)
(defvar *modeline-subthreads-results*    (make-hash-table :synchronized t))

(defmacro define-modeline-subthread (var-name command &key (func 'identity))
  (let
      ((getter-name (intern (concatenate 'string "GET-" (symbol-name var-name))))
       (runner-name var-name))
    (print getter-name)
   `(progn
      (setf (gethash ',var-name *modeline-subthreads-results*) "")
      (defun ,getter-name ()
        (gethash ',var-name *modeline-subthreads-results*))
      (defun ,runner-name ()
        (setf (gethash ',var-name *modeline-subthreads-results*)
              (->> (run-command-sync ,command)
                   (funcall ,func))))
      (export ',runner-name))))

(defun run-subthreads-once ()
  (setf *modeline-subthreads-processing* t)
  (let ((ch (lparallel:make-channel)))
    (loop for func-name in (hash-table-keys *modeline-subthreads-results*)
       do (lparallel:submit-task ch func-name))
    (loop repeat (hash-table-count *modeline-subthreads-results*)
       do (lparallel:receive-result ch)))
  (setf *modeline-subthreads-processing* nil))
;; (run-subthreads-once)

(define-modeline-subthread uptime
  "uptime")

(define-modeline-subthread volume
  "amixer sget Master | awk '/^ +Mono/{print $4}'"
  :func #'trim-newlines)

(define-modeline-subthread backlight
  "/usr/local/bin/xbacklight -get"
  :func (lambda (it)
          (trim-newlines (car (split-string it ".")))))

(define-modeline-subthread signal-strength
  "awk 'NR==3{print $3}' /proc/net/wireless"
  :func (lambda (it)
          (string-trim (concat "." (string #\newline)) it)))

;; Needs the `upower' utility to be installed in the system.
;; Run: `upower --enumerate` to learn what your battery is called.
(define-modeline-subthread battery-status
    (concat "upower -i /org/freedesktop/UPower/devices/battery_BAT0 "
            "| grep perc "
            "| awk '{print $2}'")
    :func #'trim-newlines)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-modeline-subthreads-if-not-busy ()
  (unless *modeline-subthreads-processing*
    (lparallel:submit-task
     (lparallel:make-channel)
     #'apply0 #'run-subthreads-once)))

(defun start-modeline-refresh-timer ()
  (setf *modeline-subthreads-timer*
        (run-with-timer 1 *modeline-subthreads-timeout* #'run-modeline-subthreads-if-not-busy)))

;; (start-modeline-refresh-timer)
;; (cancel-timer *modeline-subthreads-timer*)
;; (add-hook *start-hook* #'start-modeline-refresh-timer)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defvar *current-battery-status* 100)

;; (defun low-battery-alert (&optional bat_percentage)
;;   ;; use a global value if the caller didn't provide its own value
;;   ;; (for ease of testing in the REPL)
;;   (when (not bat_percentage)
;;     (setf bat_percentage *current-battery-status*))
;;   (let
;;       ((*message-window-gravity* :center) ; make alert appear at the at the center of screen
;;        (*timeout-wait* 6))              ; wait 6 sec before making alert disappear
;;     (message "Your battery is running low!~%Only ~s% remaining..." bat_percentage)))


;; (defun get-battery-status ()
;;   (let
;;       ((new-battery-status (parse-integer *bat* :junk-allowed t)))
;;     (when (and (<= *current-battery-status* 15)
;;                ;; show alert only when battery status changed since last check,
;;                ;; but don't do this if the new value is greater than the last
;;                ;; (this means we're probably plugged in and charging already)
;;                (> *current-battery-status* new-battery-status))
;;       (low-battery-alert new-battery-status))
;;     (setf *current-battery-status* new-battery-status)
;;     (format nil "~s%" new-battery-status)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defun get-uptime ()
;;   "Example uptime output:
;;     20:56:14 up  2:50,  1 user,  load average: 0,28, 0,38, 0,2"
;;   (let
;;       ((line (string-trim '(#\newline #\space) (run-shell-command "uptime" t))))
;;     (multiple-value-bind (match groups)
;;         (cl-ppcre:scan-to-strings "up *([^,]*)" line)
;;       (concat "[" (elt groups 0) "]"))))


;;
;;                              MODELINE FORMAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setf stumpwm:*screen-mode-line-format*
;;       (list
;;        ;; "Up:"
;;        ;; '(:eval (get-uptime))
;;        " ^2%d^]    ^1[|>^] %g    ^1[|>^] %W "
;;        " ^> "                           ; remaining elements become left-aligned

;;        "^2%c %t^]  "                    ; CPU usage indicators (from load-module
;;                                         ; call earlier)

;;             ;; my own indicators:
;;             "   Vol:"   '(:eval (get-volume))
;;             "   Disp:[" '(:eval (get-backlight))
;;             "]  WiFi:[" '(:eval (get-signal-strength))
;;             "]  BAT:["  '(:eval (get-battery-status)) "]  "
;;             ))



;; (setf *mode-line-position* :top)
;; (add-hook *start-hook* #'mode-line)

;; ;; TODO: debounce these to make them call ext. command less often. It sometimes
;; ;; breaks when the command is issued too often/too many times.
