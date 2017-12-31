(in-package :my-modeline)


(defun format-free-ram (output)
  (let
      ((free-ram (->> output
                     (cl-ppcre:split "[\\n ]+")
                     (nth 13)
                     read-from-string
                     )))

    (format nil "[~,2FGb]" (* free-ram 0.000001))))

;; (format-free-ram (my-threading:run-command-sync "free"))
(setf stumpwm:*screen-mode-line-format*
      (list
       ;; "Up:"
       ;; '(:eval (get-uptime)) " "
       "^2%d^]    ^1[|>^] %g    ^1[|>^] %W "
       " ^> "                           ; remaining elements become left-aligned

       "%c"                             ; CPU usage indicators (from
                                        ; contrib/modeline/cpu module)

       ;; my own indicators:
       " RAM:"   '(:eval (get-free))
       ;; " Vol:"   '(:eval (get-volume))
       ;; "  Disp:[" '(:eval (get-backlight))
       "  WiFi:[" '(:eval (get-signal-strength)) "]"
       "  ^2BAT:["  '(:eval (get-battery-status)) "]^]"
       "     "     ; a margin for tray, otherwise the tray is displayed over the
                   ; last indicator
       ))



(define-modeline-subthread free
  "free"
  :func #'format-free-ram)

;; (define-modeline-subthread uptime
;;   "uptime"
;;   :func (lambda (it)
;;           "Example uptime output:
;;                 20:56:14 up  2:50,  1 user,  load average: 0,28, 0,38, 0,2"
;;           (declare (ignorable it))
;;           (let
;;               ((line (string-trim '(#\newline #\space) (run-shell-command "uptime" t))))
;;             (multiple-value-bind (matched? groups)
;;                 (cl-ppcre:scan-to-strings "up *([^,]*)" line)
;;               (declare (ignorable matched?))
;;               (concat "[" (elt groups 0) "]")))))


;; (define-modeline-subthread volume
;;   "amixer sget Master | awk '/^ +Front Left/{print $5}'"
;;   :func #'trim-newlines)


;; (define-modeline-subthread backlight
;;   "/usr/local/bin/xbacklight -get"
;;   :func #'(lambda (it)
;;             (trim-newlines (car (split-string it ".")))))


(define-modeline-subthread signal-strength
  "awk 'NR==3{print $3}' /proc/net/wireless"
  :func #'(lambda (it)
            (string-trim (concat "." (string #\newline)) it)))


;; Needs the `upower' utility to be installed in the system.
;; Run: `upower --enumerate` to learn what your battery is called.
(define-modeline-subthread battery-status
  (concat "upower -i /org/freedesktop/UPower/devices/battery_BAT0 "
          "| grep perc "
          "| awk '{print $2}'")
  :func #'trim-newlines)
