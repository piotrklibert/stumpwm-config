(defpackage :my-utils
  (:use :cl :stumpwm :cl-arrows :split-sequence)
  (:export trim-newlines
           control-center
           pop-console
           skype
           rpi
           chrome
           suspend
           apply0
           remove-plist
           screen-display-string
           my-log
           my-log1
           send-line
           send-newline
           run-prog
           run-prog-collect-output
           runner-thread
           run-prog-async
           backlight-up
           backlight-down
           find-window find-group))
(in-package :my-utils)

;; (ql:quickload "local-package-aliases")
(local-package-aliases:set :cl-ppcre :re)

;; (ql:quickload "cl-cairo2")
;; (ql:quickload "cl-cairo2-xlib")

;; (local-package-aliases:set :cl-cairo2 :cx)

;; (let* ((w (screen-width (car *screen-list*)))
;;        (h 30)
;;        (*context* ;($cx:create-pdf-context "simpletext.pdf" 100 100)
;;          ($cx:create-xlib-image-context w h)
;;          ))
;;   ($cx:rectangle 0 0 w h *context*)
;;   ($cx:set-source-rgb 0.2 0.2 0.5 *context*)
;;   ($cx:fill-path *context*)
;;   ($cx:set-source-rgb 1 0.6 1 *context*)
;;   ($cx:move-to 10 22 *context*)
;;   ($cx:set-font-size (* 1.4 (/ h 2)) *context*)
;;   ($cx:show-text "Wed Aug 02 23:45 łłłłłł" *context*))

(defun find-window (title)
  (flet ((r (z) ($re:scan (string-downcase title) z)))
    (loop for window in (stumpwm::all-windows)
       for x = (string-downcase (window-title window))
       if (r x) return window)))

(defun find-group (gname)
  (flet ((r (z) ($re:scan (string-downcase gname) z)))
    (loop for group in (screen-groups (current-screen))
       for x = (string-downcase (group-name group))
       if (r x) return group)))


(defcommand control-center () () (run-shell-command "gnome-control-center"))
(defcommand chrome         () () (run-shell-command "google-chrome"))
(defcommand suspend        () () (run-shell-command "sudo systemctl suspend" t))
(defcommand rpi            () () (run-shell-command "urxvt -e ssh pi"))
(defcommand backlight-down () () (run-shell-command "sudo /usr/local/bin/xbacklight -time 1 -dec 10"))
(defcommand backlight-up   () () (run-shell-command "sudo /usr/local/bin/xbacklight -time 1 -inc 10"))

(defcommand pop-console    () () (run-commands "vsplit" "move-focus down"
                                               "resize 0 -175" "exec urxvt"))

(defcommand skype () ()
  (if (and (typep (current-window) 'window)
           (search "Skype" (window-title (current-window))))
      (stumpwm::hide-window (current-window))
      (run-or-pull "skypeforlinux" '(:instance "skypeforlinux"
                                     :class "skypeforlinux")
                   t t)))


(defun trim-newlines (s)
  (string-trim (string #\newline) s))

(defun apply0 (f) (apply f '()))

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
  (declare (ignorable assign screen))
  (format nil "DISPLAY=:~d" (xlib:display-display *display*)))



(defun my-log (fmt &rest args)
  (let ((*debug-level* 1))
    (apply #'dformat 1 fmt args)))

(defun my-log1 (&rest args)
  (my-log "~a~%" args))

(defun send-line (str)
  (loop for char across str
     do (stumpwm::send-meta-key (current-screen)
                       (kbd (string char)))))

(defun send-newline ()
  (stumpwm::send-meta-key (current-screen) (kbd "Return")))

(defmacro send-lines (&rest lines)
  "Send given strings as KeyEvents (character after character), inserting
additional newline between strings."
  `(progn
     ,@(loop for line in lines
          append (list `(send-line ,line)
                       '(send-newline)))))

;; Example:
;; (send-lines "asdasdsdasda"
;;             "123123131231")
