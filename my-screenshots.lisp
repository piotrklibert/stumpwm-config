(in-package :stumpwm-user)

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
     (format nil "exec scrot '~a' -e 'mv $f ~~/shots/'" target-filename))
    (message (format nil "~a" target-filename))))
