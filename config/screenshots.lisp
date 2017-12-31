(defpackage :my-screenshots
  (:use :cl :stumpwm :alexandria :cl-arrows :my-utils)
  (:export make-screen-shot
           pop-last-screen-shot))
(in-package :my-screenshots)

(defparameter *my-img-count* 0)

(defun my-reset-count ()
  (setf *my-img-count* 0))

(defun my-get-fname (num)
  (format nil "~4,'0D_screen.png" num))

(defcommand make-screen-shot () ()
  (setf *my-img-count* (1+ *my-img-count*))
  (let
      ((target-filename (my-get-fname *my-img-count*)))
    (my-threading:shell$ (format nil "scrot '~a' -e 'mv $f ~~/shots/'" target-filename))
    (message (format nil "~a" target-filename))))



(defcommand pop-last-screen-shot () ()
  (let*
      ((home (namestring (user-homedir-pathname)))
       (shot-path (format nil "~A/shots/~A" home
                          (my-get-fname *my-img-count*)))
       (dest (concatenate 'string home "/screen.png")))

    (run-shell-command (format nil "cp ~A ~A" shot-path dest) t)
    (run-shell-command (format nil "pinta ~A" dest) nil)))
