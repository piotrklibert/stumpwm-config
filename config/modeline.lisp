(defpackage :my-modeline
  (:use :cl :stumpwm :alexandria :cl-arrows :my-threading :my-utils)
  (:export volume
           battery-status
           uptime
           signal-strength
           backlight))
(in-package :my-modeline)


(defun my-setup-modeline ()
  (setf stumpwm:*mode-line-timeout* 1.3)
  (setf stumpwm:*mode-line-position* :top)
  (stumptray:add-mode-line-hooks)
  (start-modeline-refresh-timer)
  (mode-line))

(add-hook *start-hook* #'my-setup-modeline)

(defvar *modeline-subthreads-processing* nil)
(defvar *modeline-subthreads-timer*      nil)
(defvar *modeline-subthreads-timeout*    2.5)
(defvar *modeline-subthreads-results*    (make-hash-table :synchronized t))


(defmacro define-modeline-subthread (var-name command &key func)
  (let
      ((getter-name (intern (concatenate 'string "GET-" (symbol-name var-name))))
       (runner-name var-name)
       (func (or func (quote #'identity))))
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
  "Just submit tasks to the queue, don't wait for them to execute."
  (let ((*modeline-subthreads-processing* t)
        (ch (lparallel:make-channel)))
    (loop for func-name in (hash-table-keys *modeline-subthreads-results*)
       do (lparallel:submit-task ch func-name))))
;; (time (run-subthreads-once))
;; (require :sb-sprof)
;; (sb-sprof:with-profiling (:loop t :report :flat :sample-interval 0.0001)
;;   (time (loop repeat 10 do (run-subthreads-once))))


(defun run-modeline-subthreads-if-not-busy ()
  (unless *modeline-subthreads-processing*
    (lparallel:submit-task
     (lparallel:make-channel)
     #'apply0 #'run-subthreads-once)))


(defun start-modeline-refresh-timer ()
  (let ((initial-delay 1))
    (setf *modeline-subthreads-timer*
          (run-with-timer initial-delay *modeline-subthreads-timeout*
            #'run-modeline-subthreads-if-not-busy))))

(defun stop-modeline-refresh-timer ()
  (cancel-timer *modeline-subthreads-timer*)
  (setf *modeline-subthreads-timer* nil))

(when nil
  (progn (stop-modeline-refresh-timer)
         (start-modeline-refresh-timer)))
