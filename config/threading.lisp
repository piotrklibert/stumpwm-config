(defpackage :my-threading
  (:use :cl :lparallel :stumpwm :alexandria :cl-arrows :my-utils)
  (:export run-command-sync
           run-command-async
           run-and-forget
           define-modeline-subthread
           run-subthreads
           run-subthreads-once
           shell$))
(in-package :my-threading)


(defun env-replace-var (env name new-val)
  (let* ((name             (concatenate 'string name "="))
         (new-var          (concatenate 'string name new-val))
         (starts-with-name (curry #'starts-with-subseq name))
         (env              (remove-if starts-with-name env)))
    (cons new-var env)))


(defun get-environ ()
  (let* ((disp-num (xlib:display-display *display*))
        (display-id (format nil ":~a" disp-num)))
    (-> (sb-ext:posix-environ)
        (env-replace-var "DISPLAY" display-id))))


(defun run-command-sync (cmd)
  (let*
      ((env (get-environ))
       (shell-args (list "-c" cmd)))
    (with-output-to-string (out-stream)
      (sb-ext:run-program *shell-program* shell-args
                          :output out-stream
                          :error t :wait t
                          :environment env))))


(defun run-command-async (cmd)
  (let
      ((ch (make-channel)))
    (lparallel:submit-task ch #'run-command-sync cmd)
    ch))

(defun run-and-forget (cmd)
  (let ((env (get-environ))
        (shell-args (list "-c" cmd)))
    (sb-ext:run-program *shell-program* shell-args :wait nil :environment env)))


(defun shell$ (&rest commands)
  "Execute one or more shell commands. The commands must be literal string.
   Example:
      \(shell$ :wait t
               \"echo a\"
               \"echo b\")"
  (let (wait)
    (when (equalp (car commands) :wait)
      (setf wait t)
      (setf commands (cddr commands)))
    (let
        ((run-func (if wait #'run-command-sync #'run-and-forget)))
      (loop for command in commands
         do (funcall run-func command)))))
