(in-package :stumpwm-user)

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
    (apply #'sb-ext:run-program prog args
           :output (if output output t)
           :error t :wait wait :environment env opts)))

(defun run-prog-collect-output (prog &rest args)
  "run a command and read its output."
  (with-output-to-string (s)
    (run-prog prog :args args :output s :wait t)))

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
         (let
             ((out (sb-ext:process-output process)))
           (setf (symbol-value output) (handler-case
                                           (progn
                                             (format nil "~s" (read out)))
                                         (error (x)
                                           (run-shell-command cmd t))))
           (sb-ext:process-close process)))))

(defun run-prog-async (cmd output)
  (sb-thread:make-thread #'runner-thread :arguments (list cmd output)))


(defmacro define-modeline-thread (var-name command)
  `(progn
     (when (not (boundp 'modeline-external-threads))
       (defvar *modeline-external-threads* nil))
     (push ',var-name *modeline-external-threads*)
     (defvar ,var-name nil)
     (defun ,var-name ()
       (run-prog-async ,command ',var-name))))

(defun some-thread-still-runs-p (threads)
  "This HAS to be possible to write much more elegantly..."
  (not (= 0 (count-if #'identity (mapcar #'sb-thread:thread-alive-p threads)))))
