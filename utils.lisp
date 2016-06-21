(in-package :stumpwm-user)
;; (in-package :stumpwm)
;; (export '(send-line))


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


(defmacro shell$ (&rest forms)
  "Execute one or more shell commands. The commands must be literal string, but
it may contain FORMAT special sequences and must be followed with values to
substitute, just like in FORMAT.
\"
Example:
    \(shell$ \"mv '~a' '~a'\" src dest)

    \(shell$ :wait nil
             \"echo a\"
             \"echo b\")"

  (let
      ((commands '())
       wait)
    (when (equal (car forms) :wait)
      (setf wait (cadr forms))
      (pop forms)                       ; we pop both :keyword and value
      (pop forms))

    (loop for f in forms
       if (stringp f)
       do
         (push (list f) commands)
       else do
         (push f (car commands)))

    `(progn
       ,@(loop for command in (reverse commands)
            collect
              `(run-shell-command (format nil ,@(reverse command)) ,wait)))))



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
                       (progn
                         (format t "~s" "here")
                         (format nil "~s" (read out)))
                     (error (x)
                       (run-shell-command cmd t))))
             (sb-ext:process-close process))))))

(defun run-prog-async (cmd output)
  (sb-thread:make-thread #'runner-thread :arguments (list cmd output)))


(defmacro define-modeline-thread (var-name command)
  `(progn
     (when (not (boundp 'modeline-external-threads))
       (defvar modeline-external-threads nil))
     (push ',var-name modeline-external-threads)
     (defvar ,var-name nil)
     (defun ,var-name ()
       (run-prog-async ,command ',var-name))))
