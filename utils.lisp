(in-package :stumpwm-user)

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
  (format nil "DISPLAY=:~d" (xlib:display-display *display*)))

(defun swap-groups (group1 group2)
  (rotatef (slot-value group1 'number) (slot-value group2 'number)))

(defun move-group-forward (&optional (group (current-group)))
  (swap-groups group (stumpwm::next-group group (stumpwm::sort-groups (current-screen)))))

(defun move-group-backward (&optional (group (current-group)))
  (swap-groups group (stumpwm::next-group group (reverse (stumpwm::sort-groups (current-screen))))))


(defcommand control-center () ()
  (run-shell-command "gnome-control-center"))

(defcommand chrome () ()
  (run-shell-command "google-chrome"))

(defcommand suspend () ()
  (run-shell-command "sudo systemctl suspend" t))

(defcommand pop-console () ()
  (run-commands "vsplit" "move-focus down" "resize 0 -175" "exec urxvt"))

(defcommand rpi () ()
  (run-shell-command "urxvt -e ssh pi"))


(defcommand skype () ()
  (if (and (typep (current-window) 'window)
           (search "Skype" (window-title (current-window))))
      (stumpwm::hide-window (current-window))
      (run-or-pull "skypeforlinux" '(:instance "skypeforlinux"
                                     :class "skypeforlinux")
                   t t)))

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


(defmacro shell$ (&rest forms)
  "Execute one or more shell commands. The commands must be literal string, but
it may contain FORMAT special sequences and must be followed with values to
substitute, just like in FORMAT.

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
       for out = (sb-ext:process-output process)
       finally
         (progn
           (setf (symbol-value output) (handler-case
                                           (progn
                                             (my-log1 "here")
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



;; (defun switch-next-window ()
;;   (declare (safety))
;;   (restart-case
;;       (progn
;;         (let
;;             ((win (elt *my-win-list* (1+ switch-windows-counter))))
;;           (focus-window win t)
;;           (incf switch-windows-counter)))
;;     (my-restart (i)
;;       (echo (format nil "====~a" switch-windows-counter))
;;       (setf switch-windows-counter i)
;;       (switch-next-window))))
