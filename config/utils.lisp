(in-package :my-utils)

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
        ((run-func (if wait
                       #'my-threading:run-command-sync
                       #'my-threading:run-and-forget)))
      (loop for command in commands
         do (funcall run-func (format nil command))))))



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
