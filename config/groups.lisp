(defpackage :my-groups
  (:use :cl :stumpwm :stumpwm-user)
  (:export gprevrow
           my-gnew
           move-group-forward-cmd
           move-group-backward-cmd
           gnextrow))
(in-package :my-groups)


(defun random-name (length)
  (with-output-to-string (stream)
    (let ((*print-base* 36))
      (loop repeat length
         do (princ (random 36) stream)))))

(defun sorted-groups ()
  (let ((groups (screen-groups (current-screen))))
    (sort groups #'< :key #'group-number)))

(defun swap-groups (group1 group2)
  (rotatef (slot-value group1 'number) (slot-value group2 'number)))

(defun move-group-forward (&optional (group (current-group)))
  (swap-groups group (stumpwm::next-group group (stumpwm::sort-groups (current-screen)))))

(defun move-group-backward (&optional (group (current-group)))
  (swap-groups group (stumpwm::next-group group (reverse (stumpwm::sort-groups (current-screen))))))

(defcommand move-group-backward-cmd () ()
  (move-group-backward))


(defcommand move-group-forward-cmd () ()
  (move-group-forward))


(defcommand my-gnew (&optional name) ((:string "Group name? "))
  "Like normal gnew, but also moves current window to the new group. If `name'
is not provided a random string is used."
  (let*
      ((prev (current-group))
       (name (if (or (not name)
                     (= 0 (length name)))
                 (random-name 7)
                 name))
       (new (gnew name)))
    (gselect prev)
    (gmove new)
    (gselect new)))


;; ;; WORK IN PROGRESS - GROUP GRID INSTEAD OF GROUP LISP
;; (defcommand gnextrow () ()
;;   "Go to second next group from the current one or to the last group if second
;; to next doesn't exist."
;;   (let*
;;       ((group-nums (sorted-groups))
;;        (cur-num (group-number (current-group)))
;;        (current-pos (group-number
;;                      (find cur-num group-nums :key #'group-number))))
;;     (unless (gselect (nth (+ 1 current-pos)
;;                           group-nums))
;;       (gselect (car (last group-nums))))))


;; (defcommand gprevrow () ()
;;   "Go to a group before the previous from the current one or to the first group
;; if it doesn't exist."
;;   (let*
;;       ((group-nums (sorted-groups))
;;        (cur-num (group-number (current-group)))
;;        (current-pos (group-number
;;                      (find cur-num group-nums
;;                            :key #'group-number))))
;;     (handler-case
;;         (gselect (nth (- current-pos 3) group-nums))
;;       (error (x)
;;         (gselect (car group-nums))))))
