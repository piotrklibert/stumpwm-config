(defpackage :my-windows
  (:use :cl :stumpwm :alexandria :cl-arrows :my-utils)
  (:export narrow-frame
           my-remove
           my-next
           shrink-frame
           make-screen-shot
           increase-step-size
           my-vsplit
           my-prev
           pop-last-screen-shot
           my-hsplit
           decrease-step-size
           widen-frame
           grow-frame))
(in-package :my-windows)


(defparameter *ignored-windows-list* '("Skype" "CopyQ"))

(defun should-stay-hidden-p ()
  (loop
     for w in *ignored-windows-list*
     if (search w (window-title (current-window)))
     return t))


(defcommand my-next () ()
  (next)
  (when (should-stay-hidden-p)
    (my-next)))


(defcommand my-prev () ()
  (prev))

;; ;; very simple Doubly Linked List (abbreviated DLL)
(defstruct DLLNode
  prev next val)

(defun init-steps (my-steps)
  (flet ((step-prev (i)
           (cond ((= i 0) (car (last my-steps)))
                 (t (nth (1- i) my-steps))))
         (step-next (i)
           (cond ((= i (1- (length my-steps))) (car my-steps))
                 (t (nth (1+ i) my-steps)))))
    (loop for i from 0 to (1- (length my-steps))
       do
         (setf (dllnode-prev (nth i my-steps)) (step-prev i))
         (setf (dllnode-next (nth i my-steps)) (step-next i)))
    my-steps))


(defparameter my-steps
  (init-steps (mapcar (lambda (x)  (make-dllnode :val x))
                      '(50 120 200 300 500)))

  "Circular, doubly linked list of possible step sizes. It looks like this when
  initialized:
    (#1=#S(DLLNODE
           :PREV #2=#S(DLLNODE
                       :PREV #3=#S(DLLNODE
                                   :PREV #4=#S(DLLNODE
                                               :PREV #5=#S(DLLNODE
                                                           :PREV #1#
                                                           :NEXT #4#
                                                           :VAL 120)
                                               :NEXT #3#
                                               :VAL 200)
                                   :NEXT #2#
                                   :VAL 300)
                       :NEXT #1#
                       :VAL 500)
           :NEXT #5#
           :VAL 50)
     #5# #4# #3# #2#)")

(defparameter my-step-choice (car my-steps))





(defcommand increase-step-size () ()
  (setf my-step-choice (dllnode-next my-step-choice))
  (message (format nil "Step choice: ~a" (dllnode-val my-step-choice))))

(defcommand decrease-step-size () ()
  (setf my-step-choice (dllnode-prev my-step-choice))
  (message (format nil "Step choice: ~a" (dllnode-val my-step-choice))))



(defun widen/narrow-frame (&key x y)
  (let ((x (or x 0))
        (y (or y 0)))
    (run-commands (format nil "resize ~a ~a" x y))))

(defcommand widen-frame  () ()
  (widen/narrow-frame :x (dllnode-val my-step-choice)))
(defcommand narrow-frame () ()
  (widen/narrow-frame :x (- (dllnode-val my-step-choice))))
(defcommand grow-frame   () ()
  (widen/narrow-frame :y (dllnode-val my-step-choice)))
(defcommand shrink-frame () ()
  (widen/narrow-frame :y (- (dllnode-val my-step-choice))))



(defcommand my-hsplit () ()
  (hsplit)
  (move-focus :right))

(defcommand my-vsplit () ()
  (vsplit)
  (move-focus :down))


(defcommand my-remove () ()
  "Kill the window and close the frame it occupied."
  (handler-case
      (run-commands "delete" "remove")
    (error (x)
      (my-log1 x))))

(defun my-close-and-kill () ()
  (kill-window)
  (remove-split))
