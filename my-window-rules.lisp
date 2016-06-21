;; TODO:
;; Finish rewriting the code.
;;
(in-package :stumpwm-user)

(defun rearrange-windows (win)
  "A little hack to force rearranging windows even after restoring group from a
file. It calls `place-existing-windows` after a slight delay, when all the
windows have been displayed already."
  (run-with-timer 0.3 nil
    #'place-existing-windows))

(add-hook *new-window-hook*
          #'rearrange-windows)
