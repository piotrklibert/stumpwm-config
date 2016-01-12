(in-package :stumpwm-user)

(defun rearrange-windows (win)
  "A little hack to force rearranging windows even after restoring group from a
file. It calls `place-existing-windows` after a slight delay, when all the
windows have been displayed already."
  (run-with-timer 0.3 nil
    #'place-existing-windows))

(add-hook *new-window-hook*
          #'rearrange-windows)


(define-frame-preference "CHROME"
  (1 nil nil :class "google-chrome" :role "browser")
  (0 nil nil :restore "ff_group" :class "google-chrome" :role "pop-up"))

(define-frame-preference "EMACS"
  (1 nil nil :restore "chrome_group" :instance "xmessage")
  (0 nil nil :restore "ediff_group" :instance "Ediff"))

(define-frame-preference "SHELL"
  (1 nil nil :restore "chrome_group" :instance "xmessage"))

(define-frame-preference "prn"
  (1 nil nil :restore "chrome_group" :instance "xmessage"))

(define-frame-preference "FF"
  (0 nil nil :restore "ff_group" :role "selenium-ide")
  (1 nil nil :role "browser")
  (0 nil nil :restore "ff_group" :instance "Devtools"))

;; (frame-number raise lock &key create restore dump-name class instance type role title)
;; (let*
;;     ((gcount (length (screen-groups (current-screen))))
;;      (cur (group-number (current-group)))
;;      (next (+ cur 3)))
;;   next)
;; (setf s (current-screen))
;; (let ((w (prin1-to-string (screen-width s)))
;;       (h (prin1-to-string (screen-height s)))
;;       (layout "~/dump")
;;       (rules "~/rules"))
;;   rules)
;; ;; ...
;; (defvar sq)
;; (setf sd '(("CHROME" 1 T NIL :CLASS "Emacs" :INSTANCE "emacs" :TITLE NIL :ROLE NIL)
;;            ("CHROME" 0 T NIL :CLASS "google-chrome" :INSTANCE "Ediff" :TITLE NIL :ROLE "pop-up")))
;; (restore-from-file "~/dump")
;; (setf *window-placement-rules* sd)
;; (place-existing-windows)

;; > (stumpwm::tile-group-frame-tree (current-group))
;; (#S(frame 0 #S(TILE-WINDOW "Emacs - init.lisp:~/.stumpwm.d/" #x80006E) 0 0 1920 1080))
