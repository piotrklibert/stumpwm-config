;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load SWANK.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "/home/cji/.emacs.d/forked-plugins/slime/swank-loader.lisp")
(swank-loader:init)
(defcommand swank () ()
            (swank:create-server
             :port 4005
             :style swank:*communication-style*
             :dont-close t)
            (echo-string (current-screen)
                         (concat "Starting swank. M-x slime-connect RET RET, "
                                 "then (in-package :stumpwm-user).")))
(ignore-errors
  (swank))
