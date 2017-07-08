;; Load SWANK from .emacs.d/forked-plugins/ instead of other location
(load "/home/cji/.emacs.d/forked-plugins/slime/swank-loader.lisp")

(swank-loader:init)
(swank:create-server :port 4005 :dont-close t
                     :style swank:*communication-style*)
