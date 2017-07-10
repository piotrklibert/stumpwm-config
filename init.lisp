(asdf:initialize-source-registry
 '(:source-registry
   (:directory "/home/cji/.stumpwm.d/config")
   :inherit-configuration))

(ql:quickload "cl-arrows")
(ql:quickload "split-sequence")
(ql:quickload "xembed")
(ql:quickload "alexandria")
(ql:quickload "clx-truetype")
(ql:quickload "lparallel")
(ql:quickload "optima")

(use-package '(:optima :lparallel :alexandria :cl-arrows))

(setf lparallel:*kernel* (lparallel:make-kernel 8))

(set-module-dir "/home/cji/portless/stumpwm/contrib")
(load-module "ttf-fonts")

(set-prefix-key (kbd "C-z"))
(setf *input-window-gravity*     :center)
(setf *message-window-gravity*   :top-right)
(setf *mouse-focus-policy*       :click)
(setf *print-circle*             t)
(setf *timeout-wait*             7)
(setf *run-or-raise-all-screens* t)
(setf *debug-level*              0)     ; could be 0..100; 0 means no logging
(setf *debug-stream*             (open "/home/cji/.stumpwm.d/stump.log"
                                       :direction :output
                                       :if-exists :append
                                       :if-does-not-exist :create))
;; Enable SLIME/Emacs integration
(load "/home/cji/.stumpwm.d/swank.lisp")

(asdf:load-system "stumpwm-config")

(setf *startup-message* (format nil "Welcome to ~a!" (machine-instance)))
(setf *window-format* "%50t")

(use-package :my-utils)
(load "/home/cji/.stumpwm.d/utils.lisp")  ; TODO: refactor!

(shell$ :wait t
 ; generate necessary font files if needed
 "sudo mkfontscale /usr/share/fonts/bitstream-vera/"
 "sudo mkfontdir /usr/share/fonts/bitstream-vera/"
 ; make sure Xorg can find the font
 "xset fp+ /usr/share/fonts/bitstream-vera/"
 "xset fp rehash"
 ; set desktop background and default cursor shape
 "feh --bg-scale yuno.jpg"
 "xsetroot -cursor_name arrow"
 ; activate custom key mappings (eg. right small button to XF86Back)
 "xbindkeys"
 "xmodmap ~~/xmodmap.conf")


(ignore-errors
  (set-font (make-instance 'clx-truetype:font
                           :family "Bitstream Vera Sans Mono"
                           :subfamily "Roman" :size 14
                           :antialiased t)))

(add-hook *start-hook*
          #'(lambda ()
              ;; (shell$ "skypeforlinux" "copyq" "emacs" "google-chrome" "urxvt")
              ))


;; Create the three usual workspaces.
(add-hook *start-hook* #'(lambda () (run-commands "grename CHROME"
                                                  "gnewbg EMACS"
                                                  "gnewbg SHELL")))

;; Most `define-key's are grouped here.
(load "/home/cji/.stumpwm.d/keybindings.lisp")

;; Screenshots - C-` to shot and C-M-` to edit
(load "/home/cji/.stumpwm.d/screenshots.lisp")

;; Splitting and resizing windows interactively
(load "/home/cji/.stumpwm.d/windows.lisp")

;; Groups creation and switching
(load "/home/cji/.stumpwm.d/groups.lisp")

;; Modeline widgets definitions
(load "/home/cji/.stumpwm.d/modeline.lisp")
