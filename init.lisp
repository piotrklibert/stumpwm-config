(in-package :stumpwm-user)

(asdf:initialize-source-registry
 '(:source-registry
   (:directory "/home/cji/.stumpwm.d/config")
   :inherit-configuration))

(ql:quickload "cl-arrows")
(ql:quickload "cl-fad")
(ql:quickload "split-sequence")
(ql:quickload "xembed")
(ql:quickload "alexandria")
(ql:quickload "clx-truetype")
(ql:quickload "lparallel")
(ql:quickload "optima")
(ql:quickload "local-package-aliases")


(set-module-dir "/home/cji/portless/stumpwm/contrib")
(load-module "ttf-fonts")
(load-module "stumptray")
(load-module "cpu")

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
(setf *startup-message*          (format nil "Welcome to ~a!" (machine-instance)))
(setf *window-format*            "%50t")

(setf lparallel:*kernel* (lparallel:make-kernel 3))

;; Enable SLIME/Emacs integration
(load "/home/cji/.stumpwm.d/swank.lisp")

(local-package-aliases:set-aliasing-reader *readtable*)
(funcall (read-from-string "local-package-aliases:hook-into-swank"))

;; ;; =============================================================================
;; ;; Load the rest of my configuration

(asdf:load-system "stumpwm-config")

(defpackage :my-init
  (:documentation
   "We're defining this module so that we don't have to use `use-package',
   which may cause conflicts upon reloading.")
  (:use :cl :stumpwm :clx-truetype
        :optima :alexandria :cl-arrows
        :lparallel :my-threading :my-utils))

(in-package :my-init)

(defcommand q () ()
  (quit))

(defcommand reboot () ()
  (shell$ "sudo reboot"))

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
 ; activate custom key mappings (eg. right small mouse button to XF86Back)
 "xbindkeys")


(ignore-errors
  (set-font (make-instance 'clx-truetype:font
                           :family "Bitstream Vera Sans Mono"
                           :subfamily "Roman" :size 14
                           :antialiased t)))



(defun start-basic-apps ()
  (let ((fname #P"~/.selfspy/selfspy.pid.lock"))
    (if (cl-fad:file-exists-p fname)
        (delete-file fname)))

  (shell$ "skypeforlinux"
          "copyq"
          "emacs"
          "google-chrome"
          "xmodmap ~/xmodmap.conf"
          "urxvt"
          ;; TODO: remove the password from here, prompt for it instead!!!
          "selfspy -p'likeblackholesinthesky'"
          ))

(add-hook *start-hook* #'start-basic-apps)


(defun create-default-groups ()
  (run-commands "grename CHROME"
                "gnewbg EMACS"
                "gnewbg SHELL"))

(add-hook *start-hook* #'create-default-groups)

(defun move-windows-to-their-groups (&optional follow)
  (labels ((move-it (app &optional (group app))
             (ignore-errors             ; find-window and/or find-group may return nil
               (move-window-to-group (find-window app)
                                     (find-group group))))
           (move-them (&rest args)
             (loop for arg in args
                do (if (listp arg)
                       (apply #'move-it arg)
                       (funcall #'move-it arg)))))
    (move-them "emacs" "chrome" '("urxvt" "shell")))
  (when follow
    (run-with-timer 3 nil
      (lambda ()
        ;; why isn't it exported?
        (stumpwm::switch-to-group (find-group "emacs"))))))

(stumpwm:run-with-timer 3 nil #'move-windows-to-their-groups t)


;; ;; (funcall #'move-windows-to-their-groups t)

;; ;; (labels ((move-it (app &optional (group app))
;; ;;            (move-window-to-group (find-window app)
;; ;;                                  (find-group group)))
;; ;;          (move-them (&rest args)
;; ;;            (loop for arg in args
;; ;;               do (if (consp arg)
;; ;;                      (move-it (car arg) (cdr arg))
;; ;;                      (move-it arg)))))

;; ;;   (move-them "emacs" "chrome" '("urxvt" .  "shell")))
