(defpackage :my-config
  (:use :cl :stumpwm))

(defpackage :my-utils
  (:use :cl :stumpwm :cl-arrows :split-sequence)
  (:export shell$
           trim-newlines
           control-center
           pop-console
           skype
           rpi
           chrome
           suspend
           define-modeline-thread
           apply0
           remove-plist
           screen-display-string
           swap-groups
           move-group-forward
           move-group-backward
           my-log
           my-log1
           send-line
           send-newline
           run-prog
           run-prog-collect-output
           runner-thread
           run-prog-async))

(defpackage :my-threading
  (:use :cl :lparallel :stumpwm :alexandria :cl-arrows :my-utils)
  (:export run-command-sync
           run-command-async
           run-and-forget
           define-modeline-subthread
           run-subthreads
           run-subthreads-once))

(defpackage :my-modeline
  (:use :cl :stumpwm :alexandria :cl-arrows :my-threading :my-utils))

(defpackage :my-screenshots
  (:use :cl :stumpwm :alexandria :cl-arrows :my-utils)
  )

(in-package :my-config)
