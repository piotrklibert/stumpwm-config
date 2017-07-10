;; -*- mode: lisp -*-


(defsystem "stumpwm-config"
  :description ""
  :version "0.0.1"
  :author "cji"
  :licence "Public Domain"
  :depends-on ("stumpwm")
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "threading" :depends-on ("package" "utils"))
   ;; (:file "modeline" :depends-on ("package" "threading" "utils"))
   ;; (:file "screenshots" :depends-on ("package" "utils"))
   ))
