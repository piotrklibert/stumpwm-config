;; -*- mode: lisp -*-


(defsystem "stumpwm-config"
  :description ""
  :version "0.0.2"
  :author "cji"
  :licence "Public Domain"
  :depends-on ("stumpwm")
  :components
  ((:file "utils")
   (:file "threading"       :depends-on ("utils"))
   (:file "groups"          :depends-on ("utils"))     ; Group (iow. desktops) creation and switching
   (:file "keybindings")                               ; Most `define-key's are grouped here.
   (:file "modeline"        :depends-on ("threading")) ; Modeline widgets definitions
   (:file "modeline-format" :depends-on ("modeline"))  ; Modeline widgets definitions
   (:file "screenshots"     :depends-on ("utils"))     ; Screenshots - C-` to shot and C-M-` to edit
   (:file "windows"         :depends-on ("utils"))))   ; Splitting and resizing windows interactively
