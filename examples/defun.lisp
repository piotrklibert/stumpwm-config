(in-package :stumpwm)
(defun lock-rule-by-class (class)
  (list 0 T T :class class))
(defun lock-rule-by-title (title)
  (list 0 T T :title title))

(defcommand restart-xwatchsystem () ()
  "Kill old xwatchsystem instances"
  (run-shell-command "ps auxwww | egrep ' -title XWatchSystem ' | sed -e 's/\\s\\+/ /g' | cut -f 2 -d' ' | xargs kill " T)
  (run-shell-command "xwatchsystem null")
  )

(defcommand set-as-modeline (&key (window nil) (size nil)) ()
  "Set (possibly current) window as a modeline"
  (dformat 8 "Setting modeline..~%")
  (let* 
    ((win (or window (current-window)))
     (scr (window-screen win))
     (h (car (screen-heads scr)))
     (xwin (window-xwin win))
     )
    (dformat 8 "Withdrawing window ~s (~s) for modeline~%" win xwin)
    (withdraw-window win)
    (dformat 8 "Withdrawn window ~s (~s) for modeline~%" win xwin)
    (dformat 8 "Forgetting old modeline~%")
    (setf (head-mode-line h) nil)
    (dformat 8 "Adjusting modeline position - height ~s~%" size)
    (when size
      (setf (xlib:drawable-height xwin) size)
      )
    (dformat 8 "Remapping the modeline window~%")
    (place-mode-line-window scr xwin)
    ;(dformat 8 "Setting modeline window~%")
    ;(let 
    ;  ((ml (head-mode-line h)))
    ;  (set-mode-line-window ml xwin)
    ;  )
    (when (equal *mode-line-position* :bottom)
      (let* ((ml (head-mode-line h))
	     (xw (mode-line-window ml))
	     )
	(setf (mode-line-position ml) :bottom)
	(setf (xlib:drawable-y xw) (- (head-height h) (xlib:drawable-height xw)))
	(sync-mode-line ml)
	)
      )
    ))

(defcommand my-mode-line (&key (window nil)) ()
	    "Set (possibly current) window as modeline window with my special settings"
  (setf *mode-line-position* :bottom)
  (set-as-modeline :window window :size *min-frame-height*)
  )

(defun should-be-raised (window)
  (and
    (if 
      (equal (window-class window) "Carrier") 
      (and
	(equal (subseq (window-title window) 0 3) "(*)")
	)
      t)
    (if 
      (equal (window-class window) "Pidgin") 
      (and
	(equal (subseq (window-title window) 0 3) "(*)")
	)
      t)
    (if 
      (equal (window-class window) "psi") 
      (and
	(equal (subseq (window-title window) 0 2) "* ")
	)
      t)
    ))

(defun renumber-window (w n)
  (when w
    (select-window-by-number (window-number w))
    (renumber n)))

(defun local-matching-window (&rest args) ;(&key class instance type role title)
  (find-if 
    (lambda (w) 
      (apply 'window-matches-properties-p (cons w args)))
    (group-windows (screen-current-group (current-screen)))))

(defun global-matching-window (&rest args) ;(&key class instance type role title)
  (find-if 
    (lambda (w) 
      (apply 'window-matches-properties-p (cons w args)))
    (screen-windows (current-screen))))

(defcommand net-window-sort () ()
	    "Place networking-related windows in my preferred order"
  (renumber-window 
    (local-matching-window :class "Carrier" :role "buddy_list")
    1)
  (renumber-window 
    (local-matching-window :class "Pidgin" :role "buddy_list")
    1)
  (renumber-window 
    (local-matching-window :class "Carrier" :role "conversation")
    2)
  (renumber-window 
    (local-matching-window :class "Pidgin" :role "conversation")
    2)
  (renumber-window
    (local-matching-window :class "Thunderbird-bin")
    3)
  (renumber-window
    (local-matching-window :instance "Navigator")
    4))

(defcommand globally-previous () ()
	    "Switch to the previous window (possibly from another group) that had focus"
	    (let* 
	      ((window *globally-previous*)
	       (group (window-group window))
	       (frame (window-frame window)))
	      (gselect group)
	      (focus-frame group frame)
	      (focus-window window)))

(defcommand globally-previous-wt () ()
	    "Switch to the previous window (possibly from another group) that had focus"
	    (let* 
	      ((window *globally-previous*)
	       (cw *globally-current*)
	       (group (window-group window))
	       (cg (current-group))
	       (frame (window-frame window)))
	      (unless (eq group cg)
		(pull-w window))
	      (focus-frame cg frame)
	      (focus-window window)
	      (really-raise-window window)
	      (setf *globally-previous* cw *globally-current* window)))

(defcommand hibernate-pc () ()
	    "Execute suspend-to-disk"
  (fclear)
  (run-shell-command "susp"))

(defcommand wifi () ()
	    "Connect to WiFi network in a known place"
	    (run-shell-command "auto-wifi"))

(defcommand restart-thunderbird () ()
	    "Restart Thunderbird"
	    (run-shell-command "pkill thunderbird" T)
	    (run-shell-command "thunderbird"))

(defcommand cleanup-window () ()
	    "Kill current window that actually got destroyed long ago"
	    (destroy-window (current-window)))

(defun numbered-tag (n)
  (if (= n 0) "@"
    (concatenate 'string (numbered-tag (truncate (/ (- n 1) 36)))
		 (let ((x (mod (- n 1) 36)))
		   (subseq "1234567890qwertyuiopasdfghjklzxcvbnm" x (+ x 1))))))

(defcommand short-tags () ()
	    "Create short tags for quick pulls"
	    (let*
	      ((wins (screen-windows (current-screen)))
	       (counter 0)
	       )
	      (mapcar 
		(lambda(y) (windowtags::clear-tags-if 
			     (lambda(x) (equal (subseq x 0 1) "@")) y))
		wins)
	      (mapcar
		(lambda(x) 
		  (setf counter (+ counter 1))
		  (tag-window (numbered-tag counter) x))
		wins)
	      ))

(defcommand raise-short-tag (argtag) ((:rest "Short tag to pull: "))
	    "Make window current by short tag"
	    (or
	      (raise-tag (concatenate 'string "@" argtag))
	      (raise-tag argtag)))

(defcommand default-tags () ()
	    "Add default tags to all windows"
	    (mapcar
	      (lambda(x)
		(let*
		  (
		   (old-tags (window-tags x))
		   (old-tags 
		     (if
		       (find "no-auto-tags" old-tags :test 'equal)
		       old-tags
		       (remove-if 
			 (lambda (x) (cl-ppcre:scan "^CAT/" x)) 
			 old-tags)))
		   (deftags (deftags x))
		   (new-tags (union-mild old-tags deftags))
		   (zone (defzone x new-tags))
		   (new-tags (cons zone new-tags))
		   (new-tags (remove nil new-tags))
		   (new-tags (remove "nil" new-tags :test 'equalp))
		   )
		  (setf 
		    (window-tags x)
		    new-tags
		    )
		  )
		)
	      (screen-windows (current-screen))
	      )
	    (short-tags)
	    )

(defun window-alive (win)
  (let
    ((marker (random 255)))
    (xlib:change-property (window-xwin win)
			  :STUMPWM_CHECK_IF_ALIVE
			  (list marker)
			  :UINT 8)
    (equal (list marker) (xlib:get-property (window-xwin win)
				 :STUMPWM_CHECK_IF_ALIVE))))

(defcommand dead-windows-cleanup () ()
	    "Kill the windows that mysteriously disappeared"
	    (mapcar
	      (lambda(x) 
		(if (not (window-alive x)) 
		  (progn 
		    (move-window-to-group x (current-group))
		    (fclear)
		    (really-raise-window x)
		    (destroy-window x))))
	      (screen-windows (current-screen))))

(defcommand reload-defuns () ()
	    "Only load definitions of functions from rc"
	    (load-rcpart "deftags")
	    (load-rcpart "defun")
	    )

(defcommand reload-defhooks () ()
	    "Only load definitions of hooks from rc"
	    (load-rcpart "defhook"))

(defcommand reload-defkeys () ()
	    "Only load key bindings from rc"
	    (load-rcpart "defkey"))

(defcommand reload-setvars () ()
	    "Only load variable values from rc"
    (load-rcpart-old "defpass")
    (load-rcpart "defparam")
    (load-rcpart "setvar")
    )

(defcommand pull+push+renumber (argtags) ((:rest "Tags to select: "))
  "Select windows by tags and renumber them"
  (gselect (find-group (current-screen) "Default"))
  (only)
  (fclear)
  (let ((visible-window (car (reverse (select-by-tags argtags)))))
       (if visible-window (move-window-to-group visible-window (current-group)))
       (pull+push argtags)
       (number-by-tags)
       (if visible-window 
           (setf (group-windows (current-group))
                 (cons visible-window (remove visible-window (group-windows (current-group)))))))
  (if (and (not (current-window)) (group-windows (current-group))) 
      (pull-hidden-next)))

(defcommand scrollable-window-tag-list () ()
	    "Show windows and their tags in a terminal"
	    (run-shell-command "urxvt -e sh -c 'echo all-tags | TERM=rxvt ~/script/external/stumpish | less'"))

(defun resize-local-frame-to (group frame x y)
  (if x (progn
         (resize-frame group frame -999999 :width) 
         (resize-frame group frame (- x *min-frame-width*) :width)
         ))
  (if y (progn 
         (resize-frame group frame -999999 :height) 
         (resize-frame group frame (- y *min-frame-height*) :height)
         )))

(defcommand lazarus-layout () ()
  "Load my Lazarus layout"
  (ftg-only)
  (ftg-set-tags "lazarus")
  (let* (
         (group (current-group)) 
         (frame (tile-group-current-frame group))
         (header-number (frame-number frame))
         (inspector-number (split-frame group :row))
         (dummy (fselect (frame-by-number group inspector-number)))
         (form-number (split-frame group :column))
         (dummy (fselect (frame-by-number group form-number)))
         (messages-number (split-frame group :row))
         (header (frame-by-number group header-number))
         (inspector (frame-by-number group inspector-number))
         (form (frame-by-number group form-number))
         (messages (frame-by-number group messages-number))
         )
        (resize-local-frame-to group header nil 100)
        (resize-local-frame-to group inspector 210 nil)
        (resize-local-frame-to group messages nil 90)
        (mapcar (lambda (w) (pull-window w form)) (select-by-tags "lazarus"))
        (mapcar (lambda (w) (pull-window w header)) (select-by-tags "lazarus-ide-window"))
        (mapcar (lambda (w) (pull-window w inspector)) (select-by-tags "lazarus-inspector-window"))
        (mapcar (lambda (w) (pull-window w messages)) (select-by-tags "lazarus-message-window"))
        (focus-frame group form)
        ))

(defcommand dia-layout () ()
  "Load my Dia layout"
  (ftg-set-tags "dia")
  (ftg-only)
  (let* (
         (group (current-group))
         (fn (find-free-frame-number group))
         (dummy (hsplit))
         (f1 (tile-group-current-frame group))
         (f2 (frame-by-number group fn))
	 (ftg (frame-tagged-group f1))
         )
        (tag-frame "dia-main" f1)
        (tag-frame "dia-toolbar" f2)
        (resize -999999 0)
        (resize (- 160 *min-frame-width*) 0)
	(act-on-matching-windows 
	  (w :group)
	  (in-frame-tg-p w ftg)
	  (pull-window w f2))
        (mapcar (lambda(w) (pull-window w f1)) (select-by-tags "dia-toolbar"))
        (focus-frame group f2)))

(defcommand gimp-layout () ()
  "Load my Gimp layout"
  (pull+push+renumber "gimp")
  (let* (
         (group (current-group))
         (f2 (frame-by-number group (split-frame group :column)))
         (f1 (tile-group-current-frame group))
         )
        (resize-local-frame-to group f1 230 nil)
        (mapcar (lambda(w) (pull-window w f2)) (group-windows (current-group)))
        (mapcar (lambda(w) (pull-window w f1)) (select-by-tags "gimp-toolbar"))
        (focus-frame group f2)))

(defcommand xrandr (state) ((:rest "Desired XRandr state: "))
  "Switch xrandr state"
  (if (equal state "on")
    (run-shell-command "xrandr --output VGA1 --right-of LVDS1 --preferred"))
  (if (equal state "off")
    (run-shell-command "xrandr --output VGA1 --right-of LVDS1 --off")))

(defcommand irc-password () ()
  (loop for x in `( "DEL" "DEL" "DEL" "DEL" "DEL" "DEL"
		    "i" "d" "e" "n" "t" "i" "f" "y" 
                        "space" 
                        ,@(map 'list 'string *irc-pass*)
                        "Return" "C-Return" "C-w")
        do (meta (kbd x))))

(defcommand kill-freenode-from-self () ()
  (mapcar 'delete-window 
          (remove-if (lambda (x) (not (equalp (window-title x) "MichaelRaskin - Gajim")))
                     (group-windows (current-group))))
  (mapcar 'delete-window 
          (remove-if (lambda (x) (not (starts-with (window-title x) "nickserv!")))
                     (group-windows (current-group))))
  )

(defcommand force-redisplay () ()
  "Like redisplay, only resizing to 1x1"
  (let ((window (current-window)))
       (set-window-geometry 
        window 
        :width (truncate (/ (window-width window) 2))
        :height (truncate (/ (window-height window) 2)))
       (xlib:display-finish-output *display*)
       (sleep 0.1)
       (redisplay)))

(defcommand show-im-status () ()
  (let*
   ((im-windows (select-by-tags "im"))
    (im-titles (mapcar 'window-title im-windows))
    )
   (restore-psi-windows)
   (message "IM windows:~%~{~%~a~}" im-titles)
   ))

(defcommand unread-folders-thunderbird () ()
  (meta (kbd "M-v"))
  (sleep 0.1)
  (meta (kbd "f"))
  (sleep 0.1)
  (meta (kbd "u")))

(defcommand all-folders-thunderbird () ()
  (meta (kbd "M-v"))
  (sleep 0.1)
  (meta (kbd "f"))
  (sleep 0.1)
  (meta (kbd "a")))

(defcommand create-windows-only-here () ()
  (setf *new-window-preferred-frame* 
        (constantly
         (tile-group-current-frame (current-group)))))
(defcommand create-windows-focused () ()
  (setf *new-window-preferred-frame* '(:focused)))

(defcommand restore-psi-windows () ()
  (loop for x in (screen-withdrawn-windows (current-screen))
        when (cl-ppcre:scan "[*] " (window-title x))
        do (restore-window x)))

(defcommand kill-all-here () ()
	    "Kill all windows in current group"
  (loop for w in (group-windows (current-group))
        do (delete-window w)))

(defcommand kill-all-ftg () ()
	    "Kill all windows in current frame-tagged-group"
	    (act-on-matching-windows
	      (w :group)
	      (in-current-ftg-p w)
	      (delete-window w)))

(defun merge-frame (from to)
  (when to
    (act-on-matching-windows (w from) t (pull-window w to))))

(defcommand 
  (move-windows-dir tile-group) (dir) ((:direction "Direction: "))
  "Move all windows from this frame to frame num"
  (merge-frame (tile-group-current-frame (current-group))
	       (neighbour dir (tile-group-current-frame (current-group))
			  (group-frames (current-group)))))
(defcommand 
  (move-windows-num tile-group) (num) ((:number "Number: "))
  (merge-frame (tile-group-current-frame (current-group))
	       (frame-by-number (current-group) num)))

(defcommand 
  (move-windows-tag tile-group) (tag) ((:rest "Tag: "))
  "Move all windows to a frame tagged tag"
  (merge-frame (tile-group-current-frame (current-group))
	       (frame-by-number (current-group) (first-frame-by-tag tag))))

(defcommand
  frame-push-pull-tags (argtags) ((:rest "Tags: "))
  "Replace contents of current frame with windows selected by tags argtags"
  (let*
    ((tag (if (stringp argtags) 
	    (remove "" (cl-ppcre:split " " (string-upcase argtags))
		    :test 'equalp)
	    (mapcar 'string-upcase argtags))))
    (act-on-matching-windows 
      (w :frame) (not (tagged-any-p w tag))
      (push-w w))
    (act-on-matching-windows 
      (w :screen) (tagged-any-p w tag)
      (pull-w w) (pull-window w (tile-group-current-frame (current-group))))))

(defcommand 
  load-rcp (name) ((:rest "Part: "))
  "Load-rcpart wrapper"
  (load-rcpart name))

(defcommand
  reference-frame () ()
  "Create a reference frame that can house an URxvt of 80 symbols (820 px)"
  (let*
    ((group (current-group))
     (old-frame (tile-group-current-frame group))
     (ref-number (split-frame group :column 
			      (- (frame-width old-frame) 750)))
     (ref (frame-by-number group ref-number)))
    (focus-frame group ref)
    (set-ftg "ref")
    (focus-frame group (frame-by-number group (frame-number old-frame)))))

(defcommand 
  ratcenter () ()
  "Center the mouse pointer in current frame"
  (let*
    ((f (tile-group-current-frame (current-group)))
     (ml (head-mode-line (current-head (current-group))))
     (ml-height (if ml (mode-line-height ml) 0))
     (cx (+ (frame-x f) (ash (frame-width f) -1)))
     (cy (+ ml-height (frame-y f) (ash (frame-height f) -1))))
    (ratwarp cx cy)))

(defcommand 
  ratratio (rx ry) ((:string "X ratio: ") (:string "Y ratio: "))
  "Center the mouse pointer in current frame"
  (let*
    ((f (tile-group-current-frame (current-group)))
     (ml (head-mode-line (current-head (current-group))))
     (ml-height (if ml (mode-line-height ml) 0))
     (rx (format nil "~a" rx))
     (ry (format nil "~a" ry))
     (rx (let ((*read-eval* nil)) (read-from-string rx)))
     (ry (let ((*read-eval* nil)) (read-from-string ry)))
     (rx (if (numberp rx) rx 1))
     (ry (if (numberp ry) ry 1))
     (cx (+ (frame-x f) (truncate (* (frame-width f) rx))))
     (cy (+ ml-height (frame-y f) (truncate (* (frame-height f) ry)))))
    (ratwarp cx cy)))

(defcommand 
  ratclick-xmacroplay (button) ((:rest "Button: "))
  "Use XMacroPlay to send a click"
    (run-shell-command
      (format
	nil 
	"echo -e 'ButtonPress ~a\\nButtonRelease ~a' | xmacroplay $DISPLAY"
	button button)))

(defcommand withdraw-current-window () ()
	    "Withdraw current window when it is already ghost"
	    (withdraw-window (current-window)))

(defun send-keys-to-tagged (tag keys)
  (raise-short-tag tag)
  (sleep 0.1)
  (loop for x in keys do
	(meta (kbd x)))
  (globally-previous-wt))

(defcommand untag-window (argtag &optional (argwin nil)) ((:rest "Tags to remove: ") :rest)
	    "Remove tags from current window"
	    (let*
	      ((win (or argwin (current-window)))
	       (tag (string-split-by-spaces argtag))
	       (tag (mapcar 'string-upcase tag)))
	      (setf (window-tags win) (set-difference (window-tags win) tag :test 'equalp))))

(defcommand 
  vacuum-offline () ()
  "Find Vacuum window and tell it to become offline"
  (focus-frame-by-tag "TG/REF")
  (when
    (act-on-matching-windows
      (w :screen)
      (and
	(classed-p w "Vacuum")
	(resed-p w "vacuum")
	(roled-p w "MainWindow")
	)
      (pull-w w)
      (pull-window w)
      t
      )
    (ratratio 1 1)
    (ratrelwarp -10 -10)
    (ratclick-xmacroplay 1)
    )
  )

(defun resize-breaking-news (visible)
  "Resize the breaking news frame"
  (let*
    (
     (bn-frame (first-frame-by-tag "TG/BREAKING-NEWS"))
     (group (current-group))
     (tree (tile-group-frame-tree group))
     (parent (tree-parent tree bn-frame))
     (m-frame (prev-sibling parent bn-frame))
     (h (frame-height bn-frame))
     (amount 
       (if visible
	 (- 30 h)
	 (- 3 h)
	 )
       )
     )
    (expand-tree bn-frame amount :top)
    (expand-tree m-frame (- amount) :bottom)
    (tree-iterate bn-frame (lambda (leaf) (sync-frame-windows group leaf)))
    (tree-iterate m-frame (lambda (leaf) (sync-frame-windows group leaf)))
    nil
    )
  )

(defcommand 
  heavy-browser-group () ()
  "Go to heavy browser group and launch a firefox if none are running"
  (ftg-set-tags "HEAVY-BROWSER")
  (unless
    (ftg-windows)
    (run-shell-command "firefox")
    )
  )

(defcommand 
  light-browser-group () ()
  "Go to light browser group and launch a uzbl if none are running"
  (ftg-set-tags "LIGHT-BROWSER")
  (unless
    (ftg-windows)
    (run-shell-command "uzbl")
    )
  )

(defcommand
  editor-group () ()
  "Go to editor group and launch a Unicode GVim if none are running"
  (ftg-set-tags "EDITOR")
  (unless
    (ftg-windows)
    (run-shell-command "ugvim")
    )
  )

(defcommand
  local-shell-group () ()
  "Go to local shell group and launch a default screen session if none are running"
  (ftg-set-tags "SH")
  (unless
    (ftg-windows)
    (run-shell-command "urxvt -e my-screen")
    )
  )

(defcommand
  root-shell-group () ()
  "Go to root shell group and launch a default screen session if none are running"
  (ftg-set-tags "root")
  (unless
    (ftg-windows)
    (run-shell-command "urxvt -e start-su")
    )
  )

(defcommand
  media-group () ()
  "Go to root shell group and launch a default screen session if none are running"
  (ftg-set-tags "media")
  (unless
    (ftg-windows)
    (run-shell-command "urxvt -e media-screen")
    )
  )

(defcommand
  all-tags-grouped () ()
  "Show all windows with their tags in an order based on cat/ tags"
  (let*
    (
     (tag-data 
       (act-on-matching-windows
	 (x :screen)
	 t 
	 (list
	   (first
	     (sort 
	       (or 
		 (remove-if 
		   (lambda (x) (not (cl-ppcre:scan "^CAT/" x)))
		   (window-tags x)
		   )
		 '("CAT/"))
	       'string<))
	   (string-trim 
	     " "
	     (subseq 
	       (concatenate 
		 'string 
		 (window-title x)
		 (make-string 80 :initial-element #\Space)
		 )
	       0 80))
	   (window-class x)
	   (window-res x)
	   (window-role x)
	   (sort (WINDOW-tags x) 'string<)
	   )
	 ))
     (tag-data
       (sort
	 tag-data
	 (lambda 
	   (x y)
	   (or
	     (string< (first x) (first y))
	     (and
	       (string= (first x) (first y))
	       (string< (second x) (second y))
	       )
	     )
	   )
	 ))
     (zones
       (let*
	 ((res nil))
	 (loop 
	   for x in tag-data
	   unless (equal (car x) (car res))
	   do (push (car x) res)
	   )
	 (reverse res)
	 )
       )
     (split-data
       (loop 
	 for z in zones
	 collect
	 (list
	   z
	   (loop
	     for x in tag-data
	     when (equal (car x) z)
	     collect (cdr x)
	     )
	   )
	 ))
     (formatted-data
       (format
	 nil
	 ;"Window list:~{~%~{~a~%~{~{[ ~a ] ( ~a | ~a | ~a )~% -> ~{~a ~}~}~%~}~}~}"
	 "Window list:~{~%~{~a~%~{~{ [ ~a ] ( ~a | ~a | ~a )~%  ->~{ ~a~}~}~%~}~}~}"
	 split-data
	 ))
     )
    (let*
      ((*suppress-echo-timeout* t))
      (message "~a" formatted-data)
      )
    )
  )


(defcommand 
  eval+stdout (cmd) ((:rest "Eval> "))
  (message
    "~a"
    (with-output-to-string (*standard-output*)
      (let*
	((*error-output* *standard-output*))
	(handler-case
	  (format t "~{~a~^~%~}"
		  (mapcar 'prin1-to-string
			  (multiple-value-list (eval (read-from-string cmd)))))
	  (error (c)
		 (format t "~A" c)))))))

(defvar *windows-on-top* nil)
(defcommand clear-top-windows () ()
	    "Clear the list of on-the-top windows"
	    (setf *windows-on-top* nil))
(defcommand keep-window-on-top (&key (window nil)) ()
	    "Add (current) window to the on-the-top windows"
	    (pushnew (or window (current-window)) *windows-on-top*))

(defcommand 
  ssh-window (arg) ((:rest "Host: "))
  "Pull or open SSH window"
  (if
    (> (length arg) 0)
    (let*
      (
       (with-command (equal (elt arg 0) #\#))
       (host (if with-command
	       (cl-ppcre:regex-replace
		 "^#([^#]+)#(.*)"
		 arg
		 "\\1")
	       arg))
       (override-command
	 (when with-command
	   (cl-ppcre:regex-replace
	     "^#([^#]+)#(.*)"
	     arg
	     "\\2")))
       (command
	 (if with-command
	   (format nil "SSH_X_NAME=\"~a\" ~a" host override-command)
	   (format nil "SSH_X_NAME=\"~a\" ssh-std -@ 52741 ~a" host host)))
       (w (first (act-on-matching-windows 
		   (w :screen)
		   (title-re-p 
		     w 
		     (format nil "ssh session: ~a " host))
		   w)))
       )
      (if w
	(progn
	  (pull-w w)
	  (really-raise-window w)
	  )
	(progn
	  (run-shell-command
	    (format nil "urxvt -e $SHELL -c '~a'" command))
	  )
	)
      )
    (ftg-set-tags "ssh")
    ))

(defcommand 
  ssh-window-chosen () ()
  "Run a chooser, then launch ssh window"
  (let*
    ((choice
       (run-prog-collect-output 
	 "/bin/sh" 
	 "-c" 
	 "DMENU_CHOOSER_CLEANER='s/ ## .*//' dmenu-chooser-file ~/rc/ssh-hosts")))
    (ssh-window (remove #\Return (remove #\Linefeed choice)))))

(defcommand close-ssh-windows () ()
	    "Close all the SSH windows"
	    (act-on-matching-windows
	      (w :screen)
	      (tagged-p w "SSH")
	      (delete-window w)))

(defcommand close-irc-channels () ()
	    "Close all the IRC channels"
	    (act-on-matching-windows
	      (w :screen)
	      (and
		(tagged-p w "II-INTERACTIVE")
		(title-re-p w "/#")
		)
	      (delete-window w)))

(defcommand
  all-tag-names () ()
  "List all tag names"
  (message
    "~{~a~%~}"
    (reduce 
      (lambda (x y) (union x y :test 'equalp))
      (act-on-matching-windows 
	(w :screen)
	t 
	(window-tags w)))))

(defcommand
  all-tag-combos () ()
  "List all tag names with adjacet tags"
  (message
    "~{~{~a ## ~{~a ~}~}~%~}"
    (let*
      (
       (ht (make-hash-table :test 'equal))
       (res nil)
       )
      (act-on-matching-windows 
	(w :screen)
	t 
	(let*
	  ((wt (window-tags w)))
	  (loop for a in wt do
		(loop for b in wt do
		      (pushnew b (gethash a ht) :test 'equalp)))))
      (maphash 
	(lambda (k v)
	  (push (list k (sort v #'string<)) res))
	ht)
      (sort res #'string< :key 'first))))

(defcommand open-ssh-ugvim () ()
	    "Open a (possibly SSHFS) ugvim"
	    (run-shell-command
 "sh -c \"open-ssh-ugvim $(DMENU_CHOOSER_CLEANER='s/ ## .*//' dmenu-chooser-file \"$(file=\"$(mktemp /tmp/gvim-launcher-$USER-XXXXXX)\"; cat ~/rc/ugvim-targets >> \"$file\"; cat ~/.mtn-pending-changes | sed -e \"s/^/'' '' /\" >> \"$file\"; echo \"$file\" )\")\""))

(defcommand choose-tag-combo () ()
	    "Choose a window tag to pull from window tag combination list"
	    (run-shell-command
 "stumpish ftg-set-tags $(stumpish all-tag-combos | sed -re 's/^(.*)##/\\1 ## ^\\1/' | sort | dmenu-choose | sed -re 's/ ## .*//')"))

(defcommand 
  firefox-form-fill () ()
  "Use password store to fill a Firefox form"
  (let*
    ((firefox-id (window-id (current-window))))
    (run-shell-command
      (format nil 
      "
      FIREFOX_FORMFILL_URL=\"$(firefox-get-url ~a)\"; 
      firefox-fill-form \"$(firefox-formfill-options | dmenu-choose)\" ~a
      "
      firefox-id firefox-id
      ))))

(defcommand 
  center-layout (&key (left 0) (right 0) (up 0) (down 0)) ()
  "Create a frame in center"
  (let*
    (
     (group (current-group))
     (frame (tile-group-current-frame group))
     )

    (expand-tree frame (- up) :top)
    (expand-tree frame (- down) :bottom)
    (expand-tree frame (- left) :left)
    (expand-tree frame (- right) :right)

    (tree-iterate frame (lambda (leaf) (sync-frame-windows group leaf)))
    ))

(defcommand 
  do-status-split () ()
  "Perform the basic split for status windows"
  (only)
  (let*
    ((g (current-group))
     (f2n (split-frame 
	    (current-group) :row  
	    (- (frame-display-height 
		 g (tile-group-current-frame g)) 32)))
     (f2 (frame-by-number g f2n))
     (f3n
       (split-frame
	 (current-group) :row
	 (-
	   (frame-display-height g (tile-group-current-frame g))
	   30
	   )
	 ))
     (f3 (frame-by-number g f3n))
     (f1 (tile-group-current-frame g))) 
    (setf (frame-tags f1) 
	  '("TG/MAIN"))
    (setf (frame-tags f2) '("TG/MODELINE"))
    (setf (frame-tags f3) '("TG/BREAKING-NEWS"))
    (merge-frame f2 f1)
    (merge-frame f3 f1)
    )
  )

(defcommand 
  place-my-status-windows () ()
  "Place breaking-news and xwatchsystem"
  (default-tags)
  (unless
    (act-on-matching-windows 
      (w :screen)
      (tagged-p w "XWATCHSYSTEM")
      (pull-w w)
      (to-tagged-frame w "TG/MODELINE")
      w
      )
    (run-shell-command "xwatchsystem")
    )
  (unless
    (act-on-matching-windows 
      (w :screen)
      (tagged-p w "BREAKING-NEWS")
      (pull-w w)
      (to-tagged-frame w "TG/BREAKING-NEWS"))
    (run-shell-command "x-breaking-news")
    )
  (focus-frame (current-group) (first-frame-by-tag "TG/MAIN"))
  )

(defcommand
  type-password-dmenu () ()
  "Type account password"
  (run-shell-command
    (format 
      nil
      "type-password-dmenu ~a "
      (window-id (current-window)))))

(defcommand
  type-account-name () ()
  "Type username for some account"
  (run-shell-command
    (format 
      nil
      "type-account-name ~a "
      (window-id (current-window)))))

(defcommand
  type-account-full-name () ()
  "Type full account name"
  (run-shell-command
    (format
      nil 
      "type-account-full-name ~a "
      (window-id (current-window)))))
