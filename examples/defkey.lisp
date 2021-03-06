(in-package :stumpwm)

(set-prefix-key (kbd "Menu"))

(define-key *root-map* (kbd "M") "meta Menu")
(define-key *root-map* (kbd "N") "eval (run-commands \"exec pkill -9 xneur\" \"exec xneur &\")")
(define-key *root-map* (kbd "M-N") "exec pkill -9 xneur")
(define-key *root-map* (kbd "~") "command-mode")
(define-key *root-map* (kbd "ESC") "abort")
(define-key *root-map* (kbd "C-r") "loadrc")
(define-key *root-map* (kbd "O") "other")
(define-key *root-map* (kbd "L") "force-redisplay")

(define-key *root-map* (kbd "C-K") "withdraw-current-window")

(define-key *root-map* (kbd "M-c") "exec urxvt")
(define-key *root-map* (kbd "C-h") "exec :hour-sleep")
(define-key *root-map* (kbd "C-y") "exec :away")
(define-key *root-map* (kbd "P") "exec :asleep")

(define-key *root-map* (kbd "C-v") "exec ugvim")
(define-key *root-map* (kbd "C-M-v") "exec gvim")

(define-key *root-map* (kbd "u") "exec UZBL_LAUNCH_IN_SCREEN=1 uzbl")
(define-key *root-map* (kbd "M-u") "exec UZBL_LAUNCH_IN_SCREEN=1 UZBL_EXECUTABLE=~/.nix-personal/personal-result/uzbl_gtk2/bin/uzbl-core uzbl")
(define-key *root-map* (kbd "U") "exec UZBL_LAUNCH_IN_SCREEN=1 uzbl $(xclip -o)")
(define-key *root-map* (kbd "M-U") "exec UZBL_LAUNCH_IN_SCREEN=1 UZBL_EXECUTABLE=~/.nix-personal/personal-result/uzbl_gtk2/bin/uzbl-core uzbl $(xclip -o)")

(define-key *root-map* (kbd "M-w") "exec ~/.nix-personal/personal-result/webkit/bin/webkit-program-GtkLauncher")
(define-key *root-map* (kbd "M-W") "exec ~/.nix-personal/personal-result/webkit_gtk2/bin/webkit-program-GtkLauncher")

(define-key *root-map* (kbd "C-f") "exec urxvt -lsp 9 -fn 'xft:DejaVu Sans Mono:pixelsize=16:antialias=true:weight=' -e zsh -c 'xtitle web-streams; screen -D -RR -S view-web-streams -U view-web-streams'")
(define-key *root-map* (kbd "C-M") "exec urxvt -e zsh -c 'launch-view-emails'")
(define-key *root-map* (kbd "C-M-M") "exec urxvt -e zsh -c 'launch-view-emails edit-new-email'")
(define-key *root-map* (kbd "C-M-m") "exec urxvt -e zsh -c 'launch-view-emails view-mail-summaries'")
(define-key *root-map* (kbd "C-F") "exec uzbl $(find-related-uri)") 

(define-key *root-map* (kbd "C-B") "exec urxvt -e zsh -c 'xtitle breaking-news; screen -D -RR -S view-breaking-news -U view-breaking-news'")
(define-key *root-map* (kbd "C-M-B") "exec rm ~/.breaking-news/zzz-naggers; fast-lane-emails-to-breaking-news; kill-gajim-passwords ; kill-gajim-statuses; sort-gajim-config; clean-gqview-geometry; purge-all-from-pending")

(define-key *root-map* (kbd "C-M-f") "exec firefox")

(define-key *root-map* (kbd "I") "show-im-status")

(define-key *root-map* (kbd "e") "")

(define-key *root-map* (kbd "B") "exec brightness")

(define-key *root-map* (kbd "Menu") "globally-previous-wt")

(define-key *root-map* (kbd "F12") "gselect .system")
(define-key *root-map* (kbd "F12") "gselect .tag-store")
(define-key *root-map* (kbd "DEL") "gselect Default")
(define-key *root-map* (kbd "M-F11") "pull+push+renumber t")
(define-key *root-map* (kbd "F1")   "local-shell-group")
(define-key *root-map* (kbd "F2")   "eval (progn (ftg-set-tags \"im\") (number-by-tags))")
(define-key *root-map* (kbd "F3")   "ftg-set-tags tb")
(define-key *root-map* (kbd "F4")   "heavy-browser-group")
(define-key *root-map* (kbd "F5")   "light-browser-group")
(define-key *root-map* (kbd "F6")   "ftg-set-tags view")
(define-key *root-map* (kbd "F7")   "editor-group")
(define-key *root-map* (kbd "F8")   "ftg-set-tags ssh")
(define-key *root-map* (kbd "F9")   "root-shell-group")
(define-key *root-map* (kbd "M-F1") "ftg-set-tags games")
(define-key *root-map* (kbd "M-F2") "ftg-set-tags monitor")
(define-key *root-map* (kbd "M-F3") "ftg-set-tags p2p")
(define-key *root-map* (kbd "M-F4") "lazarus-layout")
(define-key *root-map* (kbd "M-F5") "ftg-set-tags qemu")
(define-key *root-map* (kbd "M-F6") "gimp-layout")
(define-key *root-map* (kbd "M-F7") "dia-layout")
(define-key *root-map* (kbd "M-F8") "media-group")

(define-key *root-map* (kbd "S-F1")   "pull-tag sh")
(define-key *root-map* (kbd "S-F2")   "pull-tag im")
(define-key *root-map* (kbd "S-F3")   "pull-tag tb")
(define-key *root-map* (kbd "S-F4")   "pull-tag ff")
(define-key *root-map* (kbd "S-F5")   "pull-tag light-browser")
(define-key *root-map* (kbd "S-F6")   "pull-tag view mplayer xine")
(define-key *root-map* (kbd "S-F7")   "pull-tag vim gvim editor")
(define-key *root-map* (kbd "S-F8")   "pull-tag ssh")
(define-key *root-map* (kbd "S-F9")   "pull-tag root")
(define-key *root-map* (kbd "S-M-F1") "pull-tag games")
(define-key *root-map* (kbd "S-M-F2") "pull-tag monitor")
(define-key *root-map* (kbd "S-M-F3") "pull-tag p2p")
;(define-key *root-map* (kbd "S-M-F4") "lazarus-layout")
(define-key *root-map* (kbd "S-M-F5") "pull-tag qemu")
;(define-key *root-map* (kbd "S-M-F6") "gimp-layout")
;(define-key *root-map* (kbd "S-M-F7") "dia-layout")

(define-key *root-map* (kbd "M-1") "select-window-by-number 11")
(define-key *root-map* (kbd "M-2") "select-window-by-number 12")
(define-key *root-map* (kbd "M-3") "select-window-by-number 13")
(define-key *root-map* (kbd "M-4") "select-window-by-number 14")
(define-key *root-map* (kbd "M-5") "select-window-by-number 15")
(define-key *root-map* (kbd "M-6") "select-window-by-number 16")
(define-key *root-map* (kbd "M-7") "select-window-by-number 17")
(define-key *root-map* (kbd "M-8") "select-window-by-number 18")
(define-key *root-map* (kbd "M-9") "select-window-by-number 19")
(define-key *root-map* (kbd "M-0") "select-window-by-number 10")

(define-key *root-map* (kbd "N") "repack-window-numbers")
(define-key *root-map* (kbd "N") "number-by-tags")

(define-key *root-map* (kbd "T") "ftg-set-tags")
(define-key *root-map* (kbd "C-T") "tag-window")
(define-key *root-map* (kbd "C-M-t") "window-tags")
(define-key *root-map* (kbd "C-M-T") "pull-tag")
;(define-key *root-map* (kbd "s-t") "ftg-mark-windows")
;(define-key *root-map* (kbd "s-T") "eval (progn (setf (window-tags (current-window)) '(\"NO-AUTO-TAGS\")))")
(define-key *root-map* (kbd "M-T") "untag-window")

(define-key *root-map* (kbd "x") "push-window")

(define-key *root-map* (kbd "d") "dead-windows-cleanup")

(define-key *root-map* (kbd "D") "default-tags")
(define-key *root-map* (kbd "V") "tag-visible")

(define-key *root-map* (kbd "/") "raise-short-tag")
(define-key *root-map* (kbd "M-/") "raise-tag")
(define-key *root-map* (kbd ".") "all-tags-grouped")
(define-key *root-map* (kbd "C-.") "scrollable-window-tag-list")

(define-key *root-map* (kbd "C-s") "ftg-set-tag-re")
(define-key *root-map* (kbd "C-S") "ftg-add-tag-re")

;(define-key *top-map* (kbd "H-Right") "move-focus right")
;(define-key *top-map* (kbd "H-Left") "move-focus left")
;(define-key *top-map* (kbd "H-Up") "move-focus up")
;(define-key *top-map* (kbd "H-Down") "move-focus down")

;(define-key *root-map* (kbd "s-Left") "move-windows-dir Left")
;(define-key *root-map* (kbd "s-Right") "move-windows-dir Right")
;(define-key *root-map* (kbd "s-Up") "move-windows-dir Up")
;(define-key *root-map* (kbd "s-Down") "move-windows-dir Down")

;(define-key *root-map* (kbd "H-F1") "frame-push-pull-tags sh")
;(define-key *root-map* (kbd "H-F2") "frame-push-pull-tags im")
;(define-key *root-map* (kbd "H-F3") "frame-push-pull-tags tb")
;(define-key *root-map* (kbd "H-F4") "frame-push-pull-tags heavy-browser")
;(define-key *root-map* (kbd "H-F5") "frame-push-pull-tags light-browser")
;(define-key *root-map* (kbd "H-F6") "frame-push-pull-tags view mplayer xine")
;(define-key *root-map* (kbd "H-F7") "frame-push-pull-tag vim gvim limp editor")
;(define-key *root-map* (kbd "H-F8") "frame-push-pull-tag ssh")
;(define-key *root-map* (kbd "H-F9") "frame-push-pull-tag root")
;(define-key *root-map* (kbd "H-M-F1") "frame-push-pull-tag games")
;(define-key *root-map* (kbd "H-M-F2") "frame-push-pull-tag monitor")
;(define-key *root-map* (kbd "H-M-F3") "frame-push-pull-tag p2p")
;(define-key *root-map* (kbd "H-M-F5") "frame-push-pull-tag qemu")
;(define-key *root-map* (kbd "H-M-F6") "frame-push-pull-tag gimp")
;(define-key *root-map* (kbd "H-M-F7") "frame-push-pull-tag dia")

(define-key *root-map* (kbd "SPC") "ftg-next-window")
(define-key *root-map* (kbd "M-g") "set-ftg")
(define-key *root-map* (kbd "C-Q") "only")
(define-key *root-map* (kbd "Q") "ftg-only")
(define-key *root-map* (kbd "M-f") "focus-frame-by-tag-re")

(define-key *root-map* (kbd "M-b") "ratcenter")

(define-key *root-map* (kbd "C-I") "exec urxvt -e sh -c 'xtitle ii-irc-summary; ii-all-new 10 | less '")
(define-key *root-map* (kbd "C-i") "exec urxvt -e ii-interactive-screen")
(define-key *root-map* (kbd "C-X") "exec urxvt -e sh -c 'xtitle mcabber-xmpp-summary; mcabber-all-new 10 | less '")
(define-key *root-map* (kbd "C-x") "exec urxvt -e mcabber-interactive-screen")
(define-key *root-map* (kbd "M-x") "exec urxvt -e mcabber-base-screen")

(define-key *root-map* (kbd "C-!") "exec launch-some-command")

;(define-key *top-map* (kbd "H-SunPageUp") "exec increase-level PCM +1")
;(define-key *top-map* (kbd "H-SunPageDown") "exec increase-level PCM -1")
;(define-key *top-map* (kbd "H-Home") "exec increase-level PCM +10")
;(define-key *top-map* (kbd "H-End") "exec increase-level PCM -10")
;(define-key *top-map* (kbd "s-SunPageUp") "exec increase-level Master +1")
;(define-key *top-map* (kbd "s-SunPageDown") "exec increase-level Master -1")
;(define-key *top-map* (kbd "s-Home") "exec increase-level Master +10")
;(define-key *top-map* (kbd "s-End") "exec increase-level Master -10")
;(define-key *top-map* (kbd "s-DEL") "exec local-levels")
;(define-key *top-map* (kbd "H-DEL") "exec no_levels")
;(define-key *top-map* (kbd "H-Pause") "exec media-screen register p p; media-screen paste p")
;(define-key *top-map* (kbd "s-Left") "exec media-screen register p h; media-screen paste p")
;(define-key *top-map* (kbd "s-Right") "exec media-screen register p l; media-screen paste p")
;(define-key *top-map* (kbd "s-Down") "exec media-screen register p H; media-screen paste p")
;(define-key *top-map* (kbd "s-Up") "exec media-screen register p L; media-screen paste p")
;(define-key *top-map* (kbd "s-Pause") "exec media-screen register p '<'; media-screen paste p")
;(define-key *top-map* (kbd "s-KP_Add") "exec media-screen readreg p ~/script/play-random-music; media-screen paste p")

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec increase-level Master +1")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec increase-level Master -1")
(define-key *top-map* (kbd "S-XF86AudioRaiseVolume") "exec increase-level Master +10")
(define-key *top-map* (kbd "S-XF86AudioLowerVolume") "exec increase-level Master -10")
(define-key *top-map* (kbd "XF86AudioMute") "exec no_levels")
(define-key *top-map* (kbd "S-XF86AudioMute") "exec local-levels")
(define-key *top-map* (kbd "S-M-XF86AudioMute") "exec my_levels")
(define-key *top-map* (kbd "M-XF86AudioMute") "exec media-screen readreg p ~/script/play-random-music; media-screen paste p")
(define-key *top-map* (kbd "C-XF86AudioMute") "exec media-screen register p p; media-screen paste p")
(define-key *top-map* (kbd "C-XF86AudioLowerVolume") "exec media-screen register p h; media-screen paste p")
(define-key *top-map* (kbd "C-XF86AudioRaiseVolume") "exec media-screen register p l; media-screen paste p")
(define-key *top-map* (kbd "C-M-XF86AudioLowerVolume") "exec media-screen register p H; media-screen paste p")
(define-key *top-map* (kbd "C-M-XF86AudioRaiseVolume") "exec media-screen register p L; media-screen paste p")

(define-key *top-map* (kbd "XF86TouchpadToggle") "exec touchpad toggle")

(define-key *top-map* (kbd "XF86Sleep") "exec s2both")
(define-key *top-map* (kbd "C-XF86Sleep") "exec standby-fast")
;(define-key *top-map* (kbd "S-XF86Sleep") "exec susp")

(define-key *root-map* (kbd "C-D") "exec sleep 1; xset dpms force off")

(define-key *top-map* (kbd "XF86Display") "exec x-randr-options")

(define-key *root-map* (kbd "C-:") "eval+stdout")

;(define-key *top-map* (kbd "s-S-Up") "exec settrans -c -o +5")
;(define-key *top-map* (kbd "s-S-Down") "exec settrans -c -o -5")
;(define-key *top-map* (kbd "s-S-Right") "exec settrans -c -o 100")
;(define-key *top-map* (kbd "s-S-Left") "exec settrans -c -o -20")
;(define-key *top-map* (kbd "s-S-Pause") "exec settrans -c -o 0")

;(define-key *top-map* (kbd "s-F2") "wifi")
;(define-key *top-map* (kbd "s-F9") "exec touchpad toggle")

(define-key *root-map* (kbd "C-F8") "ssh-window-chosen")
;(define-key *top-map* (kbd "s-s") "ssh-window-chosen")

(define-key *root-map* (kbd "C-F7") "open-ssh-ugvim")

;(define-key *top-map* (kbd "s-f") "firefox-form-fill")

(define-key *root-map* (kbd "M-i") "exec xcalib -a -i")

;(define-key *top-map* (kbd "s-c") "exec echo -e 'ButtonPress 1\\n' | xmacroplay $DISPLAY")

;(define-key *top-map* (kbd "s-v") "open-ssh-ugvim")

;(define-key *top-map* (kbd "s-t") "choose-tag-combo")
