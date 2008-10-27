;;
;; This file has to be loaded first!!!
;; He's defining the required function for the rest of the settings
;; In this file, you'll also find the different core emacs settings
;;


;;------------------------------------------------------------------------------
;;
;;	                     Basic Core Customization
;;
;;------------------------------------------------------------------------------

;; Customize various settings


;; Make sure yank-pop does at least a simple yank:
(defadvice yank-pop (around anytime (arg) activate)
  "Modification of yank-pop: if last action isn't yank, do it."
  (if (not (eq last-command 'yank))
      (yank arg)
      ad-do-it))

;
; don't display the wussy toolbar icons.
(when window-system
  (tool-bar-mode 0)  ;; slowww!
  (menu-bar-mode 0)) ;; slowww!

;; by default mouse-wheel isn't active under emacs:
(when window-system
  (mouse-wheel-mode t))

;; Set default tab width
(setq tab-width 4)

;; Copy/paste use the clipboard...
;;x-select-enable-clipboard


;; Inihibit the splash screen:
(setq inhibit-startup-message t)


;; Backup all files in a specific folder:
(if (eq window-system 'x)
    (progn 
      (setq make-backup-files t)
      (add-to-list 'backup-directory-alist (cons "." "~/.emacs-backup")))
    ;; Disable backup files « ~ »
    (setq make-backup-files nil))

;; Recover files:
(setq auto-save-list-file-prefix "~/.emacs-backup/auto-save-list/.saves-")

;; Make sure list doesn't ident the 'else' differently from the 'then'
(put 'if 'lisp-indent-function nil)

;; Disable anoying alarm bell
(setq ring-bell-function 'ignore)

;; Enable the research to loop around:
(setq isearch-wrapped t)

;; Try to flash the frame instead of beep at error/message;
;(setq visible-bell nil)

;; Display line/column numbers in the modeline
(line-number-mode t)
(column-number-mode t)

;; Set number of lines in a compilation window
(setq compilation-window-height nil)

;; unlock the eval-expression function, in case I feel the sudden urge to
;; evaluate a Lisp expression in the middle of C code.
(put 'eval-expression 'disabled nil)

;; unlock the set-goal-column command:
(put 'set-goal-column 'disabled nil)

;; unlock the downcase function -- useful for keyboard macros:
;(put 'downcase-region 'disabled nil)
;(put 'upcase-region 'disabled nil)
;(put 'narrow-to-region 'disabled nil)

;; If non-nil, `next-line' inserts newline to avoid `end of buffer' error
(setq next-line-add-newlines nil)

;; Kill trailing white space on save  
;(autoload 'nuke-trailing-whitespace "whitespace" nil t)
;(remove-hook 'write-file-hooks 'nuke-trailing-whitespace)

;; Display matching parentheses
(setq show-paren-delay 0)
(show-paren-mode t)

;; Number of buffers to display in buffer list ; nil = all
(setq buffers-menu-max-size nil)

;; To enable syntax highlighting by default in all buffers:
(global-font-lock-mode 1)

;; Ansi color -- to allow the color mode in the shell
(ansi-color-for-comint-mode-on)

;; Activate the pending delete mode: 
;;   In Pending Delete mode, typed text replaces the selected region.
(pending-delete-mode 1)

;; Toggle truncate lines:
(setq default-truncate-lines t)

;; Enable shift+key to select a region:
(pc-selection-mode) ;; Could be disabled if CUA mode is activated

;; Smaller delay before showing the tooltip.
(setq tooltip-delay 0.3)

;; Increase the size of the undo buffer:
(setq undo-limit         800000)
(setq undo-strong-limit 1000000)

;; Don't bother checking for KnR style (to speed up the analyze)
(setq c-recognize-knr-p nil)







;; Map C-x, C-c-, C-v as Cut/Copy/Paste without affecting the usual behavior of the keys
;; Map C-z to undo...
;(cua-mode)

;; Under linux: ido-mode doesn't seems bad at all -- similar to iswitchb for "find file"
;; Terrible to switch drive under windows... 
;(if (eq window-system 'x) 
;    (ido-mode))

;; Show end of buffer...
;;(setq indicate-empty-lines t)

;; Should make the cursor fix (not blinking anymore):
;;(when (blink-cursor-mode)
;; (blink-cursor-mode))

;; Automatic reload file:
;;(global-auto-revert-mode 1)

;; Set the frame name (seems to only worked when the window is reduced in the taskbar)
;; (setq frame-title-format "emacs - %b")

(when window-system
  (desktop-save-mode))

;;------------------------------------------------------------------------------
;;
;;              Customization of the different modes
;;
;;------------------------------------------------------------------------------

;; Autoload the spu mode:
;;(autoload 'spu-mode "spu-mode"  "Major mode for editing SPU assembly code." t)

;; Add additional extensions and their appropriate modes
(setq auto-mode-alist
      (append '(("\\.inl$"      . c++-mode)
		("\\.h$"        . c++-mode) ; Sadly, .h file with classes in it are still using the c-mode... :(
		("\\.[fv]p$"    . c++-mode)
		("\\.spu$"      . spu-mode)
		("\\.spu.s$"    . spu-mode)
		("\\.S$"        . spu-mode)
		("\\.ddf$"      . ddf-mode)
		("\\.xml$"      . xml-mode)
		("\\.idf$"      . idf-mode)
		)
	      auto-mode-alist))
