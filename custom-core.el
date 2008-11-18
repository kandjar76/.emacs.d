;;
;; This file has to be loaded first!!!
;; He's defining the required function for the rest of the settings
;; In this file, you'll also find the different core emacs settings
;;


;; Define a global variable to know if we're in emacs or xemacs:
(setq using-xemacs (featurep 'xemacs))


;;------------------------------------------------------------------------------
;;
;;                      Utility Functions                           
;;
;;------------------------------------------------------------------------------

(defun first-valid-color (&rest colors)
  "Returns the first valid color from color-list (a list of color names).
Returns \"black\" if no valid color is found."
  (cond ((null colors) "black")
	((is-color-valid (car colors)) (car colors))
	((apply 'first-valid-color (cdr colors)))))

(defun is-region-active()
  "Check if a region is active"
  (if (boundp 'region-active-p)
      (region-active-p) 
    mark-active))

(defun is-color-valid(col)
  "Check if the color COL is valid"
  (if (boundp 'valid-color-name-p)
      (valid-color-name-p col)
    (color-defined-p col)))

;;------------------------------------------------------------------------------
;;
;;	                     Basic Core Customization
;;
;;------------------------------------------------------------------------------

;; Customize various settings

;; don't display the wussy toolbar icons.
(if using-xemacs 
    (set-specifier default-toolbar-visible-p nil)
    (progn (tool-bar-mode nil)
	   (menu-bar-mode nil)))

;; by default mouse-wheel isn't active under emacs:
(if (not using-xemacs)
	(mouse-wheel-mode t))

;; Set default tab width
(setq tab-width 4)

;; Inihibit the splash screen:
(setq inhibit-startup-message t)

;; Disable backup files « ~ »
(setq make-backup-files nil)

;; Make sure list doesn't ident the 'else' differently from the 'then'
(put 'if 'lisp-indent-function nil)

;; Disable anoying alarm bell
(if using-xemacs
	(setq bell-volume 0)
	(setq ring-bell-function 'ignore))

;; Automatically resize the minibuffer when the user is entering
;; input, so they can always see everything they type.
(resize-minibuffer-mode)

;; Enable the research to loop around:
(setq isearch-wrapped t)

;; Try to flash the frame instead of beep at error/message;
(setq visible-bell nil)

;; Display line numbers in the modeline
(line-number-mode t)

;; Set number of lines in a compilation window
(setq compilation-window-height nil)

;; unlock the eval-expression function, in case I feel the sudden urge to
;; evaluate a Lisp expression in the middle of C code.
(put 'eval-expression 'disabled nil)

;; Enable only one command in Buffers menu (select that buffer)
(if using-xemacs 
	(setq complex-buffers-menu-p nil))

;; The find-file command will check the truenames of all visited files when 
;; deciding whether a given file is already in a buffer
(if using-xemacs
	(setq find-file-compare-truenames t))

;; In answers which are not valid completions, an extra RET must be typed 
;; to confirm the response.
;;(setq minibuffer-confirm-incomplete t)

;; If non-nil, `next-line' inserts newline to avoid `end of buffer' error
(setq next-line-add-newlines nil)

;; Kill trailing white space on save  
;(autoload 'nuke-trailing-whitespace "whitespace" nil t)
;(remove-hook 'write-file-hooks 'nuke-trailing-whitespace)

;; Set the modeline control for identifying the buffer being displayed.
;(setq-default modeline-buffer-identification '("XEmacs: %17b"))
;(setq modeline-buffer-identification '("XEmacs: %17b"))

;; Change the cursor used when the mouse is over a mode line
(if using-xemacs 
	(setq x-mode-pointer-shape "leftbutton")
	(setq x-pointer-shape "leftbutton"))

;; Display matching parentheses
(if using-xemacs
	(paren-set-mode 'paren)
	(show-paren-mode t))

;; Number of buffers to display in buffer list ; nil = all
(setq buffers-menu-max-size nil)

;; To enable syntax highlighting by default in all buffers, as well as
;; keeping the highlighting up to date as you edit the document.
;; Thanks to Daniel Pittman <daniel@rimspace.net> for this tip. 
(if using-xemacs 
	(setq font-lock-auto-fontify t)
	(global-font-lock-mode 1))

;; Avoid deactivation of region when buffer end or beginning is reached
;; XEmacs mailing list ; schrod@iti.informatik.th-darmstadt.de
(if using-xemacs
	(defadvice line-move (around catch-buffer-border-error activate)
	  "Catch errors `beginning-of-buffer' or `end-of-buffer' to avoid deactivation of region"
	  (condition-case ()
		  ad-do-it
		((beginning-of-buffer end-of-buffer) (ding nil 'buffer-bound)))))


;; UTF-8 setup to have a better gcc output
(if using-xemacs 
	(progn
	  (require 'un-define)
	  (set-coding-priority-list '(utf-8))
	  (set-coding-category-system 'utf-8 'utf-8)))

;; Ansi color -- to allow the color mode in the shell
(ansi-color-for-comint-mode-on)

;; Activate the pending delete mode: 
;;   In Pending Delete mode, typed text replaces the selected region.
(if using-xemacs
	(pending-delete-on)
	(pending-delete-mode 1))

;; Toggle truncate lines:
(toggle-truncate-lines)

;; Enable shift+key to select a region:
(pc-selection-mode)

;;------------------------------------------------------------------------------
;;
;;              Customization of the different modes
;;
;;------------------------------------------------------------------------------

;; Add additional extensions and their appropriate modes
;(setq auto-mode-alist
;      (append '(("\\.C$"       . c++-mode)
;		("\\.cc$"      . c++-mode)
;		("\\.cpp$"     . c++-mode)
;		("\\.hh$"      . c++-mode)
;		("\\.hpp$"     . c++-mode)
;		("\\.c$"       . c++-mode)
;		("\\.h$"       . c++-mode)
;		("\\.inl$"     . c++-mode)
;		("\\.tex$"     . latex-mode)
;		("\\.spu$"     . spu-mode)
;		("\\.spu.s$"   . spu-mode)
;		("\\.[fv]p$"   . c++-mode)
;		("\\.xml$"     . xml-mode))
;	      auto-mode-alist))
