;;
;; This file loads different modules, set some custom keys for loaded functions,
;; also change some color to match the black background.
;;



;;------------------------------------------------------------------------------
;;
;; Module: cl
;;
;;------------------------------------------------------------------------------

; Loaded by default with xemacs, but not with emacs.
(require 'cl) 


;;------------------------------------------------------------------------------
;;
;; Module: p4
;;
;;------------------------------------------------------------------------------

(require 'p4)


;;------------------------------------------------------------------------------
;;
;; Module: font-lock -- XEmacs only!
;;
;;------------------------------------------------------------------------------

;; Load and customize the Font-Lock syntax-highlighting package

(if using-xemacs
    (progn (require 'font-lock)
    (add-hook 'font-lock-mode-hook  'turn-on-fast-lock)
    (setq fast-lock-cache-directories '("~/.xemacs/fast-lock/"))

    ;; default syntax highlighting
    (setq-default  font-lock-use-fonts nil
		   font-lock-use-colors '(color)
		   font-lock-maximum-decoration t)

    (add-hook 'emacs-lisp-mode-hook	'turn-on-font-lock)
    (add-hook 'lisp-mode-hook	        'turn-on-font-lock)
    (add-hook 'c-mode-hook		'turn-on-font-lock)
    (add-hook 'c++-mode-hook	        'turn-on-font-lock)
    (add-hook 'asm-mode-hook            'turn-on-font-lock)
    (add-hook 'python-mode-hook         'turn-on-font-lock)
    (add-hook 'sh-mode-hook             'turn-on-font-lock)
    (add-hook 'perl-mode-hook	        'turn-on-font-lock)
    (add-hook 'tex-mode-hook	        'turn-on-font-lock)
    (add-hook 'dired-mode-hook	        'turn-on-font-lock)))


;;------------------------------------------------------------------------------
;;
;; Module: redo
;;
;;------------------------------------------------------------------------------

;; This module enable a redo function which is the exact opposite of the well
;; known undo function

(require 'redo)
(define-key global-map [(control +)] 'redo)


;;------------------------------------------------------------------------------
;;
;; Module: Bookmark
;;
;;------------------------------------------------------------------------------

;; This library will emulate the visual studio's bookmark

(load-library "bm")

;;------------------------------------------------------------------------------
;;
;; Module: highlight-current-line
;;
;;------------------------------------------------------------------------------

;; Module which add a feature: the current line will now be highlighted.

(require 'highlight-current-line)
(highlight-current-line-on t)


;;------------------------------------------------------------------------------
;;
;; Highlight current-word
;;
;;------------------------------------------------------------------------------

(require 'highlight-regexp)
(setq highlight-regexp-faces '(secondary-selection
			       highlight-selected-word-face))


(defun highlight-selected-word(word index)
  "Highlight the current word"
  (let ((highlight-regexp--face-index index))
    (highlight-regexp-string word)))

(defun dehighlight-current-word()
  "Hook -- Cancel the previous current-word highlighted"
  (remove-hook 'pre-command-hook 'dehighlight-current-word)
  (sit-for 0)
  (highlight-regexp-clear)
  (pending-delete-mode 1)) ;; For unknown reason, pending-mode is corrupted by this function... linked to the remove-hook call

(setq quick-search-current-text "")
(defun quick-search-text()
  "Extract the text to do the quick search: it's either the current if nothing is selected of the selected text"
  (if mark-active
      (prog1 (buffer-substring (region-beginning) (region-end))
	     (deactivate-mark))
      (current-word)))

(defun quick-search-forward()
  "Do a forward search using the word below the cursor. Store the current searched word in a global variable to allow a repeated search using the function repeat-search-current-word-forward or repeat-search-current-word-backward." 
  (interactive)
  (setq quick-search-current-text (quick-search-text))
  (search-forward quick-search-current-text)
  (highlight-selected-word quick-search-current-text 0)
  (add-hook 'pre-command-hook 'dehighlight-current-word))

(defun quick-search-backward()
  "Do a forward search using the word below the cursor. Store the current searched word in a global variable to allow a repeated search using the function repeat-search-current-word-forward or repeat-search-current-word-backward." 
  (interactive)
  (setq quick-search-current-text (quick-search-text))
  (search-backward quick-search-current-text)
  (highlight-selected-word quick-search-current-text 0)
  (add-hook 'pre-command-hook 'dehighlight-current-word))

(defun repeat-quick-search-forward()
  "Repeat a forward research using the stored current word (cf: search-current-word-forward / search-current-word-backword)"
  (interactive)
  (search-forward quick-search-current-text)
  (highlight-selected-word quick-search-current-text 0)
  (add-hook 'pre-command-hook 'dehighlight-current-word))

(defun repeat-quick-search-backward()
  "Repeat a forward research using the stored current word (cf: search-current-word-forward / search-current-word-backword)"
  (interactive)
  (search-backward quick-search-current-text)
  (highlight-selected-word quick-search-current-text 0)
  (add-hook 'pre-command-hook 'dehighlight-current-word))


;;
;; Use of the isearch interface to highlight the current word
;;

(setq highlist-selected-word-toggle-state nil)
(make-face 'highlight-selected-word-face)
(set-face-background 'highlight-selected-word-face (first-valid-color "lightcyan" "white"))
(set-face-foreground 'highlight-selected-word-face (first-valid-color "blue" "black"))
(defvar highlight-selected-word-face 'highlight-selected-word-face
  "Font used to highlight the selected / current word.")

(defun highlight-current-word()
  "Use isearch library to highlight the current word"
  (interactive)
  (if (not (member (current-buffer) highlist-selected-word-toggle-state))
      (progn (push (current-buffer) highlist-selected-word-toggle-state)
	     (let ((highlight-regexp--face-index 1))
	       (highlight-regexp-current-word)))
      (progn (setq highlist-selected-word-toggle-state (remq (current-buffer) highlist-selected-word-toggle-state))
	     (sit-for 0)
	     (highlight-regexp-clear))))



;;------------------------------------------------------------------------------
;;
;; Module: dabbrev-highlight
;;
;;------------------------------------------------------------------------------

; To highlight the keyword dabbrev used to complete the word:
(require 'dabbrev-highlight)

;;------------------------------------------------------------------------------
;;
;; Module: iswitchb
;;
;;------------------------------------------------------------------------------


(require 'iswitchb)
(iswitchb-default-keybindings)

(if using-xemacs 
    (progn (defun iswitchb-local-keys ()
	     (mapc (lambda (K) 
		     (let* ((key (car K)) (fun (cdr K)))
		       (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
		   '(("<right>" . iswitchb-next-match)
		     ("<left>"  . iswitchb-prev-match)
		     ("<up>"    . ignore             )
		     ("<down>"  . ignore             ))))
	   (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys))
    (progn (add-hook
	    'iswitchb-define-mode-map-hook
	    '(lambda ()
	       (define-key iswitchb-mode-map " " 'iswitchb-next-match)
	       (define-key iswitchb-mode-map [del] 'iswitchb-prev-match)
	       (define-key iswitchb-mode-map [bs] 'iswitchb-prev-match)
	       (define-key iswitchb-mode-map [right] 'iswitchb-next-match)
	       (define-key iswitchb-mode-map [left] 'iswitchb-prev-match)))
	   (icomplete-mode 1)))

(defadvice iswitchb-visit-buffer (after iswitchb-visit-buffer(buffer))
  (message buffer-file-name))
(ad-activate 'iswitchb-visit-buffer)

;;------------------------------------------------------------------------------
;;
;; Module: cscope
;;
;;------------------------------------------------------------------------------

;; CScope is a developer module for browsing source code, finding symbols...
;;
;; In order to enable it, you first need to build his database, for that
;; you have to start by creating a file containing a list of source files
;; Then you need to run cscope on this file.
;;
;; Here is a sample of a script which could do the trick:
;;
;; make ~/.cscope
;; find ~/work -name \*.cpp -or -name \*.h -or -name \*.c > ~/.cscope/cscope.files
;; cscope -b -q -k

(defvar cscope-calling-buffer-stack '())

(defun cscope-find-this-symbol-no-updates(symbol)
  "Locate a symbol in source code [no database update performed]."
  (interactive (list (cscope-prompt-for-symbol "Find this symbol [without updating the database]: " nil)))
  (let ((cscope-do-not-update-database t)
	(cscope-adjust t))
    (setq cscope-symbol symbol)
    (cscope-call (format "Finding symbol: %s" symbol)
		 (list "-0" symbol) nil 'cscope-process-filter
		 'cscope-process-sentinel)))

(defun cscope-find-global-definition-no-updates(symbol)
  "Find a symbol's global definition [no database update performed]."
  (interactive (list (cscope-prompt-for-symbol "Find this global definition [without updating the database]: " nil)))
  (let ((cscope-do-not-update-database t)
	(cscope-adjust t))
    (setq cscope-symbol symbol)
    (cscope-call (format "Finding global definition: %s" symbol)
		 (list "-1" symbol) nil 'cscope-process-filter
		 'cscope-process-sentinel)))

(defun cscope-find-this-text-string-no-updates(symbol)
  "Locate where a text string occurs [no database update performed]."
  (interactive (list (cscope-prompt-for-symbol "Find this text string [without updating the database]: " nil)))
  (let ((cscope-do-not-update-database t)
	(cscope-adjust t))
    (setq cscope-symbol symbol)
    (cscope-call (format "Finding text string: %s" symbol)
		 (list "-4" symbol) nil 'cscope-process-filter
		 'cscope-process-sentinel)))

(defun cscope-find-functions-calling-this-function-no-updates (symbol)
  "Display functions calling a function."
  (interactive (list (cscope-prompt-for-symbol "Find functions calling this function [without updating the database]: " nil)))
  (let ((cscope-do-not-update-database t)
	(cscope-adjust t))
    (setq cscope-symbol symbol)
    (cscope-call (format "Finding functions calling: %s" symbol)
		 (list "-3" symbol) nil 'cscope-process-filter
		 'cscope-process-sentinel)))

(defun cscope-find-global-definition-no-prompting-no-updates ()
  "Find a symbol's global definition without prompting."
  (interactive)
  (let ((symbol (cscope-extract-symbol-at-cursor nil))
	(cscope-do-not-update-database t)
	(cscope-adjust t))	 ;; Use fuzzy matching.
    (setq cscope-symbol symbol)
    (cscope-call (format "Finding global definition: %s" symbol)
		 (list "-1" symbol) nil 'cscope-process-filter
		 'cscope-process-sentinel)))

(defun cscope-find-this-symbol-no-prompting-no-updates()
  "Locate a symbol in source code [no database update performed -- no user prompting]."
  (interactive)
  (let ((symbol (cscope-extract-symbol-at-cursor nil))
	(cscope-do-not-update-database t)
	(cscope-adjust t))
    (setq cscope-symbol symbol)
    (cscope-call (format "Finding symbol: %s" symbol)
		 (list "-0" symbol) nil 'cscope-process-filter
		 'cscope-process-sentinel)))


(require 'xcscope)


;;(set-face-foreground 'cscope-line-face  "white")
;(cscope-set-initial-directory "~/.cscope")


;;------------------------------------------------------------------------------
;;
;; Module: spu-mode
;;
;;------------------------------------------------------------------------------

(require 'spu-mode)


;;------------------------------------------------------------------------------
;;
;; Module: ddf-mode
;;
;;------------------------------------------------------------------------------

(require 'ddf-mode)


;;------------------------------------------------------------------------------
;;
;; Module: auto-save
;;
;;------------------------------------------------------------------------------

;; Load the auto-save.el package, which lets you put all of your autosave
;; files in one place, instead of scattering them around the file system.

(if using-xemacs
    (progn (require 'auto-save)
	   (setq auto-save-directory (expand-file-name "~/.xemacs/autosaves/")
		 auto-save-directory-fallback auto-save-directory
		 auto-save-hash-p nil
		 auto-save-interval 2000)))


;;------------------------------------------------------------------------------
;;
;; Module: dired+
;;
;;------------------------------------------------------------------------------

(require 'dired+)
(define-key dired-mode-map [?S] 'dired-mark-files-containing-regexp)
(define-key dired-mode-map [?s] 'dired-mark-files-regexp)
(define-key dired-mode-map [?K] 'diredp-omit-unmarked)
(define-key dired-mode-map [?I] 'dired-kill-subdir)
(define-key dired-mode-map [(control up)] 'dired-tree-up)
(define-key dired-mode-map [(control down)] 'dired-tree-down)
(define-key dired-mode-map [(control left)] 'dired-hide-subdir)
(define-key dired-mode-map [return] 'dired-find-alternate-file)
(define-key dired-mode-map [(control return)] 'dired-find-file)
(define-key dired-mode-map [backspace] 'dired-up-directory)
(put 'dired-find-alternate-file 'disabled nil)



;;------------------------------------------------------------------------------
;;
;; Module: find-dired++
;;
;;------------------------------------------------------------------------------

(require 'find-dired++)


;;------------------------------------------------------------------------------
;;
;; Module: filecache
;;
;;------------------------------------------------------------------------------
;(require 'filecache)
;(define-key minibuffer-local-completion-map [(control tab)] 'file-cache-minibuffer-complete)
;(define-key minibuffer-local-map [(control tab)] 'file-cache-minibuffer-complete)
;(define-key minibuffer-local-must-match-map [(control tab)] 'file-cache-minibuffer-complete)

;(file-cache-add-directory-using-find "~/ndi/big-frontend/src/")

;;------------------------------------------------------------------------------
;;
;; Module: suggbind
;;
;;------------------------------------------------------------------------------

;; Turn off builtin pre-command hints, and enable a much-improved version.
(setq suggest-key-bindings nil)
(load "suggbind")


