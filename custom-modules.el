;;
;; This file loads different modules, set some custom keys for loaded functions,
;; also change some color to match the black background.
;;



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
    (require 'font-lock)
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
    (add-hook 'dired-mode-hook	        'turn-on-font-lock))


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
(define-key global-map [(control f2)]         'bm-toggle)
(define-key global-map [(f2)]                 'bm-next)
(define-key global-map [(shift f2)]           'bm-prev)
(define-key global-map [(control shift f2)]   'bm-remove-all)

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

(defun highlight-selected-word(index)
  "Highlight the current word"
  (let ((highlight-regexp--face-index index))
    (highlight-regexp-current-word)))

(defun dehighlight-selected-word()
  "De-highlight the current word"
  (highlight-regexp-clear))


(setq current-researched-word "")
(defun search-current-word-forward()
  "Do a forward search using the word below the cursor. Store the current searched word in a global variable to allow a repeated search using the function repeat-search-current-word-forward or repeat-search-current-word-backward." 
  (interactive)
  (setq current-researched-word (current-word))
  (setq isearch-string current-researched-word)
  (search-forward (current-word))
  (highlight-selected-word 0)
  (add-hook 'pre-command-hook 'dehighlight-current-word))

(defun search-current-word-backward()
  "Do a forward search using the word below the cursor. Store the current searched word in a global variable to allow a repeated search using the function repeat-search-current-word-forward or repeat-search-current-word-backward." 
  (interactive)
  (setq current-researched-word (current-word))
  (setq isearch-string current-researched-word)
  (search-backward (current-word))
  (highlight-selected-word 0)
  (add-hook 'pre-command-hook 'dehighlight-current-word))

(defun repeat-search-current-word-forward()
  "Repeat a forward research using the stored current word (cf: search-current-word-forward / search-current-word-backword)"
  (interactive)
  (search-forward current-researched-word)
  (highlight-selected-word 0)
  (add-hook 'pre-command-hook 'dehighlight-current-word))

(defun repeat-search-current-word-backward()
  "Repeat a forward research using the stored current word (cf: search-current-word-forward / search-current-word-backword)"
  (interactive)
  (search-backward current-researched-word)
  (highlight-selected-word 0)
  (add-hook 'pre-command-hook 'dehighlight-current-word))


;;
;; Use of the isearch interface to highlight the current word
;;

(defun highlight-current-word()
  "Use isearch library to highlight the current word"
  (interactive)
  (dehighlight-selected-word)
  (setq isearch-string (current-word))
  (highlight-selected-word 1)
  (add-hook 'pre-command-hook 'dehighlight-current-word)
  ;(add-local-hook 'pre-command-hook 'isearch-pre-command-hook)
  )

(defun dehighlight-current-word()
  "Hook -- Cancel the previous current-word highlighted"
  (remove-hook 'pre-command-hook 'dehighlight-current-word)
  (sit-for 0)
  (dehighlight-selected-word)
  (pending-delete-mode 1)) ;; For unknown reason, pending-mode is corrupted by this function... linked to the remove-hook call


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

(define-key global-map [(f10)]                'cscope-find-this-symbol-no-prompting-no-updates)
(define-key global-map [(f12)]                'cscope-find-global-definition-no-prompting-no-updates)

(define-key global-map [(control f9)]         'cscope-find-this-text-string-no-updates)
(define-key global-map [(control f10)]        'cscope-find-this-symbol-no-updates)
(define-key global-map [(control f11)]        'cscope-find-global-definition-no-updates)
(define-key global-map [(control f12)]        'cscope-find-functions-calling-this-function-no-updates)

(define-key global-map [(control shift f9)]   'cscope-find-this-text-string)
(define-key global-map [(control shift f10)]  'cscope-find-this-symbol)
(define-key global-map [(control shift f11)]  'cscope-find-global-definition)
(define-key global-map [(control shift f12)]  'cscope-find-functions-calling-this-function)


;;(set-face-foreground 'cscope-line-face  "white")
;(cscope-set-initial-directory "~/.cscope")


;;------------------------------------------------------------------------------
;;
;; Module: spu-mode
;;
;;------------------------------------------------------------------------------

;; Enable autoloading of various major modes
(require 'spu-mode)


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


;;------------------------------------------------------------------------------
;;
;; Module: desktop
;;
;;------------------------------------------------------------------------------

;; Enable auto-save of the emacs session:
;;
;; An initial save must however be done to activate this feature, using:
;;  Meta-X desktop-save 
;(load "desktop")
