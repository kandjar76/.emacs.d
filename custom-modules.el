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
;; Module: redo
;;
;;------------------------------------------------------------------------------

;; This module enable a redo function which is the exact opposite of the well
;; known undo function

(require 'redo)

;;------------------------------------------------------------------------------
;;
;; Module: Bookmark
;;
;;------------------------------------------------------------------------------

;; This library will emulate the visual studio's bookmark

(require 'bm)

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
(add-hook 'iswitchb-define-mode-map-hook
	    '(lambda ()
	       (define-key iswitchb-mode-map " " 'iswitchb-next-match)
	       (define-key iswitchb-mode-map [del] 'iswitchb-prev-match)
	       (define-key iswitchb-mode-map [bs] 'iswitchb-prev-match)
	       (define-key iswitchb-mode-map [right] 'iswitchb-next-match)
	       (define-key iswitchb-mode-map [left] 'iswitchb-prev-match)))
(icomplete-mode 1)

(defadvice iswitchb-visit-buffer (after iswitchb-visit-buffer(buffer))
  (message buffer-file-name))
(ad-activate 'iswitchb-visit-buffer)

;;------------------------------------------------------------------------------
;;
;; Module: cscope
;;
;;------------------------------------------------------------------------------

(require 'xcscope+)
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
(put 'dired-find-alternate-file 'disabled nil)



;;------------------------------------------------------------------------------
;;
;; Module: find-dired++
;;
;;------------------------------------------------------------------------------

(require 'find-dired++)

;;------------------------------------------------------------------------------
;;
;; Module: suggbind
;;
;;------------------------------------------------------------------------------

;; Turn off builtin pre-command hints, and enable a much-improved version.
(setq suggest-key-bindings nil)
(load "suggbind")


