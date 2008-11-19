;;;; highlight-current-word.el -- File to highlight the current word.
;;
;; Author: Cedric Lallain <kandjar76@hotmail.com>
;; Version:  1.0
;; Keywords: highlight current word
;; Description: File to highlight the current word
;; Tested with: GNU Emacs 21.x and GNU Emacs 22.x
;;
;; This file is *NOT* part of GNU Emacs.
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Highlight the current word
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;
;; Font definition:
;;

(make-face  'highlight-current-word-font)
(set-face-background 'highlight-current-word-font (first-valid-color "lightskyblue2" "thistle1"  ))
(defvar highlight-current-word-font 'highlight-current-word-font
  "Font to highlight the current word.")

(defvar highlight-current-word-line-range -1
  "In order to speed up the process, the line range will specify how many line should be highlighted 
above and below the current line. -1 to narrow to the current page only")


;;
;; Utility functions:
;;

(defun highlight-regexp-in-range(regexp start end font tag &optional priority)
  "Highlight the occurence using FONT of the regular expression REGEXP in the current buffer between START and END; 
also tag the overlays with TAG. In order to clear it with clear-tagged-overlays. The PRIORITY is optional."
  (save-excursion
    (save-match-data
      (goto-char start)
      (while (search-forward-regexp regexp end t)
	(let ((ovl (make-overlay (match-beginning 0)
				 (match-end 0))))
	  (overlay-put ovl 'face font)
	  (overlay-put ovl tag t)
	  (if priority
	      (overlay-put highlight-current-line-overlay 'priority priority))
	  )))))

(defun clear-tagged-overlays(tag)
  "Remove all overlays in the current buffer tagged with TAG;
Returns how many overlays has been found."
  (let ((ovl-lists (overlay-lists))
	(count 0))
    (setq ovl-lists (list (car ovl-lists) (cdr ovl-lists)))
    (while ovl-lists
      (let ((ovls (pop ovl-lists)))
	(while ovls
	  (if (overlay-get (car ovls) tag)
	      (progn (setq count (+ 1 count))
		     (delete-overlay (pop ovls)))
	      (pop ovls)))))
    count))



(defun highlight-current-word-in-range()
  "Highlight the current word in the requested range."
  (save-restriction
    (when (length (current-word) > 0)
      (let ((start (point-min))
	    (end (point-max)))
	(if highlight-current-word-line-range
	    (if (> highlight-current-word-line-range 0 )
		(save-excursion
		  (forward-line (- highlight-current-word-line-range))
		  (setq start (point-at-bol))
		  (forward-line (* highlight-current-word-line-range 2))
		  (setq end (point-at-eol)))
		(setq start (window-start)
		      end   (window-end))))
	(highlight-regexp-in-range (concat "\\<" (regexp-quote (current-word)) "\\>")
				   start end
				   highlight-current-word-font
				   'highlight-current-word-tag)
      ))))


(defsubst highlight-current-word-post-hook()
  "Post command"
  (let ((case-fold-search nil))
    (clear-tagged-overlays 'highlight-current-word-tag)
    (highlight-current-word-in-range)))


;;
;; Defining highlight-current-word-minor-mode
;;


(define-minor-mode highlight-current-word-mode
  "Highlight the current word."
  :init-value nil
  :global nil
  :lighter " HiCurWord"
  ;; Body of the function:
  (make-local-variable 'post-command-hook)
  (if (not highlight-current-word-mode)
      (progn (remove-hook 'post-command-hook 'highlight-current-word-post-hook)
	     (clear-tagged-overlays 'highlight-current-word-tag))
      (add-hook 'post-command-hook 'highlight-current-word-post-hook))
  (when (interactive-p)
    (message "Highlight Current Word Mode is now %s."
	     (or (and highlight-current-word-mode "Enabled") "Disbaled"))))


(defcustom highlight-current-word-mode nil
  "*Non-nil means Highight Current Word  mode is enabled.
In this mode, the current word is highlighted.
Setting this variable directly does not change the mode; instead, use
function `highlight-current-word-mode'."
  :set (lambda (symbol value) (highlight-current-word-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean)

(provide 'highlight-current-word)
