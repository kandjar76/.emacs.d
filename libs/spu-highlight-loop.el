;;;; spu-highlight-loop.el -- File to highlight registers of the current instruction in the spu mode.
;;
;; Author: Cedric Lallain <kandjar76@hotmail.com>
;; Version:  1.0
;; Keywords: spu highlight minor mode opcode
;; Description: File to highlight registers of the current instruction in the spu mode.
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
;; Navigating through rolled up loop can be difficult so view what belong to each
;; iteration of the loop; this script grayed out the instructions which doesn't 
;; belong to the iteration of the current instruction
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(require 'spu-mode)

;;
;; Font definition:
;;

(make-face  'spu-highlight-loop-inactive-line)
(set-face-foreground 'spu-highlight-loop-inactive-line "gray90")
(defvar spu-highlight-loop-inactive-line 'spu-highlight-loop-inactive-line
  "Font to highlight the inactive line in the loop code.")

(make-face  'spu-highlight-loop-previous-index-line)
(set-face-foreground 'spu-highlight-loop-previous-index-line "gray")
(defvar spu-highlight-loop-previous-index-line 'spu-highlight-loop-previous-index-line
  "Font to highlight the inactive line in the loop code.")

;;
;; Global variables:
;;

;; Define the key mapping for the spu mode:
(defvar spu-highlight-loop-mode-map
  (let ((spu-highlight-loop-mode-map (make-keymap)))
    (define-key spu-highlight-loop-mode-map [(up)]    'spu-hl-move-up)
    (define-key spu-highlight-loop-mode-map [(down)]  'spu-hl-move-down)
    (define-key spu-highlight-loop-mode-map [(left)]  'spu-hl-move-left)
    (define-key spu-highlight-loop-mode-map [(right)] 'spu-hl-move-right)
    (define-key spu-highlight-loop-mode-map [?q]      'spu-highlight-loop-mode-quit)
    spu-highlight-loop-mode-map))

;(setq spu-highlight-registers-overlay-priority 1000)


;;
;; Utility functions:
;;

(defun spu-highlight-loop-extract-loop-index(column)
  "Extract the latency value of the current instruction -- -1 no latency found"
  (let* ((comment-points (spu-find-comment-before-opcode column))
	 (comment        (and comment-points (buffer-substring-no-properties (car comment-points) (cdr comment-points))))
	 (has-loopndx    (and comment (string-match "^{[eo][0-9?] [\-0-9]}" comment)))
	 (loopndx-pos    (and comment (- (length comment) 2))))
    (if has-loopndx
	(let ((loopndx-string (substring comment loopndx-pos (1+ loopndx-pos))))
	  (if (string-match "[0-9]" loopndx-string)
	      (string-to-int loopndx-string)
	      -1))
	-1)))


(defun spu-highlight-loop-clear-overlays()
  "Remove all highlighting on loop lines"
  (let ((ovl-lists (overlay-lists))
	(count 0))
    (setq ovl-lists (list (car ovl-lists) (cdr ovl-lists)))
    (while ovl-lists
      (let ((ovls (pop ovl-lists)))
	(while ovls
	  (if (overlay-get (car ovls) 'spu-highlight-loop)
	      (progn (setq count (+ 1 count))
		     (delete-overlay (pop ovls)))
	      (pop ovls)))))
    count))
  

(defun spu-highlight-loop-update-overlays-region(start end loopndx)
  "Gray the loop line with a loop index different from LOOPNDX in
the buffer's region delimited by the range [START-END]"
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (while (and (<= (point) end)
		(not (eobp)))
      (when (and (not (spu-detect-no-opcodes-line))
		 (spu-detect-opcodes-line))
	(save-excursion
	  (beginning-of-line)
	  (let* ((evencol (spu-find-even-opcode))
		 (oddcol  (spu-find-odd-opcode))
		 (oddbeg  (or (and oddcol
				   (car (spu-find-comment-before-opcode oddcol)))
			      (and oddcol (spu-column-to-pos oddcol))
			      (point-at-eol)))
		 (evenend (or (and evencol oddbeg) (point-at-bol)))
		 (evenndx (and evencol (spu-highlight-loop-extract-loop-index evencol)))
		 (oddndx  (and oddcol  (spu-highlight-loop-extract-loop-index oddcol))))
	    (when (and evencol (/= evenndx loopndx))
	      ;; Gray the even instruction...
	      (let ((ovl (make-overlay (point-at-bol) oddbeg)))
		(if (= (1+ evenndx) loopndx)
		    (overlay-put ovl 'face 'spu-highlight-loop-previous-index-line)
		    (overlay-put ovl 'face 'spu-highlight-loop-inactive-line))
		(overlay-put ovl 'spu-highlight-loop t)))
	    (when (and oddcol (/= oddndx loopndx))
	      ;; Gray the even instruction...
	      (let ((ovl (make-overlay evenend (point-at-eol))))
		(if (= (1+ oddndx) loopndx)
		    (overlay-put ovl 'face 'spu-highlight-loop-previous-index-line)
		    (overlay-put ovl 'face 'spu-highlight-loop-inactive-line))
		(overlay-put ovl 'spu-highlight-loop t)))
	    )))
      (forward-line 1))))

(defun spu-highlight-loop-screen-update()
  (spu-highlight-loop-clear-overlays)
  (spu-highlight-loop-update-overlays-region (window-start)
					     (window-end)
					     (spu-highlight-loop-extract-loop-index (current-column))))

;;
;;
;;    SPU Interactive command:
;;
;;


(defun spu-highlight-loop-init()
  (interactive)
  (let* ((evencol (spu-find-even-opcode))
	 (oddcol  (spu-find-odd-opcode)))
    (if oddcol
	(if (>= (current-column) oddcol)
	    (progn (move-to-column oddcol)
		   (spu-highlight-loop-screen-update))
	    (if evencol
		(progn (move-to-column evencol)
		       (spu-highlight-loop-screen-update))))
	(if evencol
	    (progn (move-to-column evencol)
		   (spu-highlight-loop-screen-update))))))
	 

(defun spu-hl-move-down()
  (interactive)
  (spu-move-down)
  (spu-highlight-loop-screen-update))

(defun spu-hl-move-up()
  (interactive)
  (spu-move-up)
  (spu-highlight-loop-screen-update))

(defun spu-hl-move-left()
  (interactive)
  (spu-move-left)
  (spu-highlight-loop-screen-update))

(defun spu-hl-move-right()
  "Move furthest right opcode of the current line"
  (interactive)
  (spu-move-right)
  (spu-highlight-loop-screen-update))



(defun spu-highlight-loop-mode-quit()
  "Quit the highlight-loop-mode if currently active"
  (interactive)
  (if spu-highlight-loop-mode
      (spu-highlight-loop-mode)))

;;(defsubst spu-highlight-loop-post-hook()
;;  "Post command"
;;  (let ((case-fold-search nil))
;;    (spu-highlight-registers)))

;;
;; Defining spu-highlight-registers-minor-mode
;;


(define-minor-mode spu-highlight-loop-mode
  "Highlight the register of the current line."
  :init-value nil
  :global nil
  :lighter " LoopNav"
  :keymap spu-highlight-loop-mode-map
  ;; Body of the function:
;  (make-local-variable 'post-command-hook)
  (if spu-highlight-loop-mode
      (spu-highlight-loop-init)
      (spu-highlight-loop-clear-overlays))

;      (progn (remove-hook 'post-command-hook 'spu-highlight-loop-post-hook)
; 	     ;(spu-highlight-register-clear-overlays)
; 	     )
;      (add-hook 'post-command-hook 'spu-highlight-loop-post-hook))
  (when (interactive-p)
    (message "SPU Highlight Loop Mode is now %s."
 	     (or (and spu-highlight-loop-mode "Enabled") "Disbaled"))))
 
 
(defcustom spu-highlight-loop-mode nil
  "*Non-nil means SPU Highight Loop mode is enabled.
This mode allow the used to navigate within a rolled-up loop
Setting this variable directly does not change the mode; instead, use
function `spu-highlight-loop-mode'."
   :set (lambda (symbol value) (spu-highlight-loop-mode (or value 0)))
   :initialize 'custom-initialize-default
   :type 'boolean)
 
(provide 'spu-highlight-loop)
