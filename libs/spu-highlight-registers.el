;;;; spu-highlight-registers.el -- File to highlight registers of the current instruction in the spu mode.
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
;; When you write SPU Asm code, it could be useful to see where the current register
;; you're using is read or written. This library provides a minor mode to highlight
;; the registers of the current instruction (each with a different color).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(require 'spu-mode)

;;
;; Font definition:
;;

(make-face  'spu-highlight-registers-1)
(set-face-background 'spu-highlight-registers-1 "palegreen1")
(defvar spu-highlight-registers-1 'spu-highlight-registers-1
  "Font to highlight the first register of a SPU line.")

(make-face  'spu-highlight-registers-2)
(set-face-background 'spu-highlight-registers-2 "light sky blue")
(defvar spu-highlight-registers-2 'spu-highlight-registers-2
  "Font to highlight the second register of a SPU line.")

(make-face  'spu-highlight-registers-3)
(set-face-background 'spu-highlight-registers-3 "plum1")
(defvar spu-highlight-registers-3 'spu-highlight-registers-3
  "Font to highlight the third register of a SPU line.")

(make-face  'spu-highlight-registers-4)
(set-face-background 'spu-highlight-registers-4 "darkslategray2")
(defvar spu-highlight-registers-4 'spu-highlight-registers-4
  "Font to highlight the fourth register of a SPU line.")

(make-face  'spu-highlight-registers-5)
(set-face-background 'spu-highlight-registers-5 "gold")
(defvar spu-highlight-registers-5 'spu-highlight-registers-5
  "Font to highlight the fifth register of a SPU line.")

(make-face  'spu-highlight-registers-6)
(set-face-background 'spu-highlight-registers-6 "peru" )
(defvar spu-highlight-registers-6 'spu-highlight-registers-6
  "Font to highlight the sixth register of a SPU line.")

(make-face  'spu-highlight-registers-7)
(set-face-background 'spu-highlight-registers-7 "MediumPurple2")
(defvar spu-highlight-registers-7 'spu-highlight-registers-7
  "Font to highlight the seventh register of a SPU line.")

(make-face  'spu-highlight-registers-8)
(set-face-background 'spu-highlight-registers-8 "medium sea green")
(defvar spu-highlight-registers-8 'spu-highlight-registers-8
  "Font to highlight the eighth register of a SPU line.")

(make-face  'spu-highlight-registers-9)
(set-face-background 'spu-highlight-registers-9 "orchid")
(defvar spu-highlight-registers-9 'spu-highlight-registers-9
  "Font to highlight the ninth register of a SPU line.")

(make-face  'spu-highlight-registers-10)
(set-face-background 'spu-highlight-registers-10 "light steel blue")
(defvar spu-highlight-registers-10 'spu-highlight-registers-10
  "Font to highlight the tenth register of a SPU line.")


;;
;; Global variables:
;;

(setq spu-highlight-registers-font-list
      (list 'spu-highlight-registers-1
	    'spu-highlight-registers-2
	    'spu-highlight-registers-3
	    'spu-highlight-registers-4
	    ;; There is up to 4 arguments to the opcodes, however looking at the register definition, we might
	    ;; encounter more registers... 
	    'spu-highlight-registers-5
	    'spu-highlight-registers-6
	    'spu-highlight-registers-7
	    'spu-highlight-registers-8
	    'spu-highlight-registers-9
	    'spu-highlight-registers-10))

(defvar spu-highlight-registers-line-range -1
  "In order to speed up the process, the line range will specify how many line should be highlighted 
above and below the current line. -1 to narrow to the current page only")

;(setq spu-highlight-registers-overlay-priority 1000)


;;
;; Utility functions:
;;


(defun spu-highlight-register(reg cnt start end)
  "Highlight the occurence of the register REG in the buffer"
  (save-excursion
    (save-match-data
      (goto-char start)
      (while (search-forward-regexp reg end t)
	(let ((ovl (make-overlay (match-beginning 0)
				 (match-end 0))))
	  (overlay-put ovl 'face (nth cnt spu-highlight-registers-font-list))
	  (overlay-put ovl 'spu-highlight-register  t)
	  ;;(overlay-put highlight-current-line-overlay 'priority spu-highlight-registers-overlay-priority)
	  )))))

(defun spu-highlight-register-clear-overlays()
  "Remove all highlighting on registers"
  (let ((ovl-lists (overlay-lists))
	(count 0))
    (setq ovl-lists (list (car ovl-lists) (cdr ovl-lists)))
    (while ovl-lists
      (let ((ovls (pop ovl-lists)))
	(while ovls
	  (if (overlay-get (car ovls) 'spu-highlight-register)
	      (progn (setq count (+ 1 count))
		     (delete-overlay (pop ovls)))
	      (pop ovls)))))
    count))

(defun spu-highlight-register-list(reglist start end)
  "Highlight the register from the list"
  (let ((count 0))
    (spu-highlight-register-clear-overlays)
    (setq reglist (mapcar (lambda (x) (concat "\\<" x "\\>")) reglist))
    (while reglist
      (let ((reg (pop reglist)))
      (if (string-match "\\<[$A-Za-z_][A-Za-z_0-9]*\\>" reg)
	  (progn (spu-highlight-register reg count start end)
		 (setq count (+ count 1))))))))

(defun spu-highlight-registers-in-preprocessor-command(start end)
  "Highlight the register after .reg / .cset / .cuse"
  (let* ((line       (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
	 (clean-line (spu-clean-comments line)))
    (if (string-match "^\\.reg\\>\\|\\.cset\\|\\.cuse" clean-line)
	(let ((reglist (cdr (split-string (subst-char-in-string ?, 32 clean-line)))))
	  (spu-highlight-register-list (remove-duplicates reglist :test 'string= :from-end t) start end))
	(spu-highlight-register-clear-overlays))))

(defun spu-highlight-registers()
  "Highlight the register of the current line."
  (save-restriction
    (let ((start (point-min))
	  (end (point-max)))
      (if spu-highlight-registers-line-range
	  (if (> spu-highlight-registers-line-range 0 )
	      (save-excursion
		(forward-line (- spu-highlight-registers-line-range))
		(setq start (point-at-bol))
		(forward-line (* spu-highlight-registers-line-range 2))
		(setq end (point-at-eol)))
	      (setq start (window-start)
		    end   (window-end))))
      (if (spu-detect-opcodes-line)
	  (spu-highlight-register-list (remove-duplicates (spu-extract-registers) :test 'string= :from-end t) start end)
	  (spu-highlight-registers-in-preprocessor-command start end)))))


(defsubst spu-highlight-registers-post-hook()
  "Post command"
  (let ((case-fold-search nil))
    (spu-highlight-registers)))


;;
;; Defining spu-highlight-registers-minor-mode
;;


(define-minor-mode spu-highlight-registers-mode
  "Highlight the register of the current line."
  :init-value nil
  :global nil
  :lighter " HiRegs"
  ;; Body of the function:
  (make-local-variable 'post-command-hook)
  (if (not spu-highlight-registers-mode)
      (progn (remove-hook 'post-command-hook 'spu-highlight-registers-post-hook)
	     (spu-highlight-register-clear-overlays)
	     )
      (add-hook 'post-command-hook 'spu-highlight-registers-post-hook))
  (when (interactive-p)
    (message "SPU Highlight Registers Mode is now %s."
	     (or (and spu-highlight-registers-mode "Enabled") "Disbaled"))))


(defcustom spu-highlight-registers-mode nil
  "*Non-nil means SPU Highight Register mode is enabled.
In this mode, when the cursor is on some specific line, all registers used in
this line are highlighted.
Setting this variable directly does not change the mode; instead, use
function `spu-highlight-registers-mode'."
  :set (lambda (symbol value) (spu-highlight-registers-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean)

(provide 'spu-highlight-registers)
