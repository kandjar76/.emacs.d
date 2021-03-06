;;;; spu-highlight-stalls.el -- File to highlight the stalls on instruction due to register dependencies.
;;
;; Author: Cedric Lallain <kandjar76@hotmail.com>
;; Version:  1.0
;; Keywords: spu highlight latency
;; Description: File to highlight the stalls on instruction due to register dependencies.
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
;; When you write SPU Asm code, some instructions might stalls due to register
;; dependencies. This library provides function to highlight those dependencies
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'spu-mode)

;;
;; Font definition:
;;

(make-face  'spu-highlight-stalls-1c-font)
(set-face-background 'spu-highlight-stalls-1c-font "#FFCDCD")
(defvar spu-highlight-stalls-1c-font 'spu-highlight-stalls-1c-font
  "Font to highlight the register which are responsible for a stall of 1 cycle.")

(make-face  'spu-highlight-stalls-2c-font)
(set-face-background 'spu-highlight-stalls-2c-font "#FFC0C0")
(defvar spu-highlight-stalls-2c-font 'spu-highlight-stalls-2c-font
  "Font to highlight the register which are responsible for a stall of 2 cycles.")

(make-face  'spu-highlight-stalls-3c-font)
(set-face-background 'spu-highlight-stalls-3c-font "#FFA0A0")
(defvar spu-highlight-stalls-3c-font 'spu-highlight-stalls-3c-font
  "Font to highlight the register which are responsible for a stall of 3 cycles.")

(make-face  'spu-highlight-stalls-4c-font)
(set-face-background 'spu-highlight-stalls-4c-font "#FF8080")
(defvar spu-highlight-stalls-4c-font 'spu-highlight-stalls-4c-font
  "Font to highlight the register which are responsible for a stall of 4 cycles.")

(make-face  'spu-highlight-stalls-5c-font)
(set-face-background 'spu-highlight-stalls-5c-font "#FF4040")
(defvar spu-highlight-stalls-5c-font 'spu-highlight-stalls-5c-font
  "Font to highlight the register which are responsible for a stall of 5 cycles.")

(make-face  'spu-highlight-stalls-6c-font)
(set-face-background 'spu-highlight-stalls-6c-font "#EF0000")
(defvar spu-highlight-stalls-6c-font 'spu-highlight-stalls-6c-font
  "Font to highlight the register which are responsible for a stall of 6 cycles.")

(make-face  'spu-highlight-latency-line-font)
(set-face-background 'spu-highlight-latency-line-font "#FFE0E0")
(defvar spu-highlight-stalls-end-line-font 'spu-highlight-latency-line-line-font
  "Font to highlight the first line which won't stall with the current instruction.")


;;
;; Global variables:
;;

(setq spu-highlight-stalls-font-list
      (list nil
	    'spu-highlight-stalls-1c-font
	    'spu-highlight-stalls-2c-font
	    'spu-highlight-stalls-3c-font
	    'spu-highlight-stalls-4c-font
	    'spu-highlight-stalls-5c-font
	    'spu-highlight-stalls-6c-font))

(defvar spu-highlight-latency-highlight-line t
  "Defined how to highlight the latency issue, t means, it will highlight the first stall free 
line for the current instruction, where if set to nil, it will highlight all lines which will
cause a stall")


;;
;;
;;    SPU command:
;;
;;




(defun spu-get-read-registers(regline)
  "Returns the list of register which are going to be read by the opcode"
  (if (or (member (car regline) spu-st-opcodes)
	  (member (car regline) spu-br-opcodes))
      (cdr regline)
      (cddr regline)))

;(spu-get-read-registers '("a" "df" "te"))
;(spu-get-read-registers '("nop"))


(defun spu-get-written-register(regline)
  "Returns the register which going to get written in"
  (if (not (or (member (car regline) spu-st-opcodes)
	       (member (car regline) spu-br-opcodes)))
      (cadr regline)
      nil))

(defun spu-get-opcode-latency(regline)
  "Returns the cycle count of the current opcode"
  (spu-cycle-count (car regline)))



(defun spu-decrease-latency(reglist dec)
  "Decrement the ready-count of each register in REGLIST by DEC, if the ready-count becomes less 
than or equal to 0, the register is removed from the list."
  (remove-if (lambda(x) (<= (cdr x) 0))
	     (mapcar (lambda (x) (cons (car x) (- (cdr x) dec)))
		     reglist)))

(defun spu-check-stalls(reg reglist)
  "Check if REG belong to the reglist. Returns the couple, register / stall count or nil"
  (let ((found (remove-if (lambda(x) (not (string= reg (car x))))
			  reglist)))
    (if found (car found))))

;(spu-check-stalls "cd" '(("ab" . 2) ("cd" . 5) ("ef" . 1)))
;(spu-check-stalls "gh" '(("ab" . 2) ("cd" . 5) ("ef" . 1)))


(defun spu-check-regs(instr reglist)
  "Check if any register will stall the execution of the current opcode
Returns a list of opcode / stalling time"
  (let ((regs (spu-get-read-registers instr)))
    (if regs
	(remove-if (lambda(x) (not x))
		   (mapcar (lambda (x) (spu-check-stalls x reglist))
			   regs)))))

;(spu-check-regs '("a" "test" "cd" "gh" "ef") '(("ab" . 2) ("cd" . 5) ("ef" . 1)))
;(spu-check-regs '("a" "test" "cd" "gh" "ef") '())

(defun spu-get-stalling-cycles(stalls)
  "Return the max stalling value"
  (reduce (lambda (x y) (if (> x (cdr y)) x (cdr y)))
	  stalls
	  :initial-value 0))


(defun spu-add-reg-to-list(instr reglist)
  "Add the written register to the list... if it's already present, his ready count gets updated"
  (let ((regw (spu-get-written-register instr))
	(lat  (spu-get-opcode-latency instr))
	(updl reglist))
    (if regw 
	(progn (setq updl (remove-if (lambda(x) (string= (car x) regw)) updl))
	       (append (list (cons regw lat)) updl))
	reglist)))
;(spu-add-reg-to-list '("a" "test" "cd" "gh" "ef") '(("ab" . 2) ("cd" . 5) ("ef" . 1)))
;(spu-add-reg-to-list '("a" "test" "cd" "gh" "ef") nil)
;(spu-add-reg-to-list '("a" "test" "cd" "gh" "ef") '(("ab" . 2) ("test" . 1) ("ef" . 1)))



(defun spu-highlight-stalls(stalls start end)
  "Highlight the stalling registers between START and END"
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (save-match-data
	(goto-char (point-min))
	(while stalls
	  (let* ((reg-data (pop stalls))
		 (reg      (car reg-data))
		 (lat      (cdr reg-data))
		 (lat-str  (format "Stalls for %i cycles" lat)))
	    (while (search-forward-regexp reg nil t)
	      (let ((ovl (make-overlay (match-beginning 0)
				       (match-end 0))))
		(overlay-put ovl 'face (nth (min lat 6) spu-highlight-stalls-font-list))
		(overlay-put ovl 'spu-highlight-stalls  t)
		;;(overlay-put highlight-current-line-overlay 'priority spu-highlight-registers-overlay-priority)
		(overlay-put ovl 'help-echo lat-str))
		)))))
  (add-hook 'pre-command-hook 'spu-highlight-stalls-clear-overlays)))


(defun spu-highlight-stalls-clear-overlays()
  "Remove all highlighting on registers"
  (remove-hook 'pre-command-hook 'spu-highlight-stalls-clear-overlays)
  (sit-for 0)
  (let ((ovl-lists (overlay-lists))
	(count 0))
    (setq ovl-lists (list (car ovl-lists) (cdr ovl-lists)))
    (while ovl-lists
      (let ((ovls (pop ovl-lists)))
	(while ovls
	  (if (overlay-get (car ovls) 'spu-highlight-stalls)
	      (progn (setq count (+ 1 count))
		     (delete-overlay (pop ovls)))
	      (pop ovls)))))
    (pending-delete-mode 1) ;; For unknown reason, pending-mode is corrupted by this function... linked to the remove-hook call
    count))



(defun spu-highlight-stalling-registers(start end &optional initial-reglist)
  "Highlight the register which create a stall between START and END."
  (let ((reglist initial-reglist)
	(stalls-count 0))
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (while (and (bolp)
		  (not (bobp))
		  (< (point) end))
	(if (spu-detect-opcodes-line)
	    (progn (setq reglist (spu-decrease-latency reglist 1))
		   (let ((even (spu-find-even-opcode))
			 (odd  (spu-find-odd-opcode)))
		     (if even
			 (progn (move-to-column even)
				(let* ((instr (spu-extract-instruction))
				       (stalls (spu-check-regs instr reglist)))
				  (setq stalls-count (+ stalls-count (length stalls)))
				  (spu-highlight-stalls stalls (point-at-bol) (or (and odd (spu-column-to-pos odd)) (point-at-eol)))
				  (setq reglist (spu-decrease-latency reglist (spu-get-stalling-cycles stalls)))
				  (setq reglist (spu-add-reg-to-list instr reglist)))))
		     (if odd
			 (progn (move-to-column odd)
				(let* ((instr (spu-extract-instruction))
				       (stalls (spu-check-regs instr reglist)))
				  (setq stalls-count (+ stalls-count (length stalls)))
				  (spu-highlight-stalls stalls (spu-column-to-pos odd) (point-at-eol))
				  (setq reglist (spu-decrease-latency reglist (spu-get-stalling-cycles stalls)))
				  (setq reglist (spu-add-reg-to-list instr reglist))))))))
	(forward-line 1)))
    stalls-count))

(defun spu-stall-check()
  (interactive)
  (let ((stalls-count 0))
    (if (not mark-active)
	(message "No region selected")
	(let ((start (region-beginning))
	      (end   (region-end)))
	  (if ( > start end )
	      (let (tmp) (setq tmp end end start start tmp)))
	  (deactivate-mark)
	  (setq stalls-count (spu-highlight-stalling-registers start end))
	  (if (> stalls-count 0)
	      (message (format "%i stalling registers found." stalls-count))
	      (message "No stalls found"))))))

	  
(defun spu-nopify(start end &optional initial-reglist)
  "Insert {nop} / {lnop} lines in the selected region to compensate stalls"
  (interactive "r")
  (let* ((ind-cols (spu-compute-indent)) ;; we may want do deactivate the region before calling this function... :)
	 (spu-even-column (or (car ind-cols) spu-even-column))
	 (spu-odd-column  (or (cdr ind-cols) spu-odd-column))
	 (reglist initial-reglist)
	 (stalls-count 0))
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (while (and (bolp)
		  (not (bobp))
		  (< (point) end))
	(when (spu-detect-opcodes-line)
	  (setq reglist (spu-decrease-latency reglist 1))
	  (let ((even (spu-find-even-opcode))
		(odd  (spu-find-odd-opcode)))
	    (when even
		(move-to-column even)
		(let* ((instr (spu-extract-instruction))
		       (stalls (spu-get-stalling-cycles (spu-check-regs instr reglist)))
		       current)
		  (setq reglist (spu-decrease-latency reglist stalls))
		  (setq reglist (spu-add-reg-to-list instr reglist))
		  (end-of-line)
		  (setq current (point-at-bol))
		  (save-excursion
		    (beginning-of-line)
		    (while (> stalls 0)
		      (newline)
		      (forward-line -1)
		      (indent-to-column spu-even-column)
		      (insert "{nop}")
		      (indent-to-column spu-odd-column)
		      (insert "{lnop}")
		      (beginning-of-line)
		      (setq stalls (1- stalls))))
		  (beginning-of-line)
		  (setq end (+ end (- (point) current)))
		  (setq stalls-count (+ stalls-count stalls))))
	    (when odd
	      (progn (move-to-column odd)
		(let* ((instr (spu-extract-instruction))
		       (stalls (spu-get-stalling-cycles (spu-check-regs instr reglist)))
		       current)
		  (setq reglist (spu-decrease-latency reglist stalls))
		  (setq reglist (spu-add-reg-to-list instr reglist))
		  (end-of-line)
		  (setq current (point-at-bol))
		  (save-excursion
		    (when (> stalls 0)
		      ;; First split the line:
		      (let* ((comment (spu-find-comment-before-opcode odd))
			     (end-of-even (or (and comment (car comment)) (spu-column-to-pos odd))))
			(goto-char end-of-even)
			(newline)
			(indent-to-column spu-even-column)
			(insert "{nop}")
			(indent-to-column (or (and comment (- spu-odd-column spu-comment-size))
					      spu-odd-column))
			(forward-line -1)
			(end-of-line)
			(indent-to-column spu-odd-column)
			(insert "{lnop}")
			(forward-line 1)
			(beginning-of-line)
			(setq stalls (1- stalls)))
		      (while (> stalls 0) 
			(newline)
			(forward-line -1)
			(indent-to-column spu-even-column)
			(insert "{nop}")
			(indent-to-column spu-odd-column)
			(insert "{lnop}")
			(beginning-of-line)
			(setq stalls (1- stalls)))))
		  (beginning-of-line)
		  (setq end (+ end (- (point) current)))
		  (setq stalls-count (+ stalls-count stalls)))))))
	(forward-line 1)))
    stalls-count))


;;
;; Minor mode to highlight the next available line...
;;

(defun spu-highlight-latency-clear-overlays()
  "Remove the overlay off the latency line"
  (let ((ovl-lists (overlay-lists))
	(count 0))
    (setq ovl-lists (list (car ovl-lists) (cdr ovl-lists)))
    (while ovl-lists
      (let ((ovls (pop ovl-lists)))
	(while ovls
	  (if (overlay-get (car ovls) 'spu-highlight-latency-line)
	      (progn (setq count (+ 1 count))
		     (delete-overlay (pop ovls)))
	      (pop ovls)))))
    count))

(defun spu-highlight-latency-stallfree-line()
  (spu-highlight-latency-clear-overlays)
  (save-excursion
    (if (spu-detect-opcodes-line)
	(let ((instr-latency (spu-get-opcode-latency (spu-extract-instruction))))
	  (when (> instr-latency 1)
	    (while (and (> instr-latency 0)
			(spu-move-down))
	      (setq instr-latency (1- instr-latency)))
	    (when (= instr-latency 0)
	      ;; Create the overlay
	      (let ((ovl (make-overlay (point-at-bol) (1+ (point-at-eol)))))
		(overlay-put ovl 'face 'spu-highlight-latency-line-font)
		(overlay-put ovl 'spu-highlight-latency-line  t))))))))

(defun spu-highlight-latency-stall-lines()
  (spu-highlight-latency-clear-overlays)
  (save-excursion
    (if (spu-detect-opcodes-line)
	(let ((instr-latency (spu-get-opcode-latency (spu-extract-instruction))))
	  (when (> instr-latency 1)
	    (setq instr-latency (1- instr-latency))
	    (while (and (> instr-latency 0)
			(spu-move-down))
	      (let ((ovl (make-overlay (point-at-bol) (1+ (point-at-eol)))))
		(overlay-put ovl 'face 'spu-highlight-latency-line-font)
		(overlay-put ovl 'spu-highlight-latency-line  t))
	      (setq instr-latency (1- instr-latency))))))))


(defun spu-highlight-latency-post-hook()
  "Post command"
  (interactive)
  (if spu-highlight-latency-highlight-line
   (spu-highlight-latency-stallfree-line)
   (spu-highlight-latency-stall-lines)))

(define-minor-mode spu-highlight-latency-mode
  "Highlight the line which won't stall after running the current instruction 
 (assuming this will be the one causing the stall)."
  :init-value nil
  :global nil
  :lighter " HiStall"
  ;; Body of the function:
  (make-local-variable 'post-command-hook)
  (if (not spu-highlight-latency-mode)
      (progn (remove-hook 'post-command-hook 'spu-highlight-latency-post-hook)
	     (spu-highlight-latency-clear-overlays))
      (add-hook 'post-command-hook 'spu-highlight-latency-post-hook))
  (when (interactive-p)
    (message "SPU Highlight Stall Line Mode is now %s."
	     (or (and spu-highlight-latency-mode "Enabled") "Disbaled"))))

(defcustom spu-highlight-latency-mode nil
  "*Non-nil means SPU Highight Latency mode is enabled.
In this mode, when the cursor is on some specific line, all registers used in
this line are highlighted.
Setting this variable directly does not change the mode; instead, use
function `spu-highlight-latency-mode'."
  :set (lambda (symbol value) (spu-highlight-latency-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'spu)


(provide 'spu-highlight-stalls)