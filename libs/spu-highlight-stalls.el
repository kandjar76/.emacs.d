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

(make-face  'spu-highlight-stalls-font)
(set-face-background 'spu-highlight-stalls-font (first-valid-color "orange red" "red"))
(defvar spu-highlight-stalls-font 'spu-highlight-stalls-font
  "Font to highlight the register which are responsible for a stall.")


;;
;;
;;    SPU command:
;;
;;


(defun spu-get-read-registers(regline)
  "Returns the list of register which are going to be read by the opcode"
  (if (member (car regline) spu-st-opcodes)
      (cdr regline)
      (cddr regline)))

;(spu-get-read-registers '("a" "df" "te"))
;(spu-get-read-registers '("nop"))


(defun spu-get-write-registers(regline)
  "Returns the register which going to get written in"
  (if (member (car regline) spu-st-opcodes)
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


(defun spu-check-regs(regs-line reglist)
  "Check if any register will stall the execution of the current opcode
Returns a list of opcode / stalling time"
  (let ((regs (spu-get-read-registers regs-line)))
    (if regs
	(remove-if (lambda(x) (not x))
		   (mapcar (lambda (x) (spu-check-stall x reglist))
			   regs)))))

;(spu-check-regs '("a" "test" "cd" "gh" "ef") '(("ab" . 2) ("cd" . 5) ("ef" . 1)))
;(spu-check-regs '("a" "test" "cd" "gh" "ef") '())

(defun spu-extract-stalling-register(reglist column)
  "Read the instruction on the current line at the specify columb, 
and return a list of register which are going to make the instruction
stalls."
  (save-excursion
    (move-to-column column)
    (spu-check-stalls (spu-extract-instruction) reglist)))


(defun spu-highlight-register(reg cnt)
  "Highlight the occurence of the register REG in the buffer"
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (search-forward-regexp reg nil t)
	(let ((ovl (make-overlay (match-beginning 0)
				 (match-end 0)))
	      (def (and spu-highlight-registers-with-definition-lookup
			(or (spu-find-register-definition reg)
			    "** Not defined **"))))
	  (overlay-put ovl 'face (nth cnt spu-highlight-registers-font-list))
	  (overlay-put ovl 'spu-highlight-register  t)
	  ;;(overlay-put highlight-current-line-overlay 'priority spu-highlight-registers-overlay-priority)
	  (if def (overlay-put ovl 'help-echo def))
	  )))))

(defun spu-get-stalling-cycles(stalls)
  "Return the max stalling value"
  (reduce (lambda (x y) (if (> x (cdr y)) x (cdr y)))
	  stalls
	  :initial-value 0))



(setq tst nil)
(defun spu-highlight-stalls(stalls)
  (setq tst (cons (list stalls) tst)))
  


(defun spu-highlight-stalling-register(&optional initial-reglist)
  "Highlight the register which create a stall."
  (interactive "*")
  (let ((reglist initial-reglist))
    (if (not (is-region-active))
	(message "No region selected")
	(let ((start (region-beginning))
	      (end   (region-end)))
	  (if ( > start end )
	      (let (tmp) (setq tmp end end start start tmp)))
	  (save-excursion
	    (goto-char start)
	    (beginning-of-line)
	    (while (and (bolp)
			(not (bobp))
			(< (point) end))
		(if (spu-detect-opcodes-line)
		    (progn (setq reglist (spu-decrease-latency reglist 1))
			   (save-excursion
			     (let ((even (spu-find-even-opcode))
				   (odd  (spu-find-odd-opcode)))
			       (if even
				   (progn (move-to-column column)
					  (let ((instr (spu-extract-instruction))
						(stalls (spu-check-stalls instr reglist)))
					    (spu-highlight-stalls stalls)
					    (setq reglist (spu-decrease-latency reglist (spu-get-stalling-cycles)))
					    (setq reglist (spu-highlight-add-reg-to-list instr reglist))))