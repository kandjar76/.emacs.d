;;;; increment-numbers.el -- Set of tools to increment numbers within a region
;;
;; Author: Cedric Lallain <kandjar76@hotmail.com>
;; Version:  1.0
;; Keywords: increment number
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
;; Very often I had to increment a set of number within lines or columns;
;; For example, you want to test something in C and write:
;;  v[0] = va[0] + vb[0];
;;  v[1] = va[1] + vb[1];
;;  v[2] = va[2] + vb[2];
;;
;; What you could do is:
;;  Write the first line, dupplicated it 3 times as followed:
;;   v[0] = va[0] + vb[0];
;;   v[0] = va[0] + vb[0];
;;   v[0] = va[0] + vb[0];
;;  Select the 3 lines, and call: increment-number-multilines
;;
;; Some of the following function will work on a rectangle instead of a whole
;; line. They all recognize hexadecimal number and will do proper increment.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun detect-decimal-or-hexa-number()
  "PRIVATE - Return a list with the min and max value surrounding a number
Known issue: the detection of the hexadecimal number is not 100% accurate, if this come to be a problem
the code can be changed"
  (let ((start (point))
	end)
    (save-excursion
      (if (and (= (char-after start) ?0)
	       (eq (char-after (1+ start)) ?x) ;; Most of the time people don't use X to represent a hexa number. 
	       (and (char-after (+ start 2))
		    (string-match "[0-9A-Fa-f]" (char-to-string (char-after (+ start 2))))))
	  ;; Chance are: it's an hexa decimal number
	  (progn (goto-char (+ start 2))
		 (setq end (search-forward-regexp "[0-9A-Fa-f]+")))
	  (setq end (search-forward-regexp "[0-9]+")))
      )
    (cons start end)))


(defun increase-numbers-on-current-line(colstart colend incr)
  "PRIVATE - Increase the number found on the current line between the column [COLSTART; COLEND] by INCR"
  (save-restriction
    (let (bor)
      (move-to-column colstart)
      (setq bor (point))
      (move-to-column colend)
      (narrow-to-region bor (point))
      (goto-char (point-min))
      (while (search-forward-regexp "[0123456789]" nil t)
	(backward-char)
	(let* ((number-region (detect-decimal-or-hexa-number))
	       (start         (car number-region))
	       (end           (cdr number-region))
	       (lgt           (- end start))
	       (hex-number    (and (> lgt 1) (= (char-after (+ start 1)) ?x)))
	       (hex-prefix    (or (and hex-number 2) 0))
	       (str           (buffer-substring (+ start hex-prefix) end))
	       (new-number    (+ incr (string-to-number str (and hex-number 16))))
	       new-string)
	  (if hex-number
	      (setq new-string (concat "0x" (format (concat "%0" (number-to-string (- lgt 2)) "X") new-number)))
	      (setq new-string (format (concat "%0" (number-to-string lgt) "d") new-number)))
	  (delete-region start end)
	  (goto-char start)
	  (insert new-string))))))


;;;###autoload
(defun increment-numbers-multilines()
  "Increment the value on each lines"
  (interactive "*")
  (if mark-active
      (save-excursion
	(save-match-data
	  (let* ((region (extend-region-to-full-lines (region-beginning) (region-end)))
		 (lines  (count-lines (car region) (cdr region)))
		 (line-count 0))
	    (goto-char (car region))
	    (while (< line-count lines)
	      (save-restriction
		(let (bol eol)
		  (beginning-of-line)
		  (setq bol (point))
		  (end-of-line)
		  (setq eol (point))
		  (narrow-to-region bol eol)
		  (goto-char (point-min))
		  (while (search-forward-regexp "[0123456789]" nil t)
		    (backward-char)
		    (let* ((number-region (detect-decimal-or-hexa-number))
			   (start         (car number-region))
			   (end           (cdr number-region))
			   (lgt           (- end start))
			   (hex-number    (and (> lgt 1) (= (char-after (+ start 1)) ?x)))
			   (str           (buffer-substring (+ start (or (and hex-number 2) 0)) end))
			   (new-number    (+ line-count (string-to-number str (and hex-number 16))))
			   new-string)
		      (if hex-number
			  (setq new-string (concat "0x" (format (concat "%0" (number-to-string (- lgt 2)) "X") new-number)))
			  (setq new-string (format (concat "%0" (number-to-string lgt) "d") new-number)))
		      (delete-region start end)
		      (goto-char start)
		      (insert new-string)))))
		(forward-line 1)
		(setq line-count (1+ line-count))))))
      (message "There is no active region!")))


;;;###autoload
(defun increment-numbers-region(&optional arg)
  "Increment each number in the selected region by 1 or by the value of the prefix argument"
  (interactive "*p")
  (if mark-active
      (save-excursion
	(save-match-data
	  (let* ((incr (or arg 1))
		 (region (cons (region-beginning) (region-end)))
		 (lines  (count-lines (car region) (cdr region)))
		 (line-count 0))
	    (goto-char (car region))
	    (while (< line-count lines)
	      (save-restriction
		(let (bol eol)
		  (beginning-of-line)
		  (setq bol (point))
		  (end-of-line)
		  (setq eol (point))
		  (narrow-to-region bol eol)
		  (goto-char (point-min))
		  (while (search-forward-regexp "[0123456789]" nil t)
		    (backward-char)
		    (let* ((number-region (detect-decimal-or-hexa-number))
			   (start         (car number-region))
			   (end           (cdr number-region))
			   (lgt           (- end start))
			   (hex-number    (and (> lgt 1) (= (char-after (+ start 1)) ?x)))
			   (hex-prefix    (or (and hex-number 2) 0))
			   (str           (buffer-substring (+ start hex-prefix) end))
			   (new-number    (+ incr (string-to-number str (and hex-number 16))))
			   new-string)
		      (if hex-number
			  (setq new-string (concat "0x" (format (concat "%0" (number-to-string (- lgt 2)) "X") new-number)))
			  (setq new-string (format (concat "%0" (number-to-string lgt) "d") new-number)))
		      (delete-region start end)
		      (goto-char start)
		      (insert new-string)))))
		(forward-line 1)
		(setq line-count (1+ line-count))))))
      (message "There is no active region!")))

;;;###autoload
(defun increase-numbers-on-rectangle(start end &optional count)
  "Increment each number in the selected rectangle depending on the prefix argument COUNT:
if COUNT is integer value, each number is increased by COUNT
if COUNT is a simple prefix value (C-U), each number is increased by 1
if COUNT is nil, each number is increased by line number within the selection (starting at 0)"
  (interactive "*r\nP")
  (save-excursion
    (save-match-data
      (let ((line-ndx   0)
	    (line-count (count-lines start end))
	    colstart colend)
	;; Retrieve the initial and final column for the operation
	(save-excursion
	  (goto-char start)
	  (setq colstart (current-column))
	  (goto-char end)
	  (setq colend   (current-column))
	  (when (> colstart colend)
	    (let (tmp) (setq tmp colend colend colstart colstart tmp))))
	;; Proceed line by line:
	(goto-char start)
	(while (< line-ndx line-count)
	  ;; Do the job!
	  (cond ((not count)      (increase-numbers-on-current-line colstart colend line-ndx))
		((listp count)    (increase-numbers-on-current-line colstart colend 1))
		((integerp count) (increase-numbers-on-current-line colstart colend count))
		(t (error "Invalid COUNT value!")))
	  (forward-line 1)
	  (setq line-ndx (1+ line-ndx)))))))

(provide 'increment-numbers)
