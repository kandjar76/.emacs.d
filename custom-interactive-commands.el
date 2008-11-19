;;
;; This file is contains a set of  interactive functions
;;

;; Here is a list of the function defined here:
;;
;;
;; - electric-split-window-horizontally
;; - electric-split-window-vertically
;; - indent-whole-buffer
;; - revert-buffer-now
;; - kill-buffer-now
;; - ascii-table
;; - dos-to-unix
;; - unix-to-dos
;; - show-file-name
;; - setup-text-mode
;; - delete-backward-word
;; - swap-windows

;;
;; Miscellaneous function:
;;


(defun electric-split-window-horizontally(&optional arg)
  "Split current window into two window side by side, move the cursor to the new created window"
  (interactive "P")
  (split-window-horizontally arg)
  (other-window 1))

(defun electric-split-window-vertically(&optional arg)
  "Split current window into two window side by side, move the cursor to the new created window"
  (interactive "P")
  (split-window-vertically arg)
  (other-window 1))



(defun revert-buffer-now ()
  "Silently calls revert-buffer if the current buffer is not modified."
  (interactive)
  (if (not (buffer-modified-p))
	  (message (format "Reverted from %s" (buffer-file-name))))
  (revert-buffer nil (not (buffer-modified-p))))

(defun kill-buffer-now ()
  "Calls kill-buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

(defun ascii-table ()                                     
  "Print the ascii table."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 32))
    (while (< i 254)
	  (let ((j (min 254 (+ i 8))))
		(while (< i j)
		  (setq i (+ i 1))
		  (insert (format "%4d %c\t" i i)))
		(insert "\n")))
	(beginning-of-buffer)))

(defun dos-to-unix()
  "Convert the end-of-line of the current file from DOS to UNIX"
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
	(while (search-forward "\r" nil t) (replace-match ""))))

(defun unix-to-dos()                                       
  "Convert the end-of-line of the current file from UNIX to DOS"
  (interactive "*")
  (save-excursion
	(goto-char (point-min))
	(while (search-forward "\n" nil t) (replace-match "\r\n"))))

(defun show-file-name()
  "Display the full path of the current buffer."
  (interactive)
  (message buffer-file-name))


(defun setup-text-mode()
  "Setup the text mode to write txt docs."
  (interactive)
  (paragraph-indent-text-mode)
  (auto-fill-mode)
  (set-fill-column 80)
  (flyspell-mode))

(defun backward-delete-word(&optional arg)
  "Delete the previous word"
  (interactive "*")
  (save-excursion
    (let ((end (point)))
      (backward-word 1)
      (delete-region (point) end))))


(defadvice yank-pop (around anytime (arg))
  "Modification of yank-pop: if last action isn't yank, do it."
  (if (not (eq last-command 'yank))
      (yank arg)
      ad-do-it))
(ad-activate 'yank-pop)


(defun kill-selected-region (&optional arg)
  "Equivalent to kill-region, except, it won't kill it if the selected region isn't active"
  (interactive "*")
  (if (is-region-active)
      (kill-region (mark) (point))))


;; To move to another lib:

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

(defun increment-numbers-multilines()
  "Increment the value on each lines"
  (interactive "*")
  (if (is-region-active)
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


(defun increment-numbers-region(&optional arg)
  "Increment each number in the selected region by 1 or by the value of the prefix argument"
  (interactive "*p")
  (if (is-region-active)
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


(defun increase-numbers-on-current-line(colstart colend incr)
  "Increase the number found on the current line between the column [COLSTART; COLEND] by INCR"
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


(defsubst dired-one-or-two-files-p ()
  "Return 1 or 2 iff one or two files are marked to be treated by dired. Returns nil otherwise"
  (save-excursion
    (let (first second third)
      (goto-char (point-min))
      (setq first (re-search-forward (dired-marker-regexp) nil t))
      (setq second (re-search-forward (dired-marker-regexp) nil t))
      (setq third (re-search-forward (dired-marker-regexp) nil t))
      (and (not third)
	  (or (and second 2)
	      (and first 1))))))

(defun dired-ediff-marked-files()
  "If two files are marked in the dired buffer, runa diff between both.
If only one is marked, run the diff between the marked files and the one below the cursor."
  (interactive)
  (let ((marked-files-count (dired-one-or-two-files-p)))
    (if marked-files-count 
	(let (first second)
	  (save-excursion
	    (if (= marked-files-count 1)
		(progn (setq first (dired-get-file-for-visit))
		       (goto-char (point-min))
		       (re-search-forward (dired-marker-regexp) nil t)
		       (setq second (dired-get-file-for-visit)))
		(progn (goto-char (point-min))
		       (re-search-forward (dired-marker-regexp) nil t)
		       (setq first (dired-get-file-for-visit))
		       (re-search-forward (dired-marker-regexp) nil t)
		       (setq second (dired-get-file-for-visit))))
	    (ediff-files first second))))))

(defun dired-emerge-marked-files()
  "If two files are marked in the dired buffer, run a merge between both.
If only one is marked, run the merge between the marked files and the one below the cursor."
  (interactive)
  (let ((marked-files-count (dired-one-or-two-files-p)))
    (if marked-files-count 
	(let (first second)
	  (save-excursion
	    (if (= marked-files-count 1)
		(progn (setq first (dired-get-file-for-visit))
		       (goto-char (point-min))
		       (re-search-forward (dired-marker-regexp) nil t)
		       (setq second (dired-get-file-for-visit)))
		(progn (goto-char (point-min))
		       (re-search-forward (dired-marker-regexp) nil t)
		       (setq first (dired-get-file-for-visit))
		       (re-search-forward (dired-marker-regexp) nil t)
		       (setq second (dired-get-file-for-visit))))
	    (emerge-files-internal first second))))))

(defun sum-column (start end)
   "Adds numbers in rectangle defined by START to END."
   (interactive "r")
   (save-excursion
     (kill-rectangle start end)
     (exchange-point-and-mark)
     (yank-rectangle)
     (set-buffer (get-buffer-create "*calc-sum*"))
     (erase-buffer)
     (yank-rectangle)
     (exchange-point-and-mark-nomark)
     (let ((sum 0))
       (while (re-search-forward "[0-9]*\\.?[0-9]+" nil t)
	 (setq sum (+ sum (string-to-number (match-string 0)))))
       (message "Sum: %f" sum))))



(autoload 'operate-on-rectangle "rect")
(defun replace-string-rectangle (start end from-string to-string)
   "Replace the string FROM-STRING with the string TO-STRING in the rectangle delimited by START and END."
   (interactive "r\ns[Rectangle] Replace String: \ns[Rectangle] With: ")
   (let ((count 0))
     (operate-on-rectangle (lambda (pt ignore ignore) 
			     (let ((start (or (and (< pt (point)) pt) (point)))
				   (end   (or (and (> pt (point)) pt) (point))))
			       (goto-char start)
			       (while (search-forward from-string end t)
				 (setq count (1+ count))
				 (replace-match to-string nil t))))
			   start end nil)
     (if ( = count 1 )
	 (message "Replaced 1 occurrence")
	 (message "Replaced %i occurrences" count))))

(defun replace-regexp-rectangle (start end from-regexp to-string)
   "Replace the regexp FROM-REGEXP with the string TO-STRING in the rectangle delimited by START and END."
   (interactive "r\ns[Rectangle] Replace Regexp: \ns[Rectangle] With: ")
   (let ((count 0))
     (operate-on-rectangle (lambda (pt ignore ignore) 
			     (let ((start (or (and (< pt (point)) pt) (point)))
				   (end   (or (and (> pt (point)) pt) (point))))
			       (goto-char start)
			       (while (re-search-forward from-regexp end t)
				 (setq count (1+ count))
				 (replace-match to-string nil nil))))
			   start end nil)
     (if ( = count 1 )
	 (message "Replaced 1 occurrence")
	 (message "Replaced %i occurrences" count))))



(defun extract-string-length (&optional single)
  "Print the number of characters in the string which point is inside.
With prefix arg, use single quotes, not double quotes, as delimeters."
  (interactive "P")
  (let* ((quote-char   (or (and single ?\') ?\"))
         (quote-string (format "^%c\n" quote-char)))
    (save-excursion
      (skip-chars-forward quote-string)
      (if (/= (following-char) quote-char)
          (error "Point is not inside string"))
      (let ((length (- (point)
                       (progn
                         (skip-chars-backward quote-string)
                         (if (/= (preceding-char) quote-char)
                             (error "Point is not inside string"))
                         (point)))))
        (message "String has %d (0x%x) characters" length length)))))

(defun swap-windows ()
 "Swap the buffer in the first two windows.
  Error is generated if there is more than 2 windows :)"
 (interactive) 
 (if (not (= (count-windows) 2)) (message "You need exactly 2 windows to do this.")
     (let* ((win1 (first (window-list)))
	    (win2 (second (window-list)))
	    (buf1 (window-buffer win1))
	    (buf2 (window-buffer win2))
	    (pos1 (window-start win1))
	    (pos2 (window-start win2)))
       (set-window-buffer win1 buf2)
       (set-window-buffer win2 buf1)
       (set-window-start win1 pos2)
       (set-window-start win2 pos1))))