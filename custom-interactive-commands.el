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


;;
;; Miscellaneous function:
;;


(defun electric-split-window-horizontally(&optional arg)
  "Split current window into two window side by side, move the cursor to the new created window"
  (interactive)
  (split-window-horizontally arg)
  (other-window 1))

(defun electric-split-window-vertically(&optional arg)
  "Split current window into two window side by side, move the cursor to the new created window"
  (interactive)
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
  (interactive)
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
  (interactive "p")
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