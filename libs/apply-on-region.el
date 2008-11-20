




(defun extend-region-to-full-lines ( start end )
  "Extend the region to encapsulate only whole lines"
  (if (> start end) (let (mid) (setq mid start start end end mid)))
  (save-excursion
	(goto-char start)
	(beginning-of-line)
	(setq start (point-at-bol))
	(goto-char end)
	(if ( /= end (point-at-bol))
		(if (= (point-at-eol)  (point-max))
			(setq end (point-at-eol))
		  (progn
			(forward-line 1)
			(setq end (point-at-bol)))))
	(cons start end)))

(defun apply-on-region-lines(start end func &rest rest)
  "Call the function FUNC for every line of the region defined by: START END.
The function will receive as param BOL and EOL which will represent the two 
positions at the beginning and at the end of the current line.
In order to allow the user to current the line, the line are parsed in the
reverse order. This function will automatically save the excursion.

For example:
  (defun test-aorl()
    (interactive \"*\")
    (let ((buffer (get-buffer-create \"*test*\")))
      (display-buffer buffer)
      (if mark-active
   	  (apply-on-region-lines 
   	   (region-beginning)
   	   (region-end)
   	   (lambda (bol eol prefix)
	     (let ((line (buffer-substring-no-properties bol eol)))
	       (set-buffer buffer)
	       (goto-char (point-min))
	       (insert (concat prefix line))
	       (newline)))
   	   \"[ok]\"))))"
  (if ( > start end )
      (let (tmp) (setq tmp end end start start tmp)))
  (save-excursion
    (let ((endmark (copy-marker end)))
      (goto-char start)
      (beginning-of-line)
      (while (and (bolp)
		  (not (eobp))
		  (< (point) endmark))
	(save-excursion
	  (apply func (point-at-bol) (point-at-eol) rest))
	(forward-line)))))

(provide 'apply-on-region)