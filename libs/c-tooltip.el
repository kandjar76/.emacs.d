;;
;; Function to use with tooltip-help.el
;;  At work, man pages doesn't exist.
;;  This function will search the function on the web.
;;

(provide 'c-tooltip)

(defun th-c++-mode-handler()
  "Tooltip handler for C++"
  (let* ((name (current-word))
	 (buf (concat name ".3.html"))
	 (url (concat "http://www.rt.com/man/" name ".3.html"))
	 (cur (current-buffer)))
    (if (get-buffer buf)
	(kill-buffer buf))
    (with-current-buffer (browse-url-emacs url)
      (let ((buffer-read-only nil))
	(goto-char (point-min))
	(search-forward "<pre>")
	(forward-line 1)
	(beginning-of-line)
	(delete-region (point-min) (point))
	(set-buffer-modified-p nil)
	(search-forward "</pre>")
	(beginning-of-line)
	(delete-region (point) (point-max))
	(set-buffer-modified-p nil)
	(goto-char (point-min))
	(while (search-forward-regexp "<[^>]*>"  nil t)
	  (put-text-property (match-beginning 0)
			     (match-end 0)
			     'invisible t
			     ))
	(goto-char (point-min))
	(while (search-forward-regexp "&lt;"  nil t)
	  (put-text-property (match-beginning 0)
			     (match-end 0)
			     'display 
			     "<"
			     ))
	(goto-char (point-min))
	(while (search-forward-regexp "&gt;"  nil t)
	  (put-text-property (match-beginning 0)
			     (match-end 0)
			     'display 
			     ">"
			     ))
	(goto-char (point-min))
	(while (search-forward ""  nil t)
	  (put-text-property (match-beginning 0)
			     (match-end 0)
			     'invisible t 
			     ))
	(goto-char (point-min))
	(set-buffer-modified-p nil))
      )
    "")
  )