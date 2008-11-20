;;
;;
;; Extra for the buffer mode:
;;
;; Didn't really check what (Buffer-menu-no-header) was doing... 
;; The functions may required some 

;;;###autoload
(defun Buffer-menu-mark-file-to-revert()
  "Mark files to revert in the bufffer list"
  (interactive)
  (when (not (eq major-mode 'Buffer-menu-mode))
      (error "Invalid Mode -- Expected: Buffer-menu-mode"))
  (when (Buffer-menu-no-header)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?R)
      (forward-line 1))))

;;;###autoload
(defun Buffer-menu-mark-every-files-to-revert()
  "Mark files to which can be reverted in the bufffer list"
  (interactive)
  (when (not (eq major-mode 'Buffer-menu-mode))
      (error "Invalid Mode -- Expected: Buffer-menu-mode"))
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (not (eobp))
	(let* ((buf (Buffer-menu-buffer t))
	       (vis (verify-visited-file-modtime buf)))
	  (when (not vis)
	    (delete-char 1)
	    (insert ?R))
	(forward-line 1))))))

;;;###autoload
(defun Buffer-menu-mark-every-files-to-save()
  "Mark files to which can be reverted in the bufffer list"
  (interactive)
  (when (not (eq major-mode 'Buffer-menu-mode))
      (error "Invalid Mode -- Expected: Buffer-menu-mode"))
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (not (eobp))
	(let* ((buf (Buffer-menu-buffer t))
	       (mod (buffer-modified-p buf))
	       (wfi (save-excursion (set-buffer buf) buffer-file-name)))
	  (when (and mod wfi)
	    (beginning-of-line)
	    (forward-char 2)
	    (delete-char 1)
	    (insert ?S))
	(forward-line 1))))))


;;;###autoload
(defun Buffer-menu-diff-buffer-with-file()
  "View the difference between buffer describe by the current line and it's associated file
This require the external program 'diff' to be in your 'exec-path'"
  (interactive)
  (when (Buffer-menu-no-header)
    (let ((buf (Buffer-menu-buffer t)))
      (diff-buffer-with-file buf))))

;;================================================================================

;;;###autoload
(defadvice Buffer-menu-unmark (around new-unmark-with-modified-flag (&optional backup) activate)
  (interactive "P")
  (when (Buffer-menu-no-header)
    (let* ((buf (Buffer-menu-buffer t))
	   (mod (buffer-modified-p buf))
	   (readonly (save-excursion (set-buffer buf) buffer-read-only))
	   (buffer-read-only nil)
	   (vis (verify-visited-file-modtime buf)))
      (delete-char 3)
      (insert ?\s
	      (if readonly ?% ?\s)
	      (if mod ?* (if vis ?\s ?#)))))
  (forward-line (if backup -1 1)))


;;================================================================================

;;;###autoload
(defadvice list-buffers-noselect (after update-external-modification-flag (&optional files-only buffer-list) activate)
  (interactive)
  (with-current-buffer (get-buffer "*Buffer List*")
    (save-excursion
      (let ((buffer-read-only nil))
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((buf (Buffer-menu-buffer t))
		 (mod (buffer-modified-p buf))
		 (vis (verify-visited-file-modtime buf)))
	    (forward-char 2) 
	    (delete-char 1)
	    (insert (if mod ?* (if vis ?\s ?#)))
	    (beginning-of-line))
	  (forward-line 1))))))

;;================================================================================

;;;###autoload
(defadvice Buffer-menu-execute (after revert-selected-files activate)
  (save-excursion
    (Buffer-menu-beginning)
    (while (re-search-forward "^R" nil t)
      (let ((modp nil)
	    (rdp  nil)
	    (visp nil))
	(save-excursion
	  (set-buffer (Buffer-menu-buffer t))
	  (if buffer-file-name
	      (revert-buffer t (not (buffer-modified-p))))
	  (setq modp (buffer-modified-p))
	  (setq rdp  buffer-read-only)
	  (setq visp (verify-visited-file-modtime (current-buffer))))
	(let ((buffer-read-only nil))
	  (forward-char -1)
	  (delete-char 3)
	  (insert ?\s
		  (if rdp ?% ?\s)
		  (if modp ?* (if visp ?\s ?#)))
	  )))))

(provide 'buff-menu++)