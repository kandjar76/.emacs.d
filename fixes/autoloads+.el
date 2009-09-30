;;
;; Update of autoloads for emacs 22.1
;;


(require 'autoload)

(defun update-directory-autoloads+ (autoloadsdir &rest dirs)
  "\
Update loaddefs.el with all the current autoloads from DIRS, and no old ones.
This uses `update-file-autoloads' (which see) to do its work.
In an interactive call, you must give one argument, the name
of a single directory.  In a call from Lisp, you can supply multiple
directories as separate arguments, but this usage is discouraged.

The function does NOT recursively descend into subdirectories of the
directory or directories specified."
  (interactive "DAutoload file directory: \nDUpdate autoloads from directory: ")
  (let* ((files-re (let ((tmp nil))
		     (dolist (suf (get-load-suffixes)
				  (concat "^[^=.].*" (regexp-opt tmp t) "\\'"))
		       (unless (string-match "\\.elc" suf) (push suf tmp)))))
	 (files (apply 'nconc
		       (mapcar (lambda (dir)
				 (directory-files (expand-file-name dir)
						  t files-re))
			       dirs)))
	 (this-time (current-time))
	 (no-autoloads nil)		;files with no autoload cookies.
	 (autoloads-file
	  (expand-file-name generated-autoload-file
			    autoloadsdir))
	 (top-dir (file-name-directory autoloads-file)))

    (with-current-buffer
	(find-file-noselect (autoload-ensure-default-file autoloads-file))
      (save-excursion

	;; Canonicalize file names and remove the autoload file itself.
	(setq files (delete (autoload-trim-file-name buffer-file-name)
			    (mapcar 'autoload-trim-file-name files)))

	(goto-char (point-min))
	(while (search-forward generate-autoload-section-header nil t)
	  (let* ((form (autoload-read-section-header))
		 (file (nth 3 form)))
	    (cond ((and (consp file) (stringp (car file)))
		   ;; This is a list of files that have no autoload cookies.
		   ;; There shouldn't be more than one such entry.
		   ;; Remove the obsolete section.
		   (autoload-remove-section (match-beginning 0))
		   (let ((last-time (nth 4 form)))
		     (dolist (file file)
		       (let ((file-time (nth 5 (file-attributes file))))
			 (when (and file-time
				    (not (time-less-p last-time file-time)))
			   ;; file unchanged
			   (push file no-autoloads)
			   (setq files (delete file files)))))))
		  ((not (stringp file)))
		  ((not (file-exists-p (expand-file-name file top-dir)))
		   ;; Remove the obsolete section.
		   (autoload-remove-section (match-beginning 0)))
		  ((equal (nth 4 form) (nth 5 (file-attributes file)))
		   ;; File hasn't changed.
		   nil)
		  (t
		   (update-file-autoloads file)))
	    (setq files (delete file files)))))
      ;; Elements remaining in FILES have no existing autoload sections yet.
      (setq no-autoloads
	    (append no-autoloads
		    (delq nil (mapcar 'update-file-autoloads files))))
      (when no-autoloads
	;; Sort them for better readability.
	(setq no-autoloads (sort no-autoloads 'string<))
	;; Add the `no-autoloads' section.
	(goto-char (point-max))
	(search-backward "\f" nil t)
	(autoload-insert-section-header
	 (current-buffer) nil nil no-autoloads this-time)
	(insert generate-autoload-section-trailer))

      (save-buffer))))


(defun update-file-autoloads+ (autoloadsdir file &optional save-after)
  "Update the autoloads for FILE in `generated-autoload-file'
\(which FILE might bind in its local variables).
If SAVE-AFTER is non-nil (which is always, when called interactively),
save the buffer too.

Return FILE if there was no autoload cookie in it, else nil."
  (interactive "DAutoload file directory: \nfUpdate autoloads for file: \np")
  (let ((load-name (let ((name (file-name-nondirectory file)))
		     (if (string-match "\\.elc?\\(\\.\\|$\\)" name)
			 (substring name 0 (match-beginning 0))
		       name)))
	(found nil)
	(existing-buffer (get-file-buffer file))
	(no-autoloads nil))
    (save-excursion
      ;; We want to get a value for generated-autoload-file from
      ;; the local variables section if it's there.
      (if existing-buffer
	  (set-buffer existing-buffer))
      ;; We must read/write the file without any code conversion,
      ;; but still decode EOLs.
      (let ((coding-system-for-read 'raw-text))
	(set-buffer (find-file-noselect
		     (autoload-ensure-default-file
		      (expand-file-name generated-autoload-file
					autoloadsdir))))
	;; This is to make generated-autoload-file have Unix EOLs, so
	;; that it is portable to all platforms.
	(setq buffer-file-coding-system 'raw-text-unix))
      (or (> (buffer-size) 0)
	  (error "Autoloads file %s does not exist" buffer-file-name))
      (or (file-writable-p buffer-file-name)
	  (error "Autoloads file %s is not writable" buffer-file-name))
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  ;; Look for the section for LOAD-NAME.
	  (while (and (not found)
		      (search-forward generate-autoload-section-header nil t))
	    (let ((form (autoload-read-section-header)))
	      (cond ((string= (nth 2 form) load-name)
		     ;; We found the section for this file.
		     ;; Check if it is up to date.
		     (let ((begin (match-beginning 0))
			   (last-time (nth 4 form))
			   (file-time (nth 5 (file-attributes file))))
		       (if (and (or (null existing-buffer)
				    (not (buffer-modified-p existing-buffer)))
				(listp last-time) (= (length last-time) 2)
				(not (time-less-p last-time file-time)))
			   (progn
			     (if (interactive-p)
				 (message "\
Autoload section for %s is up to date."
					  file))
			     (setq found 'up-to-date))
			 (search-forward generate-autoload-section-trailer)
			 (delete-region begin (point))
			 (setq found t))))
		    ((string< load-name (nth 2 form))
		     ;; We've come to a section alphabetically later than
		     ;; LOAD-NAME.  We assume the file is in order and so
		     ;; there must be no section for LOAD-NAME.  We will
		     ;; insert one before the section here.
		     (goto-char (match-beginning 0))
		     (setq found 'new)))))
	  (or found
	      (progn
		(setq found 'new)
		;; No later sections in the file.  Put before the last page.
		(goto-char (point-max))
		(search-backward "\f" nil t)))
	  (or (eq found 'up-to-date)
	      (setq no-autoloads (generate-file-autoloads file)))))
      (and save-after
	   (buffer-modified-p)
	   (save-buffer))

      (if no-autoloads file))))

