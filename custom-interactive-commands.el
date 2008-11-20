;;
;; This file is contains a set of  interactive functions
;;


;;
;; Miscellaneous function:
;;



(defadvice yank-pop (around anytime (arg) activate)
  "Modification of yank-pop: if last action isn't yank, do it."
  (if (not (eq last-command 'yank))
      (yank arg)
      ad-do-it))




;; To move to another lib:

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






