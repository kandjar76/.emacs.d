(provide 'git+)

(defun git-diff-current-file()
  "Diff the current file(s) against HEAD."
  (interactive)
  (unless git-status (error "Not in git-status buffer."))
  (let* ((pos (ewoc-locate git-status))
	 (info (ewoc-data pos))
	 (files (list info)))
    (git-setup-diff-buffer
     (apply #'git-run-command-buffer "*git-diff*" "diff-index" "-p" "-M" "HEAD" "--" (git-get-filenames files)))))