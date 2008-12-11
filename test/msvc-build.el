(defun is-devstudio-solution (filename)
  (or 
   (null (file-name-extension filename))
   (string= (file-name-extension filename) "sln")))

;(read-file-name "Solution: " nil nil t nil 'is-devstudio-solution)
;(extract-projects (read-file-name "Solution: " nil nil t nil 'is-devstudio-solution))

(defun extract-projects (sln-file)
  (save-excursion
    (with-temp-buffer
      (insert-file sln-file)
      (goto-char (point-min))
      (let ((result nil)
	    (index 0))
	(while
	    (re-search-forward
	     "Project(\"{[-A-Z0-9]+}\")[ 	]+=[ 	]+\"\\([A-Za-z0-9_]+\\)\"[ 	]*,[ 	]+\"\\([\\A-Za-z0-9_.]+\\)\""
	     (point-max)
	 t) 
    (add-to-list 'result (cons (match-string-no-properties 1) (match-string-no-properties 2))))
    result))))


(defun dev-studio-build ()
  (interactive)
  (let*
      ((solution-name
	(read-file-name "Solution: " nil nil t))
       (projects
	(extract-projects solution-name))
       (action
	(completing-read "Action: " '(("Build" . 0) ("Clean" . 1) ("Run" . 2) ("RunExit" . 3) ("Debug" . 4))
			 nil t "Build"))
       (configuration
	(completing-read "Configuration: " '(("Debug" . 0) ("Release" . 1) ("Hybrid" . 2))
			 nil t "Debug"))
       (platform
	(completing-read "Platform: " '(("Win32" .0) ("x86" . 1))
			 nil t "Win32"))
       (project
	(completing-read "Project " projects
			 nil t (caar projects))))
    (compile
     (concat "Devenv \"" solution-name "\" /" action " \""  (concat configuration "|" platform) "\" /project \"" (cdr (assoc project projects)) "\""))))

