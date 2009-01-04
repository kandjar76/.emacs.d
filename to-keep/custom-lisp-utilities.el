;;
;; File containing utility functions for lisp code
;;


;;------------------------------------------------------------------------------
;;
;;                      Utility Functions                           
;;
;;------------------------------------------------------------------------------

(defun clear-spaces(string)
  "Clear the spaces and tabs at the beginning and at the end of STRING."
  (let ((begpos (string-match "[^ \t]" string))
	(endpos (string-match "[ \t]*$" string)))
    (or (and begpos
	     (substring string begpos endpos))
	"")))

;; Go to the indicated line with ‘find-file-at-point’:
(defadvice find-file-at-point (around goto-line compile activate)
  (let ((line (and (looking-at ".*:\\([0-9]+\\)")
		   (parse-integer (match-string 1)))))
    ad-do-it
    (and line (goto-line line))))

;; Display the Time in the modeline
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
