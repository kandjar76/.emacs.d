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

