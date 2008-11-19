;;
;; File containing utility functions for lisp code
;;


;;------------------------------------------------------------------------------
;;
;;                      Utility Functions                           
;;
;;------------------------------------------------------------------------------

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
			(next-line 1)
			(setq end (point-at-bol))))
	)
	(cons start end))
)

(defun alphanumericp (ch)
  "Returns t if the argument is an alphanumeric character (or underscore)."
  (or (and (>= ch ?a) (<= ch ?z))
	  (and (>= ch ?A) (<= ch ?Z))
	  (and (>= ch ?0) (<= ch ?9))
	  (eq ch ?_)))


;; Emacs function which doesn't exist in XEmacs
(if using-xemacs
    (progn 
      (defun assoc-string (key list ignore-case)
	"Return non-nil if KEY is `equal' to the car of an element of LIST. IGNORE-CASE if true will do the research ignoring the case of the key"
	(if ignore-case
	    (assoc-ignore-case key list)
	    (assoc key list)))

      ; Emacs function which doesn't exist in XEmacs
      (defun minibuffer-contents ()
	"Return the content of the minibuffer"
	(buffer-substring (point-min) (point-max)))

      ; Emacs function which doesn't exist in XEmacs
      (defun delete-minibuffer-contents ()
	"Delete the contents of the minibuffer"
	(erase-buffer))))