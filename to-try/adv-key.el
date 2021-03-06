(defmacro ad-add-advice-to-key (key expr)
  "Around advice the key KEY with expression EXPR. KEY should be
a key in the format accepted by key-binding and such, and EXPR an
expression of the same type as those required by around advices"
  `(add-hook 'pre-command-hook
	     (lambda ()
	       (when (equal (this-command-keys-vector) ,key)
		 (ad-add-advice this-command
				'(azerrswdf ;arbitrary advice name
				  nil	    ;not protected
				  t	    ;activated
				  (lambda ()
				    ,expr
				    (ad-unadvise this-command)))
				'around
				'last)
		 (ad-activate this-command)))))


;; It's particularly useful for binding the TAB key to auto-completion without disrupting
;; the existing behavior.  This code makes TAB autocomplete if the point was not moved by
;; the TAB command and we are at the end of a word :
;; 
;; (ad-add-advice-to-key [9]
;; 		      (let ((p (point)))
;; 			ad-do-it
;; 			(when (and (= p (point))
;; 				   (not (bolp))
;; 				   (looking-at "\\_>")
;;                                    (not (minibufferp)))
;; 			  (dabbrev-expand nil))))
