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
			(setq end (point-at-bol)))))
	(cons start end)))

(defun clear-spaces(string)
  "Clear the spaces and tabs at the beginning and at the end of STRING."
  (let ((begpos (string-match "[^ \t]" string))
	(endpos (string-match "[ \t]*$" string)))
    (or (and begpos
	     (substring string begpos endpos))
	"")))

(defun alphanumericp (ch)
  "Returns t if the argument is an alphanumeric character (or underscore)."
  (if ch
       (or (and (>= ch ?a) (<= ch ?z))
	   (and (>= ch ?A) (<= ch ?Z))
	   (and (>= ch ?0) (<= ch ?9))
	   (eq ch ?_))
       nil))

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


(defun apply-on-region-lines(start end func &rest rest)
  "Call the function FUNC for every line of the region defined by: START END.
The function will receive as param BOL and EOL which will represent the two 
positions at the beginning and at the end of the current line.
In order to allow the user to current the line, the line are parsed in the
reverse order. This function will automatically save the excursion.

For example:
  (defun test-aorl()
    (interactive \"*\")
    (let ((buffer (get-buffer-create \"*test*\")))
      (display-buffer buffer)
      (if (is-region-active)
   	  (apply-on-region-lines 
   	   (region-beginning)
   	   (region-end)
   	   (lambda (bol eol prefix)
	     (let ((line (buffer-substring-no-properties bol eol)))
	       (set-buffer buffer)
	       (goto-char (point-min))
	       (insert (concat prefix line))
	       (newline)))
   	   \"[ok]\"))))"
  (if ( > start end )
      (let (tmp) (setq tmp end end start start tmp)))
  (save-excursion
    (goto-char end)
    (if (bolp) (forward-line -1))
    (beginning-of-line)
    (while (and (bolp)
		(not (bobp))
		(> (point) start))
      (save-excursion (apply func (point-at-bol) (point-at-eol) rest))
      (forward-line -1))
    (apply func (point-at-bol) (point-at-eol) rest)))
