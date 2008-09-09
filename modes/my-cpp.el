(require 'cc-mode)
(require 'apply-on-region)

(defun surround-with-curly-brackets(&optional ARG)
  "Add curly brackets around the selected block
Side effect: copy the selected region inside the kill buffer"
  (interactive "P") 
  (if mark-active
      (let ((start (region-beginning))
	    (end   (region-end)))
	(let ((reg (extend-region-to-full-lines start end)))
	  (kill-region (car reg) (cdr reg))
	  (insert "{")
	  (newline)
	  (yank)
	  (insert "}")
	  (newline)
	  (let ((newend (point)))
	    (set-mark (car reg))
	    (if (> emacs-major-version 22)
		(c-indent-line-or-region nil t)
		(c-indent-line-or-region))
	    )))
      (c-electric-brace ARG)))


(defun surround-with-paren(&optional ARG)
  "Add curly brackets around the selected block
Side effect: copy the selected region inside the kill buffer"
  (interactive "P") 
  (if mark-active
      (let ((start (region-beginning))
	    (end   (region-end)))
	(kill-region start end)
	(insert "(")
	(yank)
	(insert ")")
	(let ((newend (point)))
	  (set-mark start)
	  (if (> emacs-major-version 22)
	      (c-indent-line-or-region nil t)
	      (c-indent-line-or-region))
	  )
	)
      (c-electric-paren ARG)))



(autoload 'comment-search-forward "newcomment"
  "Find a comment start between point and LIMIT.
Moves point to inside the comment and returns the position of the
comment-starter.  If no comment is found, moves point to LIMIT
and raises an error or returns nil if NOERROR is non-nil.")


(defun c-indent-line-or-region-and-comment(arg region)
  (interactive
   (list current-prefix-arg (use-region-p)))
  (c-indent-line-or-region arg region)
  (let ((comment-indent-only (lambda (bol eol &optional in-region)
			       (goto-char bol)
			       ;; Get the comment position...
			       (let ((comment-pos (comment-search-forward (line-end-position) t)))
				 (if comment-pos
				     (progn
				       ;; .. and position the current 'point' just before the comment
				       (search-backward-regexp comment-start-skip (line-beginning-position))
				       (if (and 
					    ;; Check if the line is not empty or if the previous line contain a comment.
					    ;; Another special case is: we are in applying the changes on a region
					    ;;   in that case, we check the next line: if it contain a comment, we do the alignment
					    (or (and 
						 in-region
						(save-excursion 
						  (beginning-of-line)
						  (when (not (eobp))
						    (forward-line 1)
						    (comment-search-forward (line-end-position) t))))
						(not (looking-back "^[ \t]*" (line-beginning-position)))
						(save-excursion 
						  (beginning-of-line)
						  (when (not (bobp))
						    (forward-line -1)
						    (comment-search-forward (line-end-position) t))))
					    ;; and make sure it's a comment at the end of the line...
					    (or (looking-at "//")
						(looking-at "/\\*.*\\*/[ \t]*$")))
					   (comment-indent))))))))
    (save-excursion
      (if region
	  (let ((beg (region-beginning))
		(end (region-end))
		(col comment-column))
	    ;; Let's extract the correct column for alignment
	    ;; which correspond to the longest line of code... (unless comment-column is bigger)
	    (save-excursion
	      (goto-char beg)
	      (beginning-of-line)
	      (while (and (bolp)
			  (not (eobp))
			  (< (point) end))
		(let ((comment-pos (comment-search-forward (line-end-position) t)))
		  (when comment-pos
		    (search-backward-regexp comment-start-skip (line-beginning-position))
		    (skip-chars-backward " \t")
		    (if (< col (1+ (current-column)))
			(setq col (1+ (current-column))))))
		(forward-line)))
	    ;; Now: let's align the comment in the region
	    (let ((comment-column col))
	      (apply-on-region-lines beg end comment-indent-only t)))
	  (funcall comment-indent-only (point-at-bol) (point-at-eol))))))

(setq c-font-lock-keywords c-font-lock-keywords-3)
(setq c++-font-lock-keywords c++-font-lock-keywords-3)


;; (defun my-cpp-highlight ()
;;   (setq cpp-known-face '(background-color . "dim gray"))
;;   (setq cpp-unknown-face 'default)
;;   (setq cpp-face-type 'dark)
;;   (setq cpp-known-writable 't)
;;   (setq cpp-unknown-writable 't)
;;   (setq cpp-edit-list
;; 	'((#("0" 0 1
;; 	     (c-in-sws t fontified t))
;; 	   (background-color . "dim gray")
;; 	   nil both nil)
;; 	  (#("1" 0 1
;; 	     (c-in-sws t fontified t))
;; 	   nil
;; 	   (background-color . "dim gray")
;; 	   both nil)))
;;   (cpp-highlight-buffer t))
;; 
;; (defun my-c-mode-common-hook ()
;;   (my-cpp-highlight)
;;   )
;; (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;; 
;; (defun my-c-mode-recenter ()
;;   "Recenter buffer and refresh highlighting."
;;   (interactive)
;;   (recenter)
;;   (cpp-highlight-buffer t))
;; 
;; (defun my-c-initialization-hook ()
;;   (define-key c-mode-base-map "\C-l" 'my-c-mode-recenter))
;; 
;; (add-hook 'c-initialization-hook 'my-c-initialization-hook)



;; Add symbol highlighting:
(font-lock-add-keywords 'c++-mode 
			'(;; Currently support for []|&!.+=-/%*,()<>{}
			  ("\\(\\[\\|\\]\\|[|!?:\\.\\+\\=\\&]\\|->?\\|\\/\\|\\%\\|\\*\\|,\\|(\\|)\\|>\\ |<\\|{\\|}\\)" 1 font-lock-operator-face )
			  ;; End of c++ statement 
			  ("\\(;\\)" 1 font-lock-end-statement) 
			  ;; Asm keyword
			  ("\\<__asm\\>" 1 font-lock-keyword-face))) ;; doesn't work!

(setq-default c++-font-lock-extra-types
	      (append c++-font-lock-extra-types
		      '("uint8_t" "int8_t"
			"uint16_t" "int16_t"
			"uint32_t" "int32_t"
			"uint64_t" "int64_t"
			"thread_t" "size_t")))

(defun c-lineup-inline-asm(langelem)
  "Inline ASM doesn't require a ';' to terminate the statement.
Emacs will therefore consider the following line to be 'statement-cont'.
This function just inhibate the extra indentation if that's the case."
  (save-excursion
    (let ((stop nil)
	  (eor (1- (c-point 'bol))))
      (goto-char (cdr langelem))
      (while (and (not stop)
		  (< (point) eor))
	(beginning-of-line)
	(setq stop (and (not (or (looking-at "^[ \t]*__asm\\>")
				 (looking-at "^[ \t]*$")))
			(c-point 'bol)))
	(forward-line 1))
      (if (not stop)
	  0
	  (c-lineup-math (cons (car langelem) stop))) ;; todo: we should return nil here! 
      )))
  
  

;; Perform my C/C++-specific customizations.
(defun custom-c-setup ()
  (c-set-style "gnu")

  (local-set-key [(control ?c) (?m)] 'manual-entry)
  (local-set-key [(control ?c) (?a) (?a)] 'cpp-align-variable-assignment) 
  (local-set-key [(control ?c) (?a) (?f)] 'cpp-align-function-bracket)
  (local-set-key [(control ?c) (?a) (?c)] 'cpp-align-comment)
  (local-set-key [(control ?q)] 'cpp-comment-block)
  (when (> emacs-major-version 22)
    (local-set-key [(tab)] 'c-indent-line-or-region-and-comment))

  (local-set-key [(f4)] 'next-error)
  (local-set-key [(shift f4)] 'previous-error)
  (local-set-key [?{] 'surround-with-curly-brackets)
  (local-set-key "(" 'surround-with-paren)

  (setq tab-width 4)
  (setq c-basic-offset 4)
  (c-set-offset 'case-label 2 nil)
  (c-set-offset 'statement-block-intro 4 nil)
  (c-set-offset 'substatement-open 0 nil)
  (c-set-offset 'defun-block-intro 4 nil)
  (c-set-offset 'brace-list-open 0 nil)
;  (c-set-offset 'statement-cont 'c-lineup-math)
  (c-set-offset 'statement-cont 'c-lineup-inline-asm )
  (c-set-offset 'comment-intro 'c-lineup-comment)

  (modify-syntax-entry ?_ "w"   c++-mode-syntax-table)
  (modify-syntax-entry ?_ "w"   c-mode-syntax-table)

  (setq comment-column 40)
  (make-local-variable 'default-fill-column)
  (setq default-fill-column 200)
  (setq c-indent-comment-alist 
	(list (cons 'anchored-comment (cons 'column 0))
	      (cons 'cpp-end-block (cons 'space 1)) ; Looking for: #endif... #else // ...
	      (cons 'end-block (cons 'space 1))     ;  Looking for: } // 
	      (cons 'other (cons 'align (cons 'column nil)))))
  
)

;;(font-lock-add-keywords
;; 'c-mode
;; '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))

(add-hook 'c++-mode-hook 'custom-c-setup)
(add-hook 'c++-mode-hook 'turn-on-cwarn-mode)

(add-hook 'c-mode-hook 'custom-c-setup)
(add-hook 'c-mode-hook 'turn-on-cwarn-mode)
