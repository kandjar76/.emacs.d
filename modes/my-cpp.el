(require 'cc-mode)

;; Provide context-sensitive tab completion of strings.  If the last
;; character typed was a valid identifier character (a-zA-Z0-9_), then
;; expand.  Otherwise, indent as usual.
(defadvice self-insert-command (after store-last-char nil activate compile)
  "Stores the last character typed inside self-insert-command."
  (put 'self-insert-command 'last-char-inserted last-command-char))

(defun my-c-smart-tab (&optional ARG)
  "Intelligently decides whether to indent or do word-completion."
  (interactive "P")
  ;; Special case: a region has been selected -- run the alignment on the selected region
  (if (is-region-active)
      (c-indent-line-or-region)
      ;; hack!  ANY command that fails with an error will cause last-command to be t.
      ;; Is there a better way to check whether dabbrev-expand returned an error?
      (if (or (and (or (eq last-command this-command) (eq last-command t))
		   (get this-command 'dabbrev-continue))
	      (and (eq last-command 'self-insert-command)
		   (alphanumericp (get 'self-insert-command 'last-char-inserted))))
	  (progn
	    (put this-command 'dabbrev-continue t)
	    (dabbrev-expand nil))
	  (progn
	    (put this-command 'dabbrev-continue nil)
	    (c-indent-command ARG))))
)



(defun surround-with-curly-brackets(&optional ARG)
  "Add curly brackets around the selected block"
  (interactive "P") 
  (if (is-region-active)
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
	    (c-indent-line-or-region))))
      (c-electric-brace ARG)))


(defun surround-with-paren(&optional ARG)
  "Add curly brackets around the selected block"
  (interactive "P") 
  (if (is-region-active)
      (let ((start (region-beginning))
	    (end   (region-end)))
	(kill-region start end)
	(insert "(")
	(yank)
	(insert ")")
	(let ((newend (point)))
	  (set-mark start)
	  (c-indent-line-or-region))
	)
      (c-electric-paren ARG)))




;; Perform my C/C++-specific customizations.
(defun my-c-setup ()
  (c-set-style "gnu")
  (ad-activate 'self-insert-command)

  (local-set-key [(tab)] 'my-c-smart-tab)
  (local-set-key [(control ?c) (?m)] 'manual-entry)
  (local-set-key [(control ?x) (?a) (?a)] 'cpp-align-variable-assignment) 
  (local-set-key [(control ?x) (?a) (?f)] 'cpp-align-function-bracket)
  (local-set-key [(control ?x) (?a) (?c)] 'cpp-align-comment)
  (local-set-key [(control ?q)] 'cpp-comment-block)

  (local-set-key [(control f7)] 'compile)
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
  (c-set-offset 'statement-cont 'c-lineup-math)

  (modify-syntax-entry ?_ "w"   c++-mode-syntax-table)
  (modify-syntax-entry ?_ "w"   c-mode-syntax-table)
)

(add-hook 'c++-mode-hook 'my-c-setup)
(add-hook 'c-mode-hook 'my-c-setup)
