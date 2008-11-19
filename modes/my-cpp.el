(require 'cc-mode)


(defun my-c-smart-tab (&optional ARG)
  "Intelligently decides whether to indent or do word-completion."
  (interactive "P")
  ;; Special case: a region has been selected -- run the alignment on the selected region
  (if (is-region-active)
      (c-indent-line-or-region)
      (if (looking-at "\\>")
          (dabbrev-expand nil)
	  (c-indent-command ARG) ;(indent-for-tab-command)
	  )))

(defun surround-with-curly-brackets(&optional ARG)
  "Add curly brackets around the selected block
Side effect: copy the selected region inside the kill buffer"
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
  "Add curly brackets around the selected block
Side effect: copy the selected region inside the kill buffer"
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


(setq c-font-lock-keywords c-font-lock-keywords-3)
(setq c++-font-lock-keywords c++-font-lock-keywords-3)


;; Perform my C/C++-specific customizations.
(defun my-c-setup ()
  (c-set-style "gnu")

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

;;(font-lock-add-keywords
;; 'c-mode
;; '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))

(add-hook 'c++-mode-hook 'my-c-setup)
(add-hook 'c-mode-hook 'my-c-setup)
