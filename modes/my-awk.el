;;
;; Setup a Specific identation for awk script:
;;

(defun my-awk-smart-tab (&optional ARG)
  "Intelligently decides whether to indent or do word-completion."
  (interactive "P")
  ;; Special case: a region has been selected -- run the alignment on the selected region
  (if mark-active
      (c-indent-line-or-region)
      (c-indent-command ARG))
)

(defun my-awk-setup ()
  ;;(c-set-style "gnu")
  (setq tab-width 4)
  (local-set-key [(tab)] 'my-awk-smart-tab)
  (c-set-offset 'substatement 4 nil)
  (c-set-offset 'statement-block-intro 4 nil)
  (c-set-offset 'substatement-open 0 nil)
  (c-set-offset 'defun-block-intro 4 nil)
  (local-set-key [(control ?x) (?a) (?a)] 'cpp-align-variable-assignment) 
  (local-set-key [(control ?x) (?a) (?c)] 'awk-align-comment)
  (local-set-key [(control ?q)]           'awk-comment-block)
  (modify-syntax-entry ?_ "w"   awk-mode-syntax-table)
  ;(modify-syntax-entry ?\# "< b"   awk-mode-syntax-table)
  ;(modify-syntax-entry ?\n "> b"   awk-mode-syntax-table)
)

(add-hook 'awk-mode-hook 'my-awk-setup)
