;;
;; Setup a Specific identation for awk script:
;;

(defun custom-awk-setup ()
  ;;(c-set-style "gnu")
  (setq tab-width 4)
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

(add-hook 'awk-mode-hook 'custom-awk-setup)
