(defun my-asm-setup ()
  (local-set-key [(control ?c) (control ?s)] 'display-asm-code-size)
  (setq tab-width 8)
  (local-set-key (vector asm-comment-char) 'self-insert-command)   ; Overrides annoying semicolon behavior in asm-mode
  (local-set-key ":" 'self-insert-command))

(add-hook 'asm-mode-hook 'my-asm-setup)
