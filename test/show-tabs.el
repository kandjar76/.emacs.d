(defface show-tabs--tabs
  `((((type tty) (class color))
     (:background "LemonChiffon2"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color))
     (:foreground "dark gray"))
    (t (:foreground "dark gray")))
  "Face for highlighting tabs.")
(set-face-foreground 'show-tabs--tabs "gray")


(define-minor-mode show-tabs-mode
  "Toggle tabs highlighting in current buffer.

With arg, turn Show-Whitespace mode on if and only if arg is positive.
This is a minor mode that affects only the current buffer."
  ;; the initial value
  nil
  ;; the indicator for the mode line
  " show-tabs"
  ;; the keymap
  nil
  ;; the body
  (if show-tabs-mode
      ;; Show whitespaces distinguishing spaces and tabs
      (progn
	(make-local-variable 'show-tabs--initial-display-table)
	(make-local-variable 'show-tabs--initial-font-lock-keywords)
	(setq show-tabs--initial-display-table nil)

	(if buffer-display-table
	    (setq show-tabs--initial-display-table (copy-sequence buffer-display-table))
	    (setq buffer-display-table (make-display-table)))
	(aset buffer-display-table ?\t (vector 187 ?\t))
	(setq show-tabs--initial-font-lock-keywords font-lock-keywords)
	(font-lock-add-keywords nil
				'(;; show tabs
				  ("[\t]+" (0 'show-tabs--tabs t))
				  ) t)
	(font-lock-fontify-buffer)
	)

      ;; revert to initial display
      (progn
        (setq buffer-display-table show-tabs--initial-display-table)
        (setq font-lock-keywords show-tabs--initial-font-lock-keywords)
        (font-lock-fontify-buffer)
	)))

;(logior 32 43)
;(ash ...)
;(vconcat "123")
;glyph-table