;; Author: Cedric Lallain (clallain@naughtydog.com) 
;;
;; This file provides a major mode in view to pm4-dump files.

;;
;;    Opcopde definition:
;;

(require 'cl)

;; 
(setq pm4-keywords "\\<IT_[A-Z0-9_]+\\>" )
(setq pm4-flags    "\\[wo\\]")

;;
;;    Functions to help defining the major-mode / commands:
;;

(defun pm4-string-list-to-regexp (inst-list)
  "Produce from a list of strings a single regular expression which
matches any of the individual token."
  (reduce (lambda (x y) (concat x "\\|" y))
	  (mapcar (lambda (x) (concat "\\<" x "\\>"))
		  inst-list)))


;;
;;    PM4 Commands:
;;

(defun pm4-go-to-previous-pm4cmd()
  "Go to the previous PM4 command."
  (interactive)
  (goto-char (point-at-bol))
  (search-backward-regexp pm4-keywords nil t))

(defun pm4-go-to-next-pm4cmd()
  "Go to the previous PM4 command."
  (interactive)
  (goto-char (point-at-eol))
  (when (search-forward-regexp pm4-keywords nil t)
    (goto-char (match-beginning 0))))


;;
;;    PM4 Major-Mode definition:
;;


;; User hook...
(defvar pm4-mode-hook nil)

;; Define the key mapping for the spu mode:
(defvar pm4-mode-map
  (let ((pm4-mode-map (make-keymap)))
    ;(define-key pm4-mode-map [(control c) (control f)]    'pm4-to-inl)
    (define-key pm4-mode-map [?p]    'pm4-go-to-previous-pm4cmd)
    (define-key pm4-mode-map [?n]    'pm4-go-to-next-pm4cmd)
    pm4-mode-map))


(defconst pm4-font-lock-keywords-3
  (list (cons pm4-flags 'font-lock-constant-face)
	(cons pm4-keywords 'font-lock-keyword-face)
	(list "`- Set \\([A-Z0-9_]+\\)\\>" 1 'font-lock-type-face)
	'("|\\|`-" . font-lock-builtin-face)
	;(cons (pm4-string-list-to-regexp pm4-types) 'font-lock-type-face)
	)
  "Additional expressions to highlight in PM4 mode.")


(defvar pm4-font-lock-keywords pm4-font-lock-keywords-3)

;; Syntax table:
(defvar pm4-syntax-table
  (let ((pm4-syntax-table (make-syntax-table)))

    ;; Add _ as part of the word definition:
    (modify-syntax-entry ?_ "w"   pm4-syntax-table)
    pm4-syntax-table)
  "PM4 syntax mode definition -- word and comments")



;; Spu-mode entry function:

;;;###autoload
(defun pm4-dump-mode ()
  "Major mode for editing PM4 files."
  (interactive)
  (toggle-read-only t) ;; it's just a viewer.
  (kill-all-local-variables)
  (set-syntax-table pm4-syntax-table)
  (use-local-map pm4-mode-map)
  (make-local-variable 'tab-width)
  (set (make-local-variable 'font-lock-defaults) '(pm4-font-lock-keywords nil t))
  (setq tab-width 4)
  (setq major-mode 'pm4-mode)
  (setq mode-name "PM4 Dump")
  (run-hooks 'pm4-mode-hook)
  (make-local-variable 'indent-line-function)
  (set 'indent-line-function 'pm4-indent-line))

(provide 'pm4-dump-mode)
