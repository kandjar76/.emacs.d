;; Author: Cedric Lallain (clallain@naughtydog.com) 
;;
;; This file provides a major mode in order to edit the ddf files

;;
;;
;;    Opcopde definition:
;;
;;

;; List of all even opcodes
(setq ddf-keywords '("set" "first_offset" "next_offset" "map" "align" "size" "cond"))

;; List of all odd opcodes
(setq ddf-types '("UPTR" "U8" "U16" "U32" "U64" "F32" "VU16" "VU8" "VU4" "VS16"
			"VS8" "VS4" "VF4"))


;;
;;
;;    Functions to help defining the major-mode / commands:
;;
;;


(defun ddf-string-list-to-regexp (inst-list)
  "Produce from a list of strings a single regular expression which
matches any of the individual token."
  (reduce (lambda (x y) (concat x "\\|" y))
	  (mapcar (lambda (x) (concat "\\<" x "\\>"))
		  inst-list)))


;;
;;
;;    DDF Major-Mode definition:
;;
;;



;; User hook...
(defvar ddf-mode-hook nil)

;; Define the key mapping for the spu mode:
(defvar ddf-mode-map
  (let ((ddf-mode-map (make-keymap)))
    ;(define-key ddf-mode-map [(control c) (control f)]    'ddf-to-inl)
    ddf-mode-map))


(defconst ddf-font-lock-keywords-3
  (list '("\\<\\(hide\\|show\\):" . font-lock-builtin-face)
	'("<PAD[0-9]+>" . font-lock-constant-face)
	(cons (ddf-string-list-to-regexp ddf-keywords) 'font-lock-keyword-face)
	(cons (ddf-string-list-to-regexp ddf-types) 'font-lock-type-face))
  "Additional expressions to highlight in DDF mode.")


(defvar ddf-font-lock-keywords ddf-font-lock-keywords-3)

;; Syntax table:
(defvar ddf-syntax-table
  (let ((ddf-syntax-table (make-syntax-table)))

    ;; Add _ as part of the word definition:
    (modify-syntax-entry ?_ "w"   ddf-syntax-table)

    ;; Comments:
    (modify-syntax-entry ?/  ". 14"   ddf-syntax-table) ;    "/* ... */"
    (modify-syntax-entry ?*  ". 23"   ddf-syntax-table) ;    "/* ... */"
    (modify-syntax-entry ?-  ". 12b"  ddf-syntax-table) ;    "-- ... <eol>"
    (modify-syntax-entry ?\# "< b"    ddf-syntax-table) ;    "#  ... <eol>"
    (modify-syntax-entry ?\; "< b"    ddf-syntax-table) ;    ";  ... <eol>"
    (modify-syntax-entry ?\n "> b"    ddf-syntax-table) ;    "   ... <eol>"
    ddf-syntax-table)
  "DDF syntax mode definition -- word and comments")


;; Spu-mode entry function:
(defun ddf-mode ()
  "Major mode for editing SPU assembly code."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table ddf-syntax-table)
  (use-local-map ddf-mode-map)
  (make-local-variable 'tab-width)
  (set (make-local-variable 'font-lock-defaults) '(ddf-font-lock-keywords nil t))
  (setq tab-width 4)
  (setq major-mode 'ddf-mode)
  (setq mode-name "Data Definition File")
  (run-hooks 'ddf-mode-hook))

(provide 'ddf-mode)
