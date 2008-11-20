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
    (define-key ddf-mode-map [?{] '(lambda () (interactive) (insert "{") (ddf-indent-line)))
    (define-key ddf-mode-map [?}] '(lambda () (interactive) (insert "}") (ddf-indent-line)))
    (define-key ddf-mode-map [?:] '(lambda () (interactive) (insert ":") (save-excursion
									   (beginning-of-line)
									   (if (looking-at "^[ \t]*[A-Za-z_][A-Za-z_0-9]*:")
									       (ddf-indent-line)))))
    ddf-mode-map))


(defun ddf-indent-line()
  "Indentation for idf file"
  (interactive)
  ;; We save the excursion here to not move the cursor if it is in the middle of the line
  (save-excursion
    (let (cur-indent)
      (save-excursion
	(beginning-of-line)
	;; Beginning of buffer, indent -> 0
	(if (bobp)
	    (ident-line-to 0)
	    ;; We are not in the beginning of the buffer, therefore we can consider the different indentation
	    ;; option looking at the previous line:
	    (let ((not-indented t))
	      ;; End of block: } ?
	      (if (looking-at "^[ \t]*}")
		  (progn
		    (save-excursion
		      (forward-line -1)
		      ;; We don't want an empty line
		      (while (and (not (bobp))
				  (looking-at "^[ \t]*$"))
			(forward-line -1))
		      ;; Didn't find any line non empty
		      (if (bobp)
			  (setq cur-indent 0)
			  ;; If it's an empty block, don't increase the indentation...
			  (if (and (not (looking-at "^[ \t]*{")) ; Beginning of block
				   (not (looking-at "^[ \t]*[A-Za-z_][A-Za-z_0-9]*:")))	; Label
			      (setq cur-indent (- (current-indentation) tab-width))
			      (setq cur-indent (current-indentation))))
		      (if (< cur-indent 0)
			  (setq cur-indent 0))))
		  (save-excursion
		    (let ((on-label (looking-at "^[ \t]*[A-Za-z_][A-Za-z_0-9]*:")))
		      (while not-indented
			(forward-line -1)
			;; We found the end of a previous block... -> same indentation
			(cond ((looking-at "^[ \t]*}")
			       (setq cur-indent (current-indentation)
				     not-indented nil))
			      ;; We found a beginning of a block or a label:
			      ((or (looking-at "^[ \t]*{")
				   (looking-at "^[ \t]*[A-Za-z_][A-Za-z_0-9]*:"))
			       (setq cur-indent (+ (current-indentation) tab-width)
				     not-indented nil))
			      ;; We went up to the beginning of the buffer... let's stop
			      ((bobp) (setq not-indented nil))))
		      (when on-label
			(setq cur-indent (- cur-indent tab-width))
			(if (< cur-indent 0)
			    (setq cur-indent 0)))
		      ))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	  (indent-line-to 0))
      ))
  ;; One exception with the cursor:
  (if (< (current-column) (current-indentation))
      (move-to-column (current-indentation))
  ))



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
  (run-hooks 'ddf-mode-hook)
  (make-local-variable 'indent-line-function)
  (set 'indent-line-function 'ddf-indent-line))

(provide 'ddf-mode)
