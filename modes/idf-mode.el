;; Author: Cedric Lallain (clallain@naughtydog.com) 
;;
;; This file provides a major mode in order to edit the idf files

;;
;;
;;    Opcopde definition:
;;
;;

;; List of all even opcodes

;;
;;
;;    Functions to help defining the major-mode / commands:
;;
;;


(defun idf-string-list-to-regexp (inst-list)
  "Produce from a list of strings a single regular expression which
matches any of the individual token."
  (reduce (lambda (x y) (concat x "\\|" y))
	  (mapcar (lambda (x) (concat "\\<" x "\\>"))
		  inst-list)))


;;
;;
;;    IDF Major-Mode definition:
;;
;;



;; User hook...
(defvar idf-mode-hook nil)

(defconst idf-font-lock-keywords-3
  (list '("<!--.*-->" . font-lock-comment-face)
	'("<\\(/?addr\\|/?opcode\\|/?body\\|/?unittest\\|/?postopcode\\|branch/\\|ldst/\\)>" . font-lock-variable-name-face)
	'("</?group>" . font-lock-builtin-face)
	'("</?instr>" . font-lock-type-face)
	'("<instr>\\(.*\\)$" 1 font-lock-operator-face)
	'("<unittest>\\(.*\\)$" 1 font-lock-constant-face)
	
	)
    "Additional expressions to highlight in IDF mode.")

(defun idf-indent-line()
  "Indentation for idf file"
  (interactive)
  (let (cur-indent)
    (save-excursion
      (beginning-of-line)
      ;; Beginning of buffer, indent -> 0
      (if (bobp)
	  (ident-line-to 0)
	  ;; We are not in the beginning of the buffer, therefore we can consider the different indentation
	  ;; option looking at the previous line:
	  (let ((not-indented t))
	    ;; End of block: </...> ?
	    (if (looking-at "^[ \t]*</")
		(progn
		  (save-excursion
		    (forward-line -1)
		    (while (and (not (bobp))
				(looking-at "^[ \t]*$"))
		      (forward-line -1))
		    (if (bobp)
			(setq cur-indent 0)
			;; If it's an empty block, don't increase the indentation...
			(if (not (looking-at "^[ \t]*<[^!/].*[^/]>"))
			    (setq cur-indent (- (current-indentation) tab-width))
			    (setq cur-indent (current-indentation))))
		    (if (< cur-indent 0)
			(setq cur-indent 0))))
		(save-excursion
		  (while not-indented
		    (forward-line -1)
		    ;; We found the end of a previous block... -> same indentation
		    (cond ((looking-at "^[ \t]*</")
			   (setq cur-indent (current-indentation)
				 not-indented nil))
			  ;; We found a beginning of a block:
			  ((looking-at "^[ \t]*<[^!].*[^/]>")
			   (setq cur-indent (+ (current-indentation) tab-width)
				 not-indented nil))
			  ;; We went up to the beginning of the buffer... let's stop
			  ((bobp) (setq not-indented nil)))))))))
    (if cur-indent
	(indent-line-to cur-indent)
	(indent-line-to 0))
    ))

(defun idf-indent-line-or-region()
  (interactive)
  (if mark-active
      (indent-region (region-beginning) (region-end) nil))
      (idf-indent-line))
      
;; Define the key mapping for the spu mode:
(defvar idf-mode-map
  (let ((idf-mode-map (make-keymap)))
    (define-key idf-mode-map [(tab)]    'idf-indent-line-or-region)
    idf-mode-map))


(defvar idf-font-lock-keywords idf-font-lock-keywords-3)

;; Syntax table:
(defvar idf-syntax-table
  (let ((idf-syntax-table (make-syntax-table)))

    ;; Add _ as part of the word definition:
    (modify-syntax-entry ?_ "w"   idf-syntax-table)

    ;; Comments:
    (modify-syntax-entry ?<  "(>"   idf-syntax-table) ;    "/* ... */"
    (modify-syntax-entry ?>  ")<"   idf-syntax-table) ;    "/* ... */"
    (modify-syntax-entry ?*  ". 23"   idf-syntax-table) ;    "/* ... */"
;;     (modify-syntax-entry ?/  ". 14"   idf-syntax-table) ;    "/* ... */"
;;     (modify-syntax-entry ?*  ". 23"   idf-syntax-table) ;    "/* ... */"
;;     (modify-syntax-entry ?-  ". 12b"  idf-syntax-table) ;    "-- ... <eol>"
;;     (modify-syntax-entry ?\# "< b"    idf-syntax-table) ;    "#  ... <eol>"
;;     (modify-syntax-entry ?\; "< b"    idf-syntax-table) ;    ";  ... <eol>"
;;     (modify-syntax-entry ?\n "> b"    idf-syntax-table) ;    "   ... <eol>"
    idf-syntax-table)
  "IDF syntax mode definition -- word and comments")

;; Spu-mode entry function:
(defun idf-mode ()
  "Major mode for editing SPU assembly code."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table idf-syntax-table)
  (use-local-map idf-mode-map)
  (make-local-variable 'tab-width)
  (set (make-local-variable 'font-lock-defaults) '(idf-font-lock-keywords nil t))
  (setq tab-width 4)
  (setq major-mode 'idf-mode)
  (setq mode-name "Instr Definition File")
  (run-hooks 'idf-mode-hook)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start "<!--")
  (setq comment-end "-->")
  (set 'indent-line-function 'idf-indent-line))


(provide 'idf-mode)
