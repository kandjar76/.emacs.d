;
; Copyright (c) 2005 Naughty Dog, Inc.
; A Wholly Owned Subsidiary of Sony Computer Entertainment, Inc.
; Use and distribution without consent strictly prohibited
;

;; Author: Cedric Lallain (clallain@naughtydog.com) 
;; Base on an XEmacs version written by Cort Stratton (cstratton@naughtydog.com)
;;
;; Major mode to edit SPU Assembly code.
;;
;; Notable features:
;; - Full syntax highlighting
;; - Indentation of odd and even opcodes; 
;;   add cycle count of the current opcode in a comment before it
;;   if a comment already exist, update the cycle count of the opcode
;; - Rearrange instructions in one pipeline without affecting instructions
;;   in the other pipeline (C-M-up and C-M-down)
;;
;; Potential improvements
;; - spu-trim-nops command that safely removes (comments out) all pairs of
;;   unnecessary nop/lnop opcodes.
;; - highlight the variable used in the current line in diff colors (c.f.: prodg debugger)
;; - highlight stalls
;; - correct latency for double-precision instructions.
;; - Function to automatically find the best column
;; - Function to reindent the whole spu code

;; History:
;;  v1.3: Update by Cedric Lallain
;;        - spu-swap-next-instruction (C-M-down) and spu-swap-previous-instruction (C-M-up)
;;          are now working again
;;
;;  v1.2: Update by Cedric Lallain:
;;        - Indentation added mapped to tab which also update the cycle count in the comment
;;        - New function to go to the next/previous opcode (mapped: C-< and C->)
;;        - Fixed: some odd opcode were placed in the even list.
;;
;;  v1.1: Modified version by Cedric Lallain
;;        - spu-mode is being converted to be emacs compatible
;;        - The syntax highlighing works.
;;
;;  v1.0: Original version wrote by Cord Stratton, 
;;        - Full syntax highlighting
;;        - Kill and yank individual instructions instead of whole lines
;;          (C-c C-k and C-c C-y)
;;        - Rearrange instructions in one pipeline without affecting instructions
;;          in the other pipeline (C-c C-up and C-c C-down)
;;        - Jump to the line at which the results of the current instruction
;;          can be used (C-c C-r)
;;        - Insert a properly-indented nop/lnop pair (C-c C-n for uncommented, C-c C-b for
;;          commented)



;;
;;
;; Customizable variable:
;;
;;


;; Set the column for spu even opcode (need 8 spaces for the comment about the cycle count of the opcode)
(setq spu-even-column        16)
(setq spu-odd-column         88)
(setq spu-comment-size        8)
(setq spu-reg-comment-column 24)


;;
;;
;;    Opcopde definition:
;;
;;

;; List of all even opcodes
(setq spu-even-opcodes '("nop" "heq" "heqi" "hgt" "hgti" "hlgt" "hlgti" "fa" "fs" "fm" "fma"
			 "fms" "fnma" "fnms" "dfa" "dfs" "dfm" "dfma" "dfms" "dfnma" "dfnms"
			 "cntb" "avgb" "absdb" "sumb" "shlh" "shlhi" "shl" "shli" "roth" "rothi"
			 "rot" "roti" "rothm" "rothmi" "rotm" "rotmi" "rotmah" "rotmahi" "rotma"
			 "rotmai" "mpy" "mpyi" "mpyu" "mpyui" "mpya" "mpys" "mpyh" "mpyhh"
			 "mpyhhu" "mpyhha" "mpyhhau" "fi" "cuflt" "csflt" "cfltu" "cflts" "fesd"
			 "frds" "ceqb" "ceqbi" "cgtb" "cgtbi" "clgtb" "clgtbi" "ceqh" "ceqhi"
			 "cgth" "cgthi" "clgth" "clgthi" "ceq" "ceqi" "cgt" "cgti" "clgt" "clgti"
			 "fceq" "fcmeq" "fcgt" "fcmgt" "and" "nand" "andc" "andbi" "andhi" "andi"
			 "or" "nor" "orc" "orbi" "orhi" "ori" "xor" "eqv" "xorbi" "xorhi" "xori"
			 "il" "ilh" "ila" "ilhu" "iohl" "ah" "ahi" "a" "ai" "sfh" "sfhi" "sf"
			 "sfi" "bg" "bgx" "sfx" "cg" "cgx" "addx" "xsbh"  "xshw" "xswd" "clz"
			 "selb" "sync" "syncc" "dsync"))

;; List of all odd opcodes
(setq spu-odd-opcodes '("lnop" "br" "brsl" "brhnz" "brhz" "brnz" "brz" "hbrr" "bi" "bisl"
			"bisled" "bihnz" "bihz" "binz" "biz" "hbr" "hbrp" "bra" "brasl" "hbra"
			"stop" "lqa" "lqd" "lqr" "lqx" "stqa" "stqd" "stqr" "stqx" "shlqby"
			"shlqbyi" "shlqbybi" "shlqbi" "shlqbii" "rotqby" "rotqbyi" "rotqbybi"
			"rotqbi" "rotqbii" "rotqmby" "rotqmbyi" "rotqmbybi" "rotqmbi" "rotqmbii"
			"shufb" "cbd" "cbx" "chd" "chx" "cwd" "cwx" "cdd" "cdx" "fsmbi" "fsmb"
			"fsmh" "fsm" "gbb" "gbh" "gb" "frest" "frsqest" "orx" "stopd" "iretd"
			"irete" "iret" "rchcnt" "rdch" "wrch" "bie" "bid" "fscrrd" "fscrwr" 
			"mfspr" "mtspr"))


;;
;;
;;    SPU Faces:
;
;;


;; Create faces for various opcode classes.
(make-face 'spu-even-opcode-face)
(set-face-foreground 'spu-even-opcode-face (first-valid-color "salmon1" "brightcyan"))
(defvar spu-even-opcode-face 'spu-even-opcode-face
  "Font to highlight even opcodes set in SPU Assembly mode.")

(make-face  'spu-odd-opcode-face)
(set-face-foreground 'spu-odd-opcode-face (first-valid-color "cornflowerblue" "brightblue"))
(defvar spu-odd-opcode-face 'spu-odd-opcode-face
  "Font to highlight odd opcodes set in SPU Assembly mode.")

(make-face  'spu-nop-opcode-face)
(set-face-foreground 'spu-nop-opcode-face (first-valid-color "salmon4" "darkgrey"))
(defvar spu-nop-opcode-face 'spu-nop-opcode-face
  "Font to highlight nop in SPU Assembly mode.")

(make-face  'spu-lnop-opcode-face)
(set-face-foreground 'spu-lnop-opcode-face (first-valid-color "darkslateblue" "darkgrey"))
(defvar spu-lnop-opcode-face 'spu-lnop-opcode-face
  "Font to highlight lnop in SPU Assembly mode.")


;;
;;
;;   Function required to build the data:
;;
;;

(defun spu-string-list-to-regexp (inst-list)
  "Produce from a list of strings a single regular expression which
matches any of the individual opcodes."
  (reduce (lambda (x y) (concat x "\\|" y))
	  (mapcar (lambda (x) (concat "\\<" x "\\>"))
		  inst-list)))



;;
;;
;;    SPU Internal Data
;;
;;

;; Lists of opcodes in each opcode class.  Used to determine
;; cycle counts.
;;
;; unclassified opcodes:
;; "lnop" "stop" "nop" "sync" "syncc" "dsync" "fscrrd" "fscrwr" "mfspr" "mtspr"
;; "iretd" "irete" "iret" "rchcnt" "rdch" "wrch" "stop" "stopd"

(setq spu-fx-opcodes  '("heq" "heqi" "hgt" "hgti" "hlgt" "hlgti" "ceqb" "ceqbi" "cgtb" "cgtbi"
			"clgtb" "clgtbi" "ceqh" "ceqhi" "cgth" "cgthi" "clgth" "clgthi" "ceq"
			"ceqi" "cgt" "cgti" "clgt" "clgti" "fceq" "fcmeq" "fcgt" "fcmgt" "and"
			"nand" "andc" "andbi" "andhi" "andi" "or" "nor" "orc" "orbi" "orhi"
			"ori" "xor" "eqv" "xorbi" "xorhi" "xori" "il" "ilh" "ila" "ilhu" "iohl"
			"ah" "ahi" "a" "ai" "sfh" "sfhi" "sf" "sfi" "bg" "bgx" "sfx" "cg" "cgx"
			"addx" "xsbh"  "xshw" "xswd" "clz" "selb"))

(setq spu-sp-opcodes  '("fa" "fs" "fm" "fma" "fms" "fnma" "fnms" "dfa" "dfs" "dfm" "dfma" "dfms"
			"dfnma" "dfnms"))

(setq spu-bo-opcodes  '("cntb" "avgb" "absdb" "sumb"))

(setq spu-ws-opcodes  '("shlh" "shlhi" "shl" "shli" "roth" "rothi" "rot" "roti" "rothm" "rothmi"
			"rotm" "rotmi" "rotmah" "rotmahi" "rotma" "rotmai"))

(setq spu-fi-opcodes  '("mpy" "mpyi" "mpyu" "mpyui" "mpya" "mpys" "mpyh" "mpyhh" "mpyhhu"
			"mpyhha" "mpyhhau" "fi" "cuflt" "csflt" "cfltu" "cflts" "fesd" "frds"))

(setq spu-br-opcodes  '("br" "brsl" "brhnz" "brhz" "brnz" "brz" "hbrr" "bi" "bisl" "bisled"
			"bihnz" "bihz" "binz" "biz" "hbr" "hbrp" "bra" "brasl" "hbra" "bie" "bid"))

(setq spu-ls-opcodes  '("lqa" "lqd" "lqr" "lqx" "stqa" "stqd" "stqr" "stqx" "rdchcnt" "rdch" "wrch"))

(setq spu-sh-opcodes  '("shlqby" "shlqbyi" "shlqbybi" "shlqbi" "shlqbii" "rotqby" "rotqbyi"
			"rotqbybi" "rotqbi" "rotqbii" "rotqmby" "rotqmbyi" "rotqmbybi" "rotqmbi"
			"rotqmbii" "shufb" "cbd" "cbx" "chd" "chx" "cwd" "cwx" "cdd" "cdx"
			"fsmbi" "fsmb" "fsmh" "fsm" "gbb" "gbh" "gb" "frest" "frsqest" "orx"))


(setq spu-instr-max-length (max (reduce (lambda (x y) (max x (length y))) spu-even-opcodes :initial-value 0)
				(reduce (lambda (x y) (max x (length y))) spu-odd-opcodes :initial-value 0)))


(setq spu-odd-opcode-regexp
      (concat "\\(\\({\\s-*lnop\\s-*}\\)\\|" (spu-string-list-to-regexp spu-odd-opcodes) "\\)"))

(setq spu-even-opcode-regexp
      (concat "\\(\\({\\s-*nop\\s-*}\\)\\|" (spu-string-list-to-regexp spu-even-opcodes) "\\)"))

(setq spu-no-opcodes-lines-regexp  "^[ \t]*\\([;.]\\|\\w+:\\)")


;;
;;
;;    Functions to help defining the major-mode / commands:
;;
;;


(defun spu-cycle-count (opcode)
  "Returns the cycle count of the specified opcode."
  (cond
   ((member opcode spu-fx-opcodes) 2)
   ((member opcode spu-sp-opcodes) 6)
   ((member opcode spu-bo-opcodes) 4)
   ((member opcode spu-ws-opcodes) 4)
   ((member opcode spu-fi-opcodes) 7)
   ((member opcode spu-br-opcodes) 1) ; Well, I guess that depends...
   ((member opcode spu-ls-opcodes) 6)
   ((member opcode spu-sh-opcodes) 4)
   ((member opcode '("lnop" "nop" "stop")) 1)
   (t 0))) ;(error (format "Unknown opcode: %s" opcode)))))


(defun spu-find-even-opcode()
  "Return the column of the even opcode in this line (nil if no even opcodes are found)"
  (save-excursion
    (let (start end line fwd)
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq start (point))
      (setq line (buffer-substring start end))
      (setq fwd (string-match spu-even-opcode-regexp line))
      (if fwd
	  (forward-char fwd))
      (and fwd (current-column)))))

(defun spu-find-odd-opcode()
  "Return the column of the even opcode in this line (nil if no odd opcodes are found)"
  (save-excursion
    (let (start end line fwd)
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq start (point))
      (setq line (buffer-substring start end))
      (setq fwd (string-match spu-odd-opcode-regexp line))
      (if fwd
	  (forward-char fwd))
      (and fwd (current-column)))))

(defun spu-detect-no-opcodes-line()
  "Return nil if the line could be consider as it as <with opcode> line.
A line starting with a comment, a preprocessor or a label will be consider as no-opcodes line.
If the current line respond true to those tests, N is return. N representing the number of white 
spaces / tabs present at the beginning of the line."
  (save-excursion
    (let (start end line fwd)
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq start (point))
      (setq line (buffer-substring start end))
      (and (string-match spu-no-opcodes-lines-regexp line) (string-match "[^ \t]" line)))))

(defun spu-detect-opcodes-line()
  "Return t if an opcode has been found at that line."
  (save-excursion
    (let (start end line fwd)
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq start (point))
      (setq line (buffer-substring start end))
      (or (string-match spu-even-opcode-regexp line)
	  (string-match spu-odd-opcode-regexp line)))))
  
(defun spu-find-comment-before-opcode(column)
  "Starting at the column COLUMN, the function will search backward up to the beginning of the line
or up to the previous non blank character in order to find a comment delimited by '{' and '}'. 
Return the couple ( start-comment . end-comment )."
  (save-excursion
   (beginning-of-line)
   (let ((bol (point))
	 end-comment)
     (if (> column 0)
	 (progn (move-to-column column)
		(backward-char 1)
		(while (and (> (point) bol)
			    (or (= (char-after (point)) 32)
				(= (char-after (point)) ?\t)))
		  (backward-char 1))
		;;(message (format "column: %i" (current-column)))
		(if (= (char-after) ?})
		    (progn (setq end-comment (+ (point) 1))
			   (while (and (> (point) bol)
				       (/= (char-after (point)) ?{))
			     (backward-char 1))
			   (if (= (char-after) ?{)
			       (let ((comment-string (buffer-substring (point) end-comment)))
				 (if (not (or (string= comment-string "{nop}")
					      (string= comment-string "{lnop}")))
				     (cons (point) end-comment)))))))))))

(defun spu-validate-comment(comment-string)
  "Returns t if the comment isn't a commented nop spu opcode"
  (not (or (string= comment-string "{nop}")
	   (string= comment-string "{lnop}"))))

(defun spu-update-comment-cycle-count(opcode-column comment-points)
  "Update the comment if it start with '{e...' or '{o...' with the
correct cycle count of the current spu opcode"
  (save-excursion 
    (move-to-column opcode-column)
    (let* ((specnop     (= (char-after) ?{))
	   (instr       (current-word))
	   (nop         (or specnop (or (string= instr "nop") (string= instr "lnop"))))
	   (cycle-count (spu-cycle-count instr))
	   (comment     (buffer-substring (car comment-points) (cdr comment-points)))
	   (cycle-pos   (+ (car comment-points) 2)))
      (if (and (spu-validate-comment comment)
	       (string-match "^{[eo][0-9?][ }]" comment))
	  (if nop
	      ;; Opcode is {nop} {lnop} nop or lnop
	      ;; Let's remove the comment
	      (progn (save-excursion
		       (delete-region (car comment-points) (cdr comment-points)))
		     (indent-to-column opcode-column))
	      (progn (goto-char cycle-pos)
		     (delete-char 1)
		     (insert-char (+ ?0 cycle-count) 1)))))))


(defun spu-insert-comment-cycle-count(opcode-column opcode-char-type)
  "Insert a cycle-count comment before the opcode."
  (save-excursion
    (move-to-column opcode-column)
    (let ((start (point))
	  end)
      (insert (format "{%c? -}" opcode-char-type))
      (setq end (point))
      (insert " ")
      (cons start end))))

(defun spu-clear-whitespaces-before(point)
  "Clear all but one whitespace / tabs before the point.
Assumes that the char before POINT is a whitespace or a tab"
  (goto-char point)
  (while (and (char-before)
	      (or (= (char-before) 32)
		  (= (char-before) ?\t)))
    (delete-backward-char 1))
  (insert " "))

(defun spu-end-of-line(pos)
  "Returns t if POS is at the end of line or if the only chars after are spaces/tabs"
  (save-excursion 
    (goto-char pos)
    (while (and (char-after)
		(or (= (char-after) 32)
		    (= (char-after) ?\t)))
      (forward-char 1))
    (let ((cur (point)))
      (end-of-line)
      (= cur (point)))))

(defun spu-indent-args(opcode-column)
  "Helper function for the indent function.
Indent the argument on a common column
OPCODE-COLUMN must point to the first char of the opcode"
  (save-excursion
    (let (end)
      (save-excursion
	(end-of-line)
	(setq end (point)))
      (move-to-column opcode-column)
      (forward-char)
      (if (and (not (string= (current-word) "nop"))
	       (not (string= (current-word) "lnop")))
	  (progn (while (and (char-after)
			     (/= (char-after) 32)
			     (/= (char-after) ?\t)
			     (< (point) end))
		   (forward-char 1))
		 (if (char-after)
		     (progn (while (and (char-after)
					(or (= (char-after) 32)
					    (= (char-after) ?\t))
					(< (point) end))
			      (delete-char 1)
			      (save-excursion
				(end-of-line)
				(setq end (point))))
			    (if (char-after)
				(indent-to-column (+ opcode-column 1 spu-instr-max-length))))))))))

(defun spu-indent-and-comment-opcodes()
  "Helper function for the indent function.
Assumes that the current line has / will have opcodes on it."
  (let ((even (spu-find-even-opcode))
	(odd  (spu-find-odd-opcode)))
    (cond 
     ;; Case 1: no even opcode but one odd opcode
     ((and (not even) odd)
      ;; Add a {nop} at the beginning of the line.
      (save-excursion
	(let ((comment (spu-find-comment-before-opcode odd)))
	  (if (not comment)
	      (spu-insert-comment-cycle-count odd ?o))
	  (beginning-of-line)
	  (insert "{nop}")
	  (beginning-of-line)
	  (indent-to-column spu-even-column)
	  (setq odd (spu-find-odd-opcode))
	  (setq comment (spu-find-comment-before-opcode odd))
	  (spu-update-comment-cycle-count odd comment)
	  (spu-clear-whitespaces-before (car comment))
	  (indent-to-column (- spu-odd-column spu-comment-size))
	  (setq odd (spu-find-odd-opcode))
	  (move-to-column odd)
	  (spu-clear-whitespaces-before (point))
	  (indent-to-column spu-odd-column)
	  (spu-indent-args (current-column)))))
     ;; Case 2: even opcode but no odd opcode
     ((and even (not odd))
      (save-excursion
	(let ((comment (spu-find-comment-before-opcode even)))
	  (if (not comment)
	      (progn (setq comment (spu-insert-comment-cycle-count even ?e))
		     (setq even (spu-find-even-opcode))))
	  (spu-update-comment-cycle-count even comment)
	  (spu-clear-whitespaces-before (car comment))
	  (indent-to-column (- spu-even-column spu-comment-size))
	  (setq even (spu-find-even-opcode))
	  (move-to-column even)
	  (spu-clear-whitespaces-before (point))
	  (indent-to-column spu-even-column)
	  (spu-indent-args (current-column)))))
     ;; Case 3: even and odd opcodes found
     ((and even odd)
      (save-excursion
	(let ((comment (spu-find-comment-before-opcode even)))
	  (if (not comment)
	      (progn (setq comment (spu-insert-comment-cycle-count even ?e))
		     (setq even (spu-find-even-opcode))))
	  (spu-update-comment-cycle-count even comment)
	  (spu-clear-whitespaces-before (car comment))
	  (indent-to-column (- spu-even-column spu-comment-size))
	  (setq even (spu-find-even-opcode))
	  (move-to-column even)
	  (spu-clear-whitespaces-before (point))
	  (indent-to-column spu-even-column)
	  (spu-indent-args (current-column))
	  (setq odd (spu-find-odd-opcode))
	  (setq comment (spu-find-comment-before-opcode odd))
	  (if (not comment)
	      (progn (setq comment (spu-insert-comment-cycle-count odd ?o))
		     (setq odd (spu-find-odd-opcode))))
	  (spu-update-comment-cycle-count odd comment)
	  (spu-clear-whitespaces-before (car comment))
	  (indent-to-column (- spu-odd-column spu-comment-size))
	  (setq odd (spu-find-odd-opcode))
	  (move-to-column odd)
	  (spu-clear-whitespaces-before (point))
	  (indent-to-column spu-odd-column)
	  (spu-indent-args (current-column)))))
	)))

(defun spu-indent-end-of-line()
  "Helper function for the indent function."
  (let ((col (current-column)))
    (end-of-line)
    (spu-clear-whitespaces-before (point))
    (delete-backward-char 1)
    (let ((even (spu-find-even-opcode))
	  (odd  (spu-find-odd-opcode))
	  (added-new-line nil))
      (save-excursion
	(cond
	 ;; Case 1: no even but odd
	 ((and (not even) odd)
	  (spu-indent-and-comment-opcodes))
	 
	 ;; Case 2: even but no odd
	 ((and even (not odd))
	  (if (>= col spu-odd-column)
	      (progn (insert "{lnop}")
		     (spu-indent-and-comment-opcodes)
		     (newline)
		     (setq added-new-line t)
		     (indent-to-column spu-even-column))
	      (progn (spu-indent-and-comment-opcodes)
		     (indent-to-column spu-odd-column))))
	 ;; Case 3: odd and even 
	 ((and even odd)
	  (spu-indent-and-comment-opcodes)
	  (newline)
	  (setq added-new-line t)
	  (indent-to-column spu-even-column))

	 ;; Case 4: no even, no odd
	 ((and (not even) (not odd))
	  (if (>= col spu-even-column)
	      (progn (insert "{nop}")
		     (spu-indent-and-comment-opcodes)
		     (end-of-line)
		     (indent-to-column spu-odd-column))
	      (indent-to-column spu-even-column)))))
      (if added-new-line
	  (next-line 1))
      (end-of-line))))

(defun spu-indent-and-comment(end-of-line)
  "Internal Indent function for SPU-mode"
  (let ((no-opcodes-result (spu-detect-no-opcodes-line)))
    (if no-opcodes-result
	;; Comment / Label / Preprocessor command -> beginning of the line
	(save-excursion
	  (beginning-of-line)
	  (delete-char no-opcodes-result))
	;; Otherwise we are dealing with an opcode line.
	(if (and end-of-line (spu-end-of-line (point)))
	    (spu-indent-end-of-line)
	    (spu-indent-and-comment-opcodes)))))


(defun spu-get-end-of-line-position()
  "Return the position of the end of the buffer"
  (save-excursion
    (end-of-line)
    (point)))

(defun spu-get-beginning-of-line-position()
  "Return the position of the end of the buffer"
  (save-excursion
    (beginning-of-line)
    (point)))

(defun spu-column-to-pos(column)
  "Convert the column into a position assuming that the cursor is at the current line"
  (save-excursion
    (move-to-column column)
    (point)))

(defun spu-swap-instructions(bol1 bol2 current-column)
  "Swap two instructions.
BOL1 represents the 'point' at the beginning of the initial line
BOL2 represents the 'point' at the beginning of the target line
CURRENT-COLUMN is the column value to decide which instruction to swap, the even of the one one."
  (let (swap-odd
	odd-column-1    odd-column-2
	comment-1       comment-2
	end-of-even-1   end-of-even-2
	instr-1         instr-2
	even-column-1   even-column-2
	tmp)
    (goto-char bol1)
    (setq odd-column-1  (spu-find-odd-opcode))
    (setq swap-odd      (and odd-column-1 (>= current-column odd-column-1)))
    (if (> bol1 bol2)
	(setq tmp bol1 bol1 bol2 bol2 tmp))
    
    (goto-char bol1)
    (setq odd-column-1  (spu-find-odd-opcode))
    (setq comment-1     (and odd-column-1 (spu-find-comment-before-opcode odd-column-1)))
    (setq end-of-even-1 (or (and comment-1 (car comment-1)) (and odd-column-1 (spu-column-to-pos odd-column-1)) (spu-get-end-of-line-position)))
    (goto-char bol2)
    (setq odd-column-2  (spu-find-odd-opcode))
    (setq comment-2     (and odd-column-2 (spu-find-comment-before-opcode odd-column-2)))
    (setq end-of-even-2 (or (and comment-2 (car comment-2)) (and odd-column-2 (spu-column-to-pos odd-column-2)) (spu-get-end-of-line-position)))
    (if swap-odd
	(progn (goto-char bol1)
	       (setq instr-1 (buffer-substring end-of-even-1 (spu-get-end-of-line-position)))
	       (goto-char bol2)
	       (setq instr-2 (buffer-substring end-of-even-2 (spu-get-end-of-line-position)))
	       (goto-char end-of-even-2)
	       (delete-region end-of-even-2 (spu-get-end-of-line-position))
	       (spu-clear-whitespaces-before end-of-even-2)
	       (if comment-1 
		   (indent-to-column (- spu-odd-column spu-comment-size))
		   (indent-to-column spu-odd-column))
	       (insert instr-1)

	       (goto-char bol2)
	       (setq odd-column-2 (spu-find-odd-opcode))
	       (if odd-column-2
		   (goto-char (spu-column-to-pos odd-column-2)))

	       (save-excursion
		 (goto-char end-of-even-1)
		 (delete-region end-of-even-1 (spu-get-end-of-line-position))
		 (spu-clear-whitespaces-before end-of-even-1)
		 (if comment-2 
		     (indent-to-column (- spu-odd-column spu-comment-size))
		     (indent-to-column spu-odd-column))
		 (insert instr-2))

	       (if tmp
		   (progn (goto-char bol1)
			  (setq odd-column-1 (spu-find-odd-opcode))
			  (if odd-column-1
			      (goto-char (spu-column-to-pos odd-column-1))))))
	(progn (setq instr-1 (buffer-substring bol1 end-of-even-1))
	       (setq instr-2 (buffer-substring bol2 end-of-even-2))
	       (goto-char bol2)
	       (delete-region bol2 end-of-even-2)
	       (insert instr-1)
	       (spu-clear-whitespaces-before (point))
	       (if odd-column-2
		   (if comment-2
		       (indent-to-column (- spu-odd-column spu-comment-size))
		       (indent-to-column spu-odd-column)))

	       (goto-char bol2)
	       (setq even-column-2 (spu-find-even-opcode))
	       (if even-column-2
		   (goto-char (spu-column-to-pos even-column-2)))
	       
	       (save-excursion
		 (goto-char bol1)
		 (delete-region bol1 end-of-even-1)
		 (insert instr-2)
		 (spu-clear-whitespaces-before (point))
		 (if odd-column-1
		     (if comment-1
			 (indent-to-column (- spu-odd-column spu-comment-size))
			 (indent-to-column spu-odd-column))))
	       
	       (if tmp
		   (progn (goto-char bol1)
			  (setq even-column-1 (spu-find-even-opcode))
			  (if even-column-1
			      (goto-char (spu-column-to-pos even-column-1)))))))))
      
      
;;
;;
;;    SPU Interactive command:
;;
;;


(defun spu-indent()
  "Indent function for SPU-mode"
  (interactive "*")
  (if (is-region-active)
      (let ((start (region-beginning))
	    (end   (region-end)))
	(save-excursion
	  (goto-char start)
	  (beginning-of-line)
	  (while (and (bolp)
		      (not (eobp))
		      (< (point) end))
	    (if (not (spu-find-odd-opcode))
		(save-excursion
		  (end-of-line)
		  (insert " {lnop}")))
	    (spu-indent-and-comment nil)
	    (forward-line 1))))
      (spu-indent-and-comment t)))


(defun spu-next-opcode()
  "Go to the next-opcode"
  (interactive)
  (let ((even (spu-find-even-opcode))
	(odd  (spu-find-odd-opcode))
	(col  (current-column)))
    (cond ((and even (< col even)) (move-to-column even))
	  ((and odd  (< col odd))  (move-to-column odd))
	  (t (end-of-line)
	     (forward-char) ; the function will stop if we reach the end of the buffer
	     (spu-next-opcode)))))


(defun spu-previous-opcode()
  "Go to the previous-opcode"
  (interactive)
  (let ((even (spu-find-even-opcode))
	(odd  (spu-find-odd-opcode))
	(col  (current-column)))
    (cond ((and odd  (> col odd)) (move-to-column odd))
	  ((and even (> col even))(move-to-column even))
	  (t (beginning-of-line)
	     (backward-char) ; the function will stop if we reach the beginning of the buffer
	     (spu-previous-opcode)))))



;;(defun spu-swap-point-and-mark()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (beginning-of-line)
;;     (let ((bol1 (point)))
;;       (goto-char (mark))
;;       (beginning-of-line)
;;       (let ((bol2 (point)))
;; 	(spu-swap-instructions bol1 bol2 col)))))




(defun spu-swap-next-instruction()
  "Swaps the SPU instruction currently under the point with the one on the next
line, leaving the corresponding instructions in the other pipe unaffected."
  (interactive "*")
  (if (spu-detect-no-opcodes-line)
      (message "The current line doesn't contain any opcodes.")
      (let ((column (current-column))
	    (bol1   (spu-get-beginning-of-line-position))
	    bol2)

	;; Find the next line:
	(save-excursion 
	  (forward-line 1)
	  (while (and (or (spu-detect-no-opcodes-line)
			  (not (spu-detect-opcodes-line)))
		      (not (eobp)))
	    (forward-line 1))
	  (if (and (not (spu-detect-no-opcodes-line))
		   (spu-detect-opcodes-line))
	      (setq bol2 (spu-get-beginning-of-line-position))))
	;; Swap:
	(if bol2
	    (spu-swap-instructions bol1 bol2 column)
	    (message "End of buffer!")))))


(defun spu-swap-previous-instruction()
  "Swaps the SPU instruction currently under the point with the one on the previous
line, leaving the corresponding instructions in the other pipe unaffected."
  (interactive "*")
  (if (spu-detect-no-opcodes-line)
      (message "The current line doesn't contain any opcodes.")
      (let ((column (current-column))
	    (bol1   (spu-get-beginning-of-line-position))
	    bol2)

	;; Find the next line:
	(save-excursion 
	  (forward-line -1)
	  (while (and (or (spu-detect-no-opcodes-line)
			  (not (spu-detect-opcodes-line)))
		      (not (bobp)))
	    (forward-line -1))
	  (if (and (not (spu-detect-no-opcodes-line))
		   (spu-detect-opcodes-line))
	      (setq bol2 (spu-get-beginning-of-line-position))))
	;; Swap:
	(if bol2
	    (spu-swap-instructions bol1 bol2 column)
	    (message "Beginning of buffer!")))))


(defun spu-valid-opcode-p (opcode)
  "Returns t if opcode is a valid SPU opcode; returns nil otherwise."
  (and (or (member opcode spu-even-opcodes) (member opcode spu-odd-opcodes)) t))


(defun spu-print-cycle-count (opcode)
  "Prints the cycle count of the specified opcode."
  (interactive "sOpcode: ")
  (message (format "%d cycle(s)" (spu-cycle-count opcode))))

(defun spu-add-current-word-to-register-list(comment)
  "Add the current word in the register section. ( i.e: after ';; <Registers>' )"
  (interactive "sComment: ")
  (save-excursion
    (save-match-data
      (let ((register-name (current-word)))
	(beginning-of-buffer)
	(if (not (search-forward "<Registers>" nil t))
	    (message "[error] Can't add the register, register list not found. Make sure you have the text: '<Registers>' in your file")
	    (forward-line 1)
	    (beginning-of-line)
	    (while (and (bolp)
			(not (eobp))
			(string-match "^\\.[Rr][Ee][Gg][ \t]"
				      (buffer-substring (spu-get-beginning-of-line-position) 
							(spu-get-end-of-line-position))))
	      (forward-line 1))
	    (forward-line -1)
	    (end-of-line)
	    (newline)
	    (insert (concat ".reg " register-name))
	    (if (> (length comment) 0)
		(progn (indent-to-column spu-reg-comment-column)
		       (insert (concat " ; " comment)))))))))




;(defun spu-swap-next-instruction (&optional n)
;  "Swaps the SPU instruction currently under the point with the one on the next
;line, leaving the corresponding instructions in the other pipe unaffected. If
;the optional parameter n is provided, the operation is repeated n times.  If n
;is negative, the instruction is swapped into the previous line instead of the
;next one."
;  (interactive "*P")
;  (let ((num-lines (abs (prefix-numeric-value n)))
;		(spu-evenp (< (current-column) (spu-odd-instruction-column)))
;		(move-direction (if (> (prefix-numeric-value n) 0) 1 -1))
;		)
;	;; Store the difference between the current column and the beginning og the
;	;; current instruction, so we can restore point to the "same" place after
;	;; swapping.
;	(set-register ?e (- (current-column) (if spu-evenp 0 (spu-odd-instruction-column))))
;	(while (progn (setq num-lines (- num-lines 1)) (>= num-lines 0))
;	  (move-to-column (if spu-evenp 0 (spu-odd-instruction-column)))
;	  (point-to-register ?c)
;	  ;; Figure out where we're swapping to, first.  If we can't find
;	  ;; a destination, abort without changing anything.
;	  (save-excursion
;		(while (progn (forward-line move-direction) (not (spu-valid-line-p)))
;		  (cond ((bobp) (error "Reached beginning of buffer"))
;				((eobp) (error "Reached end of buffer"))))
;		(move-to-column (if spu-evenp 0 (spu-odd-instruction-column)))
;		(point-to-register ?d)
;		)
;	  ;; Grab the instruction under the cursor.
;	  (jump-to-register ?c)
;	  (spu-store-current-instruction ?a t)
;	  (jump-to-register ?d)
;	  (spu-store-current-instruction ?b t)
;	  ;; Yank the instructions into each other's lines.
;	  (insert-register ?a)
;	  (jump-to-register ?c)
;	  (insert-register ?b)
;	  ;; Restore state
;	  (jump-to-register ?d)
;	  (move-to-column (+ (get-register ?e)
;						 (if spu-evenp 0 (spu-odd-instruction-column))))
;	  (message nil)
;	  )))



;;
;;
;;    SPU Major-Mode definition:
;;
;;



;; User hook...
(defvar spu-mode-hook nil)

;; Define the key mapping for the spu mode:
(defvar spu-mode-map
  (let ((spu-mode-map (make-keymap)))
    (define-key spu-mode-map [(control c) (control f)]    'spu-reformat-region)
    (define-key spu-mode-map [(control c) (control d)]    'spu-rollup-dependency-report-region)
    (define-key spu-mode-map [(control c) (control r)]    'spu-rollup-fusion-region)
    (define-key spu-mode-map [(tab)]                      'spu-indent)
    (define-key spu-mode-map [(control >)]                'spu-next-opcode)
    (define-key spu-mode-map [(control <)]                'spu-previous-opcode)
    (define-key spu-mode-map [(control meta down)]        'spu-swap-next-instruction)
    (define-key spu-mode-map [(control meta up)]          'spu-swap-previous-instruction)
    (define-key spu-mode-map [(control meta left)]        'spu-next-opcode)
    (define-key spu-mode-map [(control meta right)]       'spu-previous-opcode)
    (define-key spu-mode-map [(control c) ?a]             'spu-add-current-word-to-register-list)
    spu-mode-map))

(defconst spu-font-lock-keywords-3
  (list '("\\.global\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" . 1) ; exported symbol names
	'("\\_*\\<\\(\\sw\\|\\s_\\)+\\>\\_*:" . font-lock-function-name-face) ; labels
	'("\\(\\s-\\|^\\)\\.\\(\\(\\sw\\|\\s_\\)+\\)" . font-lock-builtin-face) ; preprocessor commands
	'("\\<nop\\>" . spu-nop-opcode-face)
	'("\\<lnop\\>" . spu-lnop-opcode-face)
	(cons (spu-string-list-to-regexp spu-even-opcodes) 'spu-even-opcode-face)
	(cons (spu-string-list-to-regexp spu-odd-opcodes) 'spu-odd-opcode-face))
  "Additional expressions to highlight in SPU mode.")

(defvar spu-font-lock-keywords spu-font-lock-keywords-3)

;; Syntax table:
(defvar spu-syntax-table
  (let ((spu-syntax-table (make-syntax-table)))

    ;; Add _ as part of the word definition:
    (modify-syntax-entry ?_ "w"   spu-syntax-table)

    ;; C and C++-style comments should be treated as such.  Since Emacs
    ;; only offers two styles of comments, C-style comments will
    ;; regretably pair with {}-style comments and vice-versa. 
    ;; Those comments will be from the category "a", the "b" comments
    ;; style will indeed be the comment "line"
    (modify-syntax-entry ?{  "<"      spu-syntax-table)
    (modify-syntax-entry ?}  ">"      spu-syntax-table)
    (modify-syntax-entry ?/  ". 124b" spu-syntax-table)
    (modify-syntax-entry ?*  ". 23"   spu-syntax-table)
    (modify-syntax-entry ?\# "< b"    spu-syntax-table)
    (modify-syntax-entry ?\; "< b"    spu-syntax-table)
    (modify-syntax-entry ?\n "> b"    spu-syntax-table)
    spu-syntax-table)
  "Spu syntax mode definition -- word and comments")

;; Spu-mode entry function:
(defun spu-mode ()
  "Major mode for editing SPU assembly code."
  (interactive)
  (make-local-variable 'comment-end-skip)
  (setq comment-end-skip "\\$")
  ;;(setq comment-end-skip "[ \t]*\\(\\s>\\|\\*+/\\)")
  (kill-all-local-variables)
  (set-syntax-table spu-syntax-table)
  (use-local-map spu-mode-map)
  (make-local-variable 'tab-width)
  (set (make-local-variable 'font-lock-defaults) '(spu-font-lock-keywords nil t))
  (setq tab-width 8)
  (setq major-mode 'spu-mode)
  (setq mode-name "SPU Assembly Code")
  (run-hooks 'spu-mode-hook))

(provide 'spu-mode)
