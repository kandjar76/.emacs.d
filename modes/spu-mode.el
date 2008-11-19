;
; Copyright (c) 2005 Naughty Dog, Inc.
; A Wholly Owned Subsidiary of Sony Computer Entertainment, Inc.
; Use and distribution without consent strictly prohibited
;

;; Author: Cort Stratton (cstratton@naughtydog.com)
;;         Cedric Lallain (clallain@naughtydog.com) 
;;
;; This file provides some extensions to the standard emacs asm-mode
;; which are useful when editing SPU code.  It's kinda hacky, and relies
;; more heavily than it probably should on the assumption that your code
;; meets the official NDI SPU coding standard.  Nevertheless, I've been using
;; it for months and it's treated me well.
;;
;; Notable features:
;; - Full syntax highlighting
;; - Kill and yank individual instructions instead of whole lines
;;   (C-c C-k and C-c C-y)
;; - Rearrange instructions in one pipeline without affecting instructions
;;   in the other pipeline (C-c C-up and C-c C-down)
;; - Jump to the line at which the results of the current instruction
;;   can be used (C-c C-r)
;; - Insert a properly-indented nop/lnop pair (C-c C-n for uncommented, C-c C-b for
;;   commented)
;; - Automatically schedule a block of code using the external spuscheduler program
;;   (no shortcut currently defined; to use, set the region to encompass the code you
;;   wish to schedule, then use the spu-schedule-region command)
;;
;; Potential improvements
;; - spu-indent-buffer command that tidies up ALL whitespace according
;;   to The Standard, whatever that is. It could also insert per-opcode comments
;;   and that sort of thing, perhaps.
;; - spu-trim-nops command that safely removes (comments out) all pairs of
;;   unnecessary nop/lnop opcodes.
;; - correct latency for double-precision instructions.

;; History:
;;  v1.1: Modified version by Cedric Lallain
;;        - spu-mode is now emacs compatible
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
;;        - Automatically schedule a block of code using the external spuscheduler program
;;          (no shortcut currently defined; to use, set the region to encompass the code you
;;          wish to schedule, then use the spu-schedule-region command)
;;        
;;
;;


(defun spu-valid-opcode-p (opcode)
  "Returns t if opcode is a valid SPU opcode; returns nil otherwise."
  (and (or (member opcode spu-even-opcodes) (member opcode spu-odd-opcodes)) t))


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
			"bihnz" "bihz" "binz" "biz" "hbr" "hbrp" "bra" "brasl" "hbra"))

(setq spu-ls-opcodes  '("lqa" "lqd" "lqr" "lqx" "stqa" "stqd" "stqr" "stqx"))

(setq spu-sh-opcodes  '("shlqby" "shlqbyi" "shlqbybi" "shlqbi" "shlqbii" "rotqby" "rotqbyi"
			"rotqbybi" "rotqbi" "rotqbii" "rotqmby" "rotqmbyi" "rotqmbybi" "rotqmbi"
			"rotqmbii" "shufb" "cbd" "cbx" "chd" "chx" "cwd" "cwx" "cdd" "cdx"
			"fsmbi" "fsmb" "fsmh" "fsm" "gbb" "gbh" "gb" "frest" "frsqest" "orx"))


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
   (t (error (format "Unknown opcode: %s" opcode)))))

(defun spu-print-cycle-count (opcode)
  "Prints the cycle count of the specified opcode."
  (interactive "sOpcode: ")
  (message (format "%d cycle(s)" (spu-cycle-count opcode))))


;(defun spu-mark-end-of-instruction ()
;  "Places the mark at the end of the current instruction."
;  (let ((inst-start (if (< (current-column) (spu-odd-instruction-column))
;						0
;					  (spu-odd-instruction-column))))
;	(if (eq inst-start 0)
;		(progn
;		  (set-mark (point))
;		  (move-to-column (spu-odd-instruction-column))
;		  (exchange-point-and-mark))
;	  (mark-end-of-line nil))
;	))


;(defun spu-current-opcode ()
;  "Returns the opcode of the instruction at the point."
;  (let ((inst-start (if (< (current-column) (spu-odd-instruction-column))
;						0
;					  (spu-odd-instruction-column)))
;		(result nil))
;	(save-excursion
;	  (move-to-column inst-start)
;	  (spu-mark-end-of-instruction)
;	  (narrow-to-region (point) (mark))
;	  (beginning-of-buffer)
;	  (while (and (not (eobp))
;				  (not (spu-valid-opcode-p (current-word))))
;		(forward-word)
;		)
;	  (setq result (current-word))
;	  (widen)
;	  (zmacs-deactivate-region)
;	  result
;	  )))

;(defun spu-insert-nops ()
;  "Insert a nop/lnop pair at the current line."
;  (interactive "*")
;  (let ((old-column (current-column)))
;	(beginning-of-line)
;	(insert-string (concat "\tnop" (make-string spu-odd-instruction-indent-tabs ?\t) "lnop\n"))
;	(previous-line 1)
;	(move-to-column old-column)))

;(defun spu-insert-nops-commented ()
;  "Insert a commented nop/lnop pair at the current line."
;  (interactive "*")
;  (let ((old-column (current-column)))
;	(beginning-of-line)
;	(insert-string (concat "\t{nop}" (make-string spu-odd-instruction-indent-tabs ?\t) "{lnop}\n"))
;	(previous-line 1)
;	(move-to-column old-column)))


;(defun spu-goto-result ()
;  "Moves the point to the first line where the result of the current instruction
;can safely be used."
;  (interactive)
;  (let ((num-lines (spu-cycle-count (spu-current-opcode)))
;		(col (current-column))
;		)
;	(if num-lines
;		(progn
;		  (save-excursion
;			(while (> num-lines 0)
;			  (cond ((bobp) (error "Reached beginning of buffer"))
;					((eobp) (error "Reached end of buffer")))
;			  (forward-line 1)
;			  (if (spu-valid-line-p) (setq num-lines (- num-lines 1))))
;			(point-to-register ?r)
;			)
;		  (jump-to-register ?r))
;	  )
;	(move-to-column col)
;	(message nil)
;	))

;(setq spu-odd-instruction-regexp
;	  (concat "\\({\\(o[0-9?]\\(.[0-9]\\)?\\)?}\\s-*\\)\\|\t" ; optional leading comment for odd instruction
;			  (concat "\\(" (spu-string-list-to-regexp spu-odd-opcodes) "\\|\\({\\s-*lnop\\s-*}\\)\\)") ; odd opcode
;			  ".*$" ; rest of the line
;			  ))
;(setq spu-line-regexp
;	  (concat "^\\s-*" ; beginning of line and optional whitespace before leading comment
;			  "\\({\\(e[0-9?]\\(.[0-9]\\)?\\)?}\\)\\|\t" ; optional leading comment for even instruction
;			  "\\s-*" ; whitespace before even opcode
;			  (concat "\\(" (spu-string-list-to-regexp spu-even-opcodes) "\\|\\({\\s-*nop\\s-*}\\)\\)") ; even opcode
;			  "\\s-+" ; whitespace after even opcode
;			  ".*" ; arguments to even opcode
;			  spu-odd-instruction-regexp ; and then the odd instruction.
;			  ))

;(defun spu-valid-line-p ()
;  "Returns t if the current line is a valid line of SPU code, according to a 
;fairly arbitrary defintion of \"valid\"."
;  (save-excursion
;	(beginning-of-line)
;	(mark-end-of-line nil)
;	(copy-to-register ?v (point) (mark) nil)
;	(zmacs-deactivate-region)
;	(or (string-match spu-line-regexp (get-register ?v)))
;	))

;(defvar spu-odd-instruction-indent-tabs 8
;  "*The number of tabs to indent the odd instruction.  More specifically, the
;optional comment will be indented by this many tabs; the opcode will be indented
;this many tabs plus one.")

;(defun spu-odd-instruction-column ()
;  "Returns the column number to the beginning of the odd instruction on the
;current line, if there is one."
;  (if (spu-valid-line-p)
;	  (save-excursion
;		(if t
;			(progn (beginning-of-line) ; this logic attempts to intelligently find the odd instruction
;				   (mark-end-of-line nil)
;				   (copy-to-register ?v (point) (mark) nil)
;				   (zmacs-deactivate-region)
;				   (forward-char-command (string-match spu-odd-instruction-regexp (get-register ?v)))
;				   (current-column))
;		  64)) ; Or we can just hard-code it.
;	(error "No odd instruction on the current line")))

;(defun spu-store-current-instruction (reg &optional delete-flag)
;  "Stores the current instruction in the specified register.  If delete-flag is
;non-nil, the instruction is deleted as well."
;  (let ((inst-start (if (< (current-column) (spu-odd-instruction-column))
;						0
;					  (spu-odd-instruction-column))))
;	;; Move to beginning of the current instruction
;	(move-to-column inst-start)
;	;; copy and delete the current instruction.
;	(push-mark (point))
;	(spu-mark-end-of-instruction)
;	(copy-to-register reg (point) (mark) delete-flag)
;	(pop-mark)
;	(zmacs-deactivate-region)
;	))

;(defun spu-kill-current-instruction ()
;  "Replaces the current instruction with a nop/lnop.  The instruction is
;stored in register ?k, and can be re-insterted with spu-yank-instruction"
;  (interactive "*")
;  (let ((inst-start (if (< (current-column) (spu-odd-instruction-column))
;						0
;					  (spu-odd-instruction-column)))
;		(odd-start (spu-odd-instruction-column))
;		(old-column (current-column)))
;	;; Delete the current instruction.
;	(spu-store-current-instruction ?k t)
;	;; Insert the appropriate nop instruction
;	(insert-string (if (eq inst-start 0)
;					   "\tnop"
;					 "\tlnop"))
;	;; If we inserted an even nop, insert tabs until the odd
;	;; instruction is back where it started.
;	(if (eq inst-start 0)
;		(while (< (current-column) odd-start)
;		  (insert-string "\t")))
;	(move-to-column old-column)
;	))

;(defun spu-yank-instruction ()
;  "Replaces the current instruction with the one stored in register ?k.
;Use spu-kill-current-instruction to place a instruction in ?k."
;  (interactive "*")
;  (let ((inst-start (if (< (current-column) (spu-odd-instruction-column))
;						0
;					  (spu-odd-instruction-column)))
;		(odd-start (spu-odd-instruction-column))
;		(old-column (current-column)))
;	;; Delete the current instruction.
;	(spu-store-current-instruction ?y t)
;	;; Insert the instruction in register ?k.  The second argument places
;	;; point at the end of the inserted text.
;	(insert-register ?k t)
;	;; If we inserted an even nop, insert tabs until the odd
;	;; instruction is back where it started.
;	(if (eq inst-start 0)
;		(while (< (current-column) odd-start)
;		  (insert-string "\t")))
;	(move-to-column old-column)
;	))

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

;(defun spu-swap-prev-instruction (&optional n)
;  "Performs an `spu-swap-next-instruction' in the opposite direction."
;  (interactive "*P")
;  (spu-swap-next-instruction (- (prefix-numeric-value n))))

;(defvar spu-scheduler-executable "~/src/main/bin/linux/spuscheduler"
;  "Path to the spuscheduler executable.")

;(defun spu-schedule-region (&optional loop-flag)
;  "Passes the text in the current region to the spuscheduler program, replacing it
;with the tool's output.  If loop-flag is non-nil, the scheduler is run in loop mode"
;  (interactive "*P")
;  (if loop-flag 
;	  (call-process-region (region-beginning) (region-end) spu-scheduler-executable t t t "-maxroll" "4" "-noearlyout")
;	(call-process-region (region-beginning) (region-end) spu-scheduler-executable t t t "-straight" "-noearlyout")
;	))


;(require 'derived)
;(define-derived-mode spu-mode asm-mode "SPU"
;  "Major mode for editing SPU assembly code.
;Special commands:
;\\{spu-mode-map}"
;  ;; Define keys
;  (define-key spu-mode-map [(control c) (control down)] 'spu-swap-next-instruction)
;  (define-key spu-mode-map [(control c) (down)]         'spu-swap-next-instruction)
;  (define-key spu-mode-map [(control c) (control up)]   'spu-swap-prev-instruction)
;  (define-key spu-mode-map [(control c) (up)]           'spu-swap-prev-instruction)
;  (define-key spu-mode-map [(control c) (control r)]    'spu-goto-result)
;  (define-key spu-mode-map [(control c) (control k)]    'spu-kill-current-instruction)
;  (define-key spu-mode-map [(control c) (control y)]    'spu-yank-instruction)
;  (define-key spu-mode-map [(control c) (control n)]	'spu-insert-nops)
;  (define-key spu-mode-map [(control c) (control b)]	'spu-insert-nops-commented)
;  (define-key spu-mode-map [(control c) (control f)]    'spu-reformat-region)
;  (define-key spu-mode-map [(control c) (control d)]    'spu-rollup-dependency-report-region)
;  (define-key spu-mode-map [(control c) (control r)]    'spu-rollup-fusion-region)
;  (define-key spu-mode-map [(control c) (control o)]    'spu-schedule-region)



  ;; Create faces for various opcode classes.
;  (make-face 'spu-even-opcode-face)
;  (set-face-foreground 'spu-even-opcode-face
;					   (or (and (valid-color-name-p "salmon1") "salmon1")
;						   "brightcyan"))
;  (make-face 'spu-odd-opcode-face)
;  (set-face-foreground 'spu-odd-opcode-face
;					   (or (and (valid-color-name-p "cornflowerblue") "cornflowerblue")
;						   "brightblue"))
;  (make-face 'spu-nop-opcode-face)
;  (set-face-foreground 'spu-nop-opcode-face
;					   (or (and (valid-color-name-p "salmon4") "salmon4")
;						   "darkgrey"))
;  (make-face 'spu-lnop-opcode-face)
;  (set-face-foreground 'spu-lnop-opcode-face
;					   (or (and (valid-color-name-p "darkslateblue") "darkslateblue")
;						   "darkgrey"))

;  ;; asm-mode always inserts ";" comments at the end of the current
;  ;; line.  I prefer to use ";" comments to quickly disable an entire
;  ;; line, so let's remove its special behavior!
;  (local-set-key (vector asm-comment-char) 'self-insert-command)

;  ;; Treat curly braces as comments
;  (modify-syntax-entry ?{ "< a" spu-mode-syntax-table)
;  (modify-syntax-entry ?} "> a" spu-mode-syntax-table)

;  ;; Add _ as part of the word definition:
;  (modify-syntax-entry ?_ "w"   spu-mode-syntax-table)

;  ;; C and C++-style comments should be treated as such.  Since Emacs
;  ;; only offers two styles of comments, C-style comments will
;  ;; regretably pair with {}-style comments and vice-versa.
;  (modify-syntax-entry ?/ ". 1456" spu-mode-syntax-table)
;  (modify-syntax-entry ?* ". 23" spu-mode-syntax-table)
  
;  ;; Keywords should be case-insensitive keywords
;  (setq font-lock-keywords-case-fold-search t) 
;  (put 'spu-mode 'font-lock-defaults '(spu-font-lock-keywords))
;  (setq font-lock-defaults '(spu-font-lock-keywords))
;  (setq tab-width 8)
;  (setq font-lock-maximum-decoration t)
;  (font-lock-mode 0)
;  (font-lock-mode 1))
  
;(provide 'spu-mode)



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
			 "selb" "sync" "syncc" "dsync" "fscrrd" "fscrwr" "mfspr" "mtspr" "iretd"
			 "irete" "iret" "rchcnt" "rdch" "wrch" "stopd"))

;; List of all odd opcodes
(setq spu-odd-opcodes '("lnop" "br" "brsl" "brhnz" "brhz" "brnz" "brz" "hbrr" "bi" "bisl"
			"bisled" "bihnz" "bihz" "binz" "biz" "hbr" "hbrp" "bra" "brasl" "hbra"
			"stop" "lqa" "lqd" "lqr" "lqx" "stqa" "stqd" "stqr" "stqx" "shlqby"
			"shlqbyi" "shlqbybi" "shlqbi" "shlqbii" "rotqby" "rotqbyi" "rotqbybi"
			"rotqbi" "rotqbii" "rotqmby" "rotqmbyi" "rotqmbybi" "rotqmbi" "rotqmbii"
			"shufb" "cbd" "cbx" "chd" "chx" "cwd" "cwx" "cdd" "cdx" "fsmbi" "fsmb"
			"fsmh" "fsm" "gbb" "gbh" "gb" "frest" "frsqest" "orx"))


;;
;;
;;    Function to help defining the major-mode:
;;
;;


(defun spu-string-list-to-regexp (inst-list)
  "Produce from a list of strings a single regular expression which
matches any of the individual opcodes."
    (concat "\\<" (regexp-opt inst-list "\\>")))

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
    (define-key spu-mode-map [(control c) (control f)]    'spu-reformat-region)))

;; Keyword definition for syntax highlighting:
(defconst spu-font-lock-keywords
  (list '("\\.global\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" . 1) ; exported symbol names
	'("\\_*\\<\\(\\sw\\|\\s_\\)+\\>\\_*:" . font-lock-function-name-face) ; labels
	'("\\(\\s-\\|^\\)\\.\\(\\(\\sw\\|\\s_\\)+\\)" . font-lock-builtin-face) ; preprocessor commands
	'("\\<nop\\>" . spu-nop-opcode-face)
	'("\\<lnop\\>" . spu-lnop-opcode-face)
	(list (spu-string-list-to-regexp spu-even-opcodes) 0 'spu-even-opcode-face)
	(list (spu-string-list-to-regexp spu-odd-opcodes) 0 'spu-odd-opcode-face)
	)
  "Additional expressions to highlight in SPU mode.")
