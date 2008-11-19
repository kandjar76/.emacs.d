; File to highlight in the spu mode.


;; First option: highlighing the register of the current line,



(require 'spu-mode)

;;
;; Font definition:
;;

(make-face  'spu-highlight-registers-1)
(set-face-background 'spu-highlight-registers-1 (first-valid-color "deepskyblue1"))
(defvar spu-highlight-registers-1 'spu-highlight-registers-1
  "Font to highlight the first register of a SPU line.")


(make-face  'spu-highlight-registers-2)
(set-face-background 'spu-highlight-registers-2 (first-valid-color "chartreuse1"))
(defvar spu-highlight-registers-2 'spu-highlight-registers-2
  "Font to highlight the second register of a SPU line.")


(make-face  'spu-highlight-registers-3)
(set-face-background 'spu-highlight-registers-3 (first-valid-color "coral1"))
(defvar spu-highlight-registers-3 'spu-highlight-registers-3
  "Font to highlight the third register of a SPU line.")

(make-face  'spu-highlight-registers-4)
(set-face-background 'spu-highlight-registers-4 (first-valid-color "orchid1"))
(defvar spu-highlight-registers-4 'spu-highlight-registers-3
  "Font to highlight the fourth register of a SPU line.")

;;
;; Global variables:
;;

(setq spu-highlight-registers-font-list
      (list 'spu-highlight-registers-1
	    'spu-highlight-registers-2
	    'spu-highlight-registers-3
	    'spu-highlight-registers-4))

(setq spu-highlight-register-timer nil)

;;
;; Utility functions:
;;

(defun spu-clean-curly-comments(str)
  "Return the string STR after having removed all comments formed with { and } inside."
  (let* ((pos (string-match "[{}]" str))
	 (chr (and pos (substring str pos (+ pos 1)))))
    (cond ((not chr) str)
	  ((string= chr "{") 
	   (if (string-match "}" str)
	       (concat (substring str 0 pos) 
		       (spu-clean-curly-comments (substring str (+ pos 1))))
	       (substring str 0 pos)))
	  ((string= chr "}") (spu-clean-curly-comments (substring str (+ 1 pos))))
	  )))

(defun spu-clean-c-comments(str)
  "Return the string STR after having removed all c style /* */ comments inside."
  (let* ((pos (string-match "/\\*\\|\\*/" str))
	 (cmt (and pos (substring str pos (+ pos 2)))))
    (cond ((not cmt) str)
	  ((string= cmt "/*") 
	   (if (string-match "\\*/" str (+ pos 2))
	       (concat (substring str 0 pos) 
		       (spu-clean-c-comments (substring str (+ pos 2))))
	       (substring str 0 pos)))
	  ((string= cmt "*/") (spu-clean-c-comments (substring str (+ 2 pos))))
	  )))

(defun spu-clean-line-comment(str)
  "Return the string after cleaning the comment starting with ';'"
  (let ((pos (string-match "\\(;\\|//\\)" str)))
    (if pos
	(substring str 0 pos)
	str)))

(defun spu-clean-comments(str)
  (spu-clean-c-comments 
   (spu-clean-curly-comments 
    (spu-clean-line-comment str))))


(defun spu-extract-registers()
  "Extract the register of the current instruction
Returns a list of string.
Note: this function assume that this is a line with opcodes"
  (let ((even (spu-find-even-opcode))
	(odd  (spu-find-odd-opcode))
	(cur  (current-column))
	instr)
    (if (or (and odd (> cur odd))
	    (not even))
	(setq instr (buffer-substring-no-properties (spu-column-to-pos odd)
						    (spu-get-end-of-line-position)))
	(setq instr (buffer-substring-no-properties (spu-column-to-pos even)
						    (or (and odd (spu-column-to-pos odd))
							(spu-get-end-of-line-position)))))
    (setq instr (spu-clean-comments instr))
    (setq wl    (split-string (subst-char-in-string ?, 32 instr)))
    (if (or (member (car wl) spu-even-opcodes)
	    (member (car wl) spu-odd-opcodes))
	(cdr wl))
))


(defun spu-highlight-register(reg cnt)
  "Highlight the occurence of the register REG in the buffer"
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp reg nil t)
      (let ((ovl (make-overlay (match-beginning 0)
			       (match-end 0))))
	(overlay-put ovl 'face (nth cnt spu-highlight-registers-font-list))
	(overlay-put ovl 'spu-highlight-register  t)))))

(defun spu-highlight-register-clear-overlays()
  "Remove all highlighting on registers"
  (let ((ovl-lists (overlay-lists))
	(count 0))
    (setq ovl-lists (list (car ovl-lists) (cdr ovl-lists)))
    (while ovl-lists
      (let ((ovls (pop ovl-lists)))
	(while ovls
	  (if (overlay-get (car ovls) 'spu-highlight-register)
	      (progn (setq count (+ 1 count))
		     (delete-overlay (pop ovls)))
	      (pop ovls))))
      )
    count))
	
(defun spu-highlight-register-list(reglist)
  "Highlight the register from the list"
  (let ((count 0))
    (spu-highlight-register-clear-overlays)
    (setq reglist (mapcar (lambda (x) (concat "\\<" x "\\>")) reglist))
    (while reglist
      (spu-highlight-register (pop reglist) count)
      (setq count (+ count 1)))))
  

(defun spu-highlight-registers()
  "Highlight the register of the current line."
  (if (spu-detect-opcodes-line)
      (spu-highlight-register-list (spu-extract-registers))
      (spu-highlight-register-clear-overlays)))


(defsubst spu-highlight-registers-post-hook()
  "Post command"
  (spu-highlight-registers))


;;
;; Defining spu-highlight-registers-minor-mode
;;


(define-minor-mode spu-highlight-registers-mode
  "Highlight the register of the current line."
  :init-value nil
  :global nil
  :lighter " Highlight-Registers"
  ;; Body of the function:
  (if (not spu-highlight-registers-mode)
      (progn (remove-hook 'post-command-hook 'spu-highlight-registers-post-hook)
	     (spu-highlight-register-clear-overlays)
	     )
      (add-hook 'post-command-hook 'spu-highlight-registers-post-hook))
  (when (interactive-p)
    (message "SPU Highlight Registers Mode is now %s."
	     (if spu-highlight-registers-mode "ON" "OFF"))))


(defcustom spu-highlight-registers-mode nil
  "*Non-nil means SPU Highight Register mode is enabled.
In this mode, when the cursor is on some specific line, all registers used in
this line are highlighted.
Setting this variable directly does not change the mode; instead, use
function `spu-highlight-registers-mode'."
  :set (lambda (symbol value) (spu-highlight-registers-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean)


(provide 'spu-highlight-registers)
