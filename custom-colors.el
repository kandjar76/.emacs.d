;;
;; Files used to setup the different color within emacs:
;;

;;------------------------------------------------------------------------------
;;
;;                      Utility Functions                           
;;
;;------------------------------------------------------------------------------

(defun first-valid-color (&rest colors)
  "Returns the first valid color from color-list (a list of color names).
Returns \"black\" if no valid color is found."
  (cond 
   ((and (not window-system) (member ':text colors))
    (cadr (member ':text colors)))
   ((null colors) "black")
   ((color-defined-p (car colors)) (car colors))
   ((apply 'first-valid-color (cdr colors)))))



;; Creation of fonts used by the C++ mode:
(make-face 'font-lock-operator-face)
(make-face 'font-lock-end-statement)
(setq font-lock-operator-face 'font-lock-operator-face)
(setq font-lock-end-statement 'font-lock-end-statement)




(setq default-background-color (first-valid-color "#F0F0F0" "white" :text "black"))
(setq default-foreground-color (first-valid-color "black" :text "white"))

;; Default color:
(set-face-foreground 'default default-foreground-color)
(set-face-background 'default default-background-color)

;; Highlight the current line:
(eval-after-load "highlight-current-line"
  '(set-face-background 'highlight-current-line-face (first-valid-color "#C0E0FF" "lightblue" "grey" "cyan" :text "white")))

;; I-Search:
(set-face-foreground 'isearch (first-valid-color "red"))
(set-face-background 'isearch (first-valid-color "lightgray" "grey" "white" :text "white"))

;; Color of the text selection:
(set-face-background 'region (first-valid-color "#FFE0C0" "brightcyan" :text "cyan"))

;; Quick search colors:
(set-face-background 'secondary-selection      (first-valid-color "white"))
(set-face-foreground 'secondary-selection      (first-valid-color "red"))

;; Show-matching parent / Highlight current-word:
(set-face-background 'show-paren-match-face    (first-valid-color "#E0C0FF" :text "white"))
(set-face-foreground 'show-paren-match-face    (first-valid-color "red" :text "green"))

;; Highlight C++ operators.... 
(set-face-foreground 'font-lock-operator-face (first-valid-color "red" :text "white"))
(set-face-foreground 'font-lock-end-statement (first-valid-color "red" :text "white"))

;; Color-occur background:
(set-face-background 'color-occur-face (first-valid-color "darkseagreen1" "palegreen" "grey" ))



;; Remove some background colors from diredp:
(eval-after-load "dired+"
  '(progn
     (set-face-background 'diredp-flag-mark              default-background-color)
     (set-face-foreground 'diredp-flag-mark              "blue")
     (set-face-background 'diredp-read-priv              default-background-color)
     (set-face-background 'diredp-write-priv             default-background-color)
     (set-face-background 'diredp-no-priv                default-background-color)
     (set-face-background 'diredp-exec-priv              default-background-color)
     (set-face-background 'diredp-flag-mark-line         (first-valid-color "#FFE0D0" "white"))
     (set-face-foreground 'diredp-compressed-file-suffix "red")))

;; Font used in ediff mode:
(if (> emacs-major-version 21)
    (progn 
      (eval-after-load "dired+"
	'(progn
	   (set-face-background 'ediff-current-diff-A     (first-valid-color "honeydew"))
	   (set-face-foreground 'ediff-current-diff-A     (first-valid-color "black"))
	   (set-face-background 'ediff-current-diff-B     (first-valid-color "honeydew"))
	   (set-face-foreground 'ediff-current-diff-B     (first-valid-color "black"))
	   (set-face-background 'ediff-fine-diff-A     (first-valid-color "honeydew"))
	   (set-face-foreground 'ediff-fine-diff-A     (first-valid-color "red"))
	   (set-face-background 'ediff-fine-diff-B     (first-valid-color "honeydew"))
	   (set-face-foreground 'ediff-fine-diff-B     (first-valid-color "red"))

	   (set-face-background 'ediff-odd-diff-A     (first-valid-color "lavender blush"))
	   (set-face-foreground 'ediff-odd-diff-A     (first-valid-color "dark gray"))
	   (set-face-background 'ediff-even-diff-A     (first-valid-color "lavender"))
	   (set-face-foreground 'ediff-even-diff-A     (first-valid-color "dark gray"))
	   
	   (set-face-background 'ediff-odd-diff-B     (first-valid-color "lavender blush"))
	   (set-face-foreground 'ediff-odd-diff-B     (first-valid-color "dark gray"))
	   (set-face-background 'ediff-even-diff-B     (first-valid-color "lavender"))
	   (set-face-foreground 'ediff-even-diff-B     (first-valid-color "dark gray"))))

      (eval-after-load "diff-mode"
	'(progn
	   (set-face-foreground 'diff-added       (first-valid-color "darkgreen"))
	   (set-face-foreground 'diff-removed     (first-valid-color "red"))
	   (set-face-foreground 'diff-hunk-header (first-valid-color "blue"))
	   (set-face-foreground 'diff-function    (first-valid-color "blue"))))
      )
    (progn 
      (eval-after-load "dired+"
	'(progn
	   (set-face-background 'ediff-current-diff-face-A     (first-valid-color "honeydew"))
	   (set-face-foreground 'ediff-current-diff-face-A     (first-valid-color "black"))
	   (set-face-background 'ediff-current-diff-face-B     (first-valid-color "honeydew"))
	   (set-face-foreground 'ediff-current-diff-face-B     (first-valid-color "black"))
	   (set-face-background 'ediff-fine-diff-face-A     (first-valid-color "honeydew"))
	   (set-face-foreground 'ediff-fine-diff-face-A     (first-valid-color "red"))
	   (set-face-background 'ediff-fine-diff-face-B     (first-valid-color "honeydew"))
	   (set-face-foreground 'ediff-fine-diff-face-B     (first-valid-color "red"))
      
	   (set-face-background 'ediff-odd-diff-face-A     (first-valid-color "lavender blush"))
	   (set-face-foreground 'ediff-odd-diff-face-A     (first-valid-color "dark gray"))
	   (set-face-background 'ediff-even-diff-face-A     (first-valid-color "lavender"))
	   (set-face-foreground 'ediff-even-diff-face-A     (first-valid-color "dark gray"))

	   (set-face-background 'ediff-odd-diff-face-B     (first-valid-color "lavender blush"))
	   (set-face-foreground 'ediff-odd-diff-face-B     (first-valid-color "dark gray"))
	   (set-face-background 'ediff-even-diff-face-B     (first-valid-color "lavender"))
	   (set-face-foreground 'ediff-even-diff-face-B     (first-valid-color "dark gray"))))))


;; Font used to highlight warnings:
(set-face-foreground 'font-lock-warning-face    (first-valid-color "violetred" "darkmagenta" "magenta"))
(set-face-background 'font-lock-warning-face    (first-valid-color "#F0D0D0" "grey" "white"))

;; Comment:
(set-face-foreground 'font-lock-comment-face 	        (first-valid-color "sea green" "green"))
(if (> emacs-major-version 21)
    (set-face-foreground 'font-lock-comment-delimiter-face 	(first-valid-color "sea green" "green")))

;; Color of the language keyword: const, if, else...
(set-face-foreground 'font-lock-keyword-face    (first-valid-color "blue"))

;; C/C++ Found types:
(set-face-foreground 'font-lock-type-face	(first-valid-color "firebrick" "violetred" "medium orchid" "purple" :text "white"))

;; ??? / SPU preproc (.reg / .global)
(set-face-foreground 'font-lock-builtin-face    (first-valid-color "violetred" "darkcyan" "darkmagenta" "magenta"))
(make-face-unbold 'font-lock-builtin-face)

;; C/C++ Strings:
(set-face-foreground 'font-lock-string-face	(first-valid-color "royal blue" "#1515C3" "midnightblue" "darkblue" "darkcyan" :text "cyan"))

;; elisp: function name
(set-face-foreground 'font-lock-function-name-face (first-valid-color "red" "purple3"))
(make-face-unbold 'font-lock-function-name-face)

;; Spu:
(eval-after-load "spu-highlight-registers"
  '(progn
     (set-face-background 'spu-highlight-registers-1 (first-valid-color "palegreen1" "chartreuse1"))
     (set-face-background 'spu-highlight-registers-2 (first-valid-color "light sky  blue" "moccasin" "coral1"))
     (set-face-background 'spu-highlight-registers-3 (first-valid-color "plum1" "rosybrown1" "misty rose" "plum1" "orchid1"))
     (set-face-background 'spu-highlight-registers-4 (first-valid-color "darkslategray2" "deepskyblue1"))
     (set-face-background 'spu-highlight-registers-5 (first-valid-color "gold" "deepskyblue1"))
     (set-face-background 'spu-highlight-registers-6 (first-valid-color "peru" "deepskyblue1"))
     (set-face-background 'spu-highlight-registers-7 (first-valid-color "MediumPurple2" "deepskyblue1"))
     (set-face-background 'spu-highlight-registers-8 (first-valid-color "medium sea green" "deepskyblue1"))
     (set-face-background 'spu-highlight-registers-9 (first-valid-color "orchid" "deepskyblue1"))
     (set-face-background 'spu-highlight-registers-10 (first-valid-color "light steel blue" "deepskyblue1"))))

(eval-after-load "spu-highlight-stalls"
  '(progn
     (set-face-background 'spu-highlight-stalls-1c-font (first-valid-color "#FFCDCD" "orange red" "red"))
     (set-face-background 'spu-highlight-stalls-2c-font (first-valid-color "#FFC0C0" "orange red" "red"))
     (set-face-background 'spu-highlight-stalls-3c-font (first-valid-color "#FFA0A0" "orange red" "red"))
     (set-face-background 'spu-highlight-stalls-4c-font (first-valid-color "#FF8080" "orange red" "red"))
     (set-face-background 'spu-highlight-stalls-5c-font (first-valid-color "#FF4040""orange red" "red"))
     (set-face-background 'spu-highlight-stalls-6c-font (first-valid-color "#EF0000" "orange red" "red"))
     (set-face-background 'spu-highlight-latency-line-font (first-valid-color "#FFE0E0" "orange red" "red"))))

;; Eshell:
;(when (not window-system)
;  (eval-after-load "eshell"
;    '(progn 
;       (make-face-unbold 'eshell-prompt)
;       (set-face-foreground 'eshell-ls-directory "blue"))))

(when (not window-system)
  (eval-after-load "eshell"
    '(custom-set-faces
      '(eshell-prompt ((t (:foreground "cyan" :weight normal))))
      '(eshell-ls-directory ((t (:foreground "blue"))))
)))
