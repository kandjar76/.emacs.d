;;
;; Files used to setup the different color within emacs:
;;

;; Creation of fonts used by the C++ mode:
(make-face 'font-lock-operator-face)
(make-face 'font-lock-end-statement)
(setq font-lock-operator-face 'font-lock-operator-face)
(setq font-lock-end-statement 'font-lock-end-statement)






(setq default-background-color (first-valid-color "#F0F0F0" "white"))

;; Default color:
(set-face-foreground 'default "black")
(set-face-background 'default (first-valid-color default-background-color "white"))

;; Bookmark color:
(set-face-background 'bm-face      (first-valid-color "lightcyan" "blue"))
(set-face-foreground 'bm-face      (first-valid-color "black" "blue"))

;; Highlight the current line:
(set-face-background 'highlight-current-line-face (first-valid-color "#C0E0FF" "lightblue" "grey" "cyan"))

;; I-Search:
(set-face-foreground 'isearch (first-valid-color "red"))
(set-face-background 'isearch (first-valid-color "lightgray" "grey" "white"))

;; Color of the text selection:
(set-face-background 'region (first-valid-color "#FFE0C0" "brightcyan"))

;; Quick search colors:
(set-face-background 'secondary-selection      (first-valid-color "white"))
(set-face-foreground 'secondary-selection      (first-valid-color "red"))

;; Show-matching parent / Highlight current-word:
(set-face-background 'show-paren-match-face    (first-valid-color "#E0C0FF"))
(set-face-foreground 'show-paren-match-face    (first-valid-color "red"))

;; Highlight C++ operators.... 
(set-face-foreground 'font-lock-operator-face (first-valid-color "red"))
(set-face-foreground 'font-lock-end-statement (first-valid-color "red"))



;; Remove some background colors from diredp:
(set-face-background 'diredp-flag-mark              default-background-color)
(set-face-foreground 'diredp-flag-mark              "blue")
(set-face-background 'diredp-read-priv              default-background-color)
(set-face-background 'diredp-write-priv             default-background-color)
(set-face-background 'diredp-no-priv                default-background-color)
(set-face-background 'diredp-exec-priv              default-background-color)
(set-face-background 'diredp-flag-mark-line         (first-valid-color "#FFE0D0" "white"))
(set-face-foreground 'diredp-compressed-file-suffix "red")

;; Font used in ediff mode:
(if (string= emacs-version "22.1.1")
    (progn 
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
      (set-face-foreground 'ediff-even-diff-B     (first-valid-color "dark gray"))
      )
    (progn 
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
      (set-face-foreground 'ediff-even-diff-face-B     (first-valid-color "dark gray"))))


;; Font used to highlight warnings:
(set-face-foreground 'font-lock-warning-face    (first-valid-color "violetred" "darkmagenta" "magenta"))
(set-face-background 'font-lock-warning-face    (first-valid-color "#F0D0D0" "grey" "white"))

;; Comment:
(set-face-foreground 'font-lock-comment-face 	(first-valid-color "sea green" "green"))

;; Color of the language keyword: const, if, else...
(set-face-foreground 'font-lock-keyword-face    (first-valid-color "blue"))

;; C/C++ Found types:
(set-face-foreground 'font-lock-type-face	(first-valid-color "firebrick" "violetred" "medium orchid" "purple"))

;; ??? / SPU preproc (.reg / .global)
(set-face-foreground 'font-lock-builtin-face    (first-valid-color "violetred" "darkcyan" "darkmagenta" "magenta"))
(make-face-unbold 'font-lock-builtin-face)

;; C/C++ Strings:
(set-face-foreground 'font-lock-string-face	(first-valid-color "royal blue" "#1515C3" "midnightblue" "darkblue" "darkcyan"))

;; elisp: function name
(set-face-foreground 'font-lock-function-name-face (first-valid-color "red" "purple3"))
