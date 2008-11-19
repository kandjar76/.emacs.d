;; Files used to setup the different color within xemacs:


;; Bookmark color:
(set-face-background 'bm-face      (first-valid-color "lightcyan" "blue"))
(set-face-foreground 'bm-face      (first-valid-color "black" "blue"))

;; Highlight the current line:
(set-face-background 'highlight-current-line-face (first-valid-color "#C0E0FF" "lightblue" "white"))

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




;; ???
(set-face-foreground 'font-lock-builtin-face   (first-valid-color "darkcyan" "violetred" "darkmagenta" "magenta"))

(make-face-unbold 'font-lock-builtin-face)

(set-face-foreground 'font-lock-warning-face   (first-valid-color "violetred" "darkmagenta" "magenta"))

;; Default color:
(set-face-foreground 'default "black")
(set-face-background 'default (first-valid-color "#F0F0F0" "white"))


;; Elisp Reference -- :size... 
;(set-face-foreground 'font-lock-reference-face					"red")

;; Comment:
;(set-face-foreground 'font-lock-comment-face 					(first-valid-color "darkgreen" "green"))

;; C/C++ Strings:
;(set-face-foreground 'font-lock-string-face					    (first-valid-color "#1515C3" "midnightblue" "darkblue" "darkcyan"))

;; C/C++ Found types:
;(set-face-foreground 'font-lock-type-face						(first-valid-color "violetred" "medium orchid" "purple"))

;; Lisp Strings / in compile mode: successful lines
;(set-face-foreground 'font-lock-doc-string-face					(first-valid-color "darkgreen" "green"))

;; Preprocessor command:
;(set-face-foreground 'font-lock-preprocessor-face				(first-valid-color "red" "darkgreen" "green"))

;; Color of the language keyword: const, if, else...
;(set-face-foreground 'font-lock-keyword-face (first-valid-color "blue"))

;; Color of the cursor:
;(set-face-background 'text-cursor (first-valid-color "red"))

;(set-face-foreground 'font-lock-variable-name-face				"yellow")
;(set-face-foreground 'font-lock-function-name-face 				(first-valid-color '("orange" "brightred")))
;(make-face-unitalic   'font-lock-type-face)

