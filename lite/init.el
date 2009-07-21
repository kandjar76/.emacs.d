;;
;;  Light Emacs Configuration File:
;;


;; Core setting:
(when window-system (tool-bar-mode 0))                          ; don't display the ugly toolbar icons
(when window-system (mouse-wheel-mode t))                       ; enable the mouse wheel
(setq tab-width 4)                                              ; default tab width
(setq inhibit-startup-message t)                                ; no splash screen
(setq ring-bell-function 'ignore)                               ; No ring bell!!!!
(setq isearch-wrapped t)                                        ; I-search should wrap
(line-number-mode t)                                            ; display the line number
(column-number-mode t)                                          ; display also the column number
(setq show-paren-delay 0)                                       ; No delay to display the matching parenthesis
(show-paren-mode t)                                             ; Enable show-paren-mode
(global-font-lock-mode 1)                                       ; Enable syntax highlighting
(pending-delete-mode 1)                                         ; Typed text replace the selected region
(setq default-truncate-lines t)                                 ; No wrapping by default
(pc-selection-mode)                                             ; Enable shift+arrow to select
(tooltip-mode 0)                                                ; Disable popup
(setq c-recognize-knr-p nil)                                    ; Don't bother checking for KnR style
(setq ediff-window-setup-function 'ediff-setup-windows-plain)   ; Don't create a new frame for the ediff help window
(put 'if 'lisp-indent-function nil)                             ; Make sure lisp doesn't indent else differently from the then.
(put 'eval-expression 'disabled nil)                            ; Enable lisp command


;; Modules: ido
(require 'ido)
(ido-mode t)
(setq ido-auto-merge-work-directories-length -1) ; Disable soem 
(setq ido-create-new-buffer 'always)             ; No confirmation when a new buffer is requested
(setq ido-enable-flex-matching t)                ; Enable flex matching


;; Module: Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;; Quick functions:
(defun electric-split-window-horizontally(&optional arg)
  "Split current window into two window side by side, move the cursor to the new created window"
  (interactive "P")
  (split-window-horizontally arg)
  (other-window 1))

(defun electric-split-window-vertically(&optional arg)
  "Split current window into two window side by side, move the cursor to the new created window"
  (interactive "P")
  (split-window-vertically arg)
  (other-window 1))

(defun backward-delete-word(&optional arg)
  "Delete the previous word"
  (interactive "*")
  (save-excursion
    (let ((end (point)))
      (backward-word 1)
      (delete-region (point) end))))

(defun revert-buffer-now ()
  "Silently calls revert-buffer if the current buffer is not modified."
  (interactive)
  (if (not (buffer-modified-p))
          (message (format "Reverted from %s" (buffer-file-name))))
  (revert-buffer nil (not (buffer-modified-p))))

(defun first-valid-color (&rest colors)
  "Returns the first valid color from color-list (a list of color names).
Returns \"black\" if no valid color is found."
  (cond 
   ((and (not window-system) (member ':text colors))
    (cadr (member ':text colors)))
   ((null colors) "black")
   ((color-defined-p (car colors)) (car colors))
   ((apply 'first-valid-color (cdr colors)))))


;; Key configuration:
(define-key global-map [(control right)]                'forward-word)
(define-key global-map [(control left)]                 'backward-word)
(define-key global-map [(control prior)]                'beginning-of-buffer)
(define-key global-map [(control next)]                 'end-of-buffer)
(define-key global-map [(control meta g)]               'goto-line)
(define-key global-map [(control backspace)]            'backward-delete-word)
(define-key global-map [(control f)]                    'occur)
(define-key global-map [(meta left)]                    'backward-sexp)
(define-key global-map [(meta right)]                   'forward-sexp)
(define-key global-map [(control ?c) (control ?r)]      'revert-buffer-now)
(define-key global-map [(control ?x) (?k)]              '(lambda() (interactive) (kill-buffer (current-buffer))))
(define-key global-map [(control ?x) (control ?d)]      'dired)
(define-key global-map [(control ?x)(?2)]               'electric-split-window-vertically)
(define-key global-map [(control ?x)(?3)]               'electric-split-window-horizontally)
(define-key global-map [(control ? )]                   '(lambda(arg) (interactive "P") (set-mark-command arg) (deactivate-mark)))

;; Disable some keys:
(define-key global-map [(insert)]                       nil)
(define-key global-map [(mouse-2)]                      nil)
(define-key global-map [(drag-mouse-1)]                 nil)

;; Dired keys:
(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map [?S] 'dired-mark-files-containing-regexp)
             (define-key dired-mode-map [?s] 'dired-mark-files-regexp)
             (define-key dired-mode-map [?I] 'dired-kill-subdir)
             (define-key dired-mode-map [(control up)] 'dired-tree-up)
             (define-key dired-mode-map [(control down)] 'dired-tree-down)
             (define-key dired-mode-map [(control left)] 'dired-hide-subdir)
             (define-key dired-mode-map [backspace] 'dired-up-directory)
             (define-key dired-mode-map [?n] 'dired-next-marked-file)
             (define-key dired-mode-map [?p] 'dired-prev-marked-file)))


;; Colors:
(make-face 'font-lock-operator-face)
(make-face 'font-lock-end-statement)
(setq font-lock-operator-face 'font-lock-operator-face)
(setq font-lock-end-statement 'font-lock-end-statement)

(setq default-background-color (first-valid-color "#F0F0F0" "white" :text "black"))
(setq default-foreground-color (first-valid-color "black" :text "white"))

(set-face-foreground 'default default-foreground-color)
(set-face-background 'default default-background-color)
(set-face-foreground 'isearch                           (first-valid-color "red"))
(set-face-background 'isearch                           (first-valid-color "lightgray" "grey" "white" :text "white"))
(set-face-background 'region                            (first-valid-color "#FFE0C0" "brightcyan" :text "cyan"))
(set-face-background 'show-paren-match-face             (first-valid-color "#E0C0FF" :text "white"))
(set-face-foreground 'show-paren-match-face             (first-valid-color "red" :text "green"))
(set-face-foreground 'font-lock-operator-face           (first-valid-color "red" :text "white"))
(set-face-foreground 'font-lock-end-statement           (first-valid-color "red" :text "white"))
(set-face-foreground 'font-lock-warning-face            (first-valid-color "violetred" "darkmagenta" "magenta"))
(set-face-background 'font-lock-warning-face            (first-valid-color "#F0D0D0" "grey" "white"))
(set-face-foreground 'font-lock-comment-face            (first-valid-color "sea green" "green"))
(set-face-foreground 'font-lock-keyword-face            (first-valid-color "blue"))
(set-face-foreground 'font-lock-type-face               (first-valid-color "firebrick" "violetred" "medium orchid" "purple" :text "white"))
(set-face-foreground 'font-lock-builtin-face            (first-valid-color "violetred" "darkcyan" "darkmagenta" "magenta"))
(set-face-foreground 'font-lock-string-face             (first-valid-color "royal blue" "#1515C3" "midnightblue" "darkblue" "darkcyan" :text "cyan"))
(set-face-foreground 'font-lock-function-name-face      (first-valid-color "red" "purple3"))

(make-face-unbold 'font-lock-builtin-face)
(make-face-unbold 'font-lock-function-name-face)

(if (> emacs-major-version 21)
    (set-face-foreground 'font-lock-comment-delimiter-face      (first-valid-color "sea green" "green")))

(if (> emacs-major-version 21)
    (progn 
      (eval-after-load "ediff"
        '(progn
           (set-face-background 'ediff-current-diff-A           (first-valid-color "honeydew"))
           (set-face-foreground 'ediff-current-diff-A           (first-valid-color "black"))
           (set-face-background 'ediff-current-diff-B           (first-valid-color "honeydew"))
           (set-face-foreground 'ediff-current-diff-B           (first-valid-color "black"))
           (set-face-background 'ediff-fine-diff-A              (first-valid-color "honeydew"))
           (set-face-foreground 'ediff-fine-diff-A              (first-valid-color "red"))
           (set-face-background 'ediff-fine-diff-B              (first-valid-color "honeydew"))
           (set-face-foreground 'ediff-fine-diff-B              (first-valid-color "red"))
           (set-face-background 'ediff-odd-diff-A               (first-valid-color "lavender blush"))
           (set-face-foreground 'ediff-odd-diff-A               (first-valid-color "dark gray"))
           (set-face-background 'ediff-even-diff-A              (first-valid-color "lavender"))
           (set-face-foreground 'ediff-even-diff-A              (first-valid-color "dark gray"))
           (set-face-background 'ediff-odd-diff-B               (first-valid-color "lavender blush"))
           (set-face-foreground 'ediff-odd-diff-B               (first-valid-color "dark gray"))
           (set-face-background 'ediff-even-diff-B              (first-valid-color "lavender"))
           (set-face-foreground 'ediff-even-diff-B              (first-valid-color "dark gray"))))
      (eval-after-load "diff-mode"
        '(progn
           (set-face-foreground 'diff-added                     (first-valid-color "darkgreen"))
           (set-face-foreground 'diff-removed                   (first-valid-color "red"))
           (set-face-foreground 'diff-hunk-header               (first-valid-color "blue"))
           (set-face-foreground 'diff-function                  (first-valid-color "blue")))))
    (progn 
      (eval-after-load "ediff"
        '(progn
           (set-face-background 'ediff-current-diff-face-A      (first-valid-color "honeydew"))
           (set-face-foreground 'ediff-current-diff-face-A      (first-valid-color "black"))
           (set-face-background 'ediff-current-diff-face-B      (first-valid-color "honeydew"))
           (set-face-foreground 'ediff-current-diff-face-B      (first-valid-color "black"))
           (set-face-background 'ediff-fine-diff-face-A         (first-valid-color "honeydew"))
           (set-face-foreground 'ediff-fine-diff-face-A         (first-valid-color "red"))
           (set-face-background 'ediff-fine-diff-face-B         (first-valid-color "honeydew"))
           (set-face-foreground 'ediff-fine-diff-face-B         (first-valid-color "red"))
           (set-face-background 'ediff-odd-diff-face-A          (first-valid-color "lavender blush"))
           (set-face-foreground 'ediff-odd-diff-face-A          (first-valid-color "dark gray"))
           (set-face-background 'ediff-even-diff-face-A         (first-valid-color "lavender"))
           (set-face-foreground 'ediff-even-diff-face-A         (first-valid-color "dark gray"))
           (set-face-background 'ediff-odd-diff-face-B          (first-valid-color "lavender blush"))
           (set-face-foreground 'ediff-odd-diff-face-B          (first-valid-color "dark gray"))
           (set-face-background 'ediff-even-diff-face-B         (first-valid-color "lavender"))
           (set-face-foreground 'ediff-even-diff-face-B         (first-valid-color "dark gray"))))))

(when (not window-system)
  (eval-after-load "eshell"
    '(custom-set-faces
      '(eshell-prompt ((t (:foreground "cyan" :weight normal))))
      '(eshell-ls-directory ((t (:foreground "blue")))))))

;; Welcome message:
(set-buffer "*scratch*")
(insert (format ";; Welcome to GNU Emacs %s (%s).\n" emacs-version system-configuration))
(insert  ";; Startup light-configuration files loaded succesfully.\n")
(insert ";;\n")
(set-buffer-modified-p nil)
