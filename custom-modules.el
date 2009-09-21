;;
;; This file loads different modules, set some custom keys for loaded functions,
;; also change some color to match the black background.
;;


;;
;; Module: misc-tools
;; 

(require 'misc-tools)


;;
;; Module: git
;;

(autoload 'git-status "git" "Entry point into git-status mode." t nil)
(autoload 'git-diff-current-buffer "git+" "Diff the current buffer against HEAD." t nil)
(autoload 'git-log-current-buffer "git+" "Display a log of changes to current buffer." t nil)
(eval-after-load "git"
  '(progn
     (require 'git-log-mode)
     (require 'git+)
     (require 'gitsum)))


;;
;; Module: redo
;;

;; This module enable a redo function which is the exact opposite of the well
;; known undo function
(require 'redo)


;;
;; Module: ToolTip-Help
;;

;; This library will enable tooltip using F1 key
;;(when window-system
;;  (define-key global-map [(f1)] 'th-show-help)
;;  (autoload 'th-show-help "tooltip-help" t nil))

;;
;; Module: dabbrev-highlight
;;

; To highlight the keyword dabbrev used to complete the word:
(when window-system
  (let (current-load-list) (defadvice dabbrev-expand (after dabbrev-expand-highlight activate) "Advised by dabbrev-highlight.el.\nHighlight last expanded string." (dabbrev-highlight)))
  (autoload 'dabbrev-highlight "dabbrev-highlight" "Not documented" nil nil))


;;
;; Module: cscope
;;

(autoload 'cscope-find-this-symbol-no-prompting-no-updates        "xcscope+"
  "Locate a symbol in source code [no database update performed -- no user prompting]." t nil)
(autoload 'cscope-find-global-definition-no-prompting-no-updates  "xcscope+"
  "Find a symbol's global definition without prompting [no database update performed]." t nil)
(autoload 'cscope-find-this-text-string-no-updates                "xcscope+"
  "Locate where a text string occurs [no database update performed]." t nil)
(autoload 'cscope-find-this-symbol-no-updates                     "xcscope+"  
  "Locate a symbol in source code [no database update performed]." t nil)
(autoload 'cscope-find-functions-calling-this-function-no-updates "xcscope+"
  "Display functions calling a function [no database update performed]." t nil)
(autoload 'cscope-find-global-definition-no-updates               "xcscope+"
  "Find a symbol's global definition [no database update performed]." t nil)
(autoload 'cscope-find-this-text-string                           "xcscope"
  "Locate where a text string occurs." t nil)
(autoload 'cscope-find-this-symbol                                "xcscope"
  "Locate a symbol in source code." t nil)
(autoload 'cscope-find-functions-calling-this-function            "xcscope"
  "Display functions calling a function." t nil)
(autoload 'cscope-find-global-definition                          "xcscope"
  "Find a symbol's global definition." t nil)
(autoload 'cscope-pop-mark                                        "xcscope"
  "Pop back to where cscope was last invoked." t nil)



;;
;; Module: spu-mode
;;

(autoload 'spu-mode "spu-mode" "Major mode for editing SPU assembly code." t nil)
(when window-system
  (eval-after-load "spu-mode"
    '(progn
       (require 'spu-highlight-registers)
       (require 'spu-highlight-stalls)
       (require 'spu-tooltip)
       (require 'spu-highlight-loop)
       ;; Autoactivate the loading of the SPU registers:
       (add-hook 'spu-mode-hook 'spu-highlight-registers-mode)
       (add-hook 'spu-mode-hook 'spu-highlight-latency-mode))))

;; Module: extra modes
(autoload 'ddf-mode "ddf-mode" "Major mode for editing DDF files." t nil)
(autoload 'idf-mode "idf-mode" "Major mode for editing IDF files." t nil)


;;
;; Module: align-let
;;

(autoload 'align-let "align-let"
  "Align the value expressions for the variables in a Lisp `let' form.
Point should be within or immediately in front of the let form.  It
changes for instance

    (let ((x 1)
          (foo   2)
          (zz (blah)))
      ...)

to

    (let ((x   1)
          (foo 2)
          (zz  (blah)))
      ...)


When point is somewhere in the middle of the form, possibly nested in an
expression, the beginning is found by looking for a pattern \"(sym ((...\"
or \"(and-let* (\".

The symbols that might introduce the form (`let', `let*', etc) are not hard
coded, this allows `align-let' to adapt to forms specific to various Lisp
dialects.  `and-let*' from Scheme is explicitly recognised, since it can
start with a bare variable rather than a binding form." t nil)


;;
;; Module: highlight-current-line
;;

;; Module which add a feature: the current line will now be highlighted.
(when window-system
  (require 'highlight-current-line)
  (highlight-current-line-on t)
  (setq highlight-current-line-ignore-regexp (concat highlight-current-line-ignore-regexp "\\|\\*terminal\\*\\|\\*ansi-term\\*")))


;;
;; Module: dired
;;

(when window-system
  (eval-after-load "dired"
    '(progn (put 'dired-find-alternate-file 'disabled nil)
	    (require 'dired+)))
  (eval-after-load "dired+"
    '(require 'dired++)))


;;
;; Module: buff-menu++
;;

(require 'buff-menu++)


;;
;; Module: increment-numbers
;;

(autoload 'increment-numbers-multilines "increment-numbers" 
  "Increment the value on each lines" t nil)
(autoload 'increment-numbers-region "increment-numbers" 
  "Increment each number in the selected region by 1 or by the value of the prefix argument" t nil)
(autoload 'increase-numbers-on-rectangle "increment-numbers" 
  "Increment each number in the selected rectangle depending on the prefix argument COUNT:
if COUNT is integer value, each number is increased by COUNT
if COUNT is a simple prefix value (C-U), each number is increased by 1
if COUNT is nil, each number is increased by line number within the selection (starting at 0)" t nil)


;;
;; Module: replace-rect
;;

(autoload 'replace-string-rectangle "replace-rect"
   "Replace the string FROM-STRING with the string TO-STRING in the rectangle delimited by START and END." t nil)
(autoload 'replace-regexp-rectangle "replace-rect"
   "Replace the regexp FROM-REGEXP with the string TO-STRING in the rectangle delimited by START and END." t nil)


;;
;; Module: quick-search
;;

(autoload 'quick-search-forward "quick-search" 
  "Do a forward search using the word below the cursor. Store the current searched word in a global variable to allow a repeated search using the function repeat-search-current-word-forward or repeat-search-current-word-backward." t nil) 
(autoload 'quick-search-backward "quick-search"
  "Do a forward search using the word below the cursor. Store the current searched word in a global variable to allow a repeated search using the function repeat-search-current-word-forward or repeat-search-current-word-backward." t nil)
(autoload 'repeat-quick-search-forward "quick-search"
  "Repeat a forward research using the stored current word (cf: search-current-word-forward / search-current-word-backword)" t nil)
(autoload 'repeat-quick-search-backward "quick-search"
  "Repeat a forward research using the stored current word (cf: search-current-word-forward / search-current-word-backword)" t nil)


;;
;; Module: highlight-current-word
;;

(autoload 'highlight-current-word "highlight-current-word" 
  "Use isearch library to highlight the current word" t nil)


;;
;; Module: find-dired++
;;

(when (< emacs-major-version 23)
  (eval-after-load "find-dired"
    '(require 'find-dired++))) ;; -- doesn't work with emacs 23... 


;;
;; Module: ido
;;

(require 'ido)
(ido-mode t)
(setq ido-auto-merge-work-directories-length -1)   ; 
(setq ido-create-new-buffer 'always)               ; No more confirmation prompt when I explicitly request a new buffer
(setq ido-enable-flex-matching t)                  ; Enable flex matching (sequence conserved)

(when running-at-work
  (setq ido-enable-flex-matching nil))             ; Sadly, currently this is way too slow and has to be disabled.


; (setq ido-execute-command-cache nil)
(defun ido-execute-command ()
  "Provide M-x with command completion using ido"
  (interactive)
  (let (ido-execute-command-cache)
    (call-interactively
     (intern
      (ido-completing-read
       "M-x "
       (progn
	 (unless ido-execute-command-cache
	   (mapatoms (lambda (s)
		       (when (commandp s)
			 (setq ido-execute-command-cache
			       (cons (format "%S" s) ido-execute-command-cache))))))
	 ido-execute-command-cache))))))

(global-set-key "\M-x" 'ido-execute-command)


;;
;; Uniquify
;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;
;; Module: yas/snippet
;;

(setq yas-initialize nil)
(eval-after-load "yasnippet"
  '(when (not yas-initialize)
     (yas/initialize)
     (yas/load-directory "~/.emacs.d/snippets/default")
     (define-key yas/keymap (kbd "<S-kp-tab>") 'yas/prev-field-group)
     (setq yas-initialize t)))

(when window-system
  (add-hook 'c++-mode-hook '(lambda() (require 'yasnippet)))
  (add-hook 'c-mode-hook '(lambda() (require 'yasnippet))))


;;
;; Module: c-tooltip
;;

(when running-at-work
  (eval-after-load "tooltip-help"
    '(require 'c-tooltip)))


;;
;; Module: custom-awk-script
;;

(autoload 'cpp-align-comment "custom-awk-scripts"
  "Align the eol comments within the selected region -- using a gawk script" t nil)
(autoload 'cpp-align-variable-assignment "custom-awk-scripts"
  "Align the variables and also the assignment symbol ('=')" t nil)
(autoload 'cpp-align-function-bracket "custom-awk-scripts"
  "Align the function name and also the open bracket symbols" t nil)
(autoload 'cpp-comment-block "custom-awk-scripts"
  "Comment/UnComment a block of code using the C++ type of comment '//'" t nil)
(autoload 'awk-align-comment "custom-awk-scripts"
  "Align the eol comments within the selected region -- using a gawk script" t nil)
(autoload 'awk-comment-block "custom-awk-scripts"
  "Comment/UnComment a block of code using the C++ type of comment '//'" t nil)
(autoload 'spu-reformat-region "custom-awk-scripts"
  "Reformat the current region of the code -- using a gawk script" t nil)
(autoload 'spu-rollup-dependency-report-region "custom-awk-scripts"
  "Report the dependency of a specific loop during a rollup process
Those dependency must be filled up during the previous loop!" t nil)
(autoload 'spu-rollup-fusion-region "custom-awk-scripts"
  "Fusion several loop into the final rollup version!" t nil)
(autoload 'spu-format-dump-file-region "custom-awk-scripts"
  "Format generated dump spu file" t nil)


;;
;; Module: msvc
;;

(autoload 'find-sln "sln-mode"
  "Open an sln file and create a project buffer using the data in it." t nil)

;;
;; Module: my-cpp/my-awk
;;

(autoload 'custom-c-setup    "my-cpp")
(autoload 'custom-awk-setup  "my-awk")

(add-hook 'c++-mode-hook 'custom-c-setup)
(add-hook 'c++-mode-hook 'turn-on-cwarn-mode)
(add-hook 'c-mode-hook 'custom-c-setup)
(add-hook 'c-mode-hook 'turn-on-cwarn-mode)
(add-hook 'awk-mode-hook 'custom-awk-setup)

;;
;; Module: moccur
;;

;;(autoload 'moccur "color-moccur"
;;  "Show all lines of all buffers containing a match for REGEXP.
;;The lines are shown in a buffer named *Moccur*.
;;It serves as a menu to find any of the occurrences in this buffer.
;;\\[describe-mode] in that buffer will explain how." t nil)
;;(autoload 'moccur-grep "color-moccur"
;;  "moccur-grep <regexp> shows all occurrences of <regexp> in files of current directory" t nil)
;;
;;(eval-after-load "color-moccur"
;;    '(require 'moccur-edit))


;;
;; Module: org-mode / remember / agenda
;;

(when running-at-work
  (autoload 'org-agenda "org-agenda")
  (autoload 'org-remember "org-remember")
  (setq org-agenda-files (list "~/org/tasks.org"
			       "~/org/notes.org")))


(eval-after-load "org"
  '(progn 
     (setq org-todo-keywords
	   '((sequence "TODO" "BUG" "IN PROGRESS" "STALLED" "PENDING" "|" "DONE" "FIXED" "CANCELED")))
     (setq org-todo-keyword-faces
	   '(("STALLED"   . org-warning)
	     ("PENDING"   . org-warning)
	     ("CANCELED"  . (:foreground "darkgray" :inherit bold))))
     (setq org-hide-leading-stars t) ; Show only the last '*' of the header lines
     (setq org-log-done t)           ; Log time stamp when the job is marked DONE!
     
     ;; Some settup for remember:
     (setq org-directory          "~/org")
     (setq org-default-notes-file "~/.notes")
     (setq org-agenda-tags-column -150)
     (setq org-tags-column -120)

     (defun org-insert-update-tag(arg)
       (interactive "P")
       (insert "[Update: ]")
       (backward-char 1)
       (org-time-stamp arg)
       (forward-char 1))
     (define-key org-mode-map [(control c) ?u] 'org-insert-update-tag)
))

(eval-after-load "remember"
  '(progn
     (setq remember-annotation-functions '(org-remember-annotation))
     (setq remember-handler-functions    '(org-remember-handler))
     (add-hook 'remember-mode-hook 'org-remember-apply-template)
     (setq org-remember-templates
	   '(("Todo" 
	      ?t 
	      "* TODO %^{Brief Description} %^g\t\n  DEADLINE: %^t\n\n  %?\n\n  [Added: %U]" 
	      "~/org/tasks.org" 
	      "Tasks")

	     ("Bug"  
	      ?b 
	      "* BUG %^{Head Line} %U %^g\n  %?"
	      "~/org/tasks.org"
	      "Bugs")

	     ("Note"
	      ?n
	      "* %^{Head Line} %^g\n  %?\n\n  [Added: %U]"
	      "~/org/notes.org" "Notes")
	   ))
     ))

;;
;; Module: Project-Buffer
;;

(autoload 'fsproject-create-project "fsproject")
(autoload 'project-buffer-find-file "project-buffer-mode" 
  "Create a project-buffer-mode buffer based on the content of FILENAME." t nil)
(autoload 'iproject-new "iproject"
  "Create a iproject buffer named NAME with a default-directory set to ROOT-FOLDER." t nil)

(eval-after-load "project-buffer-mode"
  '(progn
     (require 'project-buffer-mode+)
     (require 'project-buffer-occur)
     (project-buffer-mode-p-setup)
     (define-key project-buffer-mode-map [(control ?f)] 'project-buffer-occur)))

;;

(defun fsproject-ice-action-handler(action project-name project-path platform configuration)
  "fsproject-ice action handler."
  (let ((make-cmd (cond ((eq action 'build) "")
			((eq action 'clean) "clean"))))
    (if (or (eq action 'run) (eq action 'debug))
	(message "Run and Debug aren't supported.")
	(compile 
	 (concat "make -j16 -C " (file-name-directory project-path) " -f " (file-name-nondirectory project-path) " " make-cmd)))))

(defun fsproject-ice(root-folder)
  (interactive "DRoot Folder: ")
  (let ((regexp-project-name  "[Mm]akefile")
	(regexp-file-filter   '("\\.cpp$" "\\.c$" "\\.h$" "\\.ddf$" "\\.inl$" "\\.spu$" "\\.spu.s$" "\\.mak$" "Makefile"))
	(ignore-folders       '("build" "docs" "jobelf" "lib" "bin"))
;	(pattern-modifier     '(("^\\(?:.*/\\)?\\([a-zA-Z0-9_]*\\.cpp\\)$" . "source/\\1")
;			       ("^\\(?:.*/\\)?\\([a-zA-Z0-9_]*\\.\\(?:h\\|inl\\)\\)$" . "include/\\1")
;			       ("^\\(?:.*/\\)?\\([a-zA-Z0-9_]*\\.\\(?:spu\\|spu.s\\|inc.spu\\|h.spu\\)\\)$" . "spu/\\1")))
	pattern-modifier
	(build-configurations '("debug" "release"))
	(platforms            '("ps3")))
    (fsproject-create-project root-folder
			   regexp-project-name
			   regexp-file-filter
			   'fsproject-ice-action-handler
			   ignore-folders
			   pattern-modifier
			   build-configurations
			   platforms)))


;;
;; Module: mtp-chat
;;

(autoload 'mtpchat "mtpchat"
  "Entry point to start the MtpChat client" t nil)


;;
;; Module: color-occur
;;

;;(require 'color-occur)

;;================================================================================

