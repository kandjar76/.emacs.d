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
;; Module: Bookmark
;;

;; This library will emulate the visual studio's bookmark
(autoload 'bm-toggle     "bm" "Toggle bookmark at point." t nil)
(autoload 'bm-next       "bm" "Goto next bookmark." t nil)
(autoload 'bm-previous   "bm" "Goto previous bookmark." t nil)
(autoload 'bm-remove-all "bm" "Delete all visible bookmarks in current buffer." t nil)


;;
;; Module: ToolTip-Help
;;

;; This library will enable tooltip using F1 key
(when window-system
  (define-key global-map [(f1)] 'th-show-help)
  (autoload 'th-show-help "tooltip-help" t nil))


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
;; Module: highlight-current-line
;;

;; Module which add a feature: the current line will now be highlighted.
(when window-system
  (require 'highlight-current-line)
  (highlight-current-line-on t))


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
;; Module: iswitchb
;;

(require 'iswitchb)
(iswitchb-default-keybindings)
(setq iswitchb-prompt-newbuffer nil)
(add-hook 'iswitchb-define-mode-map-hook
	  '(lambda ()
	     (define-key iswitchb-mode-map [right] 'iswitchb-next-match)
	     (define-key iswitchb-mode-map [left] 'iswitchb-prev-match)))
(icomplete-mode 1)
(defadvice iswitchb-visit-buffer (after iswitchb-visit-buffer(buffer))
  (message buffer-file-name))
(ad-activate 'iswitchb-visit-buffer)


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
  (add-hook 'c-mode-hook ''(lambda() (require 'yasnippet))))


;;
;; Module: c-tooltip
;;

(when running-at-work
  (eval-after-load "tooltip-help"
    '(require 'c-tooltip)))


;;
;; Module: custom-awk-script
;;

(autoload 'cpp-align-comment "custom-awk-script"
  "Align the eol comments within the selected region -- using a gawk script" t nil)
(autoload 'cpp-align-variable-assignment "custom-awk-script"
  "Align the variables and also the assignment symbol ('=')" t nil)
(autoload 'cpp-align-function-bracket "custom-awk-script"
  "Align the function name and also the open bracket symbols" t nil)
(autoload 'cpp-comment-block "custom-awk-script"
  "Comment/UnComment a block of code using the C++ type of comment '//'" t nil)
(autoload 'awk-align-comment "custom-awk-script"
  "Align the eol comments within the selected region -- using a gawk script" t nil)
(autoload 'awk-comment-block "custom-awk-script"
  "Comment/UnComment a block of code using the C++ type of comment '//'" t nil)
(autoload 'spu-reformat-region "custom-awk-script"
  "Reformat the current region of the code -- using a gawk script" t nil)
(autoload 'spu-rollup-dependency-report-region "custom-awk-script"
  "Report the dependency of a specific loop during a rollup process
Those dependency must be filled up during the previous loop!" t nil)
(autoload 'spu-rollup-fusion-region "custom-awk-script"
  "Fusion several loop into the final rollup version!" t nil)
(autoload 'spu-format-dump-file-region "custom-awk-script"
  "Format generated dump spu file" t nil)


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



;;================================================================================

