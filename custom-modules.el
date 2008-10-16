;;
;; This file loads different modules, set some custom keys for loaded functions,
;; also change some color to match the black background.
;;



; Doesn't seems to be loaded by default with emacs.
(require 'cl) 

(when window-system
  (require 'highlight-regexp))

;; Module: git
(when window-system
  (require 'git)
  (require 'git-log-mode)
  (require 'git+)
  (require 'gitsum))

;; Module: redo
;; This module enable a redo function which is the exact opposite of the well
;; known undo function
(require 'redo)

;; Module: Bookmark
;; This library will emulate the visual studio's bookmark
(autoload 'bm-toggle     "bm" "Toggle bookmark at point." t nil)
(autoload 'bm-next       "bm" "Goto next bookmark." t nil)
(autoload 'bm-previous   "bm" "Goto previous bookmark." t nil)
(autoload 'bm-remove-all "bm" "Delete all visible bookmarks in current buffer." t nil)

;; Module: ToolTip-Help
;; This library will enable tooltip using F1 key
(when window-system
  (global-set-key (kbd "<f1>") 'th-show-help)
  (autoload 'th-show-help "tooltip-help" t nil))


;; Module: highlight-current-line
;; Module which add a feature: the current line will now be highlighted.
(when window-system
  (require 'highlight-current-line)
  (highlight-current-line-on t))



;; Module: dabbrev-highlight
; To highlight the keyword dabbrev used to complete the word:
(when window-system
  (require 'dabbrev-highlight))



;; Module: cscope
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



;; Module: spu-mode
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


;; Module: dired
(when window-system
  (require 'dired+)
  (require 'dired++)
  (put 'dired-find-alternate-file 'disabled nil))


(require 'buff-menu++)
(require 'misc-tools)
(require 'increment-numbers)
(require 'replace-rect)
(when window-system
  (require 'quick-search)
  (require 'highlight-current-word))

(when (< emacs-major-version 23)
  (require 'find-dired++)) ;; -- doesn't work with emacs 23... 


;; Module: iswitchb
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


(when window-system
  (require 'yasnippet)
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/snippets/default")
  (define-key yas/keymap (kbd "<S-kp-tab>") 'yas/prev-field-group))


(when running-at-work
  (require 'c-tooltip))

;;================================================================================

