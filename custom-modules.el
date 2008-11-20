;;
;; This file loads different modules, set some custom keys for loaded functions,
;; also change some color to match the black background.
;;



; Doesn't seems to be loaded by default with emacs.
(require 'cl) 


(require 'p4)


;; Module: git
(require 'git)
(require 'git-blame)
(require 'vc-git)


;; Module: redo
;; This module enable a redo function which is the exact opposite of the well
;; known undo function
(require 'redo)


;; Module: Bookmark
;; This library will emulate the visual studio's bookmark
(require 'bm)


;; Module: ToolTip-Help
;; This library will enable tooltip using F1 key
(require 'tooltip-help)


;; Module: highlight-current-line
;; Module which add a feature: the current line will now be highlighted.
(require 'highlight-current-line)
(highlight-current-line-on t)


;; Module: dabbrev-highlight
; To highlight the keyword dabbrev used to complete the word:
(require 'dabbrev-highlight)


;; Module: cscope
(require 'xcscope+)


;; Module: spu-mode
(require 'spu-mode)
(require 'spu-highlight-registers)
(require 'spu-highlight-stalls)
(require 'spu-tooltip)
(require 'spu-highlight-loop)

;; Autoactivate the loading of the SPU registers:
(add-hook 'spu-mode-hook 'spu-highlight-registers-mode)
(add-hook 'spu-mode-hook 'spu-highlight-latency-mode)


;; Module: extra modes
(require 'ddf-mode)
(require 'idf-mode)


;; Module: dired
(require 'dired+)
(require 'dired++)
(put 'dired-find-alternate-file 'disabled nil)


(require 'buff-menu++)
(require 'misc-tools)
(require 'increment-numbers)
(require 'replace-rect)
(require 'quick-search)
(require 'highlight-current-word)
;(require 'find-dired++) ;; -- doesn't work with emacs 23... 


;; Module: iswitchb
(require 'iswitchb)
(iswitchb-default-keybindings)
(add-hook 'iswitchb-define-mode-map-hook
	    '(lambda ()
	       (define-key iswitchb-mode-map " " 'iswitchb-next-match)
	       (define-key iswitchb-mode-map [del] 'iswitchb-prev-match)
	       (define-key iswitchb-mode-map [bs] 'iswitchb-prev-match)
	       (define-key iswitchb-mode-map [right] 'iswitchb-next-match)
	       (define-key iswitchb-mode-map [left] 'iswitchb-prev-match)))
(icomplete-mode 1)
(defadvice iswitchb-visit-buffer (after iswitchb-visit-buffer(buffer))
  (message buffer-file-name))
(ad-activate 'iswitchb-visit-buffer)


;;================================================================================

