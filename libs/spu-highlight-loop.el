;;;; spu-highlight-loop.el -- File to highlight registers of the current instruction in the spu mode.
;;
;; Author: Cedric Lallain <kandjar76@hotmail.com>
;; Version:  1.0
;; Keywords: spu highlight minor mode opcode
;; Description: File to highlight registers of the current instruction in the spu mode.
;; Tested with: GNU Emacs 21.x and GNU Emacs 22.x
;;
;; This file is *NOT* part of GNU Emacs.
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Navigating through rolled up loop can be difficult so view what belong to each
;; iteration of the loop; this script grayed out the instructions which doesn't 
;; belong to the iteration of the current instruction
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(require 'spu-mode)

;;
;; Font definition:
;;

;; (make-face  'spu-highlight-registers-1)
;; (set-face-background 'spu-highlight-registers-1 (first-valid-color "palegreen1" "chartreuse1"))
;; (defvar spu-highlight-registers-1 'spu-highlight-registers-1
;;   "Font to highlight the first register of a SPU line.")
;; 
;; 
;; (make-face  'spu-highlight-registers-2)
;; (set-face-background 'spu-highlight-registers-2 (first-valid-color "light sky  blue" "moccasin" "coral1"))
;; (defvar spu-highlight-registers-2 'spu-highlight-registers-2
;;   "Font to highlight the second register of a SPU line.")
;; 
;; 
;; (make-face  'spu-highlight-registers-3)
;; (set-face-background 'spu-highlight-registers-3 (first-valid-color "plum1" "rosybrown1" "misty rose" "plum1" "orchid1"))
;; (defvar spu-highlight-registers-3 'spu-highlight-registers-3
;;   "Font to highlight the third register of a SPU line.")
;; 
;; (make-face  'spu-highlight-registers-4)
;; (set-face-background 'spu-highlight-registers-4 (first-valid-color "darkslategray2" "deepskyblue1"))
;; (defvar spu-highlight-registers-4 'spu-highlight-registers-3
;;   "Font to highlight the fourth register of a SPU line.")
;; 

;;
;; Global variables:
;;

;; Define the key mapping for the spu mode:
(defvar spu-highlight-loop-mode-map
  (let ((spu-highlight-loop-mode-map (make-keymap)))
    (define-key spu-highlight-loop-mode-map [(up)]    'spu-reformat-region)
    (define-key spu-highlight-loop-mode-map [(down)]  'spu-rollup-dependency-report-region)
    (define-key spu-highlight-loop-mode-map [(left)]  'spu-rollup-fusion-region)
    (define-key spu-highlight-loop-mode-map [(right)] 'spu-indent)
    (define-key spu-highlight-loop-mode-map [?q]      'spu-next-opcode)
    spu-highlight-loop-mode-map))

;(setq spu-highlight-registers-overlay-priority 1000)


;;
;; Utility functions:
;;




;;(defsubst spu-highlight-registers-post-hook()
;;  "Post command"
;;  (let ((case-fold-search nil))
;;    (spu-highlight-registers)))


;;
;; Defining spu-highlight-registers-minor-mode
;;


;; (define-minor-mode spu-highlight-registers-mode
;;   "Highlight the register of the current line."
;;   :init-value nil
;;   :global nil
;;   :lighter " HiRegs"
;;   ;; Body of the function:
;;   (make-local-variable 'post-command-hook)
;;   (if (not spu-highlight-registers-mode)
;;       (progn (remove-hook 'post-command-hook 'spu-highlight-registers-post-hook)
;; 	     (spu-highlight-register-clear-overlays)
;; 	     )
;;       (add-hook 'post-command-hook 'spu-highlight-registers-post-hook))
;;   (when (interactive-p)
;;     (message "SPU Highlight Registers Mode is now %s."
;; 	     (or (and spu-highlight-registers-mode "Enabled") "Disbaled"))))
;; 
;; 
;; (defcustom spu-highlight-registers-mode nil
;;   "*Non-nil means SPU Highight Register mode is enabled.
;; In this mode, when the cursor is on some specific line, all registers used in
;; this line are highlighted.
;; Setting this variable directly does not change the mode; instead, use
;; function `spu-highlight-registers-mode'."
;;   :set (lambda (symbol value) (spu-highlight-registers-mode (or value 0)))
;;   :initialize 'custom-initialize-default
;;   :type 'boolean)
;; 
(provide 'spu-highlight-loop)
