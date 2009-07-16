;;;; misc-tools.el -- Set of little tools which doesn't fit anywhere else :)
;;
;; Author: Cedric Lallain <kandjar76@hotmail.com>
;; Version:  1.0
;; Keywords: misc function tool
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
;; Here is a list of the function define within this file
;;
;; - electric-split-window-horizontally
;; - electric-split-window-vertically
;; - revert-buffer-now
;; - kill-buffer-now
;; - dos-to-unix
;; - unix-to-dos
;; - ascii-table
;; - show-file-name
;; - setup-text-mode
;; - backward-delete-word
;; - kill-selected-region
;; - toggle-full-screen
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(defadvice show-paren-function (after show-matching-paren-offscreen
;;                                      activate)
;;  "If the matching paren is offscreen, show the matching line in the                               
;;echo area. Has no effect if the character before point is not of                                   
;;the syntax class ')'."
;;  (interactive)
;;  (let ((matching-text nil))
;;    ;; Only call `blink-matching-open' if the character before point                               
;;    ;; is a close parentheses type character. Otherwise, there's not                               
;;    ;; really any point, and `blink-matching-open' would just echo                                 
;;    ;; "Mismatched parentheses", which gets really annoying.                                       
;;    (if (char-equal (char-syntax (char-before (point))) ?\))
;;        (setq matching-text (blink-matching-open)))
;;    (if (not (null matching-text))
;;        (message matching-text))))

;;;###autoload
(defun electric-split-window-horizontally(&optional arg)
  "Split current window into two window side by side, move the cursor to the new created window"
  (interactive "P")
  (split-window-horizontally arg)
  (other-window 1))

;;;###autoload
(defun electric-split-window-vertically(&optional arg)
  "Split current window into two window side by side, move the cursor to the new created window"
  (interactive "P")
  (split-window-vertically arg)
  (other-window 1))

;;;###autoload
(defun revert-buffer-now ()
  "Silently calls revert-buffer if the current buffer is not modified."
  (interactive)
  (if (not (buffer-modified-p))
	  (message (format "Reverted from %s" (buffer-file-name))))
  (revert-buffer nil (not (buffer-modified-p))))

;;;###autoload
(defun kill-buffer-now ()
  "Calls kill-buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

;;;###autoload
(defun dos-to-unix()
  "Convert the end-of-line of the current file from DOS to UNIX"
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
	(while (search-forward "\r" nil t) (replace-match ""))))

;;;###autoload
(defun unix-to-dos()                                       
  "Convert the end-of-line of the current file from UNIX to DOS"
  (interactive "*")
  (save-excursion
	(goto-char (point-min))
	(while (search-forward "\n" nil t) (replace-match "\r\n"))))

;;;###autoload
(defun ascii-table ()                                     
  "Print the ascii table."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 32))
    (while (< i 254)
	  (let ((j (min 254 (+ i 8))))
		(while (< i j)
		  (setq i (+ i 1))
		  (insert (format "%4d %c\t" i i)))
		(insert "\n")))
	(beginning-of-buffer)))

;;;###autoload
(defun show-file-name()
  "Display the full path of the current buffer."
  (interactive)
  (message buffer-file-name))

;;;###autoload
(defun setup-text-mode()
  "Setup the text mode to write txt docs."
  (interactive)
  (paragraph-indent-text-mode)
  (auto-fill-mode)
  (set-fill-column 80)
  (flyspell-mode))

;;;###autoload
(defun backward-delete-word(&optional arg)
  "Delete the previous word"
  (interactive "*")
  (save-excursion
    (let ((end (point)))
      (backward-word 1)
      (delete-region (point) end))))

;;;###autoload
(defun kill-selected-region (&optional arg)
  "Equivalent to kill-region, except, it won't kill it if the selected region isn't active"
  (interactive "*")
  (if mark-active
      (kill-region (mark) (point))))

;;;###autoload
(defun swap-windows ()
 "Swap the buffer in the first two windows.
  Error is generated if there is more than 2 windows :)"
 (interactive) 
 (if (not (= (count-windows) 2)) (message "You need exactly 2 windows to do this.")
     (let* ((win1 (first (window-list)))
	    (win2 (second (window-list)))
	    (buf1 (window-buffer win1))
	    (buf2 (window-buffer win2))
	    (pos1 (window-start win1))
	    (pos2 (window-start win2)))
       (set-window-buffer win1 buf2)
       (set-window-buffer win2 buf1)
       (set-window-start win1 pos2)
       (set-window-start win2 pos1))))


;;;###autoload
(defun extract-string-length (&optional single)
  "Print the number of characters in the string which point is inside.
With prefix arg, use single quotes, not double quotes, as delimeters."
  (interactive "P")
  (let* ((quote-char   (or (and single ?\') ?\"))
         (quote-string (format "^%c\n" quote-char)))
    (save-excursion
      (skip-chars-forward quote-string)
      (if (/= (following-char) quote-char)
          (error "Point is not inside string"))
      (let ((length (- (point)
                       (progn
                         (skip-chars-backward quote-string)
                         (if (/= (preceding-char) quote-char)
                             (error "Point is not inside string"))
                         (point)))))
        (message "String has %d (0x%x) characters" length length)))))

;;;###autoload
(defun sum-column (start end)
   "Adds numbers in rectangle defined by START to END."
   (interactive "r")
   (save-excursion
     (kill-rectangle start end)
     (exchange-point-and-mark)
     (yank-rectangle)
     (set-buffer (get-buffer-create "*calc-sum*"))
     (erase-buffer)
     (yank-rectangle)
     (exchange-point-and-mark-nomark)
     (let ((sum 0))
       (while (re-search-forward "[0-9]*\\.?[0-9]+" nil t)
	 (setq sum (+ sum (string-to-number (match-string 0)))))
       (message "Sum: %f" sum))))


;;;###autoload
(defun toggle-fullscreen () 
  "Toggle full screen mode"
  (interactive) 
  (set-frame-parameter nil 
		       'fullscreen 
		       (and (not (frame-parameter nil 'fullscreen))
			   'fullboth)))

;; use: (w32-send-sys-command 61488) to toggle fullscreen under windows
;; use: (w32-send-sys-command 61728) to restore the window


(provide 'misc-tools)
;;--------------------------------------------------------------------------------

