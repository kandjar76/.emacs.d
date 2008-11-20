;;;; highlight-current-word.el -- File to highlight the current word.
;;
;; Author: Cedric Lallain <kandjar76@hotmail.com>
;; Version:  1.0
;; Keywords: highlight current word
;; Description: File to highlight the current word
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
;; Highlight the current word
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'highlight-regexp)
(setq highlist-selected-word-toggle-state nil)
(make-face 'highlight-selected-word-face)
(set-face-background 'highlight-selected-word-face "lightcyan")
(set-face-foreground 'highlight-selected-word-face "blue")
(defvar highlight-selected-word-face 'highlight-selected-word-face
  "Font used to highlight the selected / current word.")

(defun highlight-current-word()
  "Use isearch library to highlight the current word"
  (interactive)
  (if (not (member (current-buffer) highlist-selected-word-toggle-state))
      (progn (push (current-buffer) highlist-selected-word-toggle-state)
	     (let ((highlight-regexp--face-index 1))
	       (highlight-regexp-current-word)))
      (progn (setq highlist-selected-word-toggle-state (remq (current-buffer) highlist-selected-word-toggle-state))
	     (sit-for 0)
	     (highlight-regexp-clear))))


(provide 'highlight-current-word)
