;;;; replace-rect.el -- Replace string/regexp within a selected rectangle
;;
;; Author: Cedric Lallain <kandjar76@hotmail.com>
;; Version:  1.0
;; Keywords: rectangle replace text
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
;; Provide two functions:
;;  - replace-string-rectangle
;;  - replace-regexp-rectangle
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(autoload 'operate-on-rectangle "rect")

;;;###autoload
(defun replace-string-rectangle (start end from-string to-string)
   "Replace the string FROM-STRING with the string TO-STRING in the rectangle delimited by START and END."
   (interactive "r\ns[Rectangle] Replace String: \ns[Rectangle] With: ")
   (let ((count 0))
     (operate-on-rectangle (lambda (pt ignore ignore) 
			     (let ((start (or (and (< pt (point)) pt) (point)))
				   (end   (or (and (> pt (point)) pt) (point))))
			       (goto-char start)
			       (while (search-forward from-string end t)
				 (setq count (1+ count))
				 (replace-match to-string nil t))))
			   start end nil)
     (if ( = count 1 )
	 (message "Replaced 1 occurrence")
	 (message "Replaced %i occurrences" count))))

;;;###autoload
(defun replace-regexp-rectangle (start end from-regexp to-string)
   "Replace the regexp FROM-REGEXP with the string TO-STRING in the rectangle delimited by START and END."
   (interactive "r\ns[Rectangle] Replace Regexp: \ns[Rectangle] With: ")
   (let ((count 0))
     (operate-on-rectangle (lambda (pt ignore ignore) 
			     (let ((start (or (and (< pt (point)) pt) (point)))
				   (end   (or (and (> pt (point)) pt) (point))))
			       (goto-char start)
			       (while (re-search-forward from-regexp end t)
				 (setq count (1+ count))
				 (replace-match to-string nil nil))))
			   start end nil)
     (if ( = count 1 )
	 (message "Replaced 1 occurrence")
	 (message "Replaced %i occurrences" count))))

(provide 'replace-rect)
