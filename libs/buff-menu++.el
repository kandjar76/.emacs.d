;;; buff-menu++.el --- additional buff-menu functionality
;; Copyright (C) 2008  Cedric Lallain

;; Author: Cedric Lallain (cedric_lallain@naughtydog.com)
;; Updated: 2008-07-11

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; if not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;
;; To use, please add the following to your .init file:
;;
;;   (require 'buff-menu++)
;;   ;; Buffer-menu additional keys:
;;   (define-key Buffer-menu-mode-map [(shift r)] 'Buffer-menu-mark-every-files-to-revert)
;;   (define-key Buffer-menu-mode-map [(shift s)] 'Buffer-menu-mark-every-files-to-save)
;;   (define-key Buffer-menu-mode-map [(shift d)] 'Buffer-menu-mark-every-files-to-delete)
;;   (define-key Buffer-menu-mode-map [r] 'Buffer-menu-mark-file-to-revert)
;;   (define-key Buffer-menu-mode-map [?=] 'Buffer-menu-diff-buffer-with-file)
;;
;; Extra for the buffer mode:
;;
;; Didn't really check what (Buffer-menu-no-header) was doing... 
;; The functions may required some 
;;

;;; Code:

;;;###autoload
(defun Buffer-menu-mark-file-to-revert()
  "Mark files to revert in the bufffer list"
  (interactive)
  (when (not (eq major-mode 'Buffer-menu-mode))
      (error "Invalid Mode -- Expected: Buffer-menu-mode"))
  (when (Buffer-menu-no-header)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?R)
      (forward-line 1))))

;;;###autoload
(defun Buffer-menu-mark-every-files-to-revert()
  "Mark files to which can be reverted in the bufffer list"
  (interactive)
  (when (not (eq major-mode 'Buffer-menu-mode))
      (error "Invalid Mode -- Expected: Buffer-menu-mode"))
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (not (eobp))
	(let* ((buf (Buffer-menu-buffer t))
	       (vis (verify-visited-file-modtime buf))
	       (wfi (save-excursion (set-buffer buf) buffer-file-name))
	       (df  (and wfi (file-exists-p wfi))))
	  (when (and (not vis) df)
	    (delete-char 1)
	    (insert ?R))
	(forward-line 1))))))

;;;###autoload
(defun Buffer-menu-mark-every-files-to-delete()
  "Mark files to which are gone in the bufffer list"
  (interactive)
  (when (not (eq major-mode 'Buffer-menu-mode))
      (error "Invalid Mode -- Expected: Buffer-menu-mode"))
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (not (eobp))
	(let* ((buf (Buffer-menu-buffer t))
	       (wfi (save-excursion (set-buffer buf) buffer-file-name)))
	  (when (and wfi (not (file-exists-p wfi)))
	    (delete-char 1)
	    (insert ?D))
	(forward-line 1))))))

;;;###autoload
(defun Buffer-menu-mark-every-files-to-save()
  "Mark files to which need to saved in the bufffer list"
  (interactive)
  (when (not (eq major-mode 'Buffer-menu-mode))
      (error "Invalid Mode -- Expected: Buffer-menu-mode"))
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (not (eobp))
	(let* ((buf (Buffer-menu-buffer t))
	       (mod (buffer-modified-p buf))
	       (wfi (save-excursion (set-buffer buf) buffer-file-name)))
	  (when (and mod wfi)
	    (beginning-of-line)
	    (forward-char 2)
	    (delete-char 1)
	    (insert ?S))
	(forward-line 1))))))


;;;###autoload
(defun Buffer-menu-diff-buffer-with-file()
  "View the difference between buffer describe by the current line and it's associated file
This require the external program 'diff' to be in your 'exec-path'"
  (interactive)
  (when (Buffer-menu-no-header)
    (let ((buf (Buffer-menu-buffer t)))
      (diff-buffer-with-file buf))))

;;================================================================================

;;;###autoload
(defadvice Buffer-menu-unmark (around new-unmark-with-modified-flag (&optional backup) activate)
  (interactive "P")
  (when (Buffer-menu-no-header)
    (let* ((buf (Buffer-menu-buffer t))
	   (mod (buffer-modified-p buf))
	   (readonly (save-excursion (set-buffer buf) buffer-read-only))
	   (buffer-read-only nil)
	   (vis (verify-visited-file-modtime buf)))
      (delete-char 3)
      (insert ?\s
	      (if readonly ?% ?\s)
	      (if mod ?* (if vis ?\s ?#)))))
  (forward-line (if backup -1 1)))


;;================================================================================

;;;###autoload
(defadvice list-buffers-noselect (after update-external-modification-flag (&optional files-only buffer-list) activate)
  (interactive)
  (with-current-buffer (get-buffer "*Buffer List*")
    (save-excursion
      (let ((buffer-read-only nil))
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((buf (Buffer-menu-buffer t))
		 (mod (buffer-modified-p buf))
		 (vis (verify-visited-file-modtime buf)))
	    (forward-char 2) 
	    (delete-char 1)
	    (insert (if mod ?* (if vis ?\s ?#)))
	    (beginning-of-line))
	  (forward-line 1))))))

;;================================================================================

;;;###autoload
(defadvice Buffer-menu-execute (after revert-selected-files activate)
  (save-excursion
    (Buffer-menu-beginning)
    (while (re-search-forward "^R" nil t)
      (let ((modp nil)
	    (rdp  nil)
	    (visp nil))
	(save-excursion
	  (set-buffer (Buffer-menu-buffer t))
	  (if buffer-file-name
	      (revert-buffer t (not (buffer-modified-p))))
	  (setq modp (buffer-modified-p))
	  (setq rdp  buffer-read-only)
	  (setq visp (verify-visited-file-modtime (current-buffer))))
	(let ((buffer-read-only nil))
	  (forward-char -1)
	  (delete-char 3)
	  (insert ?\s
		  (if rdp ?% ?\s)
		  (if modp ?* (if visp ?\s ?#)))
	  )))))

(provide 'buff-menu++)