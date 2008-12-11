;;; git+.el --- Additional function for git

;; Copyright (C) 2008 Cedric Lallain <kandjar76@hotmail.com>

;; Version: 1.2

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This file provides a major mode to view the git logs

;;; History:

;; 1.0: Added git-diff-current-file
;; 1.1: Added git-diff-current-buffer and git-log-current-buffer
;; 1.2: Check doc run

;;; Code:

(require 'git)

(defun git-diff-current-file()
  "Diff the current file against HEAD."
  (interactive)
  (unless git-status (error "Not in git-status buffer"))
  (let* ((pos (ewoc-locate git-status))
	 (info (ewoc-data pos))
	 (files (list info)))
    (if git-ignore-whitespace
	(git-setup-diff-buffer
	 (apply #'git-run-command-buffer "*git-diff*" "diff-index" "-b" "-p" "-M" "HEAD" "--" (git-get-filenames files)))
	(git-setup-diff-buffer
	 (apply #'git-run-command-buffer "*git-diff*" "diff-index" "-p" "-M" "HEAD" "--" (git-get-filenames files)))
	)))

(defun git-diff-current-buffer()
  "Diff the current buffer against HEAD."
  (interactive)
  (let* ((file (list (buffer-file-name)))
	 (buffer (and (buffer-file-name) (apply #'git-run-command-buffer "*git-diff*" "diff-index" "-p" "-M" "HEAD" "--" file))))
    (cond
     ((not buffer) (error "No files attached to the current buffer"))
     ((> (buffer-size buffer) 0) (git-setup-diff-buffer buffer))
     (t  (message "The current buffer and the HEAD revision are identical")))))

(defun git-log-current-buffer()
  "Display a log of changes to current buffer."
  (interactive)
  (let* ((file-list (list (buffer-file-name)))
	 (coding-system-for-read git-commits-coding-system)
         (buffer (and (buffer-file-name) (apply #'git-run-command-buffer "*git-log*" "rev-list" "--pretty" "HEAD" "--" file-list))))
    (if buffer
	(progn (with-current-buffer buffer
		 (git-log-mode)
		 (goto-char (point-min))
		 (setq buffer-read-only t)
		 (git-log-set-files-list file-list))
	       (display-buffer buffer))
	(error "No files attached to the current buffer"))))

(define-key global-map [(control ?x) (?g) (?=)] 'git-diff-current-buffer)
(define-key global-map [(control ?x) (?g) (?l)] 'git-log-current-buffer)

(provide 'git+)

;;; git+.el ends here
