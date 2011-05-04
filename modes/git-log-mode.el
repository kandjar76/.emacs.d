;;; git-log.el --- A major mode for git log

;; Copyright (C) 2008 Cedric Lallain <kandjar76@hotmail.com>

;; Version: 1.1

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

;; 1.0: Initial version
;; 1.1: Check doc run


;;; Code:
(eval-when-compile (require 'cl))
(require 'git)

;;;; Customizations
;;;; ------------------------------------------------------------

(setq git-log-files-list nil)

(make-face 'git-log-commit-line-face)
(set-face-foreground 'git-log-commit-line-face "blue")
(set-face-background 'git-log-commit-line-face "gray")
(defvar git-log-commit-line-face 'git-log-commit-line-face
  "Font to highlight the commit header line of the git log.")

(make-face 'git-log-selected-commit-line-face)
(set-face-foreground 'git-log-selected-commit-line-face "red")
(set-face-background 'git-log-selected-commit-line-face "gray")
(defvar git-log-selected-commit-line-face 'git-log-selected-commit-line-face
  "Font to highlight the commit header line of the git log.")

(setq git-log-header-color '(foreground-color . "red"))


;;;;; GIT LOG Major-Mode definition:
;;;;; ------------------------------------------------------------


(defun git-log-next-commit-block()
  "Go to the next commit block"
  (interactive)
  (end-of-line)
  (when (not (search-forward-regexp "^commit" nil t))
    (goto-char (point-max)))
  (beginning-of-line))

(defun git-log-previous-commit-block()
  "Go to the previous commit block"
  (interactive)
  (search-backward-regexp "^commit" nil t))

(defun git-log-unmark-all()
  "Unmark all selected commit"
  (interactive)
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^commit\\*" nil t)
	(delete-char -1)))))

(defun git-log-mark-commit()
  "Mark/unmark the current commit
 (the function will also automatically unmark the previously marked commit)"
  (interactive)
  (let ((buffer-read-only nil))
    (save-excursion
      (if (not (looking-at "^commit"))
	  (git-log-previous-commit-block))
      (if (looking-at "^commit\\*")
	  (progn (forward-char 6)
		 (delete-char 1))
	  (progn (git-log-unmark-all)
		  (forward-char 6)
		 (insert "*"))))))

(defun git-log-get-current-commit-rev()
  "Retrieve the rev# of the current commit"
  (save-excursion
    (if (not (looking-at "^commit"))
	(git-log-previous-commit-block))
    (end-of-line)
    (current-word)))

(defun git-log-get-marked-commit-rev()
  "Retrive the rev# of the marked commit / nil if no commit are marked"
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "^commit\\*" nil t)
	(git-log-get-current-commit-rev))))

(defun git-log-diff-commit()
  (interactive)
  (let ((cur-rev    (git-log-get-current-commit-rev))
	(marked-rev (git-log-get-marked-commit-rev))
	(coding-system-for-read git-commits-coding-system))
    (git-setup-diff-buffer
     (if marked-rev
	 (apply #'git-run-command-buffer "*git-diff*" "diff" "-p" "-M" cur-rev marked-rev git-log-files-list)
	 (apply #'git-run-command-buffer "*git-diff*" "diff-index" "-p" "-M" cur-rev git-log-files-list)
	 ))
    ))

(defun git-log-view-current()
  (interactive)
  (let ((cur-rev    (git-log-get-current-commit-rev))
	(coding-system-for-read git-commits-coding-system))
    (git-setup-diff-buffer
     (apply #'git-run-command-buffer "*git-diff*" "log" "-p" "-M" "-1" cur-rev git-log-files-list)
     )
  ))

(defun git-log-view-file()
  (interactive)
  (unless (and git-log-files-list 
	       (not (cdr git-log-files-list)))
    (error "View File Error: Multiple files log not supported."))
  (let* ((cur-rev    (git-log-get-current-commit-rev))
	 (coding-system-for-read git-commits-coding-system)
	 (file-name  (car git-log-files-list))
	 (git-tree-name (car (split-string (git-call-process-env-string nil "ls-tree" "HEAD" file-name) "\n")))
	 (git-file-name (cadr (split-string git-tree-name "\t")))
	 (log-name   (concat "*" (file-name-nondirectory file-name) "@" (substring cur-rev 0 6) "*"))
	 (buffer     (git-run-command-buffer log-name "show" (concat cur-rev ":" git-file-name))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (let ((buffer-file-name file-name))
	(set-auto-mode)))
    (view-buffer buffer 'kill-buffer-if-not-modified)))

(defun git-log-swap-current-marked-commit()
  (interactive)
  (let (current marked)
    (save-excursion
      (when (not (looking-at "^commit"))
	(git-log-previous-commit-block))
      (setq current (point))
      (goto-char (point-min))
      (when (search-forward-regexp "^commit\\*" nil t)
	(setq marked (point-at-bol))))
    (when (and marked (/= marked current))
      (goto-char marked)
      (save-excursion
	(goto-char current)
	(git-log-mark-commit)))
      ))

(defun git-log-set-files-list(files)
  "Set the list of files attached to the log window"
  (setq header-line-format (concat (propertize " " 'display '(space :align-to 0))
				   (propertize "Git log: " 'face git-log-header-color)
				   (reduce (lambda(a b) (concat a " " b)) files)))
  (setq git-log-files-list files))

;; User hook...
(defvar git-log-mode-hook nil)

(defconst git-log-font-lock-keywords-1
  (list '("^commit\\*.*$" . git-log-selected-commit-line-face)
	'("^commit.*$" . git-log-commit-line-face)
	)
    "Additional expressions to highlight in git-log mode.")

;; Define the key mapping for the git log mode:
(defvar git-log-mode-map
  (let ((git-log-mode-map (make-keymap)))
    (define-key git-log-mode-map [(?n)]    'git-log-next-commit-block)
    (define-key git-log-mode-map [(?p)]    'git-log-previous-commit-block)
    (define-key git-log-mode-map [(?m)]    'git-log-mark-commit)
    (define-key git-log-mode-map [(?=)]    'git-log-diff-commit)
    (define-key git-log-mode-map [(?q)]    'bury-buffer)
    (define-key git-log-mode-map [(?u)]    'git-log-unmark-all)
    (define-key git-log-mode-map [(?r)]    'git-log-swap-current-marked-commit)
    (define-key git-log-mode-map [(?\ )]   'git-log-next-commit-block)
    (define-key git-log-mode-map [(?v)]    'git-log-view-current)
    (define-key git-log-mode-map [(?V)]    'git-log-view-file)
    git-log-mode-map))


(defvar git-log-font-lock-keywords git-log-font-lock-keywords-1)


;; git-log-mode entry function:
(defun git-log-mode ()
  "Major mode for viewing git logs."
  (interactive)
  (setq buffer-read-only t)
  (kill-all-local-variables)
  (use-local-map git-log-mode-map)
  (make-local-variable 'tab-width)
  (make-local-variable 'git-log-files-list)
  (setq git-log-files-list nil)
  (set (make-local-variable 'font-lock-defaults) '(git-log-font-lock-keywords nil t))
  (setq tab-width 4)
  (setq major-mode 'git-log-mode)
  (setq mode-name "Git Log")
  (setq header-line-format (concat (propertize " " 'display '(space :align-to 0))
				   (propertize "Git log: " 'face git-log-header-color)
				   "<current branch>"))
  (run-hooks 'git-log-mode-hook))


(provide 'git-log-mode)

;;; git-log-mode.el ends here
