;;; ag-occur.el --- AG Occur functionality for Silver Searcher
;;
;; Author:      Cedric Lallain <kandjar76@hotmail.com>
;; Version:     0.1
;; Keywords:    occur ag
;; Description: Occur Functionality for AG tool (a.k.a Silver Searcher)
;; Tested with: GNU Emacs 23.x and GNU Emacs 24.x
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

;;; Summary:
;;

;;
;; Provide a 'occur' like functionality for silver searcher
;;


;;; Commentary:  **************TODO*********
;;

;; HOW TO USE IT:
;;
;; Call the command `ag-occur'
;; Specify the regular expression to search, and the root folder to start the search.
;;
;;
;; TO INSTALL IT:
;;
;; You need to first install 'ag' the silver searcher.
;; Then put the following lines in your .emacs:
;;
;; (autoload 'ag-occur "ag-occur" "Silver Searcher Regexp." t nil)
;;
;; If 'ag' is not accessible from your 'PATH', you will also need to set the following
;; variable: (setq ag-occur-executable "<path-to-ag>/ag")
;;
;; KEY BINDINGS:
;;
;; <RET> - goto-occurrence
;;  o    - goto-occurrence other window
;;  v    - display occurrence



;; Not yet supported:
;;  n    - next occurrence / prev search occurrence
;;  p    - prev occurrence / next search occurrence
;;  M-n  - go to next file
;;  M-p  - go to prev file
;;  C-n  - go to next occurrence and display it
;;  C-p  - go to the previous occurrence and display it
;;  r    - rename buffer
;;  g    - refresh the research
;;  d    - delete the current line
;;  q    - quit-window
;;  ?    - show brief help


;;; History:
;;
;; v0.1: First un-official release. :P
;;


(require 'cl) ;; for: oddp

;;(require 'project-buffer-mode)



;;; Code:


(defgroup ag-occur nil
  "An occur mode for silver searcher.")



;;
;;  Global configuration variable:
;;


(defcustom ag-occur-executable
  "ag"
  "Name of the ag executable to use."
  :type 'string
  :group 'ag-occur)

;;(defvar ag-occur-context-size 32
;;  "Size of the context stored for each occurrence; to help retrieving the data after modification.")


(defface ag-occur-file-line
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "yellow")))
  "AG occur face used to highlight file line."
  :group 'ag-occur)


(defface ag-occur-line-number
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "cyan")))
  "AG  occur face used to highlight line number."
  :group 'ag-occur)


(defface ag-occur-odd-matching-line
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark)) (:foreground "white")))
  "AG occur face used to highlight odd matching line."
  :group 'ag-occur)


(defface ag-occur-even-matching-line
  '((((class color) (background light)) (:foreground "gray60"))
    (((class color) (background dark)) (:foreground "gray")))
  "AG  occur face used to highlight even matching line."
  :group 'ag-occur)


(defface ag-occur-highlight-matching-string
  '((((class color) (background light)) (:background "yellow"))
    (((class color) (background dark)) (:background "yellow")))
  "AG occur face used to highlight the matching string."
  :group 'ag-occur)

(defface ag-occur-error
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "red")))
  "AG  occur face used to highlight line number."
  :group 'ag-occur)


(defcustom ag-occur-mode-hook nil
  "Post `ag-occur-mode' initialization hook."
  :type 'hook
  :group 'ag-occur)



;;
;;  Global variables:
;;

(defvar ag-occur-regexp-history nil
  "History list of regular expressions used in AG commands.")


(defvar ag-occur-saved-root-folder nil
  "Stored root folder for later search")


;;
;;  Local variables:
;;


(defvar ag-occur-process nil)
(defvar ag-occur-process-output nil)
(defvar ag-occur-process-current-file nil)
(defvar ag-occur-process-count-for-current-file 0)
(defvar ag-occur-process-error-occurred nil)
(defvar ag-occur-process-cmd-line nil)

(defvar ag-occur-saved-regexp nil) ; (list regexp root-folder)




;;
;;  Key Bindings:
;;


;; Define the key mapping for the spu mode:
(defvar ag-occur-map
  (let ((ag-occur-map (make-keymap)))
    (define-key ag-occur-map [return] 'ag-occur-goto-occurrence)
    (define-key ag-occur-map [?o] 'ag-occur-goto-occurrence-other-window)
    (define-key ag-occur-map [?v] 'ag-occur-view-occurrence)
;;    (define-key ag-occur-map [?n] 'ag-occur-next-occurrence)
;;    (define-key ag-occur-map [?p] 'ag-occur-previous-occurrence)
;;    (define-key ag-occur-map [(meta ?n)] 'ag-occur-next-file)
;;    (define-key ag-occur-map [(meta ?p)] 'ag-occur-previous-file)
;;    (define-key ag-occur-map [(control ?n)] 'ag-occur-view-next-occurrence)
;;    (define-key ag-occur-map [(control ?p)] 'ag-occur-view-previous-occurrence)
;;    (define-key ag-occur-map [?d] 'ag-occur-delete-line)
;;    (define-key ag-occur-map [?q] 'quit-window)
;;    (define-key ag-occur-map [?r] 'ag-occur-rename-buffer)
;;    (define-key ag-occur-map [?g] 'ag-occur-refresh)
;;    (define-key ag-occur-map [??] 'ag-occur-help)
    (define-key ag-occur-map [mouse-2] 'ag-occur-mouse-find-file)
    ag-occur-map))


;;
;;  Functions:
;;


(defun ag-occur-clear-overlays()
  "Clear the ag-occur overlays from the current buffer."
  (let ((ovl-lists (overlay-lists)))
    (mapcar (lambda (overlay)
	      (when (overlay-get overlay 'ag-occur-tag)
		(delete-overlay overlay)))
	    (and ovl-lists
		 (append (car ovl-lists) (cdr ovl-lists))))))


(defun ag-occur-get-and-clear-occur-buffer()
  "Retrieve the occur buffer and returns it.
If the buffer exists; the buffer is cleared.  If the buffer
doesn't exist, a new buffer is created and initialized with
ag-occur-major-mode."
  (let ((buffer (get-buffer-create "*Ag-Occur*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(ag-occur-clear-overlays)
	(erase-buffer))
      (ag-occur-mode))
    buffer))


;;(defun ag-occur-add-occurrence(file occurrence occurrence-num regexp)
;;  "Add an OCCURRENCE from FILE in the buffer.
;;
;;FILE should be the file in which the occurrence has been found,
;;OCCURRENCE is a list of (#line matching-line before-string after-string).
;;OCCURRENCE-NUM represents the OCCURRENCE-NUM'th occurrence found in FILE"
;;
;;  (let ((occ-line-num   (car occurrence))
;;	(occ-line-str   (nth 1 occurrence))
;;	(occ-before-str (nth 2 occurrence))
;;	(occ-after-str  (nth 3 occurrence))
;;	(start-pos      (point))
;;	(cur-line       (line-number-at-pos (point))))
;;    (insert (propertize (format "%8i:" occ-line-num)
;;			'follow-link t
;;			'mouse-face 'highlight
;;			'face 'ag-occur-line-number))
;;    (insert " ")
;;    (insert (propertize occ-line-str
;;			'follow-link t
;;			'mouse-face 'highlight
;;			'face (if (oddp occurrence-num)
;;				  'ag-occur-odd-matching-line
;;				  'ag-occur-even-matching-line)))
;;    (when (not (= (point) (point-at-bol)))
;;      (insert "\n"))
;;
;;    ;; Highlight matching string:
;;    (goto-char start-pos)
;;    (forward-char 10) ; skip the line number
;;    (while (re-search-forward regexp nil t)
;;      (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
;;	(overlay-put overlay 'face 'ag-occur-highlight-matching-string)
;;	(overlay-put overlay 'ag-occur-tag t)
;;	))
;;
;;    ;; Fix the indentation:
;;    (goto-char (point-max))
;;    (forward-line -1)
;;    (while (not (= cur-line (line-number-at-pos (point))))
;;      (goto-char (point-at-bol))
;;      (insert (propertize (make-string 10 32)
;;			  'follow-link t
;;			  'mouse-face 'highlight))
;;      (forward-line -1))
;;
;;    ;; Data overlay:
;;    (let ((overlay (make-overlay start-pos (point-max))))
;;      (overlay-put overlay 'ag-occur-tag t)
;;      (overlay-put overlay 'ag-occur-context (list file occurrence regexp)))
;;    (goto-char (point-max))))
;;
;;
;;(defun ag-occur-collect-occurrences(regexp)
;;  "Create a list of occurrences by searching REGEXP in the current buffer.
;;
;;The return value is a list of ( line# matching-line before-string
;;after-string ).  This function doesn't save the position and
;;assume the position will be saved and restored by the caller if
;;required."
;;  (goto-char (point-min))
;;  (save-match-data
;;    (let (occurrences
;;	  next-start
;;	  occ-beg
;;	  occ-end
;;	  occ-bol
;;	  occ-eol
;;	  occ-line-num
;;	  occ-line-str
;;	  occ-before-str
;;	  occ-after-str)
;;      (setq next-start (re-search-forward regexp nil t))
;;      (while next-start
;;	;; Collect the data for this occcurrence:
;;	;;  consider using: jit-lock-fontify-now! To get colors on the line...
;;	(setq occ-beg (match-beginning 0))
;;	(setq occ-end (match-end 0))
;;	(goto-char occ-beg)
;;	(setq occ-bol (point-at-bol))
;;	(setq occ-line-num (line-number-at-pos))
;;	(goto-char occ-end)
;;	(setq occ-eol (point-at-eol))
;;	(setq occ-line-str (buffer-substring-no-properties occ-bol occ-eol))
;;	(setq occ-after-str (and (>= (- (point-max) occ-eol) ag-occur-context-size)
;;				 (buffer-substring-no-properties occ-eol (+ occ-eol ag-occur-context-size))))
;;	(setq occ-before-str (and (>= (- occ-bol (point-min)) ag-occur-context-size)
;;				  (buffer-substring-no-properties (- occ-bol ag-occur-context-size) occ-bol)))
;;	;; Add the occurrence to the list unless it occurs on the same line.
;;	(unless (eq occ-line-num (car (car occurrences)))
;;	  (setq occurrences (cons (list occ-line-num occ-line-str occ-before-str occ-after-str)
;;				  occurrences)))
;;	;; Carry on:
;;	(goto-char next-start)
;;	(setq next-start (re-search-forward regexp nil t)))
;;      (reverse occurrences))))
;;
;;
;;(defun ag-occur-research(project-file-name file-path project-name regexp occur-buffer)
;;  "Research REGEXP in FILE-PATH and fill OCCUR-BUFFER with the
;;different occurrences found.
;;PROJECT-FILE-NAME and PROJECT-NAME are ignored."
;;  (let (occurrences)
;;    (message "Project '%s' -- Searching in '%s'" project-name file-path)
;;    ;; Collect all occurrences in this file:
;;    (let ((file-buf (get-file-buffer file-path)))
;;      (if file-buf
;;	  (with-current-buffer file-buf
;;	    (save-excursion
;;	      (setq occurrences (ag-occur-collect-occurrences regexp))))
;;	  (when (file-exists-p file-path)
;;	    (with-temp-buffer
;;	      (insert-file-contents file-path)
;;	      (setq occurrences (ag-occur-collect-occurrences regexp))))))
;;
;;    ;; Then populate the occurr buffer with it:
;;    (when occurrences
;;      (with-current-buffer occur-buffer
;;	(let ((inhibit-read-only t))
;;	  (goto-char (point-max))
;;	  (let ((start-pos (point)))
;;		(insert (propertize (format "%i occurrence%s found in %s/%s"
;;					    (length occurrences)
;;					    (if (= 1 (length occurrences)) "" "s")
;;					    project-name
;;					    project-file-name)
;;				    'follow-link t
;;				    'mouse-face 'highlight
;;				    'face 'ag-occur-file-line))
;;		(let ((overlay (make-overlay start-pos (point))))
;;		  (overlay-put overlay 'ag-occur-tag t)
;;		  (overlay-put overlay 'ag-occur-context (list file-path nil regexp))))
;;	  (insert "\n")
;;	  (let ((occ-count 1))
;;	    (while occurrences
;;	      (let ((occurrence (pop occurrences)))
;;		(ag-occur-add-occurrence file-path occurrence occ-count regexp)
;;		(setq occ-count (1+ occ-count))))))))))
;;

(defun ag-occur-mode()
  "Major mode for output from `ag-occur'

Commands:
\\{ag-occur-map}"
  (kill-all-local-variables)
  (use-local-map ag-occur-map)
  ;;
  (setq major-mode 'ag-occur-mode)
  (setq mode-name "ag-occur")
  ;;
  (make-local-variable 'ag-occur-process)
  (make-local-variable 'ag-occur-process-output)
  (make-local-variable 'ag-occur-process-current-file)
  (make-local-variable 'ag-occur-process-count-for-current-file)
  (make-local-variable 'ag-occur-process-error-occurred)
  (make-local-variable 'ag-occur-process-cmd-line)

  (make-local-variable 'ag-occur-saved-regexp)
  ;;
  (setq buffer-read-only t)
  (setq buffer-undo-list t) ; disable undo recording
  (run-mode-hooks 'ag-occur-mode-hook))


(defun ag-occur-goto-file(file &optional other-window)
  "Go to the selected files."
  (if other-window
      (find-file-other-window file)
      (find-file file)))


(defun ag-occur-highlight-current(regexp start end)
  "Highlight all occurrences of REGEXP between START adn END points"
  (let (ovl-list)
    (unwind-protect
	(save-match-data
	  (save-excursion
	    (goto-char start)
	    (while (re-search-forward regexp end t)
	      (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
		(overlay-put overlay 'face 'ag-occur-highlight-matching-string)
		(overlay-put overlay 'ag-occur-tag t)
		(setq ovl-list (cons overlay ovl-list)))))
	  (sit-for 10))
      (mapcar (lambda (overlay)
		(delete-overlay overlay))
	      ovl-list))))


(defun ag-occur-goto-matching-string(file line matching-line before-string after-string regexp &optional other-window)
  "Go to an occurrence."
  (let* ((buffer (find-file-noselect file))
	(window (get-buffer-window buffer)))
    (if window
	(progn (select-window window)
       (set-buffer buffer))
	(if other-window
	    (switch-to-buffer-other-window buffer)
	    (switch-to-buffer buffer)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line)))

;;    ;;
    (let ((cur-pt (point))
	  (end-pt (+ (point) (length matching-line) 1))
	  aft-pt
	  bef-pt
	  found)
      (when (and after-string (search-forward after-string nil t))
	(setq aft-pt (match-beginning 0))
	(goto-char aft-pt))
      (when (and before-string (search-backward before-string nil t))
	(setq bef-pt (match-end 0))
	(goto-char bef-pt))

      (cond
       ((and aft-pt bef-pt)
	(if (search-forward matching-line aft-pt t)
	    (progn (goto-char (match-beginning 0))
		   (goto-char (point-at-bol)))
	    (if (re-search-forward regexp aft-pt t)
		(progn (goto-char (match-beginning 0))
		       (goto-char (point-at-bol)))
		(goto-char cur-pt))))
       (aft-pt
	(goto-char cur-pt)
	(if (search-forward matching-line aft-pt t)
	    (progn (goto-char (match-beginning 0))
		   (goto-char (point-at-bol)))
	    (if (re-search-forward regexp aft-pt t)
		(progn (goto-char (match-beginning 0))
		       (goto-char (point-at-bol)))
		(goto-char cur-pt))))
       (bef-pt
	(if (search-forward matching-line end-pt t)
	    (progn (goto-char (match-beginning 0))
		   (goto-char (point-at-bol)))
	    (if (re-search-forward regexp end-pt t)
		(progn (goto-char (match-beginning 0))
		       (goto-char (point-at-bol)))
		(goto-char cur-pt))))
       (t
	(goto-char cur-pt)
	(if (search-forward matching-line end-pt t)
	    (progn (goto-char (match-beginning 0))
		   (goto-char (point-at-bol)))
	    (if (re-search-forward regexp end-pt t)
		(progn (goto-char (match-beginning 0))
		       (goto-char (point-at-bol)))
		(if (search-forward matching-line nil t)
		    (progn (goto-char (match-beginning 0))
			   (goto-char (point-at-bol)))
		    (if (search-backward matching-line nil t)
			(progn (goto-char (match-beginning 0))
			       (goto-char (point-at-bol)))
			(goto-char cur-pt))))))))))


(defun ag-occur-goto-occurrence-at-pos(pos other-window)
  "Go to the occurrence found at POS."
  (let (context)
    ;; Check if there is a context at that line:
    (mapcar (lambda (overlay) (when (overlay-get overlay 'ag-occur-context)
				(setq context (overlay-get overlay 'ag-occur-context))))
	    (overlays-at pos))
    (unless context
      (error "No occurrence on this line"))
    (let ((file-name (car context))
	  (line-data (nth 1 context)))
      (if context
	  (let ((line-num (car line-data))
		(line-str (nth 1 line-data))
		(regexp (nth 2 line-data)))
	    (ag-occur-goto-matching-string file-name line-num line-str nil nil regexp other-window)
	    (ag-occur-highlight-current regexp (point) (+ (point) (length (nth 1 line-data))))
	    )
	  (ag-occur-goto-file file-name other-window)))))


;;(defun ag-occur-delete-line()
;;  "Delete the current occurrence line from this buffer."
;;  (interactive)
;;  (goto-char (point-at-bol))
;;  (let ((start (point))
;;	end
;;	(inhibit-read-only t))
;;    (if (looking-at "^[0-9]+ occurrence")
;;	(progn (ag-occur-next-file)
;;	       (setq end (if (eq start (point)) (point-max) (point)))
;;	       (delete-region start end))
;;	(progn (forward-line 1)
;;	       (setq end (point))
;;	       (save-excursion
;;		 (save-match-data
;;		   (ag-occur-previous-file)
;;		   (looking-at "^[0-9]+")
;;		   (let ((num (string-to-int (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))
;;		     (if (= num 1)
;;			 (setq start (point))
;;			 (progn (delete-region (match-beginning 0) (match-end 0))
;;				(insert (propertize (format "%i" (1- num))
;;						    'follow-link t
;;						    'mouse-face 'highlight
;;						    'face 'ag-occur-file-line)))))))
;;	       (delete-region start end)))))
;;

;;
;;  Interactive commands:
;;


(defun ag-occur-mouse-find-file(event)
  "Goto the selected occurrence."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-end event))))
  (ag-occur-goto-occurrence-at-pos (posn-point (event-end event)) t))


(defun ag-occur-goto-occurrence()
  "Goto the selected occurrence."
  (interactive)
  (ag-occur-goto-occurrence-at-pos (point) nil))


(defun ag-occur-goto-occurrence-other-window()
  "Goto the selected occurrence in another window."
  (interactive)
  (ag-occur-goto-occurrence-at-pos (point) t))


(defun ag-occur-view-occurrence()
  "View the selected occurrence without leaving the project-buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (ag-occur-goto-occurrence-at-pos (point) t)
    (let ((window (get-buffer-window buffer)))
      (when window
	(select-window window)))))


;;(defun ag-occur-next-occurrence()
;;  "Go to the next occurrence."
;;  (interactive)
;;  (forward-line 1)
;;  (goto-char (point-at-bol))
;;  (while (and (not (eobp))
;;	      (looking-at "^[0-9]+ occurrence"))
;;    (forward-line 1)))
;;
;;
;;(defun ag-occur-previous-occurrence()
;;  "Go to the next occurrence."
;;  (interactive)
;;  (forward-line -1)
;;  (goto-char (point-at-bol))
;;  (while (and (not (bobp))
;;	      (looking-at "^[0-9]+ occurrence"))
;;    (forward-line -1))
;;  (if (and (bobp)
;;	   (looking-at "^[0-9]+ occurrence"))
;;      (ag-occur-next)))
;;
;;
;;(defun ag-occur-next-file()
;;  "Go to the next file."
;;  (interactive)
;;  (let ((current (point)))
;;    (forward-line 1)
;;    (goto-char (point-at-bol))
;;    (while (and (not (eobp))
;;		(not (looking-at "^[0-9]+ occurrence")))
;;      (forward-line 1))
;;    (unless (looking-at "^[0-9]+ occurrence")
;;      (goto-char current))))
;;
;;
;;(defun ag-occur-previous-file()
;;  "Go to the next file."
;;  (interactive)
;;  (let ((current (point)))
;;    (forward-line -1)
;;    (goto-char (point-at-bol))
;;    (while (and (not (eobp))
;;		(not (looking-at "^[0-9]+ occurrence")))
;;      (forward-line -1))
;;    (unless (looking-at "^[0-9]+ occurrence")
;;      (goto-char current))))
;;
;;
;;(defun ag-occur-view-next-occurrence()
;;  "Go to the next occurrence and view it."
;;  (interactive)
;;  (ag-occur-next-occurrence)
;;  (ag-occur-view-occurrence))
;;
;;
;;(defun ag-occur-view-previous-occurrence()
;;  "Go to the next occurrence."
;;  (interactive)
;;  (ag-occur-previous-occurrence)
;;  (ag-occur-view-occurrence))
;;
;;
;;(defun ag-occur-help ()
;;  "Display help for `ag-occur' mode."
;;  (interactive)
;;  (describe-function 'ag-occur-mode))
;;
;;
;;(defun ag-occur-rename-buffer()
;;  "Rename the buffer; make its name uniq."
;;  (interactive)
;;  (let ((new-name (format "*Ag-Occur:%s*" ag-occur-saved-project-buffer)))
;;    (rename-buffer new-name t)))
;;

;;(defun ag-occur-refresh()
;;  "Refresh the buffer."
;;  (interactive)
;;  (let ((inhibit-read-only t))
;;    (ag-occur-clear-overlays)
;;    (erase-buffer)
;;    (let ((regexp       (nth 0 ag-occur-saved-regexp))
;;	  (all-files    (nth 1 ag-occur-saved-regexp)) -- root folder
;;	  (project      (nth 2 ag-occur-saved-regexp))
;;	  (occur-buffer (current-buffer)))
;;      ;; Fill the occur buffer with all occurrences:
;;      (save-excursion
;;	(set-buffer ag-occur-saved-project-buffer)
;;	(if all-files
;;	    (project-buffer-apply-to-each-file 'ag-occur-research regexp occur-buffer)
;;	    (unless (project-buffer-apply-to-marked-files 'ag-occur-research regexp occur-buffer)
;;	      (project-buffer-apply-to-project-files project 'ag-occur-research regexp occur-buffer)))))))
;;



;;
;; Utility
;;

(defun ag-occur-read-regexp(prompt)
  "Read a regular expression from the minibuffer."
  (read-from-minibuffer prompt nil nil nil 'ag-occur-regexp-history))

(defun ag-occur-read-root-folder(prompt)
  "Read a regular expression from the minibuffer."
  (read-directory-name prompt nil nil t))


;;
;; Process management:
;;


(defun ag-occur-process-filter-full-line(process line)
  "Analyze the full line of output."
  (let ((start-pos (point))
	(regexp (car ag-occur-saved-regexp)))
    (cond
     ((string-match "\\:\\([0-9]+\\)\\:" line) ;; Parsing matching string:
      (let ((file-name (substring line 0 (1- (match-beginning 1))))
	    (line-num  (string-to-int (substring line (match-beginning 1) (match-end 1))))
	    (line-str  (substring line (1+ (match-end 1) ))))
	;;(insert (concat file-name "(" line-num ") : " line-str))))
	(when (not (string-equal ag-occur-process-current-file file-name))
	  (progn ;; New file name:
	    (when ag-occur-process-current-file (insert "\n")) ;; Not the first one -> insert a blank line
	    (setq ag-occur-process-current-file file-name)
	    (insert (propertize (concat ag-occur-process-current-file ":")
				'follow-link t
				'mouse-face 'highlight
				'face 'ag-occur-file-line))
	    (let ((overlay (make-overlay start-pos (point))))
	      (overlay-put overlay 'ag-occur-tag t)
	      (overlay-put overlay 'ag-occur-context (list ag-occur-process-current-file nil)))
	    (setq ag-occur-process-count-for-current-file 0)
	    (insert "\n")))
	;; Print the occurrence match:
	;;(setq start-pos (point))
	(setq ag-occur-process-count-for-current-file (1+ ag-occur-process-count-for-current-file))
	(insert (propertize (format "%8i:" line-num)
			    'follow-link t
			    'mouse-face 'highlight
			    'face 'ag-occur-line-number))
	(insert " ")
	(while (or (string= (substring line-str (1- (length line-str))) "\r")
	   (string= (substring line-str (1- (length line-str))) "\n"))
	  (setq line-str (substring line-str 0 (1- (length line-str)))))
	(insert (propertize line-str
			    'follow-link t
			    'mouse-face 'highlight
			    'face (if (oddp ag-occur-process-count-for-current-file)
				      'ag-occur-odd-matching-line
				      'ag-occur-even-matching-line)))
         ;; Highlight matching string:
	(let ((end-pos (point)))
	  (goto-char start-pos)
	  (forward-char 10) ; skip the line number
	  (while (re-search-forward regexp end-pos t)
	    (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
	      (overlay-put overlay 'ag-occur-tag t)
	      (overlay-put overlay 'face 'ag-occur-highlight-matching-string)
	      ))
	  (goto-char end-pos)
	  (let ((overlay (make-overlay start-pos end-pos)))
	    (overlay-put overlay 'ag-occur-tag t)
	    (overlay-put overlay 'ag-occur-context (list ag-occur-process-current-file (list line-num line-str regexp))))
	  (insert "\n"))))

     ((string-match "^ERR:\\(.*\\)\n$" line) ;; Error occurred/
      (progn (insert (propertize "ERR:"
				 'face 'ag-occur-error))
	     (insert (substring line (match-beginning 1) (match-end 1)))
	     (insert "\n")
	     (setq ag-occur-process-error-occurred t)))
     (t
      (progn
	(if (not (string-match "^\n$" line)) (message "** AG Warning: Expected empty line, found: %s" line))
	(insert line)
	(setq ag-occur-process-current-file nil)
	(setq ag-occur-process-count-for-current-file 0)
	)))))


(defun ag-occur-process-filter (process output)
  "Accept silver search process output and display it to the occur buffer."
  (let ( (old-buffer (current-buffer)) )
    (unwind-protect
	(with-current-buffer (process-buffer process)
	  (let ((inhibit-read-only t)
		(moving (= (point) (process-mark process)))
		line)
	    (save-excursion
	      (goto-char (process-mark process))
	      ;; Let's extract the output line by line:
	      (setq ag-occur-process-output (concat ag-occur-process-output output))
	      (while (and ag-occur-process-output
			  (string-match "\\([^\n]+\n\\)\\(\\(.\\|\n\\)*\\)"
					ag-occur-process-output))
		;; Get a line and adjust the remaining text:
		(setq line                    (substring ag-occur-process-output (match-beginning 1) (match-end 1)))
		(setq ag-occur-process-output (substring ag-occur-process-output (match-beginning 2) (match-end 2)))
		(when (= (length ag-occur-process-output) 0) (setq ag-occur-process-output nil)) ;; Make sure we stop...

		;; Analyse and display the full line:
		(ag-occur-process-filter-full-line process line))
	      (set-marker (process-mark process) (point))))
	  (set-buffer-modified-p nil))
      (set-buffer old-buffer))))


(defun ag-occur-process-sentinel (process event)
  "Sentinel for when the AG process."
  (save-window-excursion
    (save-excursion
      (with-current-buffer (process-buffer process)
	(let ((inhibit-read-only t))
	  (goto-char (point-max))
	  (insert "\n\n")
	  (insert "--------------------------------------------------------------------------------\n")
	  (if ag-occur-process-error-occurred
	      (insert (format "Some Errors occurred with the cmd: %s\n" ag-occur-process-cmd-line))
	      (insert "Search complete.\n")
	      (delete-process process)
	      (setq ag-occur-process nil)
	      (set-buffer-modified-p nil)))))))

(defun ag-occur-call(buffer regexp root-folder)
  "Execute silver search command.
Search: REGEXP from ROOT-FOLDER and display the result in BUFFER."
  (let* ((dir (file-name-as-directory (expand-file-name root-folder)))
	 (options (list "--nocolor"  ; Disable auto-color
			"--search-files"
			regexp
			dir)))
    (with-current-buffer buffer
	(if ag-occur-process (error "An ag search is still in progress -- only one at a time is allowed"))

	;; Start a new search process:
	(setq ag-occur-process-error-occurred nil)
	(setq ag-occur-process-current-file nil)
	(setq ag-occur-process-cmd-line (concat ag-occur-executable " "(mapconcat (lambda (elt) elt) options " ")))

	(setq ag-occur-process
	      (apply 'start-process "Silver Searcher" buffer
		     ag-occur-executable options))
	(set-process-filter   ag-occur-process 'ag-occur-process-filter)
	(set-process-sentinel ag-occur-process 'ag-occur-process-sentinel)
	(process-kill-without-query ag-occur-process) ;; In case the user quit emacs, kill the process silently.
	)))


;;
;;  Entry command:
;;



(defun ag-occur(regexp root-folder)
  "Search REGEXP in the project files; if ALL-FILES is t the
research will occur in all project's files; if ALL-FILES is
false, the research will occur in all marked files unless there
are none in which case it will occur in all files of the current
project (current project is determined by the cursor position)."
  (interactive
   (list (ag-occur-read-regexp (format "Silver Searcher -- List lines matching regexp%s: "
				       (if (and (not current-prefix-arg) ag-occur-saved-root-folder)
					   (format " [from: %s]" ag-occur-saved-root-folder) "")))
	 (if (or current-prefix-arg (not ag-occur-saved-root-folder))
	     (ag-occur-read-root-folder "Search Root Folder:")
	     ag-occur-saved-root-folder)))
  (unless (and regexp (not (string-equal regexp "")))
    (error "Invalid regexp"))

  ;; Generate an occur buffer:
  (let ((occur-buffer (ag-occur-get-and-clear-occur-buffer))
	(base-directory default-directory))
      ;; Set the local variable:
      (with-current-buffer occur-buffer
	(cd base-directory)
	(setq ag-occur-saved-regexp (list regexp root-folder)))
      ;; Fill the occur buffer with all occurrences:
      (ag-occur-call occur-buffer regexp root-folder)
      (with-current-buffer occur-buffer
	(goto-char (point-min)))
      (display-buffer occur-buffer)
      ))


;;(defun ag-occur-case-sensitive(regexp all-files)
;;  "Search REGEXP in the project files; if ALL-FILES is t the
;;research will occur in all project's files; if ALL-FILES is
;;false, the research will occur in all marked files unless there
;;are none in which case it will occur in all files of the current
;;project (current project is determined by the cursor position).
;;
;;The search is case sensitive."
;;  (interactive
;;   (list (project-buffer-read-regexp (format "List lines matching regexp%s: " (if current-prefix-arg " [all files / case-sensitive]" " [case sensitive]")))
;;	 current-prefix-arg))
;;  (unless project-buffer-status (error "Not in project-buffer buffer"))
;;  (unless (and regexp (not (string-equal regexp "")))
;;    (error "Invalid regexp"))
;;  (let ((case-fold-search nil))
;;    (ag-occur regexp all-files)))


;;

(provide 'ag-occur)

;;; ag-occur.el ends here
