
(completing-read "Completion of foo:" '(("foobar1" 1)("barfoo" 2) ("foobas" 3)("foobar2" 4)) nil t "foo" )

(move-to-window-line 0)  ;; Move to the first line of the window
(move-to-window-line -1) ;; Move to the last line of the window


;;;;--------------------------------------------------
;;;;    horizontal-to-vertical
;;;;--------------------------------------------------

;; Idea and starter code from Benjamin Rutt (rutt.4+news@osu.edu) on comp.emacs
(defun window-horizontal-to-vertical ()
  "Switches from a horizontal split to a vertical split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
	(buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-horizontally)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

;;;;--------------------------------------------------
;;;;    vertical-to-horizontal
;;;;--------------------------------------------------

;; complement of above created by rgb 11/2004
(defun window-vertical-to-horizontal ()
  "Switches from a vertical split to a horizontal split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
	(buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))


;;;;--------------------------------------------------
;;;;    search-word-at-mouseclick
;;;;--------------------------------------------------


(defun search-word-at-mouseclick (event)
  "Performs a nonincremental-search-forward starting from the beginning of the
   buffer or narrowed region.  The word clicked on is the word to search for.  If
   the click is in another window the search still occurs in the current window."
  (interactive "e")
  (let (searchword)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq searchword (current-word))))
    (if searchword
	(let ((cpt (point)))
	  (goto-char (point-min))
	  (setq menu-bar-last-search-type 'string)
	  (isearch-update-ring searchword nil)
	  (if (string= searchword (car (symbol-value minibuffer-history-variable)))
	      ()
	      (set minibuffer-history-variable
		   (cons searchword (symbol-value minibuffer-history-variable))))
	  (unless (search-forward searchword nil t)
	    (goto-char cpt)
	    (error "Search Failed: \"%s\"" searchword)))
	(ding))))

(global-set-key [mouse-2]  'search-word-at-mouseclick)

;;;;--------------------------------------------------
;;;;    highlight-word-at-mouseclick
;;;;--------------------------------------------------


(defun highlight-word-at-mouseclick (event)
  "Highlight all occurances of the word clicked on. If
   the click is in another window the search still occurs in the current window."
  (interactive "e")
  (let (searchword)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq searchword (current-word))))
    (when searchword
      (sit-for 0)
      (highlight-regexp-clear)
      (let ((highlight-regexp--face-index 1))
	(highlight-regexp-regexp 
	 (concat "\\<"  (regexp-quote searchword) "\\>"))))))

(global-set-key [mouse-2]  'highlight-word-at-mouseclick)