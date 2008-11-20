
;; Highlight current-word

(require 'highlight-regexp)
(setq highlight-regexp-faces '(secondary-selection
			       highlight-selected-word-face))


(defun highlight-selected-word(word index)
  "Highlight the current word"
  (let ((highlight-regexp--face-index index))
    (highlight-regexp-string word)))

(defun dehighlight-current-word()
  "Hook -- Cancel the previous current-word highlighted"
  (remove-hook 'pre-command-hook 'dehighlight-current-word)
  (sit-for 0)
  (highlight-regexp-clear)
  (pending-delete-mode 1)) ;; For unknown reason, pending-mode is corrupted by this function... linked to the remove-hook call

(setq quick-search-current-text nil)
(defun quick-search-text()
  "Extract the text to do the quick search: it's either the current if nothing is selected of the selected text"
  (if mark-active
      (prog1 (buffer-substring (region-beginning) (region-end))
	     (deactivate-mark))
      (current-word)))

(defun quick-search-forward()
  "Do a forward search using the word below the cursor. Store the current searched word in a global variable to allow a repeated search using the function repeat-search-current-word-forward or repeat-search-current-word-backward." 
  (interactive)
  (setq quick-search-current-text (quick-search-text))
  (search-forward quick-search-current-text)
  (highlight-selected-word quick-search-current-text 0)
  (add-hook 'pre-command-hook 'dehighlight-current-word))

(defun quick-search-backward()
  "Do a forward search using the word below the cursor. Store the current searched word in a global variable to allow a repeated search using the function repeat-search-current-word-forward or repeat-search-current-word-backward." 
  (interactive)
  (setq quick-search-current-text (quick-search-text))
  (search-backward quick-search-current-text)
  (highlight-selected-word quick-search-current-text 0)
  (add-hook 'pre-command-hook 'dehighlight-current-word))

(defun repeat-quick-search-forward()
  "Repeat a forward research using the stored current word (cf: search-current-word-forward / search-current-word-backword)"
  (interactive)
  (when quick-search-current-text
    (search-forward quick-search-current-text)
    (highlight-selected-word quick-search-current-text 0)
    (add-hook 'pre-command-hook 'dehighlight-current-word)))

(defun repeat-quick-search-backward()
  "Repeat a forward research using the stored current word (cf: search-current-word-forward / search-current-word-backword)"
  (interactive)
  (when quick-search-current-text
    (search-backward quick-search-current-text)
    (highlight-selected-word quick-search-current-text 0)
    (add-hook 'pre-command-hook 'dehighlight-current-word)))

(provide 'quick-search)