set-process-coding-system
cache-long-line-scans

// Removing the \r at display time
(setq buffer-display-table (make-display-table))
(aset buffer-display-table ?\r [])


http://www.google.com



(setq button-url-regexp
      "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")
(bounds-of-thing-at-point 


;; Including load-path subdirectories:
 (setq load-path (cons my-lisp-dir load-path))
 (normal-top-level-add-subdirs-to-load-path)

;; Including specific load-path subd:
(normal-top-level-add-to-load-path
 '("emms" "erc" "planner" "w3"))

;; Finding library:
M-x locate-library
M-x list-load-path-shadows

;; Fun: 
(animate-string "fun fun fun!!!" <line> <col>)


;; Jump to visible is interesting but could be made buffer: 
;; It allow the user to switch to a specified window.
(defun jump-to-window (buffer-name)
  (interactive "bEnter buffer to jump to: ")
  (let ((visible-buffers (mapcar '(lambda (window) (buffer-name (window-buffer window))) (window-list)))
	window-of-buffer)
    (if (not (member buffer-name visible-buffers))
	(error "'%s' does not have visible window" buffer-name)
      (setq window-of-buffer
	    (delq nil (mapcar '(lambda (window) 
				  (if (equal buffer-name (buffer-name (window-buffer window)))
				      window nil)) (window-list))))
      (select-window (car window-of-buffer)))))

;; Idea: a jump to window with number // assuming you have less than 9 window opened.. 