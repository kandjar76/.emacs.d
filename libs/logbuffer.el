;; The purpose of this file is just to react like a log window.
;; Meaning: you display some strings, the buffer is going to autoscroll,
;; unless your scrolled up

(defun make-new-log-buffer (buffer-name)
  "Create a new log buffer, called BUFFER-NAME.
If BUFFER-NAME already exist, the function will just set the read-only flag.
The log buffer is returned as a result of this call."
  (let ((buffer (get-buffer-create buffer-name)))
    (save-excursion (set-buffer buffer)
		    (toggle-read-only 1)
		    buffer)))
    
(defun log-printf(buffer format-string &rest args)
  "Display the text in the log buffer at the very end of it."
  (let ((inhibit-read-only t)
	(auto-window-vscroll t)
	(user-buffer (current-buffer))
	(window (get-buffer-window buffer))
	(user-window (selected-window)))
    (set-buffer buffer)
    (let* ((current-point (point))
	   (current-bot (= current-point (point-max))))
      (goto-char (point-max))
      (insert-string (apply 'format (cons format-string args)))
      
      (if current-bot
	  (goto-char (point-max))
	  (goto-char current-point))
      (if (and window current-bot)
	  (progn (select-window window)
		 (scroll-up)
		 (select-window user-window)))
      (if current-bot (message "this is a test")))))
  
(make-new-log-buffer "*test*")
(log-printf "*test*" "coucou\n")

(setq inhibit-read-only nil)
(scroll-up-aggressively)


(point-marker)

