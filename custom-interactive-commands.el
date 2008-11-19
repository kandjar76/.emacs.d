;;
;; This file is contains a set of  interactive functions
;;

;; Here is a list of the function defined here:
;;
;;
;; - electric-split-window-horizontally
;; - electric-split-window-vertically
;; - indent-whole-buffer
;; - revert-buffer-now
;; - kill-buffer-now
;; - ascii-table
;; - dos-to-unix
;; - unix-to-dos
;; - show-file-name
;; - setup-text-mode
;; - delete-backward-word


;;
;; Miscellaneous function:
;;


(defun electric-split-window-horizontally(&optional arg)
  "Split current window into two window side by side, move the cursor to the new created window"
  (interactive)
  (split-window-horizontally arg)
  (other-window 1))

(defun electric-split-window-vertically(&optional arg)
  "Split current window into two window side by side, move the cursor to the new created window"
  (interactive)
  (split-window-vertically arg)
  (other-window 1))



(defun revert-buffer-now ()
  "Silently calls revert-buffer if the current buffer is not modified."
  (interactive)
  (if (not (buffer-modified-p))
	  (message (format "Reverted from %s" (buffer-file-name))))
  (revert-buffer nil (not (buffer-modified-p))))

(defun kill-buffer-now ()
  "Calls kill-buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

(defun ascii-table ()                                     
  "Print the ascii table."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 32))
    (while (< i 254)
	  (let ((j (min 254 (+ i 8))))
		(while (< i j)
		  (setq i (+ i 1))
		  (insert (format "%4d %c\t" i i)))
		(insert "\n")))
	(beginning-of-buffer)))

(defun dos-to-unix()
  "Convert the end-of-line of the current file from DOS to UNIX"
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
	(while (search-forward "\r" nil t) (replace-match ""))))

(defun unix-to-dos()                                       
  "Convert the end-of-line of the current file from UNIX to DOS"
  (interactive "*")
  (save-excursion
	(goto-char (point-min))
	(while (search-forward "\n" nil t) (replace-match "\r\n"))))

(defun show-file-name()
  "Display the full path of the current buffer."
  (interactive)
  (message buffer-file-name))


(defun setup-text-mode()
  "Setup the text mode to write txt docs."
  (interactive)
  (paragraph-indent-text-mode)
  (auto-fill-mode)
  (set-fill-column 80)
  (flyspell-mode))

(defun backward-delete-word(&optional arg)
  "Delete the previous word"
  (interactive "*")
  (save-excursion
    (let ((end (point)))
      (backward-word 1)
      (delete-region (point) end))))


(defadvice yank-pop (around anytime (arg))
  "Modification of yank-pop: if last action isn't yank, do it."
  (if (not (eq last-command 'yank))
      (yank arg)
      ad-do-it))
(ad-activate 'yank-pop)

