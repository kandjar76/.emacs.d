;;
;; This file is contains a set of  interactive functions
;;

;; Here is a list of the function defined here:
;;
;; - indent-whole-buffer
;; - revert-buffer-now
;; - kill-buffer-now
;; - ascii-table
;; - dos-to-unix
;; - unix-to-dos
;; - show-file-name



;;
;; Miscellaneous function:
;;

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

;(defun ascii-table ()                                     
;  "Print the ascii table."
;  (interactive)
;  (switch-to-buffer "*ASCII*")
;  (erase-buffer)
;  (insert (format "ASCII characters up to number %d.\n" 254))
;  (let ((i 32))
;    (while (< i 254)
;	  (let ((j (min 254 (+ i 8))))
;		(while (< i j)
;		  (setq i (+ i 1))
;		  (insert (format "%4d %c\t" i i)))
;		(insert "\n")))
;	(beginning-of-buffer)))

;(defun dos-to-unix()
;  "Convert the end-of-line of the current file from DOS to UNIX"
;  (interactive "*")
;  (save-excursion
;    (goto-char (point-min))
;	(while (search-forward "\r" nil t) (replace-match ""))))

;(defun unix-to-dos()                                       
;  "Convert the end-of-line of the current file from UNIX to DOS"
;  (interactive "*")
;  (save-excursion
;	(goto-char (point-min))
;	(while (search-forward "\n" nil t) (replace-match "\r\n"))))

;(defun show-file-name()
;  "Display the full path of the current buffer."
;  (interactive)
;  (message buffer-file-name))

;(defun int-to-hex(integer)
;  "Display a message with the hexadecimal value of the integer INTEGER"
;  (interactive "_sInteger value: ")
;  (let ((int (string-to-int integer)))
;	(message (format "Hexadecimal value of %i is 0x%X" int int))))

