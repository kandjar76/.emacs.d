fds


(require 'smiley)




(define-fringe-bitmap  'semantic-tag-folding-folded
      ;; a plus sign
      [#B00011000
       #B00011000
       #B00011000
       #B11111111
       #B11111111
       #B00011000
       #B00011000
       #B00011000])


(defimage todo-1--pict ((:type xpm :ascent center :data "/* XPM */
static char * todo_xpm[] = {
\"12 12 4 1\",
\"      c None\",
\".     c #FF0000\",
\"+     c #000000\",
\"@     c #424242\",
\"            \",
\"            \",
\"     .....  \",
\"    .     . \",
\"    . @@@ . \",
\"    .  @  . \",
\"    .  @  . \",
\"    .  @  . \",
\"    .  @  . \",
\"    .  @  . \",
\"    .     . \",
\"     .....  \",
\"                       \"};"))
  "Image used for the display of a todo logo.")


(add-text-properties 1 2 (list 'display
			       (list
				(list 'left-fringe todo-1--pict) "test"
				)))


(defimage todo-mode--pict ((:type xpm :ascent center :data "/* XPM */
static char * todo_xpm[] = {
\"22 12 4 1\",
\"      c None\",
\".     c #FF0000\",
\"+     c #000000\",
\"@     c #424242\",
\"                      \",
\"                      \",
\"......................\",
\".                    .\",
\". @@@  @@  @@@   @@  .\",
\".  @  @  @ @  @ @  @ .\",
\".  @  @  @ @  @ @  @ .\",
\".  @  @  @ @  @ @  @ .\",
\".  @  @  @ @  @ @  @ .\",
\".  @   @@  @@@   @@  .\",
\".                    .\",
\"......................\",
\"                       \"};"))
  "Image used for the display of a todo logo.")



(defun todo-mode--overlay-p (overlay)
  "Return whether OVERLAY is an overlay of todo mode."
  (memq (overlay-get overlay 'category)
	'(todo-mode)))

(defun todo-mode--remove-todo-pict (beg end)
  (dolist (o (overlays-in beg end))
    (when (todo-mode--overlay-p o)
      (delete-overlay o))))

(defun todo-mode--update-todo-pict (beg end image)  
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'category 'todo-mode)
    (overlay-put overlay 'display (append image (list :counter (incf *todo-mode-counter*))))))


(defun todo-mode--update-todo-picts (beg end)
  (save-excursion
    (let ((regexp "\\<[Tt][Oo][Dd][Oo]\\>")
	  (image todo-mode--pict))
      (goto-char beg)
      (while (re-search-forward regexp end t)
	(todo-mode--update-todo-pict (match-beginning group) (match-end group) image)))))


(defun todo-mode-change (beg end &optional old-len)
  (let ((beg-line (save-excursion (goto-char beg) (line-beginning-position)))
		(end-line (save-excursion (goto-char end) (line-end-position))))
	(todo-mode-remove-smileys beg-line end-line)
	(todo-mode-add-smileys beg-line end-line)))
  

;;;###autoload
(define-minor-mode todo-mode-mode
  "Minor mode for automatically replacing smileys in text with
cute little graphical smileys."
  :group 'todo-mode :lighter " :)"
  (save-excursion
	(save-restriction
	  (widen)
	  (todo-mode-remove-smileys (point-min) (point-max))
	  (if todo-mode-mode
		  (progn
			(unless smiley-cached-regexp-alist
			  (smiley-update-cache))
			(jit-lock-register 'todo-mode-change))
		(jit-lock-unregister 'todo-mode-change)))))


(provide 'todo-mode)