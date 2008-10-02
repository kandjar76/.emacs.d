;; Add my elisp directory to the Emacs default load path
;(modify-frame-parameters nil '((wait-for-wm . nil)))

(setq orig-time (current-time))

(setq load-path
      (append
       (list 
         "~/.emacs.d"
         "~/.emacs.d/external"
         "~/.emacs.d/libs"
         "~/.emacs.d/modes"
         "~/.emacs.d/personal"
        )
       load-path))

(defvar running-on-windows (memq system-type '(windows-nt cygwin)))
(defvar running-on-linux   (not running-on-windows))
(defvar running-on-x       (eq window-system 'x))
(defvar running-at-work    (not (eq (or (and (boundp 'string-match-p)
					     (string-match-p  "naughtydog" system-name))
					(string-match "naughtydog" system-name)) nil)))


;; Set the default font:
(if (eq window-system 'x)
    (if (> emacs-major-version 21)
	(if running-at-work
	    (set-default-font "9x15")
	    (set-default-font "9x15"))
	(progn
	  (create-fontset-from-fontset-spec "-*-lucida console-medium-r-*-*-12-*-*-*-*-*-*-*")
	  (cond
	   ((x-list-fonts "-*-lucida console-medium-r-*-*-12-*-*-*-*-*-*-*")
	    (set-face-font 'default "-*-lucida console-medium-r-*-*-12-*-*-*-*-*-*-*"))
	   ((x-list-fonts "-*-courier-medium-r-*-*-12-*-*-*-*-*-*-*")
	    (set-face-font 'default "-*-courier-medium-r-*-*-12-*-*-*-*-*-*-*"))
	   ))))

(when (eq window-system 'w32)
  (set-default-font "Courier")
)



(setq time1 (current-time))


(load-library "custom-core")                       ; must be loaded before -- setup the core emacs

(setq time2 (current-time))

(load-library "custom-modules")                    ; load the external modules
(setq time3 (current-time))

(load-library "custom-awk-scripts")                ; bind the awk script to some lisp functions
(load-library "custom-keys")                       ; setup the emacs keys
(load-library "custom-colors")                     ; color customization


(load-library "my-cpp") ;; slow in text mode
(load-library "my-awk")

(setq time4 (current-time))

;; Welcome message:
(set-buffer "*scratch*")
(insert (format ";; Welcome to GNU Emacs %s (%s).\n" emacs-version system-configuration))
(insert ";; Startup files loaded succesfully.\n")
(insert ";;\n")
(insert ";; BUGS:\n")
(insert ";;  . Bug    .. `alpha=\"ok\"` generates an error while trying to highlight the occurence of alpha (C-`)\n")
(insert ";;  . Bug    .. Files containing defrecord statement don't compile!\n")
(insert ";;\n")
(insert ";; TODO:\n")
(insert ";;  . Git    .. Put all the emacs backups in git\n")
(insert ";;  . Git    .. Allow the git log for the current buffer and not only within the git buffer\n")
(insert ";;  . Global .. Tree of undo\n")
(insert ";;  . Global .. c-~ -> highlight the current word globally (in every visible buffers)\n")
(insert ";;  . Global .. Edit multiple text area at once (using ctrl+mouse to highlight those area for example)\n")
(insert ";;  .        .. Another option: select the text and press ctrl+enter to put the overlay on it. ctrl+enter without any selection remove them all\n")
(insert ";;  . Mode   .. Project view\n")
(insert ";;  . Mode   .. TODO mode\n")
(insert ";;  . Mode   .. Log viewer (`-, ...)\n")
(insert ";;  . Mode   .. Update git integration\n")
(insert ";;  .        .. `- Gitk in emacs\n")
(insert ";;  .        .. `- See the git branches in emacs\n")
(insert ";;  .        .. `- Do git checkout in emacs\n")
(insert ";;  .        .. `- Git rebase in emacs\n")
(insert ";;  . C/C++  .. #if 0 recognize!!!\n")
(insert ";;  . C/C++  .. Check flymake\n")
(insert ";;  . C/C++  .. Thread comment minor mode for C/C++ (to tag RW flags on variables)\n")
(insert ";;  . C/C++  .. Split C/C++ mode into two distinct modes instead of one my-cpp\n")
(insert ";;  . C/C++  .. Function to add //----- above and below the case statement\n")
(insert ";;  . C/C++  .. Better alignment macros (assignment, function)\n")
(insert ";;  . C/C++  .. Fix indentation for the comment around the 'case' statement\n")
(insert ";;  . Buffer .. buffer-menu++ -> Make the reload asynchronous\n")


(when (> emacs-major-version 21)
  (setq time4 (time-subtract time4 time3))
  (setq time3 (time-subtract time3 time2))
  (setq time2 (time-subtract time2 time1))
  (setq time1 (time-subtract time1 orig-time))

  (insert "\n")
  (insert (format "Font Setup     - %2is %06ius\n" (cadr time1) (caddr time1)))
  (insert (format "Core           - %2is %06ius\n" (cadr time2) (caddr time2)))
  (insert (format "Modules loaded - %2is %06ius\n" (cadr time3) (caddr time3)))
  (insert (format "Other modules  - %2is %06ius\n" (cadr time4) (caddr time4))))

(set-buffer-modified-p nil)
;(custom-set-faces
; '(diff-added ((t (:inherit diff-changed :foreground "red")))))