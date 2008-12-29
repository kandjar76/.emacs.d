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
(cond 
 ((eq window-system 'x)
  (if (> emacs-major-version 21)
      (set-default-font "6x13")
      (progn
	(create-fontset-from-fontset-spec "-*-lucida console-medium-r-*-*-12-*-*-*-*-*-*-*")
	(cond
	 ((x-list-fonts "-*-lucida console-medium-r-*-*-12-*-*-*-*-*-*-*")
	  (set-face-font 'default "-*-lucida console-medium-r-*-*-12-*-*-*-*-*-*-*"))
	 ((x-list-fonts "-*-courier-medium-r-*-*-12-*-*-*-*-*-*-*")
	  (set-face-font 'default "-*-courier-medium-r-*-*-12-*-*-*-*-*-*-*"))
	 ))))
 ((eq window-system 'w32)
  (set-default-font "Courier")))



(setq time1 (current-time))


(load-library "custom-core")                       ; must be loaded before -- setup the core emacs

(setq time2 (current-time))

(load-library "custom-modules")                    ; load the external modules
(setq time3 (current-time))

(load-library "custom-keys")                       ; setup the emacs keys
(load-library "custom-colors")                     ; color customization

(setq time4 (current-time))

;; Welcome message:
(set-buffer "*scratch*")
(insert (format ";; Welcome to GNU Emacs %s (%s).\n" emacs-version system-configuration))
(insert ";; Startup files loaded succesfully.\n")
(insert ";;\n")

(when (> emacs-major-version 21)
  (setq time4 (time-subtract time4 time3))
  (setq time3 (time-subtract time3 time2))
  (setq time2 (time-subtract time2 time1))
  (setq time1 (time-subtract time1 orig-time))

  (insert "\n")
  (insert (format "Font Setup     - %2is %06ius\n" (cadr time1) (car (cdr (cdr time1)))))
  (insert (format "Core           - %2is %06ius\n" (cadr time2) (car (cdr (cdr time2)))))
  (insert (format "Modules loaded - %2is %06ius\n" (cadr time3) (car (cdr (cdr time3)))))
  (insert (format "Custom key/col - %2is %06ius\n" (cadr time4) (car (cdr (cdr time4))))))

(set-buffer-modified-p nil)
;(custom-set-faces
; '(diff-added ((t (:inherit diff-changed :foreground "red")))))