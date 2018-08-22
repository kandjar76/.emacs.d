;; Add my elisp directory to the Emacs default load path
;(modify-frame-parameters nil '((wait-for-wm . nil)))

(setq emacs-startup-time (current-time))

(setq load-path
      (append
       (list 
         "~/.emacs.d/custom"
         "~/.emacs.d/external"
         "~/.emacs.d/libs"
         "~/.emacs.d/modes"
         "~/.emacs.d/personal"
         "~/.emacs.d/to-try"

	 "~/.emacs.x/org-mode/lisp"
        )
       load-path))

(require 'tramp) ;; To prevent 'recursive load error in emacs 23.1

(defvar running-on-windows (memq system-type '(windows-nt cygwin)))
(defvar running-on-linux   (not running-on-windows))
(defvar running-on-x       (eq window-system 'x))
(defvar running-at-work    (not (eq (or (and (boundp 'string-match-p)
					     (string-match-p  "naughtydog\\|magic-dog" system-name))
					(string-match "naughtydog\\|magic-dog" system-name)) nil)))
(defvar running-at-4k      (not (eq (or (and (boundp 'string-match-p) (string-match-p  "CHINO" system-name))
					(string-match "CHINO" system-name)) nil)))

(defvar running-from-homedir (string= "~/" (symbol-value 'command-line-default-directory)))



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
  (cond
   (running-at-work
    (set-default-font "016x13"))
   (running-at-4k
    (set-default-font "-raster-Courier-normal-normal-normal-mono-25-*-*-*-c-*-iso8859-1"))
   (t (set-default-font "Courier"))
   )))

(load-library "custom-core")                       ; must be loaded before -- setup the core emacs
(load-library "custom-modules")                    ; load the external modules
(load-library "custom-keys")                       ; setup the emacs keys
(load-library "custom-colors")                     ; color customization

;; Welcome message:
(set-buffer "*scratch*")
(insert (format ";; Welcome to GNU Emacs %s (%s).\n" emacs-version system-configuration))
(let ((loading-time (time-subtract (current-time) emacs-startup-time)))
  (insert (format ";; Startup files loaded succesfully [%i.%03is].\n"
		  (car (cdr loading-time)) 
		  (/ (car (cdr (cdr loading-time))) 1000))))
(insert ";;\n")

(set-buffer-modified-p nil)
