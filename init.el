;; Add my elisp directory to the Emacs default load path
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
(defvar running-on-linux (not running-on-windows))
(defvar running-on-x (eq window-system 'x))

;; Set the default font:
(if (eq window-system 'x)
    (progn
      (create-fontset-from-fontset-spec "-*-lucida console-medium-r-*-*-12-*-*-*-*-*-*-*")
      (cond
       ((x-list-fonts "-*-lucida console-medium-r-*-*-12-*-*-*-*-*-*-*")
	(set-face-font 'default "-*-lucida console-medium-r-*-*-12-*-*-*-*-*-*-*"))
       ((x-list-fonts "-*-courier-medium-r-*-*-12-*-*-*-*-*-*-*")
	(set-face-font 'default "-*-courier-medium-r-*-*-12-*-*-*-*-*-*-*"))
       )))

(if (eq window-system 'w32)
    (progn  ;(create-fontset-from-fontset-spec "-outline-Lucida Console-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1"))
	    (set-face-font 'default "-outline-Lucida Console-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1")))
    

(load-library "custom-core")                       ; must be loaded before -- setup the core emacs
(load-library "custom-modules")                    ; load the external modules
(load-library "custom-interactive-commands")       ; add extra interactive functions
(load-library "custom-lisp-utilities")             ; add extra lisp functions
(load-library "custom-awk-scripts")                ; bind the awk script to some lisp functions
(load-library "custom-keys")                       ; setup the emacs keys
(load-library "custom-colors")                     ; color customization


(load-library "my-cpp")
(load-library "my-awk")
(load-library "my-asm")


;; Welcome message:
(set-buffer "*scratch*")
(insert (format ";; Welcome to GNU Emacs %s (%s).\n" emacs-version system-configuration))
(insert ";; Startup files loaded succesfully.\n")
