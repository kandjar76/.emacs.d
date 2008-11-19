;; Add my elisp directory to the Emacs default load path
(setq load-path
      (append
       (list 
         "~/.emacs.d"
         "~/.emacs.d/external"
		 "~/.emacs.d/libs"
         "~/.emacs.d/modes"
        )
       load-path))


;; Set the default font:
(create-fontset-from-fontset-spec "-*-lucida console-medium-r-*-*-12-*-*-*-*-*-*-*")
(set-face-font 'default "-*-lucida console-medium-r-*-*-12-*-*-*-*-*-*-*")


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




;; Automatically load the desktop:
;;(desktop-read)
;;(message "Startup files loaded succesfully -- M^x desktop-read : to restore the previously opened files.")))

