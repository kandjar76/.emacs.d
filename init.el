;; Add my elisp directory to the XEmacs default load path
(setq load-path
      (append
       (list 
         "~/.emacs.d"
         "~/.emacs.d/external/xcscope"
         "~/.emacs.d/external"
         "~/.emacs.d/modes"
        )
       load-path))



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

;;(desktop-load-default)
;;(desktop-read)

;;(message "Startup files loaded succesfully -- M^x desktop-read : to restore the previously opened files.")))
