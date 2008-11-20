
;; From http://infolab.stanford.edu/~manku/dotemacs.html

;; set maximum-buffer size for shell-mode (useful if some program that you're debugging spews out large amounts of output).
(setq comint-buffer-maximum-size 10240)

;; will truncate shell buffer to comint-buffer-maximum-size.
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;; will disalllow passwords to be shown in clear text (this is useful, for example, if you use the shell and then, login/telnet/ftp/scp etc. to other machines).
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; will remove ctrl-m from shell output.
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;; 
;; Shell mode hook:
;;
(add-hook 'shell-mode-hook
	  '(lambda ()
	     ;(make-local-variable 'default)
	     ;(set-face-background 'default "black")
             (local-set-key [home]        ; move to beginning of line, after prompt  
                            'comint-bol)
	     (local-set-key [up]          ; cycle backward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-previous-input 1)
                                 (previous-line 1))))
	     (local-set-key [down]        ; cycle forward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-next-input 1)
                                 (forward-line 1))))
             ))

(require 'my-shell)

;; shell-toggle.el stuff

;;(autoload 'shell-toggle "shell-toggle" 
;;  "Toggles between the *shell* buffer and whatever buffer you are editing." t) 
;;(autoload 'shell-toggle-cd "shell-toggle" 
;;  "Pops up a shell-buffer and insert a \"cd \" command." t)
;;(global-set-key [f4] 'shell-toggle)
;;(global-set-key [C-f4] 'shell-toggle-cd)
