;;
;; MtpChat mode to connect and chat on mtpchat server... :)
;;


(require 'tcp-client)
(require 'log-buffer)

;; Create faces for various opcode classes.
(make-face 'mtpchat-mtp-face)
(set-face-foreground 'mtpchat-mtp-face "red")
(defvar mtpchat-mtp-face 'mtpchat-mtp-face
  "Font to highlight mtp messages.")

(make-face  'mtpchat-emacs-face)
(set-face-foreground 'mtpchat-emacs-face (first-valid-color "cornflowerblue" "brightblue"))
(defvar mtpchat-emacs-face 'mtpchat-emacs-face
  "Font to highlight emacs messages.")

(make-face  'mtpchat-your-line-face)
(set-face-foreground 'mtpchat-your-line-face (first-valid-color "blue" "darkgrey"))
(defvar mtpchat-your-line-face 'mtpchat-your-line-face
  "Font to highlight a mtp chat line you sent.")

(make-face  'mtpchat-your-name-face)
(set-face-foreground 'mtpchat-your-name-face (first-valid-color "darkslateblue" "darkgrey"))
(defvar mtpchat-your-name-face 'mtpchat-your-name-face
  "Font to highlight a mtpchat line with your name in it.")

(make-face 'mtpchat-you-tell-face)
(set-face-foreground 'mtpchat-you-tell-face (first-valid-color "cadet blue" "blue"))
(defvar mtpchat-you-tell-face 'mtpchat-you-tell-face
  "Font to highlight a private msg you sent on mtpchat.")

(make-face 'mtpchat-private-tell-face)
(set-face-foreground 'mtpchat-private-tell-face (first-valid-color "forest green" "sea green"))
(defvar mtpchat-private-tell-face 'mtpchat-private-tell-face
  "Font to highlight a private msg you received on mtpchat.")


(setq mtpchat-buffer-name "*mtpchat*")
(setq mtpchat-login "Kandjar")

(setq mtpchat-connection (make-new-record tcp-connection :server "mtpchat.melting-pot.org" 
							 :port  4000
							 :keep-alive t))

(defun mtpchat-print(message)
  (log-printf mtpchat-buffer-name "%s" message))

(defun mtpchat-connection-established(buffer server port)
  (mtpchat-print (format "<Emacs> Connection established to %s:%i\n" server port)))

(defun mtpchat-connection-abort(buffer server port)
  (mtpchat-print (format "<Emacs> Connection abort (%s:%i)\n" server port)))

(defun mtpchat-connection-failed(buffer server port error)
  (mtpchat-print (format "<Emacs> Connection failed (%s:%i) -- %s\n" server port error)))

(defun mtpchat-sentinel(process event)
  (mtpchat-print (format "<Emacs> Sentinel report: %s" event)))


(defun mtpchat-filter(process message)
  (mtpchat-print message))

(defvar mtpchat-history nil)

(defun mtpchat-send(&optional message)
  (interactive)
  (if (not message)
      (progn (setq message (concat (read-from-minibuffer "Text to send: " nil nil nil 'mtpchat-history nil t) "\n"))
	     (display-buffer mtpchat-buffer-name)))
  (tcp-send (get-buffer-process mtpchat-buffer-name) message))

(defun mtpchat-query(&optional initial-context)
  (interactive)
  (let (message)
    (setq message (concat (read-from-minibuffer "Text to send: " initial-context nil nil 'mtpchat-history nil t) "\n"))
    (display-buffer mtpchat-buffer-name)
    (set-buffer mtpchat-buffer-name)
    (tcp-send (get-buffer-process mtpchat-buffer-name) message)))

(defun mtpchat-reply()  (interactive) (mtpchat-query "reply "))
(defun mtpchat-whoall() (interactive) (mtpchat-send "who all\n"))
(defun mtpchat-tell()   (interactive) (mtpchat-query "tell "))

;; Define the key mapping for the spu mode:
(defvar mtpchat-mode-map
  (let ((mtpchat-mode-map (make-keymap)))
    (define-key mtpchat-mode-map [(control ?x) ?r] 'mtpchat-reply)
    (define-key mtpchat-mode-map [(control ?x) ?t] 'mtpchat-tell)
    (define-key mtpchat-mode-map [(control ?x) ?w] 'mtpchat-whoall)
    (define-key mtpchat-mode-map [?A] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "A")))
    (define-key mtpchat-mode-map [?B] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "B")))
    (define-key mtpchat-mode-map [?C] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "C")))
    (define-key mtpchat-mode-map [?D] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "D")))
    (define-key mtpchat-mode-map [?E] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "E")))
    (define-key mtpchat-mode-map [?F] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "F")))
    (define-key mtpchat-mode-map [?G] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "G")))
    (define-key mtpchat-mode-map [?H] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "H")))
    (define-key mtpchat-mode-map [?I] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "I")))
    (define-key mtpchat-mode-map [?J] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "J")))
    (define-key mtpchat-mode-map [?K] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "K")))
    (define-key mtpchat-mode-map [?L] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "L")))
    (define-key mtpchat-mode-map [?M] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "M")))
    (define-key mtpchat-mode-map [?N] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "N")))
    (define-key mtpchat-mode-map [?O] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "O")))
    (define-key mtpchat-mode-map [?P] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "P")))
    (define-key mtpchat-mode-map [?Q] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "Q")))
    (define-key mtpchat-mode-map [?R] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "R")))
    (define-key mtpchat-mode-map [?S] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "S")))
    (define-key mtpchat-mode-map [?T] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "T")))
    (define-key mtpchat-mode-map [?U] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "U")))
    (define-key mtpchat-mode-map [?V] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "V")))
    (define-key mtpchat-mode-map [?W] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "W")))
    (define-key mtpchat-mode-map [?X] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "X")))
    (define-key mtpchat-mode-map [?Y] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "Y")))
    (define-key mtpchat-mode-map [?Z] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "Z")))
					
    (define-key mtpchat-mode-map [?a] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "a")))
    (define-key mtpchat-mode-map [?b] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "b")))
    (define-key mtpchat-mode-map [?c] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "c")))
    (define-key mtpchat-mode-map [?d] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "d")))
    (define-key mtpchat-mode-map [?e] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "e")))
    (define-key mtpchat-mode-map [?f] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "f")))
    (define-key mtpchat-mode-map [?g] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "g")))
    (define-key mtpchat-mode-map [?h] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "h")))
    (define-key mtpchat-mode-map [?i] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "i")))
    (define-key mtpchat-mode-map [?j] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "j")))
    (define-key mtpchat-mode-map [?k] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "k")))
    (define-key mtpchat-mode-map [?l] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "l")))
    (define-key mtpchat-mode-map [?m] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "m")))
    (define-key mtpchat-mode-map [?n] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "n")))
    (define-key mtpchat-mode-map [?o] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "o")))
    (define-key mtpchat-mode-map [?p] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "p")))
    (define-key mtpchat-mode-map [?q] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "q")))
    (define-key mtpchat-mode-map [?r] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "r")))
    (define-key mtpchat-mode-map [?s] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "s")))
    (define-key mtpchat-mode-map [?t] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "t")))
    (define-key mtpchat-mode-map [?u] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "u")))
    (define-key mtpchat-mode-map [?v] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "v")))
    (define-key mtpchat-mode-map [?w] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "w")))
    (define-key mtpchat-mode-map [?x] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "x")))
    (define-key mtpchat-mode-map [?y] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "y")))
    (define-key mtpchat-mode-map [?z] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "z")))
					
    (define-key mtpchat-mode-map [?0] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "0")))
    (define-key mtpchat-mode-map [?1] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "1")))
    (define-key mtpchat-mode-map [?2] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "2")))
    (define-key mtpchat-mode-map [?3] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "3")))
    (define-key mtpchat-mode-map [?4] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "4")))
    (define-key mtpchat-mode-map [?5] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "5")))
    (define-key mtpchat-mode-map [?6] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "6")))
    (define-key mtpchat-mode-map [?7] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "7")))
    (define-key mtpchat-mode-map [?8] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "8")))
    (define-key mtpchat-mode-map [?9] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "9")))

    (define-key mtpchat-mode-map [?!] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "!")))
    (define-key mtpchat-mode-map [?@] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "@")))
    (define-key mtpchat-mode-map [?#] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "#")))
    (define-key mtpchat-mode-map [?$] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "$")))
    (define-key mtpchat-mode-map [?%] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "%")))
    (define-key mtpchat-mode-map [?^] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "^")))
    (define-key mtpchat-mode-map [?&] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "&")))
    (define-key mtpchat-mode-map [?*] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "*")))
    (define-key mtpchat-mode-map [?\(] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "(")))
    (define-key mtpchat-mode-map [?\)] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query ")")))

    (define-key mtpchat-mode-map [?:] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query ":")))
    (define-key mtpchat-mode-map [?\;] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query ";")))
    (define-key mtpchat-mode-map [?,] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query ",")))
    (define-key mtpchat-mode-map [?.] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query ".")))

    (define-key mtpchat-mode-map [?<] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "<")))
    (define-key mtpchat-mode-map [?>] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query ">")))
    (define-key mtpchat-mode-map [?/] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "/")))
    (define-key mtpchat-mode-map [??] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "?")))

    (define-key mtpchat-mode-map [?-] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "-")))
    (define-key mtpchat-mode-map [?_] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "_")))
    (define-key mtpchat-mode-map [?+] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "+")))
    (define-key mtpchat-mode-map [?=] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "=")))

    (define-key mtpchat-mode-map [?\[] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "[")))
    (define-key mtpchat-mode-map [?\]] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "]")))
    (define-key mtpchat-mode-map [?{] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "{")))
    (define-key mtpchat-mode-map [?}] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "}")))

    (define-key mtpchat-mode-map [?'] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "'")))
    (define-key mtpchat-mode-map [?\"] '(lambda () "Query to send a string to MtpChat." (interactive) (mtpchat-query "\"")))

    mtpchat-mode-map))

(defconst mtpchat-font-lock-keywords
  (list '("^<Mtp> You \\(tell\\|ask\\|reply\\).*$" . mtpchat-you-tell-face)
	'("^<Mtp> \\w+ \\(tells\\|asks\\|replies\\).*$" . mtpchat-private-tell-face)
        '("^<Mtp>.*$" . mtpchat-mtp-face)
	'("^<Emacs>.*$" . mtpchat-emacs-face)
	(cons (concat "^<" mtpchat-login ">.*$" ) 'mtpchat-your-line-face)
	(cons mtpchat-login 'mtpchat-your-name-face))
  "Additional expressions to highlight in MtpChat lines mode.")


(defun mtpchat-mode()
  (kill-all-local-variables)
  (setq mode-name "MtpChat")
  (setq major-mode 'mtpchat-mode)
  (setq mode-line-process '(":%s"))
  (use-local-map mtpchat-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(mtpchat-font-lock-keywords nil t)))



(defun mtpchat-start()
  (interactive)
  (let ((mtpchat-hooks (make-new-record tcp-hooks
					:connection-established-handler 'mtpchat-connection-established
					:connection-abort-handler 'mtpchat-connection-abort
					:connection-failed-handler 'mtpchat-connection-failed
					:sentinel-handler 'mtpchat-sentinel
					:filter-handler 'mtpchat-filter)))
    (make-new-log-buffer mtpchat-buffer-name)
    (tcp-connect mtpchat-buffer-name mtpchat-connection mtpchat-hooks)
    (save-excursion 
      (set-buffer mtpchat-buffer-name)
      (mtpchat-mode))))

(provide 'mtpchat)
