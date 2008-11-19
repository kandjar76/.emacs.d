;;
;; MtpChat mode to connect and chat on mtpchat server... :)
;;


(require 'tcp-client)


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
  (save-excursion 
    (set-buffer mtpchat-buffer-name)
    (goto-char (point-max))
    (insert message)))

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


(defun mtpchat-send(&optional message)
  (interactive)
  (if (not message)
      (progn (setq message (concat (read-from-minibuffer "Text to send: " nil nil nil nil nil t) "\n"))
	     ))
  (tcp-send (get-buffer-process mtpchat-buffer-name) message))

;; Define the key mapping for the spu mode:
(defvar mtpchat-mode-map
  (let ((mtpchat-mode-map (make-keymap)))
    ;(define-key mtpchat-mode-map [(control c) (control f)]    'ddf-to-inl)
    mtpchat-mode-map))

(defconst mtpchat-font-lock-keywords
  (list '("^<Mtp> You \\(tell\\|ask\\).*$" . mtpchat-you-tell-face)
	'("^<Mtp> \\w+ \\(tells\\|asks\\).*$" . mtpchat-private-tell-face)
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
    (tcp-connect mtpchat-buffer-name mtpchat-connection mtpchat-hooks)
    (save-excursion 
      (set-buffer mtpchat-buffer-name)
      (mtpchat-mode))))
