;;
;; MtpChat mode to connect and chat on mtpchat server... :)
;;


(require 'tcp-client)

;;
;; Variables: 
;;

(defvar mtpchat--main-buffer-name "*mtpchat*"
  "Name of the MtpChat main buffer")

(defvar mtpchat--marker nil 
  "Marker for mtpchat")
(make-variable-buffer-local 'mtpchat--marker)

(defvar mtpchat--incomplete-line-save nil 
  "Store the incomplete line here waiting for the rest")
(make-variable-buffer-local 'mtpchat-newline-insert)

(defvar mtpchat--history nil 
  "Input history")

(defvar mtpchat--connection 
  (make-new-record tcp-connection :server "mtpchat.melting-pot.org" 
				  :port  4000
				  :keep-alive t)
  "MtpChat Connection Settings")

(defvar mtpchat--input-prefix "MTP> "
  "Prompt for entering a mtpchat text...")

(defvar mtpchat--input-start-marker nil)

;;
;; Network handlers:
;;

(defun mtpchat--connection-established(buffer server port)
  (setq mtpchat--incomplete-line-save nil) ;; new connection -- no incomplete lines...
  (mtpchat--insert 'mtpchat-system (format "<Emacs> Connection established to %s:%i\n" server port)))

(defun mtpchat--connection-abort(buffer server port)
  (mtpchat--insert 'mtpchat-system (format "<Emacs> Connection abort (%s:%i)\n" server port)))

(defun mtpchat--connection-failed(buffer server port error)
  (mtpchat--insert 'mtpchat-system (format "<Emacs> Connection failed (%s:%i) -- %s\n" server port error)))

(defun mtpchat--sentinel(process event)
  (mtpchat--insert 'mtpchat-system (format "<Emacs> Sentinel report: %s" event)))

(defun mtpchat--filter(process message)
  (mtpchat--insert-data message))


;;
;; MtpChat Hooks:
;;


(defgroup mtpchat-group nil
  "Group for the mtpchat variables...")

(defcustom mtpchat--validate-message-hook nil
  "Hook called before inserting the message into the buffer; 
this hook allow the filtering of displayed text by setting 
the variable mtpchat--valid-message to t or nil."
  :group 'mtpchat-group
  :type 'hook)


(defcustom mtpchat--modify-hook nil
  "Hook called once the text has been inserted inside 
the buffer; the purpose of these hooks is to modify the 
appearance and content of the inserted text"
 :group 'mtpchat-group
 :type 'hook)

;; fill: reformat the long lines...
;    (erc-put-text-property 0 (length mark-s) 'face msg-face str)
;   (erc-put-text-property (length mark-s) (+ (length mark-s) (length nick))
;			   'face nick-face str)
;    (erc-put-text-property (+ (length mark-s) (length nick)) (length str)
;			   'face msg-face str)
;; control-highlight: remote key code...
;; button...
;; add time stamp  (read-nonsticky t) (intangible t)
;;  --LEFT OR RIGHT

(defcustom mtpchat--post-insert-hook nil
  "Hook called after the modify hook, at this point the
text is finalized. No more modification should be done here."
  :group 'mtpchat-group
  :type 'hook)
;; read-only
;; modeline track

;; (set-window-dedicated-p window t)

(defcustom mtpchat-mode-hook nil
  "Hook run after `mtpchat-mode' setup is complete."
  :group 'mtpchat-group
  :type 'hook)

;;
;; Utility functions:
;;


(defun mtpchat--insert-data(message)
  "Receiving data from the server, this function will cut these data 
into full lines. Non-full lines will not be processed for now."
  (let* ((strlist (split-string (concat mtpchat--incomplete-line-save message) "\n\r?")))
    (setq mtpchat--incomplete-line-save nil)
    (while (and strlist (cdr strlist))
      (when (> (length (car strlist)) 0)
	(mtpchat--insert 'mtpchat-data (car strlist)))
      (setq strlist (cdr strlist)))
    ;; Last line is incomplete; stored for later...
    ;; exception: "<Mtp> Login:" or "<Mtp> Password:"
    (if (or (string-match "^<Mtp> Login: $" message)
	    (string-match "<Mtp> Password: $" message))
	(mtpchat--insert 'mtpchat-data (car strlist))
	(setq mtpchat--incomplete-line-save (car strlist)))
	))

(defun mtpchat--insert(type message)
  "Display the message MESSAGE in a mtpchat buffer, 
TYPE reprensents the type of message, currently two type are
supported:
 'mtpchat-system (internal display) and 'mtpchat-data (data
 received from the server)."
  (let ((mtpchat-buffer (get-buffer mtpchat--main-buffer-name)))
    (when (bufferp mtpchat-buffer)
      (with-current-buffer mtpchat-buffer
	(save-excursion
	  (goto-char (marker-position mtpchat--marker))
	  (let ((mtpchat--skip-message nil)
		(start-pos (marker-position mtpchat--marker)))
	    (run-hook-with-args 'mtpchat--validate-message-hook type message)
	    (when (null mtpchat--skip-message)
	      (when (not (string-match "\n\r?$" message))
		(setq message (concat message "\n")))
	      (let ((inhibit-read-only t))
		(insert-before-markers message))
	      (let ((end-pos (point)))
		(save-restriction
		  (narrow-to-region start-pos end-pos)
		  (run-hooks 'mtpchat--modify-hook)
		  (run-hooks 'mtpchat--post-insert-hook))))))))))

(defun mtpchat--make-read-only()
  "Post insert hook functions: set the text property to be read-only"
  (add-text-properties (point-min) (point-max)
		       '(read-only t front-sticky t rear-nonsticky t)))


(defun mtpchat--send ()
  (interactive)
  (let ((to-send (buffer-substring-no-properties (marker-position mtpchat--input-start-marker)
						 (point-max))))
    (when (> (length to-send) 0)
      (delete-region (marker-position mtpchat--input-start-marker) (point-max))
      ;(mtpchat--insert 'mtpchat to-send)
      (tcp-send (get-buffer-process mtpchat--main-buffer-name) (concat to-send "\n"))
      )))

;; Define the key mapping for the mtpchat mode:
(defvar mtpchat-mode-map
  (let ((mtpchat-mode-map (make-keymap)))
    (define-key mtpchat-mode-map [return] 'mtpchat--send)

    ;(define-key hexl-mode-map [remap self-insert-command] 'hexl-self-insert-command)

    mtpchat-mode-map))

;(defconst mtpchat-font-lock-keywords
;  (list '("^<Mtp> You \\(tell\\|ask\\|reply\\).*$" . mtpchat-you-tell-face)
;	'("^<Mtp> \\w+ \\(tells\\|asks\\|replies\\).*$" . mtpchat-private-tell-face)
;        '("^<Mtp>.*$" . mtpchat-mtp-face)
;	'("^<Emacs>.*$" . mtpchat-emacs-face)
;	(cons (concat "^<" mtpchat-login ">.*$" ) 'mtpchat-your-line-face)
;	(cons mtpchat-login 'mtpchat-your-name-face))
;  "Additional expressions to highlight in MtpChat lines mode.")
;

(defun mtpchat--scroll-to-bottom (window display-start)
  "Scroll to the bottom of the display when the user is typing some texts...
Function added to `window-scroll-functions' by mtpchat-mode"
  (if (window-live-p window)
      (if (> (point) mtpchat--marker)
	  (save-excursion
	    (goto-char (point-max))
	    (recenter (- (window-body-height window) 3))
	    (sit-for 0)))))


(defun mtpchat-mode(&optional prompt)
  (kill-all-local-variables)
  (goto-char (point-max))


  ;; Mode definition:
  (setq mode-name "MtpChat")
  (setq major-mode 'mtpchat-mode)
  (setq mode-line-process '(":%s"))
  (use-local-map mtpchat-mode-map)
  (set (make-local-variable 'truncate-lines) nil)

  ;; Buffer Local
  (make-variable-buffer-local 'mtpchat--incomplete-line-save)

  ;; Get rid of the ^M at the end of the lines:
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\r [])

  ;; Marker creation:
  (setq mtpchat--marker (make-marker))
  (setq mtpchat--input-start-marker (make-marker))


  ;; We may want ot check if we already are running a previously open session 
  ;(forward-line 0)
  ;(when (get-text-property (point) 'mtp-prompt)...
  (let ((start-pos (point)))
    (insert "\n") ;; it also solve the issue where the prompt is at the beginning of the buffer --> doesn't scroll!!!
    (add-text-properties start-pos (point) 
			 '(read-only t rear-nonsticky t)))

  ;; Setup the output markers:
  (set-marker mtpchat--marker (point-max))

  ;; Set the prompt:
  (insert (or prompt mtpchat--input-prefix))
  (add-text-properties (marker-position mtpchat--marker)
		       (point-max)
		       '(read-only t intangible t rear-nonsticky t mtp-prompt t front-sticky t))
 
  ;; Setup the input marker:
  (set-marker mtpchat--input-start-marker (point-max))
  
  ;; Make sure we always try to scroll to the bottom of the screen
  (add-hook 'window-scroll-functions 'mtpchat--scroll-to-bottom nil t)

  ;; Now let setup the mtpchat-hooks:
  (make-local-hook 'mtpchat--post-insert-hook)
  (make-local-hook 'mtpchat--modify-hook)
  (make-local-hook 'mtpchat--validate-message-hook)

  (add-hook 'mtpchat--post-insert-hook 'mtpchat--make-read-only)
  ;; 

  ;; Finally, let's run the user mode-hooks 
  )

;;;###autoload
(defun mtpchat()
  "Entry point to start the MtpChat client"
  (interactive)
  (let ((mtpchat-hooks (make-new-record tcp-hooks
					:connection-established-handler 'mtpchat--connection-established
					:connection-abort-handler 'mtpchat--connection-abort
					:connection-failed-handler 'mtpchat--connection-failed
					:sentinel-handler 'mtpchat--sentinel
					:filter-handler 'mtpchat--filter)))
    (get-buffer-create mtpchat--main-buffer-name)
    (save-excursion 
      (set-buffer mtpchat--main-buffer-name)
      ;; Setup the mtpchat-mode
      (mtpchat-mode)
      (tcp-connect mtpchat--main-buffer-name mtpchat--connection mtpchat-hooks))))

(provide 'mtpchat)
