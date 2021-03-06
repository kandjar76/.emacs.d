;;
;; MtpChat mode to connect and chat on mtpchat server... :)
;;


;; TODO:
;; -- knowing who's connected
;; -- button --> finger on user // url!
;; -- autocompletion of nicks!
;; -- history of the prompt! (using ctrl+up/ctrl+down or ctrl+n/ctrl+p or research with ctrl+???)
;; -- highlight mode line with a different color if the user name has been said or beeped! (for the main buffer only)


;; BUG:
;; -- infinite loop if bad pwd / null nickname... (idea: split into 3 functions)
;; -- multiline in private tell window doesn't worked properly.. :)
;; -- notification is never canceled!
;; -- the name of the mode is invalid in priv channel (it has ':no process' attached to it)


(require 'cl)
(require 'record-type)
(require 'tcp-client)

;;
;; Variables: 
;;

(defvar mtpchat--main-buffer-name "*mtpchat*"
  "Name of the MtpChat main buffer")

(defvar mtpchat--marker nil 
  "Marker for mtpchat")

(defvar mtpchat--incomplete-line-save nil 
  "Store the incomplete line here waiting for the rest")

(defvar mtpchat--history nil 
  "Input history")

(defvar mtpchat--connection 
  (make-new-record tcp-connection :server "mtpchat.melting-pot.org" 
				  :port  4000
				  :keep-alive t)
  "MtpChat Connection Settings")

(defvar mtpchat--login nil
  "MtpChat Login name")
(defvar mtpchat--passwd nil
  "MtpChat Password")

(defvar mtpchat--input-prefix "MTP> "
  "Prompt for entering a mtpchat text...")
(defvar mtpchat--input-start-marker nil)


(defvar mtpchat--header-line ""
  "Header line displayed on top of the MtpChat buffer")

(defvar mtpchat--topic ""
  "Topic of MtpChat")

(defvar mtpchat--prefix nil 
  "Marker for prefix message to append to messages to be sent")

(defvar mtpchat--buffer-list nil
  "List of mtpchat's buffer")

(defvar mtpchat--hidden-modified-buffer-list nil
  "List of mtpchat's buffer which got modified while hidden to the user")

(defvar mtpchat--notification-string nil
  "String to display in the mode line")
(put 'mtpchat--notification-string 'risky-local-variable t)

;;
;; Fonts:
;;

;; Create faces for various opcode classes.
(make-face 'mtpchat-prompt-face)
(set-face-background 'mtpchat-prompt-face "slategray1")
(make-face-bold 'mtpchat-prompt-face)
(defvar mtpchat-prompt-face 'mtpchat-prompt-face
  "Font to highlight mtp prompt.")

(make-face 'mtpchat-time-stamp-face)
(set-face-foreground 'mtpchat-time-stamp-face "limegreen")
(defvar mtpchat-time-stamp-face 'mtpchat-time-stamp-face
  "Font to highlight mtp timestamp.")

(make-face 'mtpchat-mtp-face)
(set-face-foreground 'mtpchat-mtp-face "red")
(defvar mtpchat-mtp-face 'mtpchat-mtp-face
  "Font to highlight mtp messages.")

(make-face  'mtpchat-emacs-face)
(set-face-foreground 'mtpchat-emacs-face "cornflowerblue")
(defvar mtpchat-emacs-face 'mtpchat-emacs-face
  "Font to highlight emacs messages.")

(make-face  'mtpchat-your-line-face)
(set-face-foreground 'mtpchat-your-line-face "blue")
(defvar mtpchat-your-line-face 'mtpchat-your-line-face
  "Font to highlight a mtp chat line you sent.")

(make-face  'mtpchat-your-name-face)
(set-face-foreground 'mtpchat-your-name-face "darkslateblue")
(defvar mtpchat-your-name-face 'mtpchat-your-name-face
  "Font to highlight a mtpchat line with your name in it.")

(make-face 'mtpchat-you-tell-face)
(set-face-foreground 'mtpchat-you-tell-face "cadet blue")
(defvar mtpchat-you-tell-face 'mtpchat-you-tell-face
  "Font to highlight a private msg you sent on mtpchat.")

(make-face 'mtpchat-private-tell-face)
(set-face-foreground 'mtpchat-private-tell-face "forest green")
(defvar mtpchat-private-tell-face 'mtpchat-private-tell-face
  "Font to highlight a private msg you received on mtpchat.")

(make-face 'mtpchat-emote-face)
(set-face-foreground 'mtpchat-emote-face "deep pink")
(defvar mtpchat-emote-face 'mtpchat-emote-face
  "Font to highlight emote messages.")

(make-face 'mtpchat-away-face)
(set-face-foreground 'mtpchat-away-face "LavenderBlush4")
(defvar mtpchat-away-face 'mtpchat-away-face
  "Font to highlight away messages.")

(make-face 'mtpchat-wall-star)
(set-face-foreground 'mtpchat-wall-star "gray")
(defvar mtpchat-wall-star 'mtpchat-wall-star
  "Font to highlight the '*' displayed with the wall data .")

(make-face 'mtpchat-wall-mail-nick)
(set-face-foreground 'mtpchat-wall-mail-nick "DarkGoldenrod")
(defvar mtpchat-wall-mail-nick 'mtpchat-wall-mail-nick
  "Font to highlight the nicknames displayed with the wall and mail data .")

(make-face 'mtpchat-mail-number)
(set-face-foreground 'mtpchat-mail-number "darkgray")
(defvar mtpchat-mail-number 'mtpchat-mail-number
  "Font to highlight the '*' displayed with the mail number .")


;;
;; Network handlers:
;;

(defun mtpchat--connection-established(buffer server port)
  (setq mtpchat--incomplete-line-save nil) ;; new connection -- no incomplete lines... 
  (set-process-coding-system (get-buffer-process mtpchat--main-buffer-name) 'iso-latin-1)
  ;; Add the notification string to the mtpchat list:
  (mtpchat--setup-notification)
  (with-current-buffer buffer 
    (add-hook 'mtpchat--validate-message-hook 'mtpchat--auto-login))
  (let ((mtpchat-buffer (get-buffer mtpchat--main-buffer-name)))
    (mtpchat--insert mtpchat-buffer 'mtpchat-system (format "<Emacs> Connection established to %s:%i\n" server port))))

(defun mtpchat--connection-abort(buffer server port)
  (let ((mtpchat-buffer (get-buffer mtpchat--main-buffer-name)))
    (mtpchat--insert mtpchat-buffer 'mtpchat-system (format "<Emacs> Connection abort (%s:%i)\n" server port))))

(defun mtpchat--connection-failed(buffer server port error)
  (let ((mtpchat-buffer (get-buffer mtpchat--main-buffer-name)))
    (mtpchat--insert mtpchat-buffer 'mtpchat-system (format "<Emacs> Connection failed (%s:%i) -- %s\n" server port error))))

(defun mtpchat--sentinel(process event)
  (let ((mtpchat-buffer (get-buffer mtpchat--main-buffer-name)))
    (mtpchat--insert mtpchat-buffer 'mtpchat-system (format "<Emacs> Sentinel report: %s" event))))

(defun mtpchat--filter(process message)
  (mtpchat--display message))

;;
;; Regular expression:
;;

(defvar mtpchat-regexp--mail         "^ ?[0-9]+ [0-9][0-9]/[0-9][0-9]/[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] \\w+ :")
(defvar mtpchat-regexp--wall         "^[0-9][0-9]/[0-9][0-9]/[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] \\w+ ")

(defvar mtpchat-regexp--topic        "^<Mtp> \\w+ topic : ")
(defvar mtpchat-regexp--topic-set    "^<Mtp> \\w+ set channel \\w+ topic to ")

(defvar mtpchat-regexp--you-tell     "^<Mtp> You \\(tell\\|ask\\|reply to\\) \\w+: ")
(defvar mtpchat-regexp--private-tell "^<Mtp> \\w+ \\(tells you\\|asks you\\|replies\\): ")
(defvar mtpchat-regexp--tell-away    "^<Mtp> \\w+ is away and may not be hearing you")


;;
;; Fontification structure:
;;

(setq mtpchat--data-finder 
      '(
	(mtpchat-mtp-face          'login             "<Mtp> Login")
	(mtpchat-mtp-face          'alreadylog        "<Mtp> You are already logged in, try with another login name")
	(mtpchat-mtp-face          'invalidpass       "<Mtp> Incorrect password")
		           
	(nil                       'welcome           "Welcome to ")
	(mtpchat-mtp-face          'away              "<Mtp> You are away")
	(mtpchat-mtp-face          'back              "<Mtp> You are back")
	(mtpchat-away-face         'tellaway          "<Mtp> (/w+):(nick) is away and may not be hearing you")
	(mtpchat-mtp-face          'useraway          "<Mtp> (/w+):(nick) is away")
	(mtpchat-mtp-face          'userback          "<Mtp> (/w+):(nick) is back")
	(mtpchat-mtp-face          'wellogin          "<Mtp> Welcome, (/w+):(login)/.")
		           
	(mtpchat-emote-face        'emote             "<Mtp> /*(/w+):(nick) (.+):(text)$")
	(mtpchat-mtp-face          'shout             "<Mtp> (/w+):(nick) shouts: (.+):(text)$")
	(mtpchat-mtp-face          'youshout          "<Mtp> You shout: (.+):(text)$")
	(mtpchat-you-tell-face     'youtell           "<Mtp> You {tell|ask|reply to} (/w+):(nick): (.+):(text)$")
	(mtpchat-private-tell-face 'tell              "<Mtp> (/w+):(nick) {{tell|ask}s you|replies}: (.+):(text)$")
	(mtpchat-mtp-face          'kicked            "<Mtp> (/w+):(nick) is kicked out by (/w+):(text)/b")
	(mtpchat-mtp-face          'kick              "<Mtp> You kick (/w+):(nick)/b")
	(mtpchat-mtp-face          'topjoin           "<Mtp> (/w+):(channel) topic : (.*):(topic)$")
	(mtpchat-mtp-face          'topicset          "<Mtp> (/w+):(nick) set channel (/w+):(channel) topic to (.*):(topic)$")
	
	(mtpchat-mtp-face          'comefrom          "<Mtp> (/w+):(nick) {comes in from channel|appears from the shadows}")
	(mtpchat-mtp-face          'leavechannel      "<Mtp> (/w+):(nick) left channel")
	(mtpchat-mtp-face          'fadechannel       "<Mtp> (/w+):(nick) fades into the shadows")
	(mtpchat-mtp-face          'joinedchannel     "<Mtp> (/w+):(nick) joined channel (/w+):(channel)/b")
	(mtpchat-mtp-face          'join              "<Mtp> You join channel (/w+):(channel)/b")
	(mtpchat-mtp-face          'leave             "<Mtp> You leave channel")
	
	(mtpchat-mtp-face          'comein            "<Mtp> (/w+):(nick) comes in")
	(mtpchat-mtp-face          'leaves            "<Mtp> (/w+):(nick) {leaves|disconnects}")
	
	(mtpchat-mtp-face          'bump              "<Mtp> {(/w+):(nick) is bumped|You bump (/w+):(nick) out}")
	(mtpchat-mtp-face          'bumped            "<Mtp> You are bumped out by (/w+):(nick)/b")
	(xxx                       'usrbeep           "<Mtp> (/w+):(nick) beeps you")
	(xxx                       'youbeep           "<Mtp> You beep (/w+):(nick)/b")
	
	(mtpchat-mtp-face          'msgbegin          "<Mtp> Your message/(s/)")
	(mtpchat-mtp-face          'msgend            "<Mtp> You have (/d+|no):(nbmsgs) message")
	
	(xxx                       'msgline           " ?(/d+):(numeric) (/d/d///d/d///d/d /d/d:/d/d:/d/d):(msgtime) (/w+):(nick) : (.*):(text)$")
	(xxx                       'wallline          "(/d/d///d/d///d/d /d/d:/d/d:/d/d|/w/w/w /w/w/w /d/d /d/d/d/d /d/d:/d/d:/d/d):(msgtime) (/w+):(nick) (.*):(text)$")
		           
	(mtpchat-mtp-face          'whobegin          " ?Login +Group +Channel +Idle +On +For +C +From host")
	(nil                       'wholine           "(/w+):(name) +(/w+):(group) +([a-zA-Z0-9]+):(channel) +([/*0-9][0-9A-Za-z/*]+):(idle) +[^ ]+ +(.):(client)")
	(mtpchat-mtp-face          'whoend            "<Mtp> There {are|is} currently /d+ users?")
	
	(mtpchat-mtp-face          'finger            "Login +: (/w+):(nick)")
	(mtpchat-mtp-face          'sysalias          "<Mtp> System Aliases")
	(mtpchat-mtp-face          'usralias          "<Mtp> User Aliases")
	(mtpchat-mtp-face          'history           "<Mtp> History")
	(mtpchat-mtp-face          'uptime            "<Mtp> Uptime")
	(mtpchat-mtp-face          'wall              "<Mtp> Wall")
	(mtpchat-mtp-face          'helpfor           "<Mtp> Help for ")
	(mtpchat-mtp-face          'infobeg           "{ Login|Channel| Group|Command}")
	(nil                       'infosep           "-")
	(mtpchat-mtp-face          'infoend           "<Mtp> {There|End of}")
	(mtpchat-mtp-face          'you                       "<Mtp> You")
	(mtpchat-mtp-face          'getpass           "<Mtp> Password")
	(mtpchat-mtp-face          'servermoved       "<Mtp> Main server moved at ([^:]+):(server):(/d+):(port)/b")
	(mtpchat-mtp-face          'badserver         "<Mtp> Active server is at ([^:]+):(server):(/d+):(port)/b")
	(xxx                       'notloggedin       "<Mtp> User (/w+):(nick) is not logged in !")
	(mtpchat-mtp-face          'mtpdatas          "<Mtp> ")
	(mtpchat-emacs-face        'mtpdatas          "<Emacs> ")
	
	(xxx                       'kfilesendserver   "/|(/w+):(nick)/| /[Rainbow/]/[Send/]/[(.*):(filename)/]/[(/d+):(size)/]/[(/d+):(id)/]/[(.*):(ip)/]/[(/d+):(port)/]/[(.*):(password)/](.*):(text)$" )
	(xxx                       'kfilesend         "/|(/w+):(nick)/| /[Rainbow/]/[Send/]/[(.*):(filename)/]/[(/d+):(size)/]/[(/d+):(id)/](.*):(text)$" )
	(xxx                       'kfileacceptserver "/|(/w+):(nick)/| /[Rainbow/]/[Accept/]/[(/d+):(id)/]/[(.*):(ip)/]/[(/d+):(port)/]/[(.*):(password)/](.*):(text)$" )
	(xxx                       'kfileaccept       "/|(/w+):(nick)/| /[Rainbow/]/[Accept/]/[(/d+):(id)/](.*):(text)$" )
	(xxx                       'kfilerefuse       "/|(/w+):(nick)/| /[Rainbow/]/[Refuse/]/[(/d+):(id)/](.*):(text)$" )
	
	(xxx                       'ksenddatadrawline "/|(/w+):(nick)/| /}[lL] (/d+):(color) (/d+):(x1) (/d+):(y1) (/d+):(x2) (/d+):(y2) (/d+):(width)")
	(xxx                       'kchanneldrawline  "<(/w+)> /}[lL] (/d+):(color) (/d+):(x1) (/d+):(y1) (/d+):(x2) (/d+):(y2) (/d+):(width)")
	
	(xxx                       'kdatas            "/|(/w+):(nick)/| (....):(request) (.*):(data)$" )
	(xxx                       'senddata          "/|(/w+):(nick)/| (.*):(text)$" )
	
	(nil                       'sentence          "<(/w+):(nick)> (.*):(text)$")
	(nil                       'block             "[^<0-9]")
	))


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

;    (erc-put-text-property 0 (length mark-s) 'face msg-face str)
;   (erc-put-text-property (length mark-s) (+ (length mark-s) (length nick))
;			   'face nick-face str)
;    (erc-put-text-property (+ (length mark-s) (length nick)) (length str)
;			   'face msg-face str)
;; control-highlight: remote key code...
;; button...
;; add time stamp 
;;  --LEFT OR RIGHT

(defcustom mtpchat--post-insert-hook nil
  "Hook called after the modify hook, at this point the
text is finalized. No more modification should be done here."
  :group 'mtpchat-group
  :type 'hook)
;; modeline track

;; (set-window-dedicated-p window t)

(defcustom mtpchat-mode-hook nil
  "Hook run after `mtpchat-mode' setup is complete."
  :group 'mtpchat-group
  :type 'hook)



;;
;; Utility functions:
;;


(defun mtpchat--insert-data(mtpchat-buffer message)
  "Receiving data from the server, this function will cut these data 
into full lines. Non-full lines will not be processed for now."
  (let* ((strlist (split-string (concat mtpchat--incomplete-line-save message) "\n\r?")))
    (setq mtpchat--incomplete-line-save nil)
    (while (and strlist (cdr strlist))
      (when (> (length (car strlist)) 0)
	(mtpchat--insert mtpchat-buffer 'mtpchat-data (car strlist)))
      (setq strlist (cdr strlist)))
    ;; Last line is incomplete; stored for later...
    ;; exception: "<Mtp> Login:" or "<Mtp> Password:"
    (if (or (string-match "^<Mtp> Login: $" message)
	    (string-match "<Mtp> Password: $" message))
	(mtpchat--insert mtpchat-buffer 'mtpchat-data (car strlist))
	(setq mtpchat--incomplete-line-save (car strlist)))
	))

(defun mtpchat--insert(mtpchat-buffer type message)
  "Display the message MESSAGE in MTPCHAT-BUFFER, 
TYPE reprensents the type of message, currently two type are
supported:
 'mtpchat-system (internal display) and 'mtpchat-data (data
 received from the server)."
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
		(run-hooks 'mtpchat--post-insert-hook)))))))))

(defun mtpchat--message-to-buffer-list(message)
  "Return a list of buffer to display MESSAGE. 
This function may create new buffers."
  (list (get-buffer mtpchat--main-buffer-name))
)

(defun mtpchat--display(message)
  (let ((buffer-list (mtpchat--message-to-buffer-list message)))
    (while buffer-list
      (mtpchat--insert-data (car buffer-list) message)
      (setq buffer-list (cdr buffer-list)))))


(defun mtpchat--make-read-only()
  "Post insert hook functions: set the text property to be read-only"
  (add-text-properties (point-min) (point-max)
		       '(read-only t front-sticky t rear-nonsticky t)))


(defun mtpchat--buffer-name-to-notification-string(buffer)
  "Convert a buffer name to a notification string including text properties"
  (when (buffer-live-p buffer)
    (let* ((map (make-sparse-keymap))
	   (name (buffer-name buffer))
	   (str  (or (and (string= name mtpchat--main-buffer-name) "Mtp") 
		    (format "%c" (nth 5 (string-to-list (upcase name))))))) ;; (concat "*mtp-" (downcase nick) "*")
      (define-key map (vector 'mode-line 'mouse-3)
	`(lambda (e)
	   (interactive "e")
	   (save-selected-window
	     (select-window
	      (posn-window (event-start e)))
	     (switch-to-buffer ,buffer))))
      (put-text-property 0 (length str) 'local-map map str)
      (put-text-property 0 (length str)
			 'help-echo (concat "mouse-3: switch to buffer " name) str)
      (put-text-property 0 (length str)
			 'mouse-face 'mode-line-highlight str)
      str)))


(defun mtpchat--setup-notification()
  "Initialize the mtpchat notification string in the mode-line"
  (add-to-list 'mode-line-modes
	       '(t mtpchat--notification-string))
  (add-hook 'window-configuration-change-hook
	    'mtpchat--update-notification))

(defun mtpchat--kill-notification()
  "Cancel the mtpchat notification in the main line"
  (interactive)
  (setq mode-line-modes
	(remove '(t mtpchat--notification-string) mode-line-modes))
  (remove-hook 'window-configuration-change-hook
	       'mtpchat--update-notification))


(defun mtpchat--update-notification()
  "Update the notification text above the mode line"
  (setq mtpchat--hidden-modified-buffer-list
	(remove nil (mapcar '(lambda (buf)
			       (and (buffer-live-p buf)
				    (not (get-buffer-window buf 'visible))
				    buf))
			    mtpchat--hidden-modified-buffer-list)))
  (let* ((str-list (mapcar '(lambda (buf) (mtpchat--buffer-name-to-notification-string buf))
			   mtpchat--hidden-modified-buffer-list))
	 (str (and str-list (reduce '(lambda (s1 s2)
				       (cond ((not s2) s1)
					     ((not s1) s2)
					     ((string= s2 "Mtp") (concat s2 "-" s1))
					     ((string= s1 "Mtp") (concat s1 "-" s2))
					     (t (concat s1 s2))))
				    str-list))))
    (if (not str)
	(setq mtpchat--notification-string "")
	(if (and (= (length str) 4)
		 (string= str "Mtp-"))
	    (setq mtpchat--notification-string (concat "[" (substring str 0 3) "] "))
	    (setq mtpchat--notification-string (concat "[" str "] "))
    ))))


(defun mtpchat--add-to-notify-list()
  "Add the current buffer to the notification list if it has been modified. Also update the value of the variable"
  (when (/= (point-min) (point-max))
    (when (not (get-buffer-window (current-buffer) 'visible))
      (add-to-list 'mtpchat--hidden-modified-buffer-list (current-buffer))
      (mtpchat--update-notification))))


(defun mtpchat--send ()
  (interactive)
  (let ((to-send (buffer-substring-no-properties (marker-position mtpchat--input-start-marker)
						 (point-max))))
    (when (> (length to-send) 0)
      (delete-region (marker-position mtpchat--input-start-marker) (point-max))
      ;(mtpchat--insert 'mtpchat to-send)
      (tcp-send (get-buffer-process mtpchat--main-buffer-name) (concat mtpchat--prefix to-send "\n"))
      )))


(defun mtpchat--remove-away-time()
  (goto-char (point-min))
  (when (looking-at "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9] ")
    (delete-char 9)))


(defun mtpchat--update-topic()
  (goto-char (point-min))
  (when (/= (point-min) (point-max))
    (let ((offset (or (and (= (char-before (point-at-eol)) 13) -1) 0))
	  channel nick)
      (cond 
       ((looking-at mtpchat-regexp--topic)
	(forward-char 6)
	(setq channel (current-word))
	(search-forward "topic : ")
	;; Hack 1- is to get rid of the ^M 
	(setq mtpchat--topic (buffer-substring-no-properties (point) (+ (point-at-eol) offset))))
       ((looking-at mtpchat-regexp--topic-set)
	(forward-char 6)
	(setq nick (current-word))
	(search-forward "set channel ")
	(setq channel (current-word))
	(search-forward "topic to ")
	;; Hack 1- is to get rid of the ^M 
	(setq mtpchat--topic (buffer-substring-no-properties (point) (+ (point-at-eol) offset))))
       )
      (when channel
	;; Topic set!!! 
	(setq mtpchat--header-line (concat channel " topic: " mtpchat--topic))
	(when nick
	  (setq mtpchat--header-line (concat mtpchat--header-line " (set by " nick ")")))
	
	;; Update of the top line
	(setq header-line-format (concat (propertize " "
						     'display
						     '(space :align-to 0))
					 mtpchat--header-line))
	))))
      


(defun mtpchat--reformat-mail-line()
  (goto-char (point-min))
  (when (looking-at mtpchat-regexp--mail)
    ;; By default they consider a maximum of 99 mails for alignment, increase the limit to 999
    (if (not (looking-at "^[0-9][0-9][0-9]"))
	(insert " "))
    ;; Align the ':'
    (search-forward-regexp " :")
    (let ((missing-spaces (- 32 (current-column))))
      (when (> missing-spaces 0)
	(move-to-column 22)
	(insert (make-string missing-spaces 32))))
    ;; Remove the date/time + add tooltip on the #
    (add-text-properties (point-min) (+ (point-min) 3)
			 (list 'help-echo 
			       (buffer-substring-no-properties (+ (point-min) 4)
							       (+ (point-min) 22))
			       'face mtpchat-mail-number))
    (add-text-properties (+ (point-min) 4) (+ (point-min) 22)
			 (list 'invisible t))
    ;; Change the face of the nickname:
    (add-text-properties (+ (point-min) 23) (+ (point-min) 31)
			 (list 'face mtpchat-wall-mail-nick))
			       

    ;; Fill the region:
    (let ((fill-column 80)
	  (fill-prefix (make-string 15 32)))
      (fill-region (point-min) (point-max) t t))
    ;;
    ))

(defun mtpchat--reformat-wall-line()
  (goto-char (point-min))
  (when (looking-at mtpchat-regexp--wall)
    (add-text-properties (point-min) (+ (point-min) 17)
			 (list 'display "*"
			       'help-echo (buffer-substring-no-properties (point-min) (+ (point-min) 17))
			       'face mtpchat-wall-star))
    (forward-char 18)
    (add-text-properties (+ (point-min) 17)
			 (+ (point-min) 18 (length (current-word)))
			 (list 'face mtpchat-wall-mail-nick))

    (let ((fill-column 80)
	  (fill-prefix (make-string 11 32))) ; Nick size + '*' + 2 spaces 
      (fill-region (point-min) (point-max) t t))))

(defun mtpchat--reformat-chat-line()
  (goto-char (point-min))
  (when (looking-at "^<\\w+> ")
    (search-forward "> ")
    (let ((fill-column 80)
	  (fill-prefix (make-string (current-column) 32))) ; 34 alignment + 10 timestamps
      (fill-region (point-min) (point-max) t t))))


(defun mtpchat--intercept-private-message()
  (goto-char (point-min))
  (let (nick user msg-eol buf-name buffer)
    (cond ((looking-at mtpchat-regexp--you-tell) 
	   (search-forward ":")
	   (setq nick (current-word))
	   (setq user (or mtpchat--login "You"))
	   (forward-char 1)
	   (setq msg-eol (buffer-substring-no-properties (point) (point-max))))
	  ((looking-at mtpchat-regexp--private-tell)
	   (forward-char 6)
	   (setq nick (current-word))
	   (setq user nick)
	   (search-forward ":")
	   (forward-char 1)
	   (setq msg-eol (buffer-substring-no-properties (point) (point-max))))
	  ((looking-at mtpchat-regexp--tell-away)
	   (forward-char 6)
	   (setq nick (current-word))
	   (setq msg-eol (buffer-substring-no-properties (point-min) (point-max)))
	  ))
    (when nick
      (setq buf-name (concat "*mtp-" (downcase nick) "*"))
      (setq buffer (get-buffer buf-name)))
    (when (and nick msg-eol)
      (when (not buffer)
	(setq buffer (get-buffer-create buf-name))
	(with-current-buffer buffer
	  ;; Setup the mtpchat-mode
	  (mtpchat-mode "PRIV> " (concat "tell " nick " "))))
      (delete-region (point-min) (point-max))
      (if user 
	  (mtpchat--insert-data buffer (concat "<" user "> " msg-eol))
	  ;; Currently no display support regarding the 'away' message...
	  )
      )))



(defun mtpchat--add-local-time()
  (goto-char (point-min))
  (when (/= (point-min) (point-max))
    (let ((curtime (current-time))
	  (fts (format-time-string "[%H:%M:%S]" (current-time))))
      (insert fts)
      (add-text-properties (point-min) (point)
			   '(face mtpchat-time-stamp-face))
      (insert " ")
      (add-text-properties (point-min) (point)
			   '(intangible t front-sticky t rear-nonsticky t))
      (forward-line 1)
      (while (not (equal (point) (point-max)))
	(forward-line 0)
	(insert fts)
	(add-text-properties (point-at-bol) (point)
			     '(face mtpchat-time-stamp-face))
	(insert " ")
	(add-text-properties (point-at-bol) (point)
			     '(intangible t front-sticky t rear-nonsticky t))
	
					;(insert (make-string 11 32))
					;(add-text-properties (point-at-bol) (point)
					;		   '(intangible t front-sticky t rear-nonsticky t))
	(forward-line 1))
      )))
  

(defun mtpchat--auto-login(type msg)
  (when (and (string-match "^<Mtp> Login: $" msg)
	     mtpchat--login)
    (tcp-send (get-buffer-process mtpchat--main-buffer-name) (concat mtpchat--login "\n")))
  (when (and (string-match "^[^<]*<Mtp> Password: $" msg)
	     mtpchat--passwd)
    (tcp-send (get-buffer-process mtpchat--main-buffer-name) (concat mtpchat--passwd "\n")))
  (when (string-match "^<Mtp> Welcome, " msg)
    (tcp-send (get-buffer-process mtpchat--main-buffer-name) "set client zzz .o(v0.5)\n")
    (remove-hook 'mtpchat--validate-message-hook 'mtpchat--auto-login)))



(defun mtpchat--fontify ()
  (goto-char (point-min))
  (cond ((looking-at mtpchat-regexp--you-tell) 
	 (add-text-properties (point-min) (point-max) '(face mtpchat-you-tell-face)))
	
	((looking-at mtpchat-regexp--private-tell) 
	 (add-text-properties (point-min) (point-max) '(face mtpchat-private-tell-face)))

	((looking-at "^<Mtp> \\*")
	 (add-text-properties (point-min) (point-max) '(face mtpchat-emote-face)))

	((looking-at mtpchat-regexp--tell-away)
	 (add-text-properties (point-min) (point-max) '(face mtpchat-away-face)))

	((looking-at "^<Mtp>")
	 (add-text-properties (point-min) (point-max) '(face mtpchat-mtp-face)))

	((looking-at "^<Emacs>")
	 (add-text-properties (point-min) (point-max) '(face mtpchat-emacs-face)))

	((looking-at "^<Emacs>")
	 (add-text-properties (point-min) (point-max) '(face mtpchat-emacs-face)))

	((looking-at "^<Kandjar>")
	 (add-text-properties (point-min) (point-max) '(face mtpchat-your-line-face)))

	((looking-at "kandjar")
	 (add-text-properties (point-min) (point-max) '(face mtpchat-your-name-face)))
   ))

;; Define the key mapping for the mtpchat mode:
(defvar mtpchat-mode-map
  (let ((mtpchat-mode-map (make-keymap)))
    (define-key mtpchat-mode-map [return] 'mtpchat--send)
    (define-key mtpchat-mode-map [?\r] 'mtpchat--send)

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


(defun mtpchat--scroll-to-bottom (window display-start)
  "Scroll to the bottom of the display when the user is typing some texts...
Function added to `window-scroll-functions' by mtpchat-mode"
  (if (window-live-p window)
      (if (> (point) mtpchat--marker)
	  (save-excursion
	    (goto-char (point-max))
	    (recenter (- (window-body-height window) 3))
	    (sit-for 0)))))


(defun mtpchat-mode(&optional prompt prefix)
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
  (make-variable-buffer-local 'mtpchat--marker)
  (make-variable-buffer-local 'mtpchat--prefix)
  (make-variable-buffer-local 'mtpchat--input-start-marker)

  ;; Private buffer has a prefix
  (setq mtpchat--prefix prefix)

  ;; Get rid of the ^M at the end of the lines:
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\r [])
  ;(aset buffer-display-table ?\240 ?\a)

  ;; Indent with space only:
  (setq indent-tabs-mode nil)

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
		       (1- (point-max))
		       '(face mtpchat-prompt-face))
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

  (add-hook 'mtpchat--modify-hook 'mtpchat--fontify)
  (when (not prefix)
    (add-hook 'mtpchat--modify-hook 'mtpchat--update-topic)
    (add-hook 'mtpchat--modify-hook 'mtpchat--intercept-private-message))
  (add-hook 'mtpchat--modify-hook 'mtpchat--remove-away-time)

  (when (not prefix)
    (add-hook 'mtpchat--modify-hook 'mtpchat--reformat-mail-line t)
    (add-hook 'mtpchat--modify-hook 'mtpchat--reformat-wall-line t))

  (add-hook 'mtpchat--modify-hook 'mtpchat--reformat-chat-line t)
  (add-hook 'mtpchat--modify-hook 'mtpchat--add-local-time t)

  (add-hook 'mtpchat--post-insert-hook 'mtpchat--make-read-only)
  (add-hook 'mtpchat--post-insert-hook 'mtpchat--add-to-notify-list)
  ;; 

  (add-to-list 'mtpchat--buffer-list (buffer-name))
  ;; (member/delete/add-to-list)

  ;; Finally, let's run the user mode-hooks 
  (run-hooks 'mtpchat-mode-hook)
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
					:filter-handler 'mtpchat--filter))
	(server (read-string "Server: " (get-tcp-connection-server mtpchat--connection) nil  
			     (get-tcp-connection-server mtpchat--connection)))
	(login (read-string "Login: " (user-login-name)))
	(pwd   (read-passwd "Password: ")))
    (set-tcp-connection-server mtpchat--connection server)
    (setq mtpchat--login  login)
    (setq mtpchat--passwd pwd)
    (get-buffer-create mtpchat--main-buffer-name)
    ;; Clear the list of buffer... 
    (setq mtpchat--buffer-list nil)
    (switch-to-buffer (get-buffer mtpchat--main-buffer-name))
    ;(save-excursion 
      ;(set-buffer mtpchat--main-buffer-name)
      ;; Setup the mtpchat-mode
      (mtpchat-mode)
      (tcp-connect mtpchat--main-buffer-name mtpchat--connection mtpchat-hooks)))

(provide 'mtpchat)
