;; Author: Cedric Lallain
;;
;; The purpose of this library is to provide a network interface to ease the creation of 
;; network related script.
;; 
;; This library is dependending on struct-type.el


(require 'record-type)


(defrecord tcp-hooks
  "TCP-Client Handlers"
  :connection-established-handler 'functionp ;; (<call> buffer server port)
  :connection-failed-handler      'functionp ;; (<call> buffer server port message)
  :connection-abort-handler       'functionp ;; (<call> buffer server port)
  :sentinel-handler               'functionp ;; (<call> process event) 
  :filter-handler                 'functionp ;; (<call> process received-data)
)

(defrecord tcp-connection
  "Handle a tcp connection"
  :server 'stringp
  :port 'integerp
  :keep-alive 'atom)

(defun tcp-connect (buffer-name connection hooks)
  "Try to established a connection on a specific server

BUFFER-NAME refers to the name of the current buffer to handle the connection.
If the buffer exist, it's going to be displayed, if not, it's first going to be
created and then displayed.

CONNECTION is a record of the type tcp-connection which should contain 
the connection information.

HOOKS is also a record whose type is: tcp-hooks. It contains all different handlers 
you may want to set to intercept the connection data."
  (cond ((not (stringp          buffer-name))          (error "tcp-connect error: Invalid type -- BUFFER-NAME must be a string."))
	((not (tcp-connection-p connection))           (error "tcp-connect error: Invalid type -- CONNECTION's type must be: tcp-connection"))
	((not (tcp-hooks-p      hooks))                (error "tcp-connect error: Invalid type -- HOOKS's type must be: tcp-hooks"))
	((null (get-tcp-connection-server connection)) (error "tcp-connect error: Invalid server name (nil)"))
	((null (get-tcp-connection-port   connection)) (error "tcp-connect error: Invalid server port (nil)"))
	(t (let* ((buffer             (get-buffer-create buffer-name))
		  (process            (get-buffer-process buffer))
		  (server             (get-tcp-connection-server connection))
		  (port               (get-tcp-connection-port   connection))
		  (proc-name          (format "tcp-connection:%s:%i" server port))
		  (abort-handler      (get-tcp-hooks-connection-abort-handler hooks))
		  (error-handler      (get-tcp-hooks-connection-failed-handler hooks))
		  (connection-handler (get-tcp-hooks-connection-established-handler hooks))
		  (sentinel-handler   (get-tcp-hooks-sentinel-handler hooks))
		  (filter-handler     (get-tcp-hooks-filter-handler hooks)))
	     (progn (display-buffer buffer)
		    (sit-for 0) ;; force redisplay
		    (if process
			(progn (delete-process process)
			       (setq process 0)))
		    (condition-case data
			(setq process (open-network-stream proc-name buffer server port))
			(quit (and abort-handler
				   (funcall abort-handler buffer server port)))
			(file-error (cond ((string= (cadr data) "connection failed")
					(and error-handler
						(funcall error-handler buffer server port (caddr data))))
					  ((string= (cadr data) "make client process failed")
					   (and error-handler
						(funcall error-handler buffer server port (caddr data))))
					  (t (signal (car data) (cdr data)))))
			(error (if (and (stringp (cadr data))
					(string-match "^Unknown host" (cadr data)))
					(and error-handler
					(funcall error-handler buffer server port (cadr data)))
				 (apply 'error data))))
		    (if process 
			(progn (if sentinel-handler
				   (set-process-sentinel process sentinel-handler))
			       (if filter-handler
				   (set-process-filter process filter-handler))
			       (if (get-tcp-connection-keep-alive connection)
				   (set-network-process-option process :keepalive t))
			       (set-process-buffer process buffer)
		    (and connection-handler
			 (funcall connection-handler buffer server port))
			       )))))))


(defun tcp-send(process data)
  (process-send-string process data))

(defun tcp-kill(process)
  (delete-process process))

;(defun tcp-default-keep-alive()
;  (featurep 'make-network-process '(:keepalive t)))


(provide 'tcp-client)




;;(defun tmp-error-report(buffer server port error)
;;  (save-excursion
;;    (set-buffer buffer)
;;    (insert (format "[error] %s:%i -- %s\n" server port error))))
;;
;;(defun tmp-connection-report(buffer server port)
;;  (save-excursion
;;    (set-buffer buffer)
;;    (insert (format "[connect] %s:%i -- Connection established\n" server port))))
;;
;;(defun tmp-abort-report(buffer server port)
;;  (save-excursion
;;    (set-buffer buffer)
;;    (insert (format "[error] %s:%i -- Abort connection\n" server port))))
;;
;;(defun tmp-sentinel-report(process event)
;;  (save-excursion
;;    (set-buffer (process-buffer process))
;;    (insert (format "[event] Process: %s had the event -- %s" process event))))
;;
;;(defun tmp-filter-report(process message)
;;  (save-excursion
;;    (set-buffer (process-buffer process))
;;    (insert (format "[got] %s" message))))
;;
;;
;;(tcp-connect "*ok*" 
;;	     (make-new-record tcp-connection :server "150.0.2.157" :port 8530 :keep-alive t)
;;	     (make-new-record tcp-hooks 
;;			      :connection-failed-handler 'tmp-error-report
;;			      :connection-established-handler 'tmp-connection-report
;;			      :connection-abort-handler 'tmp-abort-report))
;;
;;(tcp-connect "*mtp*" 
;;	     (make-new-record tcp-connection :server "zen.dtdns.net" :port 4000 :keep-alive t)
;;	     (make-new-record tcp-hooks 
;;			      :connection-failed-handler 'tmp-error-report
;;			      :connection-established-handler 'tmp-connection-report
;;			      :connection-abort-handler 'tmp-abort-report
;;			      :sentinel-handler 'tmp-sentinel-report
;;			      :filter-handler 'tmp-filter-report))