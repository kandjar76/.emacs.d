;;
;;
;; Mtp Mode:
;;
;;


;; Mtp Chat customization:
(setq mtp-server     "zen.dtdns.net")
(setq mtp-port       4000)
(setq mtp-login      "kandjar")
(setq mtp-password   "")


;; Extra data:
(defconst mtp-process      "MtpChat")
(defconst mtp-main-buffer  "*MtpChat*")

(defun mtp-start()
  (interactive)
  (open-network-stream mtp-process mtp-main-buffer mtp-server mtp-port))

(defun mtp-close()
  (interactive)
  (delete-process mtp-process))

(defun mtp-login()
  (interactive)
  (process-send-string mtp-process mtp-login)
  (process-send-string mtp-process "\r\n")
  (process-send-string mtp-process mtp-password)
  (process-send-string mtp-process "\r\n"))

(defun mtp-send(data-to-send)
  (interactive "sEnter the string to send: ")
  (process-send-string mtp-process data-to-send)
  (process-send-string mtp-process "\r\n"))

