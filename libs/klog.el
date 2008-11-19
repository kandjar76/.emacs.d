;; Log mode...

;; Provide a set of function to log.
;; 
;; klog-header( title)
;; klog-printf(type format ...)
;; klog-push(type format ...)
;; klog-pop()
;; klog-new-line(type ...)
;; 
;; A log file will look like:
;;
;;  12/06/06 - 12:06PM Log : My log file to test those features...
;;  --------------------------------------------------------------------------------
;;  [info] `- first log entry 
;;  [info] `- second log entry
;;  [warn] `- small issue
;;  [errs] `- a std error
;;  [crit] `- a major issue
;;  [info] `- entering a block
;;  [info] |   `- log within this block
;;  [info] |   `- second log within the same block
;;  [errs] `- a std error
;;  [crit] `- a major issue
;;  [info] `- entering a block
;;  [info] |   `- log within this block
;;  [info] |   `- second log within the same block
;;  [info] |   `- second log within the same block
;;  [info] |   |   `- second log within the same block
;;  [info] |   |   `- second log within the same block
;;  [info] |   |   `- second log within the same block
;;  [info] |   `- second log within the same block
;;  [todo] `- A todo item
;;  [note] `- Just a comment

;;  [info] `- first log entry 
;;  [info] `- second log entry
;;  [warn] `- small issue
;;  [errs] `- a std error
;;  [crit] `- a major issue
;;  [info] `- entering a block
;;  [info] |   `- log within this block
;;  [info] |   `- second log within the same block
;;  [errs] `- a std error
;;  [crit] `- a major issue
;;  [info] `- entering a block
;;  [info] |   `- log within this block
;;  [info] |   `- second log within the same block
;;  [info] |   `+ second log within the same block
;;  [info] |   `- second log within the same block
;;  [todo] `- A todo item
;;  [note] `- Just a comment

(require 'log-buffer)
(require 'enum-type)

(defun make-new-klog-buffer (buffer-name)
  "Create a klog buffer, called BUFFER-NAME
If BUFFER-NAME already exist, this function will set the read-only flag, 
and setup the klog mode for this buffer"
  (let ((klog-buffer (make-new-log-buffer buffer-name)))
    (set-buffer klog-buffer)
    (klog-mode)))


;(make-new-klog-buffer "*klog-test*")

