;; Log mode...

;; Provide a set of function to log.
;; 
;; klog-header( title)
;; klog-printf(type format ...)
;; klog-push(type format ...)
;; klog-pop(type format ...)
;; klog-new-line(type ...)
;; 
;; A log file will look like:
;;
;;  12/06/06 - 12:06PM Log : My log file to test those features...
;;  --------------------------------------------------------------------------------
;;  [info] -> first log entry 
;;  [info] -> second log entry
;;  [warn] -> small issue
;;  [errs] -> a std error
;;  [crit] -> a major issue
;;  [info] +> entering a block
;;  [info] | -> log within this block
;;  [info] | -> second log within the same block
;;  [info] +> end of block
;;  [errs] -> a std error
;;  [crit] -> a major issue
;;  [info] +> entering a block
;;  [info] | -> log within this block
;;  [info] | -> second log within the same block
;;  [info] | +> second log within the same block
;;  [info] | | -> second log within the same block
;;  [info] | | -> second log within the same block
;;  [info] | | -> second log within the same block
;;  [info] | +> second log within the same block
;;  [info] +> end of block
;;  [todo] -> A todo item
;;  [note] -> Just a comment


;(defun klog-header(buffer title)
;  (