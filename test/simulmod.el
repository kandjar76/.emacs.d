
;;
;; Idea: use: Ctrl Z to mark the beginning and the end of the region to modify

;; C-u c-z to cancel the marked area
;; c-z c-z to mark the current work 
;; c-z a to search for all matching region as the current/last...

;; Do we allow that accross buffers???

;; Select area + Ctrl-Z to mark that area
;;
;;
;; Once multiple area are selected, any change in this area will apply the same changes in the others


(defun simmod--mark-set 