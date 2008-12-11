set-process-coding-system
cache-long-line-scans

// Removing the \r at display time
(setq buffer-display-table (make-display-table))
(aset buffer-display-table ?\r [])


http://www.google.com



(setq button-url-regexp
      "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")
(bounds-of-thing-at-point 


;; Including load-path subdirectories:
 (setq load-path (cons my-lisp-dir load-path))
 (normal-top-level-add-subdirs-to-load-path)

;; Including specific load-path subd:
(normal-top-level-add-to-load-path
 '("emms" "erc" "planner" "w3"))

;; Finding library:
M-x locate-library
M-x list-load-path-shadows

;; Fun: 
(animate-string "fun fun fun!!!" <line> <col>)