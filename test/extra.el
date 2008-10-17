set-process-coding-system
cache-long-line-scans

// Removing the \r at display time
(setq buffer-display-table (make-display-table))
(aset buffer-display-table ?\r [])


http://www.google.com



(setq button-url-regexp
      "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")
(bounds-of-thing-at-point 