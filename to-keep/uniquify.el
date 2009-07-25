(require 'cl)

(defun uniquify (strings)
  "Take a list of strings, and mangle them slightly so they're unique."
  (let ((table (make-hash-table :test 'equal)))
    (loop for s in strings collect 
          (let ((count (gethash s table)))
            (if (null count)
                (puthash s 0 table)
                (puthash s (1+ count) table))
            (if (null count)
                s
                (format "%s%d" s (1+ count)))))))

(uniquify '("aaa" "bb" "ccc" "ccc" "ccc"))

