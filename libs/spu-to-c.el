
(require 'apply-on-region)

(defun spu-to-c--args(list)
  "Convert the list of argument into a string such as: arg0, arg1, arg2, ..."
  (reduce 'concat (append (list (car list)) 
			  (mapcar '(lambda(x) (concat ", " x)) 
				  (cdr list)))))


(defun spu-to-c--write-instr(instr)
  "Insert the C function corresponding to the spu instruction"
  ;; General case:
  (insert (format "const qword %s = si_%s(%s);" 
		  (cadr instr) 
		  (car instr) 
		  (spu-to-c--args (cddr instr)))))

(defun spu-to-c--line(start end)
  "Convert spu code into C code using si_ instrinsics"
  (let (odd even)
    (when (looking-at ".*{o[0-9]")
      (end-of-line)
      (setq odd  (spu-extract-instruction)))
    (beginning-of-line)
    (when (looking-at ".*{e[0-9]")
      (setq even (spu-extract-instruction)))
    (delete-region start end)
    (when even (spu-to-c--write-instr even))
    (when (and even odd) (insert "\n"))
    (when odd  (spu-to-c--write-instr odd))
    ))

(defun spu-to-c(start end)
  (interactive "r")
  (if mark-active
      (apply-on-region-lines start end 'spu-to-c--line)
      (message "No region selected.")))
