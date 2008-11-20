;;
;; In order to use this function: 
;;   you need to add a stopd just after the loop you want to estimate
;;

(defun occluder-test()
  (interactive)
  (let ((buffer (get-buffer-create "*occ*")))
    (with-current-buffer buffer
      (spu-mode)
      (erase-buffer))
    (call-process-shell-command 
       "spu-lv2-objdump -d ~/ship/edge/target/spu/src/edge/geom/edgegeom_occlusion.o" 
;       "spu-lv2-objdump -d ~/ship/edge/target/spu/src/edge/geom/edgegeom_transform.o" 
       nil buffer t)
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward "stopd")
      (save-excursion
	(end-of-line)
	(delete-region (point) (point-max)))
      (search-backward "\tbr")
      (search-forward-regexp "# ")
      (let ((addr (current-word)))
	(search-backward-regexp (concat "^ *" addr ":"))
	(delete-region (point-min) (point)))
      (call-process-region
       (point-min)
       (point-max)
       gawk-executable-path t t t 
       "-f" gawk-script-spu-instruction-path
       "-f" gawk-script-format-dump-file-path)
      (dos-to-unix)
      (spu-nopify (point-min) (point-max))
      (font-lock-fontify-buffer)
      )
    (display-buffer buffer)))
