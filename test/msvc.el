(require 'project-buffer-mode)
;; 
;; Sample code / Notes...
;; 


;(defun project-buffer-refresh-nodes(status)
;  "Refresh displayed buffer"
;  (ewoc-map (lambda (data) t)
;	    status
;	    ))
  
;;(ewoc-data (ewoc-locate project-buffer-status))
;;(ewoc-invalidate project-buffer-status pos)
;;(ewoc-goto-prev project-buffer-status 1)
;;(ewoc-goto-next project-buffer-status 1)




;;(split-string "/test/blah/" "/")

;      (when proj-found
;	(let ((folder-data (project-buffer-extract-folder (project-buffer-node->name node-data) (project-buffer-node->type node-data))))
;	  (split-string
;	  ))

;;(compare-strings "abcdef" nil nil "abc" nil nil)


;;
;; Test commands:
;;


(defun test-projbuff()
  (interactive)
  (let ((buffer (generate-new-buffer "test-project-buffer")))
    (display-buffer buffer)
    (with-current-buffer buffer
      (cd "~/temp")
      (project-buffer-mode)

      (project-buffer-insert project-buffer-status (project-buffer-create-node "test1" 'project "test1.sln" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "src/gfr.cpp" 'file  "~/temp/gfr.cpp" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "src/abc.cpp" 'file  "~/temp/abc.cpp" "test1"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "test2" 'project "test2.sln" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "header/zzz.h" 'file  "~/temp/zzz.h" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "src/roo.c" 'file  "~/temp/roo.c" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "script.awk" 'file "~/temp/script.awk" "test2"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "header/xtra.h" 'file "~/temp/xtra.h" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "blah.h" 'file "~/temp/blah.h" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "src/zzz.cpp" 'file  "~/temp/zzz.cpp" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "aha.h" 'file "~/temp/aha.h" "test1"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "other" 'project  "other.sln" "other"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "test.h" 'file "~/temp/test.h" "other"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "src/apl.c" 'file  "~/temp/apl.c" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "src/foo.cpp" 'file  "~/temp/foo.c" "other"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "test2.h" 'file "~/temp/test2.h" "other"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "fold" 'project  "fold.sln" "fold"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc/dee/test.c" 'file  "~/test.c" "fold"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc/dee/grr.c" 'file  "~/grr.c" "fold"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc/testw.c" 'file  "~/testw.c" "fold")) 
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc/rrr/rdf.c" 'file  "~/rdf.c" "fold"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc/def/gla.c" 'file  "~/gla.c" "fold"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "blue/green/red.c" 'file  "~/red.c" "fold"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc/rrr/gth.c" 'file  "~/gth.c" "fold"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc/rrr/zgth.c" 'file  "~/zgth.c" "fold"))
)))

(defun test-projbuff-old()
  (interactive)
  (let ((buffer (generate-new-buffer "test-project-buffer")))
    (display-buffer buffer)
    (with-current-buffer buffer
      (project-buffer-mode)

      (project-buffer-insert project-buffer-status (project-buffer-create-node "test1" 'project "test1.sln" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "gfr.cpp" 'file  "~/temp/gfr.cpp" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc.cpp" 'file  "~/temp/abc.cpp" "test1"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "test2" 'project "test2.sln" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "zzz.h" 'file  "~/temp/zzz.h" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "roo.c" 'file  "~/temp/roo.c" "test2"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "xtra.h" 'file "~/temp/xtra.h" "test1"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "other" 'project  "other.sln" "other"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "apl.c" 'file  "~/temp/apl.c" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "foo.cpp" 'file  "~/temp/foo.c" "other"))
)))





(defun sln-extract-project(sln-file)
  "Extract projects from the SLN file"
  (save-excursion
    (with-temp-buffer
      (insert-file sln-file)
      (goto-char (point-min))
      (let ((result nil))
	(while (re-search-forward "Project(\"{[-A-Z0-9]+}\")[ 	]+=[ 	]+\"\\([A-Za-z0-9_]+\\)\"[ 	]*,[ 	]+\"\\([\\A-Za-z0-9_.]+\\)\""
				  (point-max)  t) 
	  (add-to-list 'result (cons (match-string-no-properties 1) (match-string-no-properties 2))))
	result))))

(defun create-project-buffer(sln-file)
  "Create a project buffer"
  (let ((buffer (create-file-buffer sln-file))
	(sln-projects (sln-extract-projects sln-file))
	current) ;; list of proj-nane / project file
    (while sln-projects
      (setq current (pop sln-projects))
      (vcproj-extract 
    ;;(switch-to-buffer buffer)
    ;;(project-buffer-mode)
    buffer))))