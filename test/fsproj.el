;;
;; fsproj - File System Project
;;

(require 'project-buffer-menu)
(require 'record)

;; TODO
;;
;; - [x] Extract the whole file list
;; - [ ] Detect the projects
;; - [ ] Create a list of files associated to their project 
;; - [ ] Keep the relative path based on the project 'root' folder
;; - [ ] Remap the files
;; - [ ] Create the project buffer window
;; - [ ] Add the files to it
;; - [ ] Create the reload project function, map it to 'g


;;
;; Structure definition:
;;

(defrecord fsproj-def
  "Structure to create a fsproj"
  :root-folder    'stringp   ;; where the recursive file search start (e.g: "~/work")
  :file-filter    'listp     ;; which files to include in the project (e.g: '("\.h$" "\.cpp$"))
  :proj-filename  'stringp   ;; name of the file which will determine where the project start (e.g: "[Mm]akefile" or "prj/Makefile")
  :ignore-folder  'stringp   ;; which folder to ignore (eg: '("temp" "build")
  :command-hook   'functionp ;; function which will be called to build the project.
  :remap-patterns 'listp     ;; list of remapping pattern (e.q: '( (".*/include" . "include") ("source/\(.*\)$" "\1")) -- could be nil
)



;;
;;  User
;;


(defun fsproj-collect-files(root file-filter &optional ignore-folders)
  "Parse the ROOT folder and all of it's sub-folder, and create a file list.
FILE-FILTER is a list of regexp which are used to filter the file list.
If IGNORE-FOLDERS is non nil, it should specify a list of folder name to ignore."
  (let ((dir-list (directory-files-and-attributes root t))
	(ign-reg  (regexp-opt ignore-folders))
	file-list)
    (while dir-list
      (let* ((cur-node (pop dir-list))
	     (filename (car cur-node))
	     (is-dir   (eq (car (cdr cur-node)) t))
	     (is-file  (not (car (cdr cur-node))))
	     (basename (file-name-nondirectory filename)))
	(cond 
	 ;; if the current node is a directory different from "." or "..", all it's file gets added to the list
	 ((and is-dir
	       (not (string-equal basename "."))
	       (not (string-equal basename ".."))
	       (or (not ignore-folders)
		   (not (string-match ign-reg basename))))
	       (setq dir-list (append dir-list (directory-files-and-attributes filename t))))
	 ;; if the current node is a file: check with the filter, if it pass the check, it 
	 ((and is-file
	       (some '(lambda (item) 
			;(message "checking: %s and %s" basename item)
			(string-match item basename)) file-filter))
	  (setq file-list (append file-list (list filename)))))))
    file-list
    ))

