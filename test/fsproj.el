;;
;; fsproj - File System Project
;;

(require 'cl)
(require 'project-buffer-menu)
(require 'record)

;; TODO
;;
;; - [x] Extract the whole file list
;; - [X] Detect the projects
;; - [X] Create a list of files associated to their project 
;; - [X] Keep the relative path based on the project 'root' folder
;; - [X] Remap the files
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

;; File-Filter regexp list applied to basename only
;; Proj-Regexp regexp applied to the full path
;; Ignore-Folder string list


;;
;;  User
;;


(defun fsproj-collect-files(root project-regexp file-filter &optional ignore-folders)
  "Parse the ROOT folder and all of it's sub-folder, and create a file list.
FILE-FILTER is a list of regexp which are used to filter the file list.
PROJECT-REGEXP should represent a regular expression which will help finding the project folders
If IGNORE-FOLDERS is non nil, it should specify a list of folder name to ignore.

The return is a list of two lists: ((project...) (files...))
Note: the project list is sorted in descending alphabetic order."
  (let ((dir-list (directory-files-and-attributes root t))
	(ign-reg  (regexp-opt ignore-folders))
	file-list proj-list)
    (while dir-list
      (let* ((cur-node (pop dir-list))
	     (fullpath (car cur-node))
	     (is-dir   (eq (car (cdr cur-node)) t))
	     (is-file  (not (car (cdr cur-node))))
	     (basename (file-name-nondirectory fullpath)))
	(cond 
	 ;; if the current node is a directory different from "." or "..", all it's file gets added to the list
	 ((and is-dir
	       (not (string-equal basename "."))
	       (not (string-equal basename ".."))
	       (or (not ignore-folders)
		   (not (string-match ign-reg basename))))
	       (setq dir-list (append dir-list (directory-files-and-attributes fullpath t))))
	 ;; if the current node is a file
	 (is-file
	  ;; check against the file filter, if it succeed: add the file to the file-list
	  (when (some '(lambda (item) (string-match item basename)) file-filter)
	    (setq file-list (cons fullpath file-list)))
	  ;; check also against the project-regexp: if succeed, we had the base directory of the project of the project list
	  ;; (including the final '/')
	  (let ((pos (string-match project-regexp fullpath)))
	    (when pos
	      (setq proj-list (cons (cons (file-name-directory (substring fullpath 0 pos)) fullpath) proj-list)))
	  )))))
    (cons (sort proj-list '(lambda (a b) (string-lessp (car a) (car b)))) file-list)))


(defun fsproj-extract-project-file-list(current-project file-list)
  "Extract the file which belongs to CURRENT-PROJECT from FILE-LIST.
Return a list of two lists: ((current project file list..) (remaining files...)."
  (let (project-file-list
	remaining-files
	(lgt (length current-project)))
    (while file-list
      (let ((current-file (pop file-list)))
	(if (string-equal (substring current-file 0 lgt) current-project)
	    (setq project-file-list (cons current-file project-file-list))
	    (setq remaining-files (cons current-file remaining-files)))))
    (cons project-file-list remaining-files)))


(defun fsproj-parent-of-p(child parent)
  "Check if CHILD is a child of the directory PARENT.
CHILD and PARENT are two string representing directories."
  (let* ((clist (and child  (split-string child "/")))
	 (plist (and parent (split-string parent "/")))
	 (cont t)
	 res)
    (while (and clist plist cont)
      (let ((cname (pop clist))
	    (pname (pop plist)))
	(setq cont (string-equal cname pname))))
    (and cont (null plist))))
  

(defun fsproj-resolve-conflict(conflict-list)
  "Solve the CONFLICT-LIST and return the list of final names.
The code assume that no folders will be named with a '(n)' suffix."
  (let* ((name-check (make-hash-table :test 'equal))
	 (name-list  (mapcar (lambda (node) (let* ((prj  (file-name-nondirectory (car node)))
						   (sub  (cdr node))
						   (name (if sub (concat sub "/" prj) prj))
						   (cnt  (gethash name name-check)))
					      (if cnt 
						  (setq cnt (1+ cnt)) 
						  (setq cnt 1))
					      (puthash name cnt name-check)
					      (format "%s (%i)" name cnt)))
			     conflict-list)))
    (mapcar (lambda (name) (if (string-match " (1)$" name)
			       (let ((subname (substring name 0 (- (length name) 4))))
				 (if (= (gethash subname name-check) 1)
				     subname
				     name))
			       name))
	    name-list)))
					  
(defun fsproj-generate-project-names(project-list)
  "Return a list of project names based on the project paths contained in PROJECT-LIST.
Making sure each name is uniq. This function will also detect subproject and add the master project name as prefix.
PROJECT-LIST should be a list of couple: (project-path . project-file-name)"
  (let ((project-base-list (mapcar (lambda (path) (substring (car path) 0 -1)) project-list))
	(project-name-list (mapcar (lambda (path) (file-name-nondirectory (substring (car path) 0 -1))) project-list))
	(project-ht (make-hash-table :test 'equal))
	subproject-list)

    ;; Extract the subproject list:
    (let ((path-list project-base-list)
	  subprojects
	  sub-list)
      (while path-list
	(let ((current (pop path-list))
	      subproj)
	  (while (and (not subproj) subprojects)
	    (if (fsproj-parent-of-p current (cdr (car subprojects)))
		(setq subproj     (car (car subprojects))
		      subprojects (cons (cons (concat (car (car subprojects)) "/" (file-name-nondirectory current)) 
					      current)
					subprojects))
		(setq subprojects (cdr subprojects))))
	  (when (not subprojects)
	    (setq subprojects (list (cons (file-name-nondirectory current) current))))
	  (setq sub-list (cons subproj sub-list))))
      ;;
      (setq subproject-list (reverse sub-list)))

    ;; 

    ;; Build the hash table:
    ;; each node of the hash table will be initially be a list of : '("basepath" "subproj")
    ;; Note: subproj can be nil. 

    ;; First path: initialization of the hash table throught the list: 
    (let ((name-list project-name-list)
	  (base-list project-base-list)
	  (sub-list  subproject-list))
      (while base-list
	(let ((cur-name (pop name-list))
	      (cur-base (pop base-list))
	      (cur-subp (pop sub-list)))
	  (puthash cur-name 
		   (cons (cons cur-base cur-subp)
			 (gethash cur-name project-ht)) 
		   project-ht))))

    ;; The second path will solve the conflicts and patch theses values.
    ;; Each node is a list of '("basepath" "subproj") and will be converted
    ;; into a list a string corresponding to the final name for each project
    ;; Note: the initial list has been build in reverse order
    (let ((name-list project-name-list))
      (while name-list
	(let* ((cur-name (pop name-list))
	       (cur-node (gethash cur-name project-ht)))
	  (when (listp (car cur-node))
	    (puthash cur-name (fsproj-resolve-conflict cur-node) project-ht)))))

    ;; The third will retrieve the conflict-less name
    (let ((name-list project-name-list)
	  reversed-list)
      (while name-list
	(let* ((cur-name (pop name-list))
	       (cur-node (gethash cur-name project-ht)))
	  (setq reversed-list (cons (pop cur-node) reversed-list))
	  (puthash cur-name cur-node project-ht)))
      (reverse reversed-list))
))


(defun fsproj-extract-file-names(current-project file-list modifier)
  "Return the CURRENT-PROJECT's converted FILE-LIST."
  (let ((prj-lgt (length current-project)))
    (mapcar (lambda (name)
	      (let ((sub-name (substring name prj-lgt (length name))))
		(if modifier
		    (reduce (lambda (str node) (replace-regexp-in-string (car node) (cdr node) str t))
			    modifier
			    :initial-value sub-name)
		    sub-name)))
	    file-list )))
;;


(defun fsproj-create-project-nodes-list(root project-regexp file-filter &optional ignore-folders pattern-modifier)
  "Parse the ROOT folder and sub-folders, and create a node list to add them into a project-buffer.
FILE-FILTER is a list of regexp which are used to filter the file list.
PROJECT-REGEXP should represent a regular expression which will help finding the project folders
If IGNORE-FOLDERS is non nil, it should specify a list of folder name to ignore.
If PATTERN-MODIFIER is non nil, it should specify a list of couple string (regexp . replace) which are going to get apply 
to the final project file name.

The return value is a list of nodes, each node will also be a list as described:
  '(proj-name proj-base-path (file-list) (file-full-path-list)"
  (let* ((collected-list (fsproj-collect-files root project-regexp file-filter ignore-folders))
	 (project-name-list (fsproj-generate-project-names (car collected-list)))
	 (file-list (cdr collected-list))
	 (project-list (car collected-list))
	 project-node-list)
    (while project-list
      (let* ((current-project      (pop project-list))
	     (current-project-name (pop project-name-list))
	     (extracted-data       (fsproj-extract-project-file-list (car current-project) file-list))
	     node)
	(setq file-list (cdr extracted-data))
	(setq node (list current-project-name
			 (cdr current-project)
			 (fsproj-extract-file-names (car current-project) (car extracted-data) pattern-modifier)
			 (car extracted-data)))
	(setq project-node-list (cons node project-node-list))))
    (reverse project-node-list)))

			 
