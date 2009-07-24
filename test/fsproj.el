;;
;; fsproj - File System Project
;;

(require 'cl)
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
	      (setq proj-list (cons (file-name-directory (substring fullpath 0 pos)) proj-list)))
	  )))))
    (list (reverse (sort proj-list 'string-lessp)) file-list)))


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
    (list project-file-list remaining-files)))



