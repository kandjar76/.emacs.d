;;; project-buffer-mode.el --- Generic mode to browse project file
;;
;; Author:   Cedric Lallain <kandjar76@hotmail.com>
;; Version:  0.9
;; Keywords: project mode buffer
;; Description: Generic mode to handler projects.
;; Tested with: GNU Emacs 22.x and GNU Emacs 23.x
;;
;; This file is *NOT* part of GNU Emacs.
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; Commentary:
;; 

;; project-buffer-mode is a generic mode to handle project.
;; It provides a hierarchical view of a project.
;;
;; In order to use it:
;; - you need a script to parse your project file.
;;   + the script has to initialize a buffer calling (project-buffer-mode)
;;   + then add each file using the function: (project-buffer-insert...
;; Check below for a sample code
;;
;;
;; Shortkey in the project-buffer-mode:
;;    +    -> collapse/expand folder/project (cursor has to be on a folder/project)
;;    m    -> mark the 'matching regexp' filename or the current file
;;    u    -> unmark file
;;    t    -> toggle marked files
;;    M    -> mark all
;;    U    -> unmark all
;;    f    -> open marked files
;;    q    -> cancel search or bury project-buffer
;;    ?    -> show brief help!!
;;    /    -> search file name matching regexp
;;    n    -> next file matching regexp
;;    p    -> prev file matching regexp
;;    v    -> view current file in view-mode
;;    o    -> find file at current pos in other window
;;    s    -> (un)mark files containing regexp...
;;   <TAB> -> collapse/expand folder/project (work if the cursor is on a file)
;;   <RET> -> open file at cursor pos
;;   <BCK> -> go to parent
;;   <SPC> -> next line
;; S-<SPC> -> prev line
;; C-<DWN> -> move to the next folder/project
;; C-<UP>  -> move to the previous folder/project
;; C-<LFT> -> expand if collapsed move to the first folder; move inside if expanded
;; C-<RGT> -> move up if folded collapsed; collapse if in front of folder ; move to the folded if in front of a file
;;    c s  -> Toggle search mode
;;    c v  -> Toggle view mode (flat / flat with the foldershidden / folder)
;;    c b  -> switch to the next build configuration
;;    c t  -> switch the master project to be the current project
;;    c p  -> switch to the next platform
;;    c B  -> prompt to change build configuration
;;    c T  -> prompt for the master project (project to build)
;;    c P  -> prompt to change platform
;;    B    -> launch build
;;    C    -> launch clean
;;    D    -> launch run/with debugger
;;    R    -> launch run/without debugger
;;
;; Future improvement:
;;    c    -> compile current file / marked files? [?]
;;    T    -> touch marked files (need a variable to make sure touch is always available)
;;    h    -> find corresponding header/source (need regexps to match one and the other such as: source/header = ( "\.c\(pp\)?" . "\.h\(pp\)?" ) )
;;    d    -> show/hide project dependencies
;;    b    -> buils marked files
;;    S    -> seach in all marked files


;;; TODO:
;;
;;  - show project dependencies
;;     e.g: [+] ProjName1           <deps: ProjName3, ProjName2>
;;  - test color in dark background
;;  - adding button to collapse/expand folders/projects
;;  - add collapsed all / expand all commands
;;  - check if there is any other one: update function which could use the parent field.
;;  - make sure no interactive function are complicated!!! (cf: toggle-expand-collapsed)
;;     it's better to create a function and call it in the command
;;  - grayed out exclude from build files??
;;  - different color for files referenced in the proj but don't exist?
;;  - add delete-node function


;;; Sample:
;;
;; (defun test-projbuff()
;;   (interactive)
;;   (let ((buffer (generate-new-buffer "test-project-buffer")))
;;     (display-buffer buffer)
;;     (with-current-buffer buffer
;;       (cd "~/temp")
;;       (project-buffer-mode)
;; 
;;       (project-buffer-insert "test1" 'project "test1.sln" "test1")
;;       (project-buffer-insert "src/gfr.cpp" 'file  "~/temp/gfr.cpp" "test1")
;;       (project-buffer-insert "src/abc.cpp" 'file  "~/temp/abc.cpp" "test1")
;; 
;;       (project-buffer-insert "test2" 'project "test2.sln" "test2")
;;       (project-buffer-insert "header/zzz.h" 'file  "~/temp/zzz.h" "test2")
;;       (project-buffer-insert "src/roo.c" 'file  "~/temp/roo.c" "test2")
;;       (project-buffer-insert "script.awk" 'file "~/temp/script.awk" "test2")
;; )))



;;; History:
;; 
;; v1.0: First public release.


(require 'cl)
(require 'ewoc)


;;; Code:


;;
;;  Buffer local variables:
;;

(defvar project-buffer-status nil)
(defvar project-buffer-view-mode nil)
(defvar project-buffer-cache-project nil)
(defvar project-buffer-cache-subdirectory nil)
(defvar project-buffer-platforms-list nil)
(defvar project-buffer-current-platform nil)
(defvar project-buffer-build-configurations-list nil)
(defvar project-buffer-current-build-configuration nil)
(defvar project-buffer-master-project nil)
(defvar project-buffer-projects-list nil)

;;
;; History:
;;

(defvar project-buffer-regexp-history nil
  "History list of regular expressions used in project-buffer commands.")


;;
;;  User hook:
;;

(defvar project-buffer-action-hook nil
  "Hook to perform the actions (build, clean, run...)

The function should follow the prototype:
  (lambda (action project-name project-path platform configuration)
 Where ACTION represents the action to apply to the project,
 it may be: 'build 'clean 'run 'debug,
 PROJECT-NAME is the name of the master project,
 PROJECT-PATH is the file path of the project
 PLATFORM is the name of the selected platform,
 and CONFIGURATION correspond to the selected build configuration."
)


;;
;;  Data type:
;;

;; Structure to store data attached to each ewoc-node.
;; Each node represents either a file or a project or a folder indide the project"
(defstruct (project-buffer-node
	    (:copier nil)
	    (:constructor project-buffer-create-node (name type filename project &optional hidden))
	    (:conc-name project-buffer-node->))
  name				;; string displayed to represent the file (usually the file.ext)
  type				;; project? file? folder?

  marked			;; is the file marked?
  hidden			;; hidden files (currently: = project/folder close)
  collapsed			;; is the folder/project collapsed or not?
  project-collapsed		;; t if the project the file belong to is collapsed

  matched			;; the file matches the regexp search

  filename			;; full path to the filename
  project			;; name of the project the file belongs to
  parent			;; parent node (parent folder or project or nil)

  platform-list			;; list of the platform available for the project (valid in project node only)
  build-configurations-list	;; list of build configuration avalailable for the project (valid in project node only)
)


;;
;;  Font
;;

(defgroup project-buffer nil
  "A special mode to manager project files."
)

(defface project-buffer-project-face
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "salmon")))
  "Project buffer mode face used to highlight project nodes."
  :group 'project-buffer)

(defface project-buffer-master-project-face
  '((default (:inherit project-buffer-project-face :bold t)))
  "Master project buffer mode face used to highlight project nodes."
  :group 'project-buffer)

(defface project-buffer-folder-face
  '((((class color) (background light)) (:foreground "purple"))
    (((class color) (background dark)) (:foreground "salmon")))
  "Project buffer mode face used to highlight folder nodes."
  :group 'project-buffer)

(defface project-buffer-file-face
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark)) (:foreground "white")))
  "Project buffer mode face used to highlight file nodes."
  :group 'project-buffer)

(defface project-buffer-project-button-face
  '((((class color) (background light)) (:foreground "gray50"))
    (((class color) (background dark)) (:foreground "gray50")))
  "Project buffer mode face used highligh [ and ] in front of the project name."
  :group 'project-buffer)
  
(defface project-buffer-indent-face
  '((((class color) (background light)) (:foreground "gray50"))
    (((class color) (background dark)) (:foreground "gray50")))
  "Project buffer mode face used to highlight indent characters."
  :group 'project-buffer)

(defface project-buffer-mark-face
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "tomato")))
  "Project buffer mode face used highligh marks."
  :group 'project-buffer)

(defface project-buffer-filename-face
  '((((class color) (background light)) (:foreground "gray50"))
    (((class color) (background dark)) (:foreground "gray50")))
  "Project buffer mode face used highligh file names."
  :group 'project-buffer)

(defface project-buffer-matching-file-face
  '((default (:inherit project-buffer-file-face :bold t)))
  "Project buffer mode face used matching file."
  :group 'project-buffer)


(defcustom project-buffer-new-project-collapsed t
  "Newly added project will be collapsed by default."
  :group 'project-buffer
  :type 'boolean)

(defcustom project-buffer-search-in-files-mode 'narrow-marked-files
  "Enumarated value, set to 'narrow-marked-files it will search in the selected marked files, removing the one failing the research,
set to 'all-files it will launch the search on all files in the projects, 'current-project will only search with the current project
Note: if no files are marked while using narrow-marked-files, the search will occur in all files in the project."
  :group 'project-buffer
  :type '(choice (const :tag "Narrow the marked files" 'narrow-marked-files)
		 (const :tag "All files" 'all-files)
		 (const :tag "Current project" 'current-project)))


;;
;;  Key Bindings:
;;

;; Define the key mapping for the spu mode:
(defvar project-buffer-mode-map
  (let ((project-buffer-mode-map (make-keymap)))
    (define-key project-buffer-mode-map [?+] 'project-buffer-toggle-expand-collapse)
    (define-key project-buffer-mode-map [?\t] 'project-buffer-toggle-expand-collapse-even-on-file)
    (define-key project-buffer-mode-map [?m] 'project-buffer-mark-matched-files-or-current-file)
    (define-key project-buffer-mode-map [?u] 'project-buffer-unmark-matched-files-or-current-file)
    (define-key project-buffer-mode-map [?M] 'project-buffer-mark-all)

    (define-key project-buffer-mode-map [?U] 'project-buffer-unmark-all)
    (define-key project-buffer-mode-map [?t] 'project-buffer-toggle-all-marks)
    (define-key project-buffer-mode-map [?f] 'project-buffer-find-marked-files)
    (define-key project-buffer-mode-map [?/] 'project-buffer-search-forward-regexp)
    (define-key project-buffer-mode-map [?n] 'project-buffer-goto-next-match)

    (define-key project-buffer-mode-map [?p] 'project-buffer-goto-prev-match)
    (define-key project-buffer-mode-map [?v] 'project-buffer-view-file)
    (define-key project-buffer-mode-map [?c ?s] 'project-buffer-toggle-search-mode)
    (define-key project-buffer-mode-map [?c ?v] 'project-buffer-toggle-view-mode)
    (define-key project-buffer-mode-map [?c ?b] 'project-buffer-next-build-configuration)
    (define-key project-buffer-mode-map [?c ?p] 'project-buffer-next-platform)
    (define-key project-buffer-mode-map [?c ?t] 'project-buffer-select-current-as-master-project)
    (define-key project-buffer-mode-map [?c ?B] 'project-buffer-choose-build-configuration)
    (define-key project-buffer-mode-map [?c ?P] 'project-buffer-choose-platform)
    (define-key project-buffer-mode-map [?c ?T] 'project-buffer-choose-master-project)
    (define-key project-buffer-mode-map [backspace] 'project-buffer-goto-dir-up)

    (define-key project-buffer-mode-map [?\ ] 'project-buffer-next-file)
    (define-key project-buffer-mode-map [(shift ?\ )] 'project-buffer-prev-file)
    (define-key project-buffer-mode-map [return] 'project-buffer-find-file)
    (define-key project-buffer-mode-map [?o] 'project-buffer-find-file-other-window)
    (define-key project-buffer-mode-map [(control left)] 'project-buffer-goto-dir-up-or-collapsed)

    (define-key project-buffer-mode-map [(control right)] 'project-buffer-next-file-or-expand)
    (define-key project-buffer-mode-map [(control up)] 'project-buffer-go-to-previous-folder-or-project)
    (define-key project-buffer-mode-map [(control down)] 'project-buffer-go-to-next-folder-or-project)
    (define-key project-buffer-mode-map [??] 'project-buffer-help)
    (define-key project-buffer-mode-map [?q] 'project-buffer-quit)

    (define-key project-buffer-mode-map [?B] 'project-buffer-perform-build-action)
    (define-key project-buffer-mode-map [?C] 'project-buffer-perform-clean-action)
    (define-key project-buffer-mode-map [?R] 'project-buffer-perform-run-action)
    (define-key project-buffer-mode-map [?D] 'project-buffer-perform-debug-action)
    (define-key project-buffer-mode-map [?s] 'project-buffer-mark-files-containing-regexp)

    project-buffer-mode-map))


;;
;;  Internal Utility Functions:
;;


(defun project-buffer-mark-matching-file(status regexp)
  "Check each file name and mark the files matching the regular expression REGEXP"
  (let ((node (ewoc-nth status 0)))
    (while node
      (let* ((node-data (ewoc-data node))
	     (node-type (project-buffer-node->type node-data))
	     (node-name (project-buffer-node->name node-data))
	     (file      (file-name-nondirectory node-name)))
	(when (string-match regexp file)
	  (let ((parent (project-buffer-find-node-up status node)))
	    (while (and parent
			(not (eq (project-buffer-node->type (ewoc-data parent)) 'project))
			(not (project-buffer-node->matched (ewoc-data parent))))
	      (setf (project-buffer-node->matched (ewoc-data parent)) t)
	      (ewoc-invalidate status parent)
	      (setq parent (project-buffer-find-node-up status parent))
	      ))
	  (setf (project-buffer-node->matched node-data) t)
	  (ewoc-invalidate status node)
	  ))
      (setq node (ewoc-next status node)))))


(defun project-buffer-read-regexp(prompt)
  "Read a regular expression from the minibuffer."
  (read-from-minibuffer prompt nil nil nil 'project-buffer-regexp-history))


(defun project-buffer-clear-matched-mark(status)
  "Clear 'matched' flag"
  (let (result)
    (ewoc-map (lambda (node)
		(when (project-buffer-node->matched node)
		  (setf (project-buffer-node->matched node) nil)
		  (setq result t)))
	      status)
    result))


(defun project-buffer-get-marked-nodes(status)
  "Return the list of marked node or the current node if none are marked"
  (or (ewoc-collect status (lambda (node) (project-buffer-node->marked node)))
      (list (ewoc-data (ewoc-locate status)))))


(defun project-buffer-convert-name-for-display(node-data)
  "Convert the node name into the displayed string depending on the project-buffer-view-mode."
  (let* ((node-name  (project-buffer-node->name node-data))
	 (file-color (if (project-buffer-node->matched node-data) 'project-buffer-matching-file-face 'project-buffer-file-face))
	 (node-color (if (eq (project-buffer-node->type node-data) 'file) file-color 'project-buffer-folder-face)))
    (cond ((eq project-buffer-view-mode 'flat-view)
	   (concat (propertize " `- " 'face 'project-buffer-indent-face)
		   (and (file-name-directory node-name)
			(propertize (file-name-directory node-name) 'face 'project-buffer-folder-face))
		   (propertize (file-name-nondirectory node-name) 'face file-color)))
	  ((eq project-buffer-view-mode 'folder-hidden-view)
	   (concat (propertize " `- " 'face 'project-buffer-indent-face)
		   (propertize (file-name-nondirectory node-name) 'face file-color)))
	  ((eq project-buffer-view-mode 'folder-view)
	   (let ((dir-list (split-string node-name "/"))
		 (str (if (project-buffer-node->collapsed node-data) " `+ " " `- "))
		 (cur 1))
	     (while (< cur (length dir-list))
	       (setq str (concat " |  " str)
		     cur (1+ cur)))
	     (concat (propertize str 'face 'project-buffer-indent-face)
		     (propertize (file-name-nondirectory node-name) 'face node-color) )))
	  ((eq project-buffer-view-mode 'marked-view)
	   (concat (propertize " - " 'face 'project-buffer-indent-face)
		   (and (file-name-directory node-name) 
			(propertize (file-name-directory node-name) 'face 'project-buffer-folder-face))
		   (propertize (file-name-nondirectory node-name) 'face file-color)))
	  (t (format "Unknown view mode: %S" project-buffer-view-mode) ))))


(defun project-buffer-prettyprint(node)
  "Pretty-printer function"
  (let ((node-collapsed (project-buffer-node->collapsed node))
	(node-name     (project-buffer-node->name  node))
	(node-marked   (project-buffer-node->marked node))
	(node-type     (project-buffer-node->type node))
	(node-hidden   (project-buffer-node->hidden node))
	(node-matching (project-buffer-node->matched node))
	(node-prjcol   (project-buffer-node->project-collapsed node))
	(node-project  (project-buffer-node->project node)))
    (if (eq project-buffer-view-mode 'marked-view)
	(when (and (eq node-type 'file)
		   (or node-marked node-matching))
	  (insert (concat " " 
			  (if node-marked (propertize "*" 'face 'project-buffer-mark-face) " ")
			  " "
			  (propertize (if (> (length node-project) 16)
					  (substring node-project 0 16)
					  node-project)
				      'face 'project-buffer-project-face)))
   	  (indent-to-column 19)
	  (insert (concat (project-buffer-convert-name-for-display node)
			  "\n")))
	(when (or (and (eq project-buffer-view-mode 'folder-view)
		       (or (not node-hidden)
			   node-matching))
		  (and (not (eq project-buffer-view-mode 'folder-view))
		       (not (eq node-type 'folder))
		       (or (not node-prjcol)
			   node-matching))
		  (eq node-type 'project))
	  (insert (concat " " 
			  (if node-marked (propertize "*" 'face 'project-buffer-mark-face)" ")
			  " "
			  (cond ((not (eq node-type 'project)) "   ")
				(node-collapsed                (propertize "[+]" 'face 'project-buffer-project-button-face) )
				(t                             (propertize "[-]" 'face 'project-buffer-project-button-face) ))
			  " "
			  (or (and (eq node-type 'project)  (propertize node-name 'face (or (and project-buffer-master-project
												 (string= node-name (car project-buffer-master-project))
												 'project-buffer-master-project-face)
											    'project-buffer-project-face)))
			      (project-buffer-convert-name-for-display node))))
	  (when (and (eq project-buffer-view-mode 'folder-hidden-view)
		     (project-buffer-node->filename node)
		     (eq (project-buffer-node->type node) 'file))
	    (indent-to-column 40)
	    (insert (concat " " (propertize (project-buffer-node->filename node) 
					    'face 'project-buffer-filename-face))))
	  (insert "\n"))
	)))


(defun project-buffer-refresh-ewoc-hf(status)
  "Refresh ewoc header/footer"
  (ewoc-set-hf status
	       (concat (format "Project view mode:   %s\n" project-buffer-view-mode)
		       (format "Platform:            %s\n" (or project-buffer-current-platform "N/A"))
		       (format "Build configuration: %s\n" (or project-buffer-current-build-configuration "N/A"))
		       (format "Search mode:         %s\n" project-buffer-search-in-files-mode)
		       "\n\n") ""))


(defun project-buffer-extract-folder(name type)
  "Return the folder associated to the node's NAME of the type TYPE.
Return nil if TYPE is project."
  (cond ((eq type 'folder) name)
	((eq type 'project) nil)
	(t (let ((dirname (file-name-directory name)))
	     (and dirname (substring dirname 0 -1))))))


(defun project-buffer-directory-lessp(dir1 dir2 type2)
  "Return t if DIR1 is less than (DIR2,TYPE2)."
  (let* ((list1  (and dir1 (split-string dir1 "/")))
	 (list2  (and dir2 (split-string dir2 "/")))
	 (cnt 0))
    (if (and list1 list2)
	(progn (while (and (< cnt (length list1))
			   (< cnt (length list2))
			   (string= (nth cnt list1) (nth cnt list2)))
		 (setq cnt (1+ cnt)))
	       (if (and (< cnt (length list1))
			(< cnt (length list2)))
		   (string-lessp (nth cnt list1) (nth cnt list2))
		   (and (eq type2 'file)
			(< cnt (length list1)))
		   ))
	(null list2))))


(defun project-buffer-parent-of-p(child parent)
  "Check if CHILD is a child of the directory PARENT."
  (let* ((clist (and child  (split-string child "/")))
	 (plist (and parent (split-string parent "/")))
	 (cont t)
	 res)
    (while (and clist plist cont)
      (let ((cname (pop clist))
	    (pname (pop plist)))
	(setq cont (string-equal cname pname))))
    (and cont (null plist))))


(defun project-buffer-find-node-up(status node)
  "Return the directory or project in which the node belong.
This may change depending on the view mode."
  (if (eq project-buffer-view-mode 'folder-view)
      (project-buffer-node->parent (ewoc-data node))
      (let ((parent (project-buffer-node->parent (ewoc-data node))))
	(when parent
	  (while (not (eq (project-buffer-node->type (ewoc-data parent)) 'project))
	    (setq parent (project-buffer-node->parent (ewoc-data parent))))))))


(defun project-buffer-search-project-node(status project-name)
  "Return the node of the project node named PROJECT-NAME or nil if absent"
  (if (string-equal (car project-buffer-cache-project) project-name)
      (cdr project-buffer-cache-project)
      (let ((node (ewoc-nth status 0)))
	(while (and node
		    (or (not (eq (project-buffer-node->type (ewoc-data node)) 'project))
			(not (string-equal (project-buffer-node->name (ewoc-data node)) project-name))))
	  (setq node (ewoc-next status node)))
	node)))


(defun project-buffer-set-project-platforms-data(status project platform-list)
  "Attached the list of platform contained in PLATFORM-LIST to the project named PROJECT."
  (let ((node (project-buffer-search-project-node status project)))
    ;; Now, if the project has been found:
    (when node
      (setf (project-buffer-node->platform-list (ewoc-data node)) platform-list)
      ;; also:
      (while platform-list
	(add-to-list 'project-buffer-platforms-list (pop platform-list) t))
      (unless project-buffer-current-platform
	(setq project-buffer-current-platform (car project-buffer-platforms-list)))))
  (project-buffer-refresh-ewoc-hf status))


(defun project-buffer-set-project-build-configurations-data(status project build-configuration-list)
  "Attached the list build configurations in BUILD-CONFIGURATION-LIST to the project named PROJECT."
  (let ((node (project-buffer-search-project-node status project)))
    ;; Now, if the project has been found:
    (when node
      (setf (project-buffer-node->build-configurations-list (ewoc-data node)) build-configuration-list)
      ;; also:
      (while build-configuration-list
	(add-to-list 'project-buffer-build-configurations-list (pop build-configuration-list) t))
      (unless project-buffer-current-build-configuration
	(setq project-buffer-current-build-configuration (car project-buffer-build-configurations-list)))))
  (project-buffer-refresh-ewoc-hf status))


(defun project-buffer-insert-node(status data)
  "Insert a file in alphabetic order in it's project/directory."
  (let ((node           (ewoc-nth status 0))
	(folder-data    (project-buffer-extract-folder (project-buffer-node->name data)      (project-buffer-node->type data)))
	(name-data      (file-name-nondirectory (project-buffer-node->name data)))
	(type-data      (project-buffer-node->type data))
	(proj-data      (project-buffer-node->project data))
	(node-data      nil)
	(here           nil)
	(proj-found     nil)
	(folder         nil)
	(hidden-flag    nil)
	(skip           nil)
	(proj-root-node nil)
	(folder-node    nil)
	(parent-node    nil)
	)
    (when (eq type-data 'folder)
      (error "Not supported -- in particular project-buffer-directory-lessp may returns a incorrect value"))
    

    ;; Cache check:
    (when project-buffer-cache-project
      (cond
       ;; cache-project < current-project -> we can start the search from here (at least).
       ((string-lessp (car project-buffer-cache-project) proj-data)
	(setq node (cdr project-buffer-cache-project)
	      project-buffer-cache-subdirectory nil))

       ;; cache-project == current-project -> check the folders...
       ((string-equal (car project-buffer-cache-project) proj-data)
	;; cache-subdir < current-subdir -> we can start from here.
	;; cache-subdir = current-subdir -> good starting point
	(if (and project-buffer-cache-subdirectory
		 folder-data
		 (or (string-equal (car project-buffer-cache-subdirectory) folder-data)
		     (project-buffer-directory-lessp (car project-buffer-cache-subdirectory) folder-data 'folder)))
	    (setq node (cdr project-buffer-cache-subdirectory)
		  proj-root-node (cdr project-buffer-cache-project)
		  proj-found t)
	    (setq node (cdr project-buffer-cache-project)
		  project-buffer-cache-subdirectory nil)))
       ;; other wise: cache miss...
       (t
	(setq project-buffer-cache-project nil
	      project-buffer-cache-subdirectory nil))))
	 
    ;; Search where to insert the node:
    (while (and node (not here) (not skip))
      (setq node-data (ewoc-data node))

      (cond
       ;; data.project < node.project -> insert here...
       ((string-lessp proj-data (project-buffer-node->project node-data))
	(if (eq (project-buffer-node->type data) 'project)
	    (setq here node)
	    (setq here (and proj-found node)
		  skip (not proj-found))))

       ;; node.project == data.project -> check folder/file name
       ((string-equal proj-data (project-buffer-node->project node-data))
	(if (eq (project-buffer-node->type data) 'project)
	    ;; If we're trying to add the project when the project already exist... we'll skip it.
	    (setq skip t)
	    ;; Otherwise:
	    (let* ((folder-db   (project-buffer-extract-folder (project-buffer-node->name node-data) (project-buffer-node->type node-data)))
		   (name-db     (file-name-nondirectory (project-buffer-node->name node-data)))
		   (type-db     (project-buffer-node->type node-data)))
	      ;; Are we're on the project line???
	      (if (eq type-db 'project)
		  (setq proj-root-node node)
		  (if (and folder-db folder-data)
		      ;; Both the current node and the new one have a directory
		      (progn (when (and (eq type-db 'folder)
					(project-buffer-parent-of-p (project-buffer-node->name data) folder-db))
			       (setq folder-node node))
			     (cond ((project-buffer-directory-lessp folder-data folder-db type-db)
				    (setq here node))
				   
				   ((string-equal folder-data folder-db)
				    (when (eq type-db 'folder)
				      (setq folder-node node))
				    (setq folder folder-data)
				    (if (eq type-data 'folder)
					(setq skip t)
					(unless (eq type-db 'folder)
					  (when (string-lessp name-data name-db)
					    (setq here node)))))
				   
				   (t (setq folder folder-db))))
		      ;; Either:
		      ;; - the current node has no folder, meaning:
		      ;;   -> either the new node has a directory in which case we'll add it here.
		      ;;   -> or we'll search for the right place to add it.
		      ;; - the current node has a folder, meaning:
		      ;;   -> the new one has no folder, therefore, we need to carry on until we reach the no-folder area.
		      (unless folder-db
			(if folder-data
			    (setq here node)
			    (when (string-lessp name-data name-db)
			      (setq here node)))))))
	      (setq proj-found t))
	))

       ;; Carry on...
       (setq node (ewoc-next status node)))

    ;; Insert before here...
    (when (not skip)

      ;; Here we can set the parent folder:
      (if folder-node
	(setf (project-buffer-node->parent data) folder-node)
	(setf (project-buffer-node->parent data) proj-root-node))

      ;; Once the node added we will need to check if it should be hidden or not.
      ;; At first, if it's a file, it will be hidden to not have any glitch in the displayed buffer
      (if (eq type-data 'project)
	  (progn (setf (project-buffer-node->project-collapsed data) project-buffer-new-project-collapsed)
		 (setf (project-buffer-node->collapsed data) project-buffer-new-project-collapsed)
		 (add-to-list 'project-buffer-projects-list name-data)
		 (unless project-buffer-master-project
		   (setq project-buffer-master-project (cons name-data nil)))) ; to prevent blinking
	  (progn (setf (project-buffer-node->hidden data) t)
		 (unless proj-root-node
		   (error "Project '%s' not found" proj-data))))

      (if here
	  (setq node (ewoc-enter-before status here data))
	  (setq node (ewoc-enter-last status data)))

      (when (eq type-data 'project)
	(unless (cdr project-buffer-master-project)
	  (setq project-buffer-master-project (cons name-data node)))
	(setq proj-root-node node))

      ;;

      ;; If it's not a project type, search up in all possible parent to see if the node is supposed to be visible or not
      (unless (eq type-data 'project)
	(let* ((shown t)
	       (parent (project-buffer-find-node-up status node)))
	  (setf (project-buffer-node->project-collapsed data) (project-buffer-node->project-collapsed (ewoc-data parent)))
	  (setq shown (not (and parent (project-buffer-node->collapsed (ewoc-data parent)))))
	  (while (and parent
		      shown
		      (not (eq (project-buffer-node->type (ewoc-data parent)) 'project)))
	    (setq parent (project-buffer-find-node-up status parent))
	    (setq shown  (not (and parent (project-buffer-node->collapsed (ewoc-data parent)))))
	    )
	  (setq hidden-flag (not shown)))
	(unless hidden-flag
	  (setf (project-buffer-node->hidden data) nil)
	  (ewoc-invalidate status node)))

      ;; In case some folder needed to be created:
      (when folder-data
	(let* ((db-list     (and folder (split-string folder "/")))
	       (curr-list   (split-string folder-data "/"))
	       (cnt 0))
	  (while (and (< cnt (length curr-list))
		      (< cnt (length db-list))
		      (string= (nth cnt db-list) (nth cnt curr-list)))
	    (setq cnt (1+ cnt)))
	  ;; Add the extra folder:
	  (if (< cnt (length curr-list))
	      (let ((ndx 0)
		    (str nil))
		(while (< ndx cnt)
		  (setq str (or (and str (concat str "/" (nth ndx curr-list)))
				(nth ndx curr-list)))
		  (setq ndx (1+ ndx)))
		(while (< ndx (length curr-list))
		  (setq str (or (and str (concat str "/" (nth ndx curr-list)))
				(nth ndx curr-list)))

		  (setq parent-node (or folder-node proj-root-node))
		  (let ((new-data (project-buffer-create-node str 'folder folder proj-data hidden-flag)))
		    (setf (project-buffer-node->project-collapsed new-data) (project-buffer-node->project-collapsed data))
		    (setq folder-node (ewoc-enter-before status node new-data)))
		  (setf (project-buffer-node->parent (ewoc-data folder-node)) parent-node)
		  
		  (setf (project-buffer-node->parent data) folder-node)
		  (setq ndx (1+ ndx)))))
	  ))
      )

    ;; Save the project root node:
    ;; - to speed up the next insert (we stop looking for the project if it's the same one)
    (setq project-buffer-cache-project (cons proj-data proj-root-node))
    (setq project-buffer-cache-subdirectory (and folder-node
						 (cons folder-data folder-node)))
))

(defun project-buffer-delete-file-node(status name project)
  "Delete the node named NAME which belongs to PROJECT.
Empty folder node will also be cleared up."
  (let ((node           (ewoc-nth status 0))
	(folder-data    (project-buffer-extract-folder name 'file))
	(proj-data      project)
	(found          nil)
	(folder-found   nil)
	(node-data      nil))

    ;; Cache check: <deleting a file node doesn't update the cache>
    (when project-buffer-cache-project
      (cond
       ;; cache-project < current-project -> we can start the search from here (at least).
       ((string-lessp (car project-buffer-cache-project) proj-data)
	(setq node (cdr project-buffer-cache-project))

       ;; cache-project == current-project -> check the folders...
       ((string-equal (car project-buffer-cache-project) proj-data)
	;; cache-subdir < current-subdir -> we can start from here.
	;; cache-subdir = current-subdir -> good starting point
	(if (and project-buffer-cache-subdirectory
		 folder-data
		 (or (string-equal (car project-buffer-cache-subdirectory) folder-data)
		     (project-buffer-directory-lessp (car project-buffer-cache-subdirectory) folder-data 'folder)))
	    (setq node (cdr project-buffer-cache-subdirectory))
	    (setq node (cdr project-buffer-cache-project))))
       ;; other wise: cache miss...
       )))

    ;; Search where is the node to delete:
    (while (and node (not found))
      (setq node-data (ewoc-data node))

      (cond
       ;; data.project < node.project -> not found...
       ((string-lessp proj-data (project-buffer-node->project node-data))
	(setq node nil))

       ;; node.project == data.project -> check folder/file name
       ((string-equal proj-data (project-buffer-node->project node-data))
	(let* ((folder-db (project-buffer-extract-folder (project-buffer-node->name node-data) (project-buffer-node->type node-data)))
	       (type-db   (project-buffer-node->type node-data)))
	  ;; Make sure it's not the project line:
	  (unless (eq type-db 'project)
	    (setq found (and (string-equal (project-buffer-node->name node-data) name) node))))))

      ;; next node:
      (setq node (and node (ewoc-next status node))))

    ;; Time to delete it:
    (when found
      (let ((parent-node (project-buffer-node->parent node-data))
	    (inhibit-read-only t))
	;; Delete the found node:
	(ewoc-delete status found)
	
	;; Now it's time to check the parent node the file belong to:
	(while parent-node
	  (let ((next-node   (ewoc-next status parent-node))
		(parent-data (ewoc-data parent-node)))
	    (if (and next-node
		     (eq (project-buffer-node->parent (ewoc-data next-node)) parent-node))
		(setq parent-node nil)
		(let ((new-parent-node (and (not (eq (project-buffer-node->type parent-data) 'project))
					    (project-buffer-node->parent parent-data))))
		  (if (not new-parent-node)
		      (project-buffer-delete-project-node status project parent-node)
		      (ewoc-delete status parent-node))
		  (setq parent-node new-parent-node))
		)))
	))
    ))

(defun project-buffer-delete-project-node(status proj-name proj-node)
  "Delete the project node PROJ-NODE.
Each files/folder under the project will also be deleted."
  (when proj-node
    (let ((proj-data (ewoc-data proj-node))
	  (prev-node (ewoc-prev status proj-node))
	  (curr-node proj-node))
      ;; Let's start by removing the project from the project list:
      (setq project-buffer-projects-list (remove proj-name project-buffer-projects-list))

      ;; Delete the nodes:
      (let ((inhibit-read-only t))
	(while (and curr-node
		    (string-equal (project-buffer-node->project (ewoc-data curr-node)) proj-name))
	  (let ((next-node (ewoc-next status curr-node)))
	    (ewoc-delete status curr-node)
	    (setq curr-node next-node)
	    )))
      
      ;; Now: the master project may need to be readjusted
      (when (string-equal proj-name (car project-buffer-master-project))
	(if curr-node
	    ;; By default the next project become the new master one:
	    (progn  (setq project-buffer-master-project (cons (project-buffer-node->project (ewoc-data curr-node)) curr-node))
		    (ewoc-invalidate status curr-node))
	    ;; Otherwise: if the previous node is invalid, it's project will become the new master one:
	    (if prev-node
		(let ((prev-parent (project-buffer-node->parent (ewoc-data prev-node))))
		  (while (not (eq (project-buffer-node->type (ewoc-data prev-parent)) 'project))
		    (setq prev-parent (project-buffer-node->parent (ewoc-data prev-parent))))
		  (setq project-buffer-master-project (cons (project-buffer-node->project (ewoc-data prev-parent)) prev-parent))
		  (ewoc-invalidate status prev-parent))
		(setq project-buffer-master-project nil))))
      )))


(defun project-buffer-refresh-all-items(status)
  "Refresh all ewoc item from the buffer."
  (ewoc-map (lambda (info)  t) status)) ; (ewoc-refresh status) doesn't work properly.


(defun project-buffer-perform-action-hook(action)
  "Call the user hook to perform ACTION."
  (run-hook-with-args 'project-buffer-action-hook
		      action
		      (car project-buffer-master-project)
		      (project-buffer-node->filename (ewoc-data (cdr project-buffer-master-project)))
		      project-buffer-current-platform
		      project-buffer-current-build-configuration))

;;
;;  External functions:
;;


(defun project-buffer-mode ()
  "Major mode to view project.

Commands:
\\{project-buffer-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq mode-name "project-buffer"
	major-mode 'project-buffer-mode
	buffer-read-only t)
  (use-local-map project-buffer-mode-map)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (let ((status (ewoc-create 'project-buffer-prettyprint "" "" t)))
      (make-local-variable 'project-buffer-status)
      (make-local-variable 'project-buffer-view-mode)
      (make-local-variable 'project-buffer-cache-project)
      (make-local-variable 'project-buffer-cache-subdirectory)
      (make-local-variable 'project-buffer-platforms-list)
      (make-local-variable 'project-buffer-current-platform)
      (make-local-variable 'project-buffer-build-configurations-list)
      (make-local-variable 'project-buffer-current-build-configuration)
      (make-local-variable 'project-buffer-master-project)
      (make-local-variable 'project-buffer-projects-list)

      (setq project-buffer-status status)
      (setq project-buffer-view-mode 'folder-view)
      (setq project-buffer-cache-project nil)
      (setq project-buffer-cache-subdirectory nil)
      (setq project-buffer-platforms-list nil)
      (setq project-buffer-current-platform nil)
      (setq project-buffer-build-configurations-list nil)
      (setq project-buffer-current-build-configuration nil)
      (setq project-buffer-master-project nil)
      (setq project-buffer-projects-list nil)

      (project-buffer-refresh-ewoc-hf status))))


(defun project-buffer-insert (name type filename project)
  "Insert a file in alphabetic order in it's project/directory.
NAME is the name of the file in the project with it's virtual project directory,
both name and directory may be virtual
TYPE type of the node in the project: should be either 'project or 'file
FILENAME should be either a full path to the project's file or a relative path based
on the current directory of the buffer
PROJECT is the name of the project in which to insert the node
note: regarding the project node, it's recommended to have NAME = PROJECT"
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-insert-node project-buffer-status
			      (project-buffer-create-node name type filename project)))

(defun project-buffer-delete-file (name project)
  "Delete the node named NAME which belongs to PROJECT.
Empty folder node will also be cleared up."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-delete-file-node project-buffer-status name project))


(defun project-buffer-delete-project (project)
  "Delete the project PROJECT.
Each files/folder under the project will also be deleted."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-delete-project-node project-buffer-status 
				      project
				      (project-buffer-search-project-node project-buffer-status project)))


(defun project-buffer-set-project-platforms(project platform-list)
  "Attached the list of platform contained in PLATFORM-LIST to the project named PROJECT."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-set-project-platforms-data project-buffer-status
					     project
					     platform-list))

(defun project-buffer-set-project-build-configurations(project build-configuration-list)
  "Attached the list build configurations in BUILD-CONFIGURATION-LIST to the project named PROJECT."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-set-project-build-configurations-data project-buffer-status
							project
							build-configuration-list))

(defun project-buffer-search-and-mark-files(status regexp project marked-flag)
  "Search REGEXP in with all files if PROJECT is nil or in each file of the specified PROJECT. 
If REGEXP is found, the marked-flag field associated to the file get set to MARKED-FLAG
The function returns the number of files whose marked-flag field changed"
  (let ((count 0))
    (ewoc-map (lambda (node) 
		(when (and (eq (project-buffer-node->type node) 'file)				; check only files
			   (or (not project)							; ( if a project is specified,
			       (string-equal (project-buffer-node->project node) project))	;   make sure it matches the node's project )
			   (not (eq (project-buffer-node->marked node) marked-flag)))		; which aren't already (un)marked (based on request)
		  ;; Check if the file contain the regexp:
		  (let ((filename (project-buffer-node->filename node)))
		    (when (and filename
			       (file-readable-p filename)
			       (let ((fbuf (get-file-buffer filename)))
				 (message "Project '%s' -- Searching in '%s'" (project-buffer-node->project node) (project-buffer-node->name node))
				 (if fbuf
				     (with-current-buffer fbuf
				       (save-excursion
					 (goto-char (point-min))
					 (re-search-forward regexp nil t)))
				     (with-temp-buffer
				       (insert-file-contents filename)
				       (goto-char (point-min))
				       (re-search-forward regexp nil t)))))
		      (setf (project-buffer-node->marked node) marked-flag)
		      (setq count (1+ count))
		      t  )))) ; to force the update of the display.
	      status)
    count))


(defun project-buffer-refine-mark-files(status regexp marked-flag)
  "Search REGEXP in with all marked files. 
If REGEXP is found, the marked-flag field associated to the file get set to MARKED-FLAG
The function returns the number of files whose marked-flag field changed
Note: if no files are marked, the search will occur in all existing files of the project"
  (let ((count 0)
	marked-file-found)
    (ewoc-map (lambda (node) 
		(when (and (eq (project-buffer-node->type node) 'file)	; check only files
			   (project-buffer-node->marked node))		; which are already marked
		  (setq marked-file-found t)
		  ;; Check if the file contain the regexp:
		  (let ((filename (project-buffer-node->filename node)))
		    (when (and filename
			       (file-readable-p filename)
			       (let ((found (let ((fbuf (get-file-buffer filename)))
					      (message "Project '%s' -- Searching in '%s'" (project-buffer-node->project node) (project-buffer-node->name node))
					      (if fbuf
						  (with-current-buffer fbuf
						    (save-excursion
						      (goto-char (point-min))
						      (re-search-forward regexp nil t)))
						  (with-temp-buffer
						    (insert-file-contents filename)
						    (goto-char (point-min))
						    (re-search-forward regexp nil t))))))
				 (or (and found (not marked-flag))
				     (and (not found) marked-flag))))
		      (setf (project-buffer-node->marked node) nil)
		      (setq count (1+ count))
		      t  )))) ; to force the update of the display.
	      status)
    (if marked-file-found
	count
	(project-buffer-search-and-mark-files status regexp nil marked-flag))))


;;
;;  Interactive commands:
;;


(defun project-buffer-goto-dir-up ()
  "Go to the project/folder containing the current file/folder."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate status)))
    (setq node (and node (project-buffer-find-node-up status node)))
    (when node
      (ewoc-goto-node status node))))


(defun project-buffer-goto-dir-up-or-collapsed ()
  "Go to the project/folder containing the current file/folder unless the cursor is on a expanded folder/project in which case, it will collapse it."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status    project-buffer-status)
	 (node      (ewoc-locate status))
	 (node-data (and node (ewoc-data node))))
    (when node
      (if (or (eq (project-buffer-node->type node-data) 'file)
	      (project-buffer-node->collapsed node-data))
	  (progn (setq node (and node (project-buffer-find-node-up status node)))
		 (when node (ewoc-goto-node status node)))
	  (project-buffer-toggle-expand-collapse)
	  ))))


(defun project-buffer-search-forward-regexp (regexp)
  "Search file matching REGEXP."
  (interactive "sSearch forward (regexp): ")
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-clear-matched-mark project-buffer-status)
  (when (and regexp
	     (> (length regexp) 0))
    (let* ((status project-buffer-status)
	   (node (ewoc-locate status)))
      (project-buffer-mark-matching-file project-buffer-status regexp)
      ;; goto first match
      (while (and node
		  (or (not (eq (project-buffer-node->type (ewoc-data node)) 'file))
		      (not (project-buffer-node->matched (ewoc-data node)))))
	(setq node (ewoc-next status node)))
      ;; if failed: go to the last search instead
      (unless node
	(setq node (ewoc-locate status))
	(while (and node
		    (or (not (eq (project-buffer-node->type (ewoc-data node)) 'file))
			(not (project-buffer-node->matched (ewoc-data node)))))
	  (setq node (ewoc-prev status node))))
      (if node
	(ewoc-goto-node status node)
	(message "Search failed: %s." regexp)))))


(defun project-buffer-goto-next-match ()
  "Go to the next matching."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate status)))
    (if node (setq node (ewoc-next status node)))
    ;; goto first match
    (while (and node
		(or (not (eq (project-buffer-node->type (ewoc-data node)) 'file))
		    (not (project-buffer-node->matched (ewoc-data node)))))
      (setq node (ewoc-next status node)))
    (if node
	(ewoc-goto-node status node)
	(message "Failing forward search."))))


(defun project-buffer-goto-prev-match ()
  "Go to the previous matching."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate status)))
    (if node (setq node (ewoc-prev status node)))
    ;; goto first match
    (while (and node
		(or (not (eq (project-buffer-node->type (ewoc-data node)) 'file))
		    (not (project-buffer-node->matched (ewoc-data node)))))
      (setq node (ewoc-prev status node)))
    (if node
	(ewoc-goto-node status node)
	(message "Failing backward search."))))


(defun project-buffer-quit ()
  "Burry project-buffer mode or cancel the research."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (unless (project-buffer-clear-matched-mark project-buffer-status)
    (bury-buffer)))


(defun project-buffer-help ()
  "Display help for project-buffer mode."
  (interactive)
  (describe-function 'project-buffer-mode))


(defun project-buffer-next-file (&optional n)
  "Move the cursor down N files."
  (interactive "p")
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (ewoc-goto-next project-buffer-status n))


(defun project-buffer-next-file-or-expand ()
  "Go to the project/folder containing the current file/folder unless the cursor is on a expanded folder/project in which case, it will collapse it."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status    project-buffer-status)
	 (node      (ewoc-locate status))
	 (node-data (and node (ewoc-data node))))
    (when node
      (if (or (eq (project-buffer-node->type node-data) 'file)
	      (not (project-buffer-node->collapsed node-data)))
	  (ewoc-goto-next status 1)
	  (project-buffer-toggle-expand-collapse)
	  ))))


(defun project-buffer-prev-file (&optional n)
  "Move the cursor up N files."
  (interactive "p")
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (ewoc-goto-prev project-buffer-status n))


(defun project-buffer-find-marked-files ()
  "Run find-files on the marked files."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((file-list (project-buffer-get-marked-nodes project-buffer-status))
	 (cnt 0)
	 buffer)
    (project-buffer-clear-matched-mark project-buffer-status)
    (while file-list
      (let ((node (pop file-list)))
	(when (eq (project-buffer-node->type node) 'file)
	  (setq buffer (find-file-noselect (project-buffer-node->filename node))
		cnt (1+ cnt)))))
    (cond ((> cnt 1) (message "Find %i files." cnt))
	  ((= cnt 1) (display-buffer buffer))
	  (t (message "No files selected")))))


(defun project-buffer-go-to-previous-project ()
  "Go to previous project line."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate project-buffer-status))
	 (search (ewoc-prev status node)))
    (while (and search
		(not (eq (project-buffer-node->type (ewoc-data search)) 'project)))
      (setq search (ewoc-prev status search)))
    (when search
      (ewoc-goto-node status search))))


(defun project-buffer-go-to-previous-folder-or-project ()
  "If the cursor is on a file, go up to the previous project/folder.
If the cursor is on a folder, search up for the previous project/folder.
If the cursor is on a project, go to previous project."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status    project-buffer-status)
	 (node      (ewoc-locate project-buffer-status))
	 (node-data (and node (ewoc-data node))))
    (cond ((eq (project-buffer-node->type node-data) 'file)
	   (project-buffer-goto-dir-up))
	  ((eq (project-buffer-node->type node-data) 'folder)
	   (let ((search (ewoc-prev status node)))
	     (while (and search
			 (eq (project-buffer-node->type (ewoc-data search)) 'file))
	       (setq search (ewoc-prev status search)))
	     (when search
	       (ewoc-goto-node status search))))
	  ((eq (project-buffer-node->type node-data) 'project)
	   (let ((search (ewoc-prev status node)))
	     (while (and search
			 (not (eq (project-buffer-node->type (ewoc-data search)) 'project)))
	       (setq search (ewoc-prev status search)))
	     (when search
	       (ewoc-goto-node status search))))
	  (t (error "Unknown node type! (%S)" (project-buffer-node->type node-data))))))


(defun project-buffer-go-to-next-project ()
  "Go to next project line."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate project-buffer-status))
	 (search (ewoc-next status node)))
    (while (and search
		(not (eq (project-buffer-node->type (ewoc-data search)) 'project)))
      (setq search (ewoc-next status search)))
    (when search
      (ewoc-goto-node status search))))


(defun project-buffer-go-to-next-folder-or-project ()
  "If the cursor is on a file, go down to the next project/folder.
If the cursor is on a folder, search down for the next project/folder.
If the cursor is on a project, go to next project."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status    project-buffer-status)
	 (node      (ewoc-locate project-buffer-status))
	 (node-data (and node (ewoc-data node)))
	 (fold-ok   (and node
			 (not (eq (project-buffer-node->type node-data) 'project))
			 (eq project-buffer-view-mode 'folder-view)))
	 (search    (and node (ewoc-next status node))))
  
    (while (and search
		(not (eq (project-buffer-node->type (ewoc-data search)) 'project))
		(not (and fold-ok
			  (eq (project-buffer-node->type (ewoc-data search)) 'folder))))
	    (setq search (ewoc-next status search)))
    (when search
      (ewoc-goto-node status search))))


(defun project-buffer-find-file ()
  "Open the file that the cursor is on."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node)))
    (project-buffer-clear-matched-mark project-buffer-status)
    (if (eq (project-buffer-node->type node-data) 'file)
	(find-file (project-buffer-node->filename node-data))
	(project-buffer-toggle-expand-collapse))))


(defun project-buffer-find-file-other-window ()
  "Open the file that the cursor is on in another window."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node)))
    (project-buffer-clear-matched-mark project-buffer-status)
    (if (eq (project-buffer-node->type node-data) 'file)
	(find-file-other-window (project-buffer-node->filename node-data))
	(project-buffer-toggle-expand-collapse))))


(defun project-buffer-mark-file ()
  "Mark the file that the cursor is on and move to the next one."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node)))
    (when (eq (project-buffer-node->type node-data) 'file)
      (setf (project-buffer-node->marked node-data) t)
      (ewoc-invalidate project-buffer-status node))
    (ewoc-goto-next project-buffer-status 1)))


(defun project-buffer-unmark-file ()
  "Unmark the file that the cursor is on and move to the next one."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node)))
    (setf (project-buffer-node->marked node-data) nil)
    (ewoc-invalidate project-buffer-status node)
    (when (eq node (ewoc-locate project-buffer-status))
      (ewoc-goto-next project-buffer-status 1))))


(defun project-buffer-mark-all ()
  "Mark all files."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (ewoc-map (lambda (node) (when (and (eq (project-buffer-node->type node) 'file)
				      (not (project-buffer-node->marked node)))
                             (setf (project-buffer-node->marked node) t)))
	    project-buffer-status))


(defun project-buffer-unmark-all ()
  "Unmark all files."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (ewoc-map (lambda (node) (when (and (eq (project-buffer-node->type node) 'file)
				      (project-buffer-node->marked node))
                             (setf (project-buffer-node->marked node) nil) t))
	    project-buffer-status))


(defun project-buffer-toggle-all-marks ()
  "Toggle all file mark."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (ewoc-map (lambda (node) (when (eq (project-buffer-node->type node) 'file)
			     (setf (project-buffer-node->marked node) (not (project-buffer-node->marked node))) t))
	    project-buffer-status))


(defun project-buffer-toggle-expand-collapse-even-on-file ()
  "Expand / Collapse project and folder that the cursor is on.
If the cursor is on a file - search up for the nearest folder and collapse it."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node      (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node))
	 (status    project-buffer-status))
    (project-buffer-clear-matched-mark status)
    (when (eq (project-buffer-node->type node-data) 'file)
      (setq node (and node (project-buffer-find-node-up status node)))
      (when node (ewoc-goto-node status node)))
    (when node
      (project-buffer-toggle-expand-collapse))))


(defun project-buffer-toggle-expand-collapse ()
  "Expand / Collapse project and folder that the cursor is on.
If the cursor is on a file - nothing will be done."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node      (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node))
	 (status    project-buffer-status)
	 prj-sel
	 hidden-flag
	 project
	 skip-under
	 folder)
    (project-buffer-clear-matched-mark status)
    (unless (eq (project-buffer-node->type node-data) 'file)
      (when (eq (project-buffer-node->type node-data) 'folder)
	(setq folder (project-buffer-node->name node-data)))
      (setf (project-buffer-node->collapsed node-data) (not (project-buffer-node->collapsed node-data)))
      (setq hidden-flag (project-buffer-node->collapsed node-data))
      (setq prj-sel (eq (project-buffer-node->type node-data) 'project))
      (when prj-sel
	(setf (project-buffer-node->project-collapsed node-data) hidden-flag))
      (ewoc-invalidate status node)
      (setq project (project-buffer-node->project node-data)
	    node (ewoc-next status node))
      (while node
	(setq node-data (ewoc-data node))
	(when skip-under
	  (unless (project-buffer-parent-of-p (project-buffer-node->name  node-data) skip-under)
	    (setq skip-under nil)))
	(if (and (string-equal (project-buffer-node->project node-data) project)
		 (or (not folder)
		     (project-buffer-parent-of-p (project-buffer-node->name  node-data) folder)))
	    (progn
	      (when prj-sel
		(setf (project-buffer-node->project-collapsed node-data) hidden-flag)
		(ewoc-invalidate status node))
	      (unless skip-under
		(setf (project-buffer-node->hidden node-data) hidden-flag)
		(ewoc-invalidate status node)
		(if (and (eq (project-buffer-node->type node-data) 'folder)
			 (project-buffer-node->collapsed node-data)
			 (not hidden-flag))
		    (setq skip-under (project-buffer-node->name node-data))))
	      (setq node (ewoc-next status node)))
	    (setq node nil))))))


(defun project-buffer-toggle-view-mode ()
  "Toggle between the different view mode (folder-view / flat-view / folder-hidden-view)."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (ewoc-locate project-buffer-status)))
    (setq project-buffer-view-mode
	  (cond ((eq project-buffer-view-mode 'folder-view)        'flat-view)
		((eq project-buffer-view-mode 'flat-view)          'folder-hidden-view)
		((eq project-buffer-view-mode 'folder-hidden-view) 'marked-view)
		((eq project-buffer-view-mode 'marked-view)        'folder-view)
		))
    (let ((status project-buffer-status))
      (message "View mode set to: %s" project-buffer-view-mode)
      (project-buffer-refresh-all-items status)
      (project-buffer-refresh-ewoc-hf status)
      (ewoc-goto-node status node)
      )))


(defun project-buffer-toggle-search-mode()
  "Toggle between the different search-in-files mode (narrow-marked-files / all-files / current-project)."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let ((node (ewoc-locate project-buffer-status)))
    (setq project-buffer-search-in-files-mode
	  (cond ((eq project-buffer-search-in-files-mode 'narrow-marked-files) 'all-files)
		((eq project-buffer-search-in-files-mode 'all-files)         'current-project)
		((eq project-buffer-search-in-files-mode 'current-project)   'narrow-marked-files)))
    (let ((status project-buffer-status))
      (message "Search mode set to: %s" project-buffer-search-in-files-mode)
      (project-buffer-refresh-ewoc-hf status)
      (ewoc-goto-node status node)
      )))


(defun project-buffer-choose-build-configuration()
  "Ask the user for the build configuration using a completion list"
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (unless project-buffer-build-configurations-list (error "No build configuration available"))
  (if (cdr project-buffer-build-configurations-list)
      (let ((new-build-configuration (completing-read "Build-Configuration: " project-buffer-build-configurations-list nil t)))
	(when (and new-build-configuration (> (length new-build-configuration) 0))
	  (setq project-buffer-current-build-configuration new-build-configuration)))
      (message "This is the only one build configuration available."))
  (project-buffer-refresh-ewoc-hf project-buffer-status))


(defun project-buffer-next-build-configuration ()
  "Select next build configuration (rotate through them)."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (unless project-buffer-build-configurations-list (error "No build configuration available"))
  (if (cdr project-buffer-build-configurations-list)
      (let ((current (member project-buffer-current-build-configuration project-buffer-build-configurations-list)))
	(unless current (error "The current build configuration is invalid"))
	(setq project-buffer-current-build-configuration (or (and (cdr current) (cadr current))
							 (car project-buffer-build-configurations-list)))
	(message "Build configuration set to: %s" project-buffer-current-build-configuration))
      (message "This is the only one build configuration available."))
  (project-buffer-refresh-ewoc-hf project-buffer-status))


(defun project-buffer-choose-platform ()
  "Ask the user for the platform using a completion list."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (unless project-buffer-platforms-list (error "No build configuration available"))
  (if (cdr project-buffer-platforms-list)
      (let ((new-platform (completing-read "Platform: " project-buffer-platforms-list nil t)))
	(when (and new-platform (> (length new-platform) 0))
	  (setq project-buffer-current-platform new-platform)))
      (message "This is the only one platform available."))
  (project-buffer-refresh-ewoc-hf project-buffer-status))


(defun project-buffer-next-platform ()
  "Select next platform (rotate through them)."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (unless project-buffer-platforms-list (error "No build configuration available"))
  (if (cdr project-buffer-platforms-list)
      (let ((current (member project-buffer-current-platform project-buffer-platforms-list)))
	(unless current (error "The current build configuration is invalid"))
	(setq project-buffer-current-platform (or (and (cdr current) (cadr current))
						    (car project-buffer-platforms-list)))
	(message "Platform set to: %s" project-buffer-current-platform))
      (message "This is the only one platform available."))
  (project-buffer-refresh-ewoc-hf project-buffer-status))


(defun project-buffer-choose-master-project ()
  "Prompt the user for the master project."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((status    project-buffer-status)
	(proj-name (completing-read "Enter the master project: " project-buffer-projects-list nil t)))
    (when (and proj-name
	      (> (length proj-name) 0))
      (let ((old-node (cdr project-buffer-master-project))
	    (cur-node (project-buffer-search-project-node status proj-name)))
	;; Let's replace the old node by the new one
	(message "Results: %s %s" proj-name (project-buffer-node->name (ewoc-data cur-node)))
	(setq project-buffer-master-project (cons (project-buffer-node->name (ewoc-data cur-node)) cur-node))
	;; Force the refresh:
	(ewoc-invalidate status old-node)
	(ewoc-invalidate status cur-node)
	(ewoc-goto-node status cur-node)))))


(defun project-buffer-select-current-as-master-project ()
  "Make the current project the new master project."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((status project-buffer-status)
	(old-node (cdr project-buffer-master-project))
	(cur-node (ewoc-locate project-buffer-status)))
    ;; Search for the project node:
    (while (and cur-node
		(not (eq (project-buffer-node->type (ewoc-data cur-node)) 'project)))
      (setq cur-node (project-buffer-find-node-up status cur-node)))
    ;; Let's replace the old node by the new one
    (setq project-buffer-master-project (cons (project-buffer-node->name (ewoc-data cur-node))
					      cur-node))
    ;; Force the refresh:
    (ewoc-invalidate status old-node)
    (ewoc-invalidate status cur-node)
    (ewoc-goto-node status cur-node)))


(defun project-buffer-perform-build-action ()
  "Run the user hook to perform the build action."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-perform-action-hook 'build))


(defun project-buffer-perform-clean-action ()
  "Run the user hook to perform the build action."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-perform-action-hook 'clean))


(defun project-buffer-perform-run-action ()
  "Run the user hook to perform the build action."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-perform-action-hook 'run))


(defun project-buffer-perform-debug-action ()
  "Run the user hook to perform the build action."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-perform-action-hook 'debug))


(defun project-buffer-mark-files-containing-regexp (regexp &optional unmark)
  "Mark all files containing REGEXP -- A prefix argument means to UNMARK the files containing the REGEXP instead."
  (interactive
   (list (project-buffer-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
					     " files containing (regexp): "))
	 current-prefix-arg))
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node))
	 (current-project (project-buffer-node->project node-data))
	 (count (cond ((eq project-buffer-search-in-files-mode 'narrow-marked-files)
		      (project-buffer-refine-mark-files project-buffer-status regexp (not unmark)))
		     ((eq project-buffer-search-in-files-mode 'all-files)
		      (project-buffer-search-and-mark-files project-buffer-status regexp nil (not unmark)))
		     ((eq project-buffer-search-in-files-mode 'current-project)
		      (project-buffer-search-and-mark-files project-buffer-status regexp current-project (not unmark))))))
    (message "%i files %s." 
	     count 
	     (if (or unmark
		     (eq project-buffer-search-in-files-mode 'narrow-marked-files))
		 "unmarked" "marked"))
    ))


(defun project-buffer-mark-matched-files-or-current-file(force-marked-current)
  "Mark the matched files or the current file if no filename research are in progress or if FORCE-MARKED-CURRENT is set."
  (interactive "P")
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let (result)
    (unless (or force-marked-current
		(not (project-buffer-node->matched (ewoc-data (ewoc-locate project-buffer-status)))))
      (ewoc-map (lambda (node-data)
		  (when (and (eq (project-buffer-node->type node-data) 'file)
			     (project-buffer-node->matched node-data))
		    (setf (project-buffer-node->marked node-data) t)
		    (setq result t)))
		project-buffer-status))
    (unless result
      (project-buffer-mark-file))))


(defun project-buffer-unmark-matched-files-or-current-file(force-unmarked-current)
  "Unmark the matched files or the current file if no filename research are in progress or if FORCE-UNMARKED-CURRENT is set."
  (interactive "P")
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let (result)
    (unless (or force-unmarked-current
		(not (project-buffer-node->matched (ewoc-data (ewoc-locate project-buffer-status)))))
      (ewoc-map (lambda (node-data)
		  (when (and (eq (project-buffer-node->type node-data) 'file)
			     (project-buffer-node->matched node-data))
		    (setf (project-buffer-node->marked node-data) nil)
		    (setq result t)))
		project-buffer-status))
    (unless result
      (project-buffer-unmark-file))))


(defun project-buffer-view-file ()
  "Examine the current file using the view mode."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node)))
    (when (eq (project-buffer-node->type node-data) 'file)
      (view-file (project-buffer-node->filename node-data)))))
      



;;
(provide 'project-buffer-mode)

;;; project-buffer-mode.el ends here
