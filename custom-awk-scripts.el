;; 
;; Script used to bind the awk scripts to emacs lisp functions
;;


;;------------------------------------------------------------------------------
;;
;; Configuration:
;;
;;------------------------------------------------------------------------------

(defvar gawk-executable-path "qawk"
  "Path to gawk (if required)")

(defun user-home-directory() (expand-file-name "script" "~/.emacs.d"))

(defvar gawk-script-align-comments (concat (user-home-directory) "/align-comments.awk")
  "Path to the script used to align the eol comments.")
(defvar gawk-script-align-variable-assignment (concat (user-home-directory) "/align-variable-assignment.awk")
  "Path to the script used to align the variable name and also to align the assignment symbol.")
(defvar gawk-script-comment-block (concat (user-home-directory) "/comment-block.awk")
  "Path to the script used to comment a block of code.")


(defvar gawk-script-spu-instruction-path (concat (user-home-directory) "/spu-instr.awk")
  "Path to the script which defined the spu instruction set.")
(defvar gawk-script-reformat-path (concat (user-home-directory) "/reformat.awk")
  "Path to the reformat script.")
(defvar gawk-script-rollup-dependency-report-path (concat (user-home-directory) "/rollup-dependency-report.awk")
  "Path to the script which generate the dependency report for a portion of code.")
(defvar gawk-script-rollup-fusion-path (concat (user-home-directory) "/rollup-fusion.awk")
  "Path to the script which fusion several loop into one.")
(defvar gawk-script-format-dump-file-path (concat (user-home-directory) "/format-dump-file.awk")
  "Path to the sxript which will format a dump text generated by the compiler.")



;;------------------------------------------------------------------------------
;;
;; C++ related script
;;
;;------------------------------------------------------------------------------

(defun cpp-align-comment()
  "Align the eol comments within the selected region -- using a gawk script"
  (interactive "*") 
  (if (is-region-active)
	  (save-excursion
		(let ((reg (extend-region-to-full-lines (region-beginning) (region-end))))
		  (call-process-region 
		   (car reg) 
		   (cdr reg) 
		   gawk-executable-path t t t 
		   "-f" gawk-script-align-comments)
		  (dos-to-unix)))
	  (message "There is no active region!")))

(defun cpp-align-variable-assignment()
  "Align the variables and also the assignment symbol ('=')"
  (interactive "*") 
  (if (is-region-active)
	  (save-excursion
		(let ((reg (extend-region-to-full-lines (region-beginning) (region-end))))
		  (call-process-region 
		   (car reg) 
		   (cdr reg) 
		   gawk-executable-path t t t 
		   "-f" gawk-script-align-variable-assignment)
		  (dos-to-unix)))
	(message "There is no active region!")))

(defun cpp-align-function-bracket()
  "Align the function name and also the open bracket symbols"
  (interactive "*") 
  (if (is-region-active)
	  (save-excursion
		(let ((reg (extend-region-to-full-lines (region-beginning) (region-end))))
		  (call-process-region 
		   (car reg) 
		   (cdr reg) 
		   gawk-executable-path t t t 
		   "-vassignment=("
		   "-f" gawk-script-align-variable-assignment)
		  (dos-to-unix)))
	(message "There is no active region!")))

(defun cpp-comment-block()
  "Comment/UnComment a block of code using the C++ type of comment '//'"
  (interactive "*") 
  (let ((start (if (is-region-active) (region-beginning) (point-at-bol)))
		(end   (if (is-region-active) (region-end) (point-at-eol))))
	(save-excursion
	  (let ((reg (extend-region-to-full-lines start end)))
		(call-process-region 
		 (car reg) 
		 (cdr reg) 
		 gawk-executable-path t t t 
		 "-f" gawk-script-comment-block)
		(dos-to-unix)))))


;;------------------------------------------------------------------------------
;;
;; Awk related script
;;
;;------------------------------------------------------------------------------

(defun awk-align-comment()
  "Align the eol comments within the selected region -- using a gawk script"
  (interactive "*") 
  (if (is-region-active)
	  (save-excursion
		(let ((reg (extend-region-to-full-lines (region-beginning) (region-end))))
		  (call-process-region 
		   (car reg) 
		   (cdr reg) 
		   gawk-executable-path t t t 
		   "-vcomment=#"
		   "-f" gawk-script-align-comments)
		  (dos-to-unix)))
	  (message "There is no active region!")))

(defun awk-comment-block()
  "Comment/UnComment a block of code using the C++ type of comment '//'"
  (interactive "*") 
  (let ((start (if (is-region-active) (region-beginning) (point-at-bol)))
		(end   (if (is-region-active) (region-end) (point-at-eol))))
	(save-excursion
	  (let ((reg (extend-region-to-full-lines start end)))
		(call-process-region 
		 (car reg) 
		 (cdr reg) 
		 gawk-executable-path t t t 
		 "-vcomment=#~"
		 "-f" gawk-script-comment-block)
		(dos-to-unix)))))


;;------------------------------------------------------------------------------
;;
;; Spu related script
;;
;;------------------------------------------------------------------------------

(defun spu-reformat-region()
  "Reformat the current region of the code -- using a gawk script"
  (interactive "*") 
  (save-excursion
	(call-process-region 
	 (region-beginning) 
	 (region-end) 
	 gawk-executable-path t t t 
     "-f" gawk-script-spu-instruction-path
	 "-f" gawk-script-reformat-path)
	(dos-to-unix)))

(defun spu-rollup-dependency-report-region()
  "Report the dependency of a specific loop during a rollup process
Those dependency must be filled up during the previous loop!"
  (interactive "*")
  (save-excursion
	(call-process-region 
	 (region-beginning) 
	 (region-end) 
	 gawk-executable-path t t t 
     "-f" gawk-script-spu-instruction-path
	 "-f" gawk-script-rollup-dependency-report-path)
	(dos-to-unix)))

(defun spu-rollup-fusion-region()
  "Fusion several loop into the final rollup version!"
  (interactive "*") 
  (save-excursion
	(call-process-region 
	 (region-beginning) 
	 (region-end) 
	 gawk-executable-path t t t 
     "-f" gawk-script-spu-instruction-path
	 "-f" gawk-script-rollup-fusion-path)
	(dos-to-unix)))

(defun spu-format-dump-file-region()
  "Format generated dump spu file"
  (interactive "*")
  (save-excursion
	(call-process-region
	 (region-beginning)
	 (region-end)
	 gawk-executable-path t t t 
     "-f" gawk-script-spu-instruction-path
	 "-f" gawk-script-format-dump-file-path)
	(dos-to-unix)))

;;------------------------------------------------------------------------------
;;
;; DDF related script
;;
;;------------------------------------------------------------------------------
