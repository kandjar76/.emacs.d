;;; record-type.el -- Redefinition of find-dired
;;
;; Author:   Cedric Lallain
;; Version:  0.9
;; Keywords: record-type
;; Description: Record type definition.
;; Tested with: GNU Emacs 21.x and GNU Emacs 22.x

;; This file is *NOT* part of GNU Emacs.

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

;;; Commentary:



;; TODO: Docs!
;;  default value


;; Control function to remember:
;;
;;  integer-or-marker-p  | int
;;  stringp              | string
;;  symbolp              | symbol
;;  fboundp              | function
;; 


;;------------------------------------------------------------------------------
;;
;;   Private Section
;;
;;------------------------------------------------------------------------------

(defmacro defrecord-define-set-code (record-type field)
  "PRIVATE - Define the prive set function to be able to set the field FIELD value in the record RECORD-TYPE"
  (let ((field-name (make-symbol "field-name-private"))
	(set-function-name (make-symbol "set-function-name-private")))
    (let* ((field-name (if (string= (substring (symbol-name field) 0 1) ":")
			   (substring (symbol-name field) 1)
			   (symbol-name field)))
	   (set-function-name (eval `(intern ,(concat "set-" (symbol-name record-type) "-" field-name "-private")))))
      `(defun ,set-function-name (instance value) (set-record-field instance ,field value)))))

(defmacro defrecord-define-set-macro-code (record-type field)
  "PRIVATE - Define the set macro to have a direct write on the field FIELD in the record RECORD-TYPE"
  (let ((field-name (make-symbol "field-name"))
	(set-macro-name (make-symbol "set-macro-name"))
	(set-function-name (make-symbol "set-function-name")))
    (let* ((field-name (if (string= (substring (symbol-name field) 0 1) ":")
			   (substring (symbol-name field) 1)
			   (symbol-name field)))
	   (set-macro-name (eval `(intern ,(concat "set-" (symbol-name record-type) "-" field-name))))
	   (set-function-name (eval `(intern (concat "set-" (symbol-name record-type) "-" field-name "-private")))))
      `(defmacro ,set-macro-name (instance value) (list 'setq instance (list ',set-function-name instance value))))))

(defmacro defrecord-define-get-code (record-type field)
  "PRIVATE - Define the get function to return the value of the FIELD in an instance of the record RECORD-TYPE"
  (let ((field-name (make-symbol "field-name"))
	(record-name (make-symbol "record-name"))
	(get-function-name (make-symbol "get-function-name")))
    (let* ((field-name (if (string= (substring (symbol-name field) 0 1) ":")
			   (substring (symbol-name field) 1)
			   (symbol-name field)))
	   (record-name (symbol-name record-type))
	   (get-function-name (eval `(intern ,(concat "get-" record-name "-" field-name)))))
      `(defun ,get-function-name (instance) (get-record-field instance ,field)))))


(defmacro defrecord-define-field-functions(record-type field2)
  "PRIVATE - Create the get and set functions for the record RECORD-TYPE."
  `(progn (defrecord-define-set-code ,record-type ,(eval field2))
	  (defrecord-define-set-macro-code ,record-type ,(eval field2))
	  (defrecord-define-get-code ,record-type ,(eval field2))))


(defun defrecord-private(record-type comment &rest args)
  "PRIVATE - Function used to create the structure of RECORD-TYPE.
Create a record type with a specific comment COMMENT.
A record is constituated with a list of coupe symbol / type-checker"
  (if (stringp comment)
      (let ((new-type (list :define-record comment))
	    (field-defs args))
	(while field-defs
	  (let ((field-name (pop field-defs))
		(type-checker (pop field-defs)))
	    (if (symbolp field-name)
		(if (symbol-function type-checker)
		    (setq new-type (append new-type (list (cons field-name type-checker)))))
		(error "Wrong type of argument, expecting a symbol type"))))
	(set record-type new-type))
      (error "Invalid name type: the comment must be a string!")))


(defmacro defrecord-define-predicate (record-type)
  "PRIVATE - Define the predicate function for the record RECORD-TYPE"
  (let ((record-name (make-symbol "record-name"))
	(predicate-function-name (make-symbol "predicate-function-name")))
    (let* ((record-name (symbol-name record-type))
	   (predicate-function-name (eval `(intern ,(concat record-name "-p")))))
      `(defun ,predicate-function-name (instance) (and (instancep instance)
						       (eq ,record-type (car instance)))))))

;; ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

;;------------------------------------------------------------------------------
;;
;;   Public Section
;;
;;------------------------------------------------------------------------------

(defmacro defrecord(record-type comment &rest args)
  "Create a record type RECORD-TYPE.
A record is constituated with a list of coupe symbol / type-checker"
  (let ((fields (make-symbol "fields"))
	(current-field-name (make-symbol "current-field-name")))
    `(progn (defrecord-private (quote ,record-type) ,comment ,@args) 
	    (defrecord-define-predicate ,record-type)
	    (let ((fields (cddr ,record-type)))
	      (while fields 
		(let ((current-field-name (caar fields)))
		  (defrecord-define-field-functions ,record-type current-field-name)
		  (pop fields)))))))

(defun recordp(record)
  "Return t if RECORD's type if record"
  (if (and (listp record)
	   (and (symbolp (car record))
		(eq (car record) :define-record))
	   (stringp (cadr record)))
      (let ((result t)
	    (to-check (cddr record)))
	(while (and result to-check)
	  (setq result (and result
			    (symbolp (caar to-check))
			    (symbolp (cdar to-check))
			    (fboundp (cdar to-check))))
	  (pop to-check))
	result)))

(defun instancep(instance)
  "Return t if INSTANCE is an instance of a record type"
  (and (listp instance) 
       (recordp (car instance))))

(defun get-record-comment(record)
  "Return the comment attached to the record!"
  (and (recordp record)
       (cadr record)))

(defun make-new-record(record-type &rest args)
  "Create a new instance of the record"
  (if (recordp record-type)
      (let* ((instance (append (list record-type) (mapcar (lambda(head) nil) (cddr record-type)))))
        (apply 'set-record-fields instance args))))

(defun get-record-type(instance)
  "Return the record type of the instance INSTANCE"
  (and (instancep instance)
       (car instance)))

(defun has-record-field(record-type field-name)
  "Return true if the record RECORD-TYPE has the field FIELD-NAME"
  (if (and (recordp record-type)
	   (symbolp field-name))
      (let ((fields (cddr record-type))
	    (found nil))
	(while (and (not found)
		    fields)
	  (setq found (eq (car (pop fields))
			      field-name)))
	found)))

(defun set-record-field(instance field-name field-value)
  "Set a specific field value in INSTANCE"
  (if (and (instancep instance)
	   (symbolp field-name))
      (let ((new-instance (list (car instance)))
	    (fields (cddar instance))
	    (values (cdr instance))
	    (not-found t))
	(while (and not-found
		    fields)
	  (if (and (eq (caar fields) field-name)
		   (funcall (cdar fields) field-value))
	      (progn (setq not-found nil)
		     (setq new-instance (append new-instance (list field-value) (cdr values))))
	      (progn (setq new-instance (append new-instance (list (pop values))))
		     (pop fields))))
	(if (not not-found)
	    new-instance
	    (error (format "This record doesn't contain the field %s or the associated value's type is invalid"
			   ;(get-record-name (car instance))
			   (symbol-name field-name)))))
      (if (instancep instance)
	  (error "Invalid data: you need to pass an instance of a struct to this function!")
	  (error "Invalid data: missing field"))))

(defun set-record-fields(instance &rest args)
  "Set the value of several fields to the record INSTANCE"
  (let ((new-instance instance))
    (while args 
      (setq new-instance (set-record-field new-instance (pop args) (pop args))))
    new-instance))

(defun get-record-field(instance field-name)
  "Retrieve the value of a specific field"
  (if (and (instancep instance)
	   (symbolp field-name))
      (let ((fields (cddar instance))
	    (values (cdr instance))
	    (not-found t)
	    (field-value nil))
	(progn (while (and not-found
			   fields)
		 (progn (if (eq (caar fields) field-name)
			    (progn (setq not-found nil)
				   (setq field-value (car values))))
			(pop fields)
			(pop values)))
	       field-value))))


;; Code sample:
;; --------------------------
;;
;; (defrecord test "Comment for the record test" 
;;   :string 'stringp
;;   :symbol 'symbolp
;;   :int 'number-or-marker-p)
;; 
;; (recordp test)
;; (get-record-comment test)
;; (get-record-type (make-new-record test))
;; (has-record-field test :string)
;;
;; (setq itest (make-new-record test))
;; (setq itest (set-record-field itest :symbol 'sym))
;; (get-record-field (set-record-fields itest :int  3 :string "test") :int)
;;
;;
;;
;; (defrecord test2 "This is a comment for test2" 
;;   :string 'stringp
;;   :symbol 'symbolp
;;   :int 'number-or-marker-p)
;;
;; (setq itest2 (make-new-record test2))
;; (set-test2-string itest2 "pouet")
;; (get-test2-int itest2)
;;
;; (setq itest3 (make-new-record test2 :string "pouet" :int 50))
;;
;; (let ((field-name "test"))
;;   (get-test2-string itest2)
;;   (message field-name))
;; (let ((field-name "test"))
;;   (set-test2-string itest2 "ok")
;;   (message field-name))


(provide 'record-type)