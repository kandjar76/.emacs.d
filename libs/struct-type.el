;;-----------------------------------------------------
;;
;;   Complex type helper:
;;
;;-----------------------------------------------------

(defun defrecord(record-type name &rest type-def)
  "Create a record type named NAME.
A record has a name and is constituated with a list of coupe symbol / type-checker"
  (if (stringp name)
      (let ((new-type (list :define-record name)))
	(while type-def
	  (let ((name (pop type-def))
		(type-checker (pop type-def)))
	    (if (symbolp name)
		(if (symbol-function type-checker)
		    (setq new-type (append new-type (list (cons name type-checker)))))
		(error "Wrong type of argument, expecting a symbol type"))))
	(set record-type new-type))
      (error "Invalid name type: the name must be a string!")))

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

(defun get-record-name(record)
  "Return the name of the recordure!"
  (and (recordp record)
       (cadr record)))

(defun new-record(record-type)
  "Create a new instance of the record"
  (if (recordp record-type)
      (append (list record-type) (mapcar (lambda(head) nil) (cddr record-type)))))

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
	    (error (format "The record %s doesn't contain the field %s or the associated value's type is invalid"
			   (get-record-name (car instance))
			   (symbol-name field-name)))))
      (error "Invalid data!")))

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
;; (defrecord 'test "test" 
;;   :string 'stringp
;;   :symbol 'symbolp
;;   :int 'number-or-marker-p)
;; 
;; (recordp test)
;; (get-record-name test)
;; (get-record-type (new-record test))
;; (has-record-field test :string)
;; (set-record-field (new-record test) :symbol 'sym)
;; (get-record-field (set-record-fields (new-record test) :int  3 :string "test") :int)
;;

(provide 'record-type)