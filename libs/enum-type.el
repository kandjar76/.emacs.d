;;; enum-type.el -- Providing the definition of an enumerated type within emacs.
;;
;; Author: Cedric Lallain <kandjar76@hotmail.com>
;; Version:  1.0
;; Keywords: enum type
;; Description: Provide the definition of an enumerated type
;; Tested with: GNU Emacs 21.x and GNU Emacs 22.x
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; When you write a function you sometimes want to restrict the value to some
;; specific symbols. This files allow you to create an enumerated type.
;; It will provide a predicate in order to test is a specific symbol belongs
;; to this type.
;;
;; (defenum my-test 'test1 'test2)
;; (my-test-p 'test1) => t
;; (my-test-p 'test3) => nil
;; my-test => (:enum-type 'test1 'test2)
;;
;; e.g:
;;  (defenum boolean 'true 'false)
;;  (defenum logmode "Log mode" 'debug-verbose 'verbose 'quiet)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;------------------------------------------------------------------------------
;;
;;   Private Section
;;
;;------------------------------------------------------------------------------

(defmacro defenum-define-predicate (enum-type)
  "PRIVATE - Define the predicate function for the enum type ENUM-TYPE"
  (let ((enum-name (make-symbol "enum-name"))
	(predicate-function-name    (make-symbol "predicate-function-name"))
	(predicate-function-comment (make-symbol "predicate-function-comment")))
    (let* ((enum-name (symbol-name enum-type))
	   (predicate-function-name (eval `(intern ,(concat enum-name "-p"))))
	   (predicate-function-comment (concat "Return t if SYMBOL belong to the enumerated type " enum-name)))
      `(defun ,predicate-function-name (symbol) ,predicate-function-comment 
	 (and (symbolp symbol)
	      (member symbol (cdr ,enum-type))
	      t)))))

;; ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

;;------------------------------------------------------------------------------
;;
;;   Public Section
;;
;;------------------------------------------------------------------------------

(defmacro defenum(enum-type &rest args)
  "Create a enumerated type ENUM-TYPE."
  (let ((symbols-list (make-symbol "fields"))
	(comment      (make-symbol "comment")))
    (let ((comment (and (stringp (car-safe args))
			(pop args)))
	  (symbols-list (reduce (lambda (a b) (and a (symbolp (car b)))) (cons t args))))
      (if symbols-list
	  `(progn (defconst ,enum-type (list :enum-type ,@args) ,comment)
		  (defenum-define-predicate ,enum-type))
	  (error "Invalid symbols list. Syntax: (defenum [comment] {symbol})")))))

(defun enump(enum-type)
  "Returns t if ENUM-TYPE is a enumerated type"
  (and (eq :enum-type (car-safe enum-type))
       (reduce (lambda (a b) (and a (symbolp b))) (cons t (cdr-safe enum-type)))
       t))

(provide 'enum-type)