;;; -*- Mode: Lisp; Package: util; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Defmemo is like defun, but creates a weak hash-table that memoizes
;;;the results of the call to the defined function, so the next time
;;;it is called with the same arguments, the value is looked up
;;;rather than computed again.  This only makes sense if the
;;;computation is costly.  The hash-table is created with weak-keys so
;;;that the computed objects may be garbage-collected if they are not
;;;otherwise referenced in the computation.
;;;
;;; Defmemo is used exactly with the same syntax as defun, and
;;;operates in the same way. In addition to the defun that it creates,
;;;it also does a defparameter of a hash-table that stores a mapping
;;;from the arguments to the value. We parse the lambda-list of the
;;;procedure begin defined to extract all the variables that are bound
;;;by the defun, and a list of their values is used as the hash
;;;key. This handles optional, keyword and rest arguments, but
;;;introduces some possible redundancy.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :util
  (:use :common-lisp)
  (:export "defmemo"))


(in-package :util)

;;; this is indeed weird
(defmacro defmemo (procname (&rest args) &body body)
  
  (let* ((arglistvar (gensym))
	 (valvar (gensym))
	 (foundvar (gensym))
	 (vars (lambda-list-vars args))
	 (hashname (intern (concatenate 'string "*" (string procname) "-hash*"))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defparameter ,hashname (make-hash-table :test #'equal :weak-keys t))
       (defun ,procname ,args
	 (let ((,arglistvar (list ,@vars)))
	   (multiple-value-bind (,valvar ,foundvar)
	       (gethash ,arglistvar ,hashname)
	     (unless ,foundvar
	       (setq ,valvar (progn ,@body))
	       (setf (gethash ,arglistvar ,hashname) ,valvar))
	     ,valvar)))
       (export '(,hashname ,procname))
       )
  ))

(defun lambda-list-vars (lambda-list)
  "Given a lambda-list such as what one might write for a defun,
  returns a list of the variable names used there."
  (do ((l lambda-list (cdr l))
       (ans nil)
       (opt-key nil)
       (rest nil))
      ((null l) (nreverse ans))
    (cond ((member (car l) '(&optional &key &allow-other-keys))
	   (setq opt-key t rest nil))
	  ((eq (car l) '&rest)
	   (setq rest t))
	  (rest (push (car l) ans))
	  (opt-key
	   (push (if (atom (car l)) (car l) (caar l)) ans))
	  (t (push (car l) ans)))))
