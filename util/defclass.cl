;;; -*- Mode:Lisp; Package: util; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation
|#
(defpackage :util
  (:use :common-lisp :regexp)
  (:export "defclass*"))

(in-package :util)

(defmacro defclass*
    (class-name superclass-names slot-specifiers &rest class-options) 
  "Shorthand notation for defclass.
In defclass, a slot-specifier that is just a name does not create
accessor, initform or initarg options. In this variant, we do. Any
slot-options that are explicitly given will be used in place of our
defaults."
  (let ((specs
	 (mapcar
	  #'(lambda (spec)
	      (when (symbolp spec) (setq spec (list spec)))
	      (let ((pl (cdr spec))
		    (clauses (list :accessor (car spec)
				   :initarg (intern (car spec) :keyword)
				   :initform nil))
		    (none (list 'none)))
		(labels ((handle (kwd)
				 (let ((kwv (getf pl kwd none)))
				   (if (not (eq kwv none))
				       (setf (getf clauses kwd) kwv)))))
		  (mapc #'handle '(:accessor :reader :writer :allocation
					     :initform :initarg :type :documentation)))
		(cons (car spec) clauses)))
	  slot-specifiers)))
    `(defclass ,class-name ,superclass-names ,specs ,@class-options)))

