;;; -*- Mode: Lisp; Package: util; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 08/01/2012 creation
|#

;;;; This is a set of utilities to manipulate an xml version of a
;;;; .plist file, as used by (among others) a lot of Apple utilities,
;;;; such as iPhoto.
;;;;
;;;; A plist xml file seems to consist of a top-level of just two
;;;; expressions, the first of which just gives the version number of
;;;; the plist format.  The second is a dict structure.  Dicts are all
;;;; of the form
;;;; (dict (key "...") datum (key "...") datum ...), or approximately
;;;; like a Lisp property list.  A datum may be any of the following:
;;;; a boolean, (true) or (false)
;;;; an atomic value (integer 3) (real 1234.5678) or (string "foobar")
;;;; an array, (array datum datum ...)
;;;; a dict, as above

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :pxml))

(defpackage :util
  (:use :common-lisp #+allegro :excl)
  (:export "read-plist" "getp" "getv" "getpv" "getav" "getpath"
	   "explore-plist"))

(in-package :util)

(defun read-plist (filename)
  "Returns the top-level dict in a .plist file."
  (cadr (car (read-xml filename :package :user))))

(defmacro do-plist-dict (((k v) dict) &body body)
  (let ((l (gensym)))
    `(do ((,l (cdr ,dict) (cddr ,l)))
	 ((null ,l))
       (let ((,k (cadar ,l))
	     (,v (and (cdr ,l) (getv (cadr ,l)))))
	 ,@body))))

(defmacro do-plist-array (((i v) array) &body body)
  (let ((l (gensym)))
    `(do ((,l (cdr ,array) (cdr ,l))
	  (,i 1 (1+ ,i)))
	 ((null ,l))
       (let ((,v (car ,l)))
	 ,@body))))

(defun getp (dict key &optional (test #'equal))
  "Returns the content of a dict that is indexed by the given key."
  (assert (and (consp dict) (eq (car dict) 'user::dict))
      ()
    "Getp called on a non-dict: ~s"
    dict)
  (do-plist-dict ((k v) dict)
    (when (funcall test k key) (return v))))

(defun getv (val)
  "Returns a Lisp-appropriate value from a plist val, essentially
  stripping off type markers and converting to the right type."
  (cond ((null val) nil)
	((atom val) val)
	((member (car val) '(user::integer user::real))
	 (read-from-string (cadr val)))
	((eq (car val) 'user::string)
	 (and (cdr val) (cadr val) (princ-to-string (cadr val))))
	((member (car val) '(user::true user::false))
	 (car val))
	(t val)))

(defun getpv (dict key &optional (test #'equal))
  (getv (getp dict key test)))

(defun getav (array selector)
  "Returns an element of array specified by selector. That can be a
  1-based index or a 2-list if the array consists of dicts, giving a
  key and (optionally) value that selects the dict."
  (assert (eq (car array) 'user::array)
      ()
    "Getav called on non-array: ~s" array)
  (cond ((integerp selector)
	 (and (> selector 0)
	      (<= selector (length array))
	      (elt array selector)	; 1-based, NOT 0-based!
	      ))
	((listp selector)
	 (let ((k (car selector)))
	   (dolist (e (cdr array) nil)
	     (let ((it (getpv e k)))
	       (when (or (null (cdr selector))
			 (not (consp (cdr selector)))
			 (null (cadr selector))
			 (equal it (cadr selector)))
		 (return e))))))
	(t (format t "~%Invalid selector for getav: ~s" selector))))

;;;; This is a vaguely xpath-like facility to make searching through a
;;;; nested plist structure convenient.
;;;; We descend from the original dict, each time using up one element
;;;; of path.  If we are at a dict, that element is used to find the
;;;; key of the target. If we are at an array, then the element should
;;;; be a 2-list of 

(defun getpath (data path &optional (test #'equal))
  (cond ((null path) (getv data))
	((eq (car data) 'user::array)
	 (getpath (getav data (car path)) (cdr path) test))
	((eq (car data) 'user::dict)
	 (assert (atom (car path)) ()
	   "Getpath called on a dict with non-atomic selector: ~s"
	   (car path))
	 (getpath (getp data (princ-to-string (car path)) test)
		  (cdr path)
		  test))
	(t (error "Invalid path ~s in getpath: ~s" path data))))

(defun explore-plist (dict)
  (let ((paths nil))
    (labels ((expl (it path)
	       (when *plist-debug*
		 (let ((*print-length* 3)
		       (*print-level* 2))
		   (print it)))
	       (cond ((or (atom it)
			  (member (car it)
				  '(user::true user::false
				    user::string user::integer
				    user::real))))
		     ((eq (car it) 'user::dict)
		      (let ((newpath (cons (car it) path)))
			(pushnew newpath paths :test #'equal)
			(do-plist-dict ((k v) it)
			  (declare (ignore k))
			  (expl v newpath))))
		     ((eq (car it) 'user::array)
		      (let ((newpath (cons (car it) path)))
			(pushnew newpath paths :test #'equal)
			(dolist (a (cdr it))
			  (expl a newpath))))
		     (t (format t "~%Unknown plist: ~s" it)))))
      (expl dict nil)
      paths)))

(defparameter *plist-debug* nil
  "Controls printing of debugging info for the plist functions.")

(defun search-key (data key &optional (test #'equal))
  (when *plist-debug*
    (let ((*print-length* 3)
	  (*print-level* 2))
      (print data)))
  (cond ((atom data) nil)
	((eq (car data) 'user::array)
	 (dolist (e (cdr data))
	   (let ((found? (search-key e key test)))
	     (when found? (return found?)))))
	((eq (car data) 'user::dict)
	 (do-plist-dict ((k v) data)
	   (if (funcall test k key)
	       (return v)
	     (search-key v key test))))
	(t nil)))
