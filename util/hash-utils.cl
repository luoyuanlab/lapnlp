;;; -*- Mode: Lisp; Package: User; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 05/20/2011 create
|#

(defpackage :util
  (:use :common-lisp)
  (:export 
   "hash-keys"
   "hash-vals"
   "hash-table-alist"
   "alist-hash-table"
   "list-hash-table"
   "cnt-gethash"
   "hash-table-val-desc-alist"
   "hash-table-val-ascd-alist"
   "hash-table-key-desc-alist"
   "hash-table-key-ascd-alist"
   "read-idx-hash"
   "write-idx-hash"
   "read-hash"
   "write-hash"))

(in-package :util)

(defun hash-table-alist (table)
  "Returns an association list containing the keys and values of hash table TABLE."
  (let ((alist nil))
    (maphash #'(lambda (k v)
		 (push (cons k v) alist))
             table)
    alist))

(defun alist-hash-table (alist)
  "Converts from an association list to a hash table"
  (let* ((table (make-hash-table :test #'equalp)))
    (mapcar #'(lambda (a) (setf (gethash (car a) table) (cdr a))) alist)
    table))

(defun list-hash-table (list)
  "Converts from an association list to a hash table"
  (let* ((table (make-hash-table :test #'equalp)))
    (mapcar #'(lambda (a) 
		(pushnew (gethash (first a) table) (second a) :test #'equalp)) 
	    list)
    table))

(defun hash-keys (table)
  "Returns a list of keys of hash table"
  (let ((keys nil))
    (maphash #'(lambda (k v)
		 (declare (ignorable v))
		 (push k keys))
             table)
    keys))

(defun hash-vals (table)
  "Returns a list of keys of hash table"
  (let ((vals nil))
    (maphash #'(lambda (k v)
		 (declare (ignorable k))
		 (push v vals))
             table)
    vals))

(defun hash-table-val-desc-alist (table)
  "Returns the sorted a list of the form (key . val) according to val in 
descending order"
  (sort (hash-table-alist table) #'> :key #'cdr))

(defun hash-table-val-ascd-alist (table)
  "Returns the sorted a list of the form (key . val) according to val in 
ascending order"
  (sort (hash-table-alist table) #'< :key #'cdr))

(defun hash-table-key-desc-alist (table)
  "Returns the sorted a list of the form (key . val) according to key in 
descending order"
  (sort (hash-table-alist table) #'> :key #'car))

(defun hash-table-key-ascd-alist (table)
  "Returns the sorted a list of the form (key . val) according to key in 
ascending order"
  (sort (hash-table-alist table) #'< :key #'car))


(defun write-idx-hash (h fn)
  (let* ((f (open-new fn)))
    (dolist (kv (hash-table-val-ascd-alist h))
      (format f "~a~%" (car kv)))
    (close f)))

(defun read-idx-hash (fn)
  (let* ((f (open fn :direction :input))
	 (h (make-hash-table :test #'equalp))
	 (i 0)
	 ln)
    (loop (unless (setf ln (read-ne-line f)) (return))
	  (setf (gethash ln h) (incf i)))
    (close f)
    h))

(defun write-hash (h fn)
  (let* ((f (open-new fn)))
    (dolist (kv (hash-table-alist h))
      (format f "~a| ~a~%" (car kv) (cdr kv)))
    (close f)))

(defun print-hash (h)
  (let* ((kvs (hash-table-alist h)))
    (dolist (kv kvs)
      (format t "~a| ~a~%" (car kv) (cdr kv)))
    ))

(defun read-hash (fn &key (fmt "int"))
  (let* ((f (open fn :direction :input))
	 (h (make-hash-table :test #'equalp))
	 ln)
    (loop (unless (setf ln (read-ne-line f)) (return))
	  (destructuring-bind (k v)
	      (split-re "\\| " ln)
	    (when (equalp fmt "int")
	      (setf k (parse-integer k))
	      (setf v (parse-integer v)))
	    (setf (gethash k h) v)))
    (close f)
    h))

(defun cnt-gethash (k h)
  (let* ((v (gethash k h)))
    (unless v
      (setf v (1+ (hash-table-count h)))
      (setf (gethash k h) v))
    v))
