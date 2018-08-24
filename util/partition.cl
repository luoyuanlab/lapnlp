;;; -*- Mode:Lisp; Package: util; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 08/01/2012 creation
|#

;;; Utilities to partition and count elements of a sequence,
;;;
;;; partition splits a sequence into a set of sequences, each of which shares
;;;  the same key; for example, a list of classes could be split into
;;;  subsets that meet at the same time using time as the key.
;;;
;;; count-all performs a computation similar to partition, but only returns
;;;  the number of elements sharing each key; if the key is the element
;;;  itself, this counts the number of duplications

(defpackage :util
  (:use :common-lisp :regexp)
  (:export "partition" "count-all" "hash-counts"))

(in-package :util)

(defun hash-counts (ht &optional (sorter #'>))
  "Returns a list of 2-lists, each holding one item of the input hash
  table and the value (count) associated with it. If sorter is non
  NIL, it is used as a comparator to sort these 2-lists by the count."
  (let ((ans nil))
    (maphash #'(lambda (key val)
		 (push (list key val) ans))
	     ht)
    (if (null sorter)
	ans
      (sort ans sorter :key #'cadr))))

(defun partition (seq &key (key nil) (test #'eq) (sort nil))
  "Creates a partition of SEQ on unique values of KEY (default is the
  item itself, though this is not often useful).
  Result is a-list of (key . partition) pairs.
  Comparisons by TEST, which must be a valid test for hash-tables
  Result is sorted by the comparator applied to result pairs, unless NIL."
  (let ((h (make-hash-table :test test))
	(res nil))
    (map nil #'(lambda (x)
		 (let ((v (if key (funcall key x) x)))
		   (push x (gethash v h nil))))
	 seq)
    (maphash #'(lambda (k v) (push (cons k v) res))
	     h)
    (if sort
	(sort res sort)
      res)))

(defun count-all (seq &key
		      (key nil)
		      (test #'eq)
		      (sort #'(lambda (a b) (> (cdr a) (cdr b)))))
  "Counts the number of occurrences of each element of SEQ, where
  equality is determined by TEST, which must be an acceptable test for
  creating hash-tables. Returns a list of pairs, each holding an
  element of SEQ and the number of its occurrences. SORT, if not NIL,
  is an ordering on these pairs, and defaults to returning them in
  descending order of counts."
  (let ((h (make-hash-table :test test))
	(res nil))
    (map nil #'(lambda (x)
		 (let ((v (if key (funcall key x) x)))
		   (incf (gethash v h 0))))
	 seq)
    (maphash #'(lambda (k v) (push (cons k v) res))
	     h)
    (if sort
	(sort res sort)
      res)))
