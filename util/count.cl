;;; -*- Mode: Lisp; Package: util; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 08/01/2012 creation

Utility to count the elements of a sequence, for example to create a
histogram.
|#
(defpackage :util
  (:use :common-lisp :regexp)
  (:export "count-all" "write-counts"))
(in-package :util)

(defun count-all (seq &optional (test #'equal) (order #'>))
  "Counts the elements of SEQ, using an efficient hash-table
   implementation. TEST must be an acceptable type for
   constructing a hash-table, and order is the sort order
   that determines the ordering of results by counts.
   The result is a list of lists, each of which contains a
   (unique) element of SEQ and its count of occurrences."
  (let ((ht (make-hash-table :test test))
        (res nil))
    (map nil #'(lambda (item)
		 (incf (gethash item ht 0)))
      seq)
    (maphash #'(lambda (k v) (push (list k v) res)) ht)
    (sort res order :key #'cadr)))


(defun write-counts (ct filename)
  (with-open-file 
      (f filename :direction :output :if-exists :supersede)
    (dolist (x ct) (format f "~4d ~a~%" (cadr x) (car x)))))
