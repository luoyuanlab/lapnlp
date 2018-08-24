;;; -*- Mode: Lisp; Package: late -*-


#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 08/08/2012 creation 
Finds the format marker in records, save them as annotations. Currently 
the pattern are based on specific observations, I defer the generalization as 
later steps
Note: This needs to precede tokenization step.
|#

(defpackage :late
  (:use :common-lisp :util)
  (:export 
   "format-mark"))

(in-package :late)

(defparameter *fmark-separator1-pat* 
  (compile-re "^[\\s\\-]*-[\\s\\-]*$" :multiple-lines t))
(defparameter *fmark-separator2-pat* 
  (compile-re "^[\\s\\=]*=[\\s\\=]*$" :multiple-lines t))

(defmethod format-mark ((doc document))
  "Add format-marker annotations to document. Sentence annotations must be present.
Input
======
doc: document to be format marker analyzed."
  (assert (gtype 'gformat-mark-type)
      ()
    "No format mark type specified!")
  
  
  (let* ((txt (content doc))
		 (tlen (length txt))
		 mstart mend)
    (do* ((probe 0 mend))
		((>= probe tlen))
	  ;; separator pattern 1
	  (multiple-value-bind (m? whole)
		  (match-re *fmark-separator1-pat* txt :start probe :return :index)
		(cond 
		 (m?
		  (setf mstart (car whole)
				mend (cdr whole))
		  (let* ((fmark (make-instance (gtype 'gformat-mark-type)
						  :document doc
						  :start mstart
						  :end mend
						  :sw-ver (gsw-ver 'gformat-mark)
						  :setting (gsetting 'gformat-mark))))
			(add-annotation doc fmark)))
		 (t
		  (setf mend tlen)))))
    	  	  
	;; separator pattern 2
    (do* ((probe 0 mend))
		((>= probe tlen))
	  (multiple-value-bind (m? whole)
		  (match-re *fmark-separator2-pat* txt :start probe :return :index)
		(cond 
		 (m?
		  (setf mstart (car whole)
				mend (cdr whole))
		  (let* ((fmark (make-instance (gtype 'gformat-mark-type)
						  :document doc
						  :start mstart
						  :end mend
						  :sw-ver (gsw-ver 'gformat-mark)
						  :setting (gsetting 'gformat-mark))))
			(add-annotation doc fmark)))
		 (t
		  (setf mend tlen))))))
  (add-analysis doc :ganalysis 'gformat-mark))
