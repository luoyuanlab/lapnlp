;;; -*- Mode: Lisp; Package: late -*-

#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 04/26/2012 creation 

Finds the cross reference among records, save them as annotations. Currently 
the pattern are based on specific observations, I defer the generalization as 
later steps
Note: This needs to precede tokenization step.
|#

(defpackage :late
  (:use :common-lisp :util)
  (:export 
   "cross-ref"))

(in-package :late)

(defparameter *cref-square-brackets-pattern* "\\[[^\\[\\]\\(\\)]*[A-Z]+-?\\d+-?[A-Z]?-?\\d{3,}[^\\[\\]\\(\\)]*\\]")

(defparameter *cref-round-brackets-pattern* "\\([^\\[\\]\\(\\)]*[A-Z]+-?\\d+-?[A-Z]?-?\\d{3,}[^\\[\\]\\(\\)]*\\)")

(defparameter *cref-none-provided-pattern* "\\(?\\[?\\{?none\\s+(provided|given|entered)\\.?\\}?\\]?\\)?")

(defparameter *cref-sent-pattern* "^\\W*\\(?\\[?((also\\s+)?see\\s+|please\\s+(also\\s+)?see|please\\s+(also\\s+)?refer|please\\s+(also\\s+)?call)")

(defparameter *cref-literature-pattern* "\\([^\\(\\)]*(et +al|\\d+:\\d+-\\d+|\\d+;\\d+:\\d+)[^\\(\\)]*\\)")

(defparameter *cref-sent-del-addendum-pattern* "THIS\\s+ADDENDUM\\s+HAS\\s+BEEN\\s+DELETED")

;; templates for FDA requirement
(defparameter *cref-sent-fda-pat* "(FDA|Food\\s+and\\s+Drug\\s+Administration)")

(defun scan-pat (doc pat pat-name)
  (format t "looking at pat: ~a~%" pat-name)
  (let* ((txt (content doc))
	 (tlen (length txt))
	 mstart mend)
    (do* ((probe 0 mend))
	((>= probe tlen))
      (multiple-value-bind (m? whole)
	  (match-re pat txt :start probe :return :index)
	(cond 
	 (m?
	  (setf mstart (car whole)
		mend (cdr whole))
	  (format t "~&cross-ref (~a-~a): ~a~%" mstart mend (subseq txt mstart mend))
	  (let* ((cx-ref (make-instance (gtype 'gcross-ref-type)
					:document doc
					:start mstart
					:end mend
					:sw-ver (gsw-ver 'gcross-ref)
					:setting (gsetting 'gcross-ref))))
	    (add-annotation doc cx-ref)))
	 (t
	  (setf mend tlen)))))))

(defun scan-sen-pat (sen pat pat-name)
  (let* ((doc (document sen)))
    (when (match-re pat (content sen) :case-fold t)
      (let ((ann (make-instance (gtype 'gcross-ref-type)
				:document doc
				:start (start sen)
				:end (end sen)
				:sw-ver (gsw-ver 'gcross-ref)
				:setting (gsetting 'gcross-ref))))
	(format t "~&looking at sen pat: ~a~%" pat-name)
	(format t "~&cross-ref (~a-~a): ~a~%" (start sen) (end sen) (content sen))
	(add-annotation doc ann)))))

(defmethod cross-ref ((doc document))
  "Add cross-reference annotations to document. Sentence annotations must be present.
Input
======
doc: document to be cross ref analyzed."
  (assert (gtype 'gcross-ref-type)
	  ()
	  "No cross ref type specified!")
  (unless (analyzedp doc :ganalysis 'gsentencize)
    (format t "~&Warning: Cross-ref depends on having sentence-split ~
                 document ~a, which has not occurred.  Use (sentencize doc).~%" 
	    doc)
    (return-from cross-ref nil))
  

  (scan-pat doc *cref-literature-pattern* "literature")
  (scan-pat doc *cref-square-brackets-pattern* "square-bra")
  (scan-pat doc *cref-round-brackets-pattern* "round-bra")
  (scan-pat doc *cref-none-provided-pattern* "none-provided")
  
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (scan-sen-pat sen *cref-sent-pattern* "see")
    (scan-sen-pat sen *cref-sent-fda-pat* "fda")
    (scan-sen-pat sen *cref-sent-del-addendum-pattern* "del-adden")
    )
  
  (add-analysis doc :ganalysis 'gcross-ref))

(defmethod cross-ref ((corp corpus))
  (dolist (docid (documents corp))
    (let* ((doc (document docid)))
      (format t "~&cross ref analyzing: ~a~%" (name doc))
      (cross-ref doc)
      (save doc))))
