;;; -*- Mode: Lisp; Package: late -*-
#||
yluo - 08/16/2018 clean and reorganization
yluo - 06/08/2015 rewrite using asdf framework
yluo - 06/19/2013 creation 
Finds the range adj among records such as in 'single to multiple nuclei'
the pattern are based on specific observations for immunologic factors, I defer the generalization as later steps

JJ to JJ
JJ to RB VBN
||#

(defpackage :late
  (:use :common-lisp :util)
  (:export 
   "range-adj"
   "has-overlap-range-adj?"))

(in-package :late)


(defparameter *range-adj-pat* "(?i)(@+\\s+)*(#+\\s+)+(versus|to)(\\s+@+)*(\\s+#+)+")
;; no and

(defmethod range-adj ((sen sentence-annotation))
  (let* ((senstr (content sen))
	 (slen (length senstr))
	 (sstart (start sen))
	 (doc (document sen))
	 mstart mend)
    (setf senstr (replace-re senstr "[#@]" "_"))

    (dolist (adv-pos (adv-ptags sen))
      (when (not (annotations-spanning adv-pos :type 'coded-text))
	(let* ((pstart (- (start adv-pos) sstart))
	       (pend (- (end adv-pos) sstart))
	       (rstr (make-array (- pend pstart)
				 :element-type 'character
				 :initial-element #\@)))
	  (setf senstr (replace senstr rstr :start1 pstart :end1 pend)))))

    (dolist (adj-pos (adj-ptags sen))
      (when (not (annotations-spanning adj-pos :type 'coded-text))
	(let* ((pstart (- (start adj-pos) sstart))
	       (pend (- (end adj-pos) sstart))
	       (rstr (make-array (- pend pstart)
				 :element-type 'character
				 :initial-element #\#)))
	  (setf senstr (replace senstr rstr :start1 pstart :end1 pend)))))

    (dolist (vbn-pos (vbn-ptags sen))
      (when (not (annotations-spanning vbn-pos :type 'coded-text))
	(let* ((pstart (- (start vbn-pos) sstart))
	       (pend (- (end vbn-pos) sstart))
	       (rstr (make-array (- pend pstart)
				 :element-type 'character
				 :initial-element #\#)))
	  (setf senstr (replace senstr rstr :start1 pstart :end1 pend)))))

    (do* ((probe 0 mend))
	((>= probe slen))
      (multiple-value-bind (m? whole)
	  (match-re *range-adj-pat* senstr :start probe :return :index :case-fold t)
	(cond
	 (m?
	  (setf mstart (car whole)
		mend (cdr whole))
	  (format t  "~&[range adj] ~a(~a): ~a-~a [~a]~%"
		  (name doc) (id sen) mstart mend 
		  (subseq (content sen) mstart mend))
	  (let* ((ia (make-instance 'range-adj
				    :document doc
				    :start (+ sstart mstart)
				    :end (+ sstart mend)
				    :sw-ver (gsw-ver 'grange-adj)
				    :setting (gsetting 'grange-adj))))
	    (add-annotation doc ia)
	    (add-annotation sen ia)))
	 (t
	  (setf mend slen)))))))


(defmethod range-adj ((doc document))
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (range-adj sen))
  (add-analysis doc :ganalysis 'grange-adj))

(defun range-adj-status (a)
  (if (annotations-spanning a :type 'range-adj)
      "part"
    nil))

(defun has-overlap-range-adj? (a)
  (annotations-overlap a :type 'range-adj))
