;;; -*- Mode: Lisp; Package: late -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 06/08/2013 creation 

Finds the coded-text among records, save them as annotations. Currently 
the pattern are based on specific observations, I defer the generalization as 
later steps
|#

(defpackage :late
  (:use :common-lisp :util :umlsize)
  (:export 
   "coded-text"
   "coded-status"))

(in-package :late)

;; . is in the end in case pattern is at the end of sentence
(defparameter *cunit* "[a-z][\\w\\-/+]+(\\+|-|bright|dim|\\s+focal\\s+golgi\\+|\\s+focal\\s*\\+|\\s+\\(faint\\))(\\s+\\((faint|with\\s+high\\s+background)\\))?")
(defparameter *pmdb-pattern* 
  (concatenate 'string 
	       "(?i)\\(?" *cunit* "(,?(\\s+and)?\\s+" *cunit* 
	       ")*\\)?\\s*,?(?=(\\W|$))"))

(defmethod coded-text ((sen sentence-annotation))
  (let* ((senstr (content sen))
	 (sstart (start sen))
	 (send (end sen))
	 (slen (- send sstart))
	 (doc (document sen))
	 mstart mend)

    ;; handles 'cyclin D1-'
    (dolist (tui (if-tuis sen))
      (let* ((tstr (content tui))
	     (tstart (- (start tui) sstart))
	     (tend (- (end tui) sstart)))
	(setf tstr (replace-re tstr "\\s" "x"))
	(setf senstr (replace senstr tstr :start1 tstart :end1 tend))))

    (format t "~&[coded-text] senstr: ~a~%" senstr)
    
    (do* ((probe 0 mend))
	((>= probe slen))
      (multiple-value-bind (m? whole)
	  (match-re *pmdb-pattern* senstr :start probe :return :index :case-fold t)
	(cond
	 (m?
	  (setf mstart (car whole)
		mend (cdr whole))
	  (multiple-value-bind (sm? suff)
	      (match-re "\\s+$" (subseq senstr mstart mend))
	    (when sm?
	      (setf mend (- mend (length suff)))))
	  
	  
	  (let* ((ca (make-instance 'coded-text
				    :document doc
				    :start (+ sstart mstart)
				    :end (+ sstart mend)
				    :sw-ver (gsw-ver 'gcoded-text)
				    :setting (gsetting 'gcoded-text)))
		 ca-str)
	    (add-annotation doc ca)
	    (add-annotation sen ca)
	    
	    (setf ca-str (content ca))
	    (let* ((m1 (match-re "^\\(?(?<g1>.*?)\\)?\\s*,?$" ca-str
				 :return :match :single-line t))
		   (old-mstart (start ca))
		   g1)
	      (when (and m1 (not (in-noun-ph? ca)))
		(setf g1 (re-submatch m1 nil nil "g1" :type :index))
		(setf (start ca) (+ old-mstart (car g1))
		      (end ca) (+ old-mstart (cdr g1)))))

	    
	    (setf ca-str (content ca))

	    (when (/= (count #\( ca-str) (count #\) ca-str))
	      (format t "~&[coded-text] imbalanced paren in ~a~%" ca-str))
	    (format t "~&[coded-text] ~a(~a): ~a-~a [~a]~%" 
		    (name doc) (id sen) (start ca) (end ca) ca-str)))
	 (t
	  (setf mend slen)))))))

(defmethod coded-text ((doc document))
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (coded-text sen))
  (add-analysis doc :ganalysis 'gcoded-text)
  )


(defmethod coded-text ((corp corpus))
  (dolist (docid (documents corp))
    (let* ((doc (document docid)))
      (when (and (ganalysis 'gcoded-text)
		 ;; (not (analyzedp doc :ganalysis 'gcoded-text))
		 )
	(format t "~&~a document ~a~%" (ganalysis 'gcoded-text) (name doc))
	(time-profiling (coded-text doc))
	(save doc)))))



(defun coded-status (a)
  (if (annotations-spanning a :type 'coded-text)
      "part"
    nil))
