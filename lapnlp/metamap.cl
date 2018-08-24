;;; -*- Mode: Lisp; Package: late -*-
#| 
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 06/06/2013 creation 
|#

(defpackage :metamap
  (:use :common-lisp :util :umlsize :late)
  (:export
   "load-metamap"
   "metamap"
   "write-unmasked-sen"
   "write-unmasked-corpus"))

(in-package :metamap)

(defparameter *debug-metamap* nil)

(defun load-metamap ()
  (unless *metamap*
    (setf *metamap* (mmwrapper.MetaMapMEDG:MetaMapMEDG))))

(defmethod metamap ((sen sentence-annotation))
  (load-metamap)
  (when (annotations-spec sen :type (gtype 'gtoken-type))
    (let* ((doc (document sen))
	   (senstr (replace-re (content sen) "\\n" " "))
	   (sen-offset (start sen))
	   (jl-res (mmwrapper.MetaMapMEDG:processSen *metamap* senstr))
	   (jnegs (mmwrapper.MetaMapMEDG:getNegations jl-res))
	   (jcons (mmwrapper.MetaMapMEDG:getConcepts jl-res))
	   (neg-len (mmwrapper.MetaMapMEDG:negationsLength jnegs))
	   (con-len (mmwrapper.MetaMapMEDG:conceptsLength jcons)))
      (dotimes (i neg-len)
	(let* ((neg (mmwrapper.MetaMapMEDG:getNegation jnegs i))
	       (nstart (mmwrapper.MetaMapMEDG:getNegStart neg))
	       (nend (mmwrapper.MetaMapMEDG:getNegEnd neg))
	       (na (make-instance 'metamap-neg
				  :document doc
				  :start (+ sen-offset nstart)
				  :end (+ sen-offset nend)
				  :sw-ver (gsw-ver 'gmetamap)
				  :setting (gsetting 'gmetamap))))
	  (add-annotation doc na)
	  (add-annotation sen na)))

      (dotimes (i con-len)
	(let* ((con (mmwrapper.MetaMapMEDG:getConcept jcons i))
	       (cstart (mmwrapper.MetaMapMEDG:getConceptStart con))
	       (cend (mmwrapper.MetaMapMEDG:getConceptEnd con))
	       (head? (mmwrapper.MetaMapMEDG:conceptIsHead con))
	       (cname (mmwrapper.MetaMapMEDG:getConceptName con))
	       (cui (mmwrapper.MetaMapMEDG:getCUI con))
	       (ca (make-instance 'metamap-con
				  :document doc
				  :start (+ sen-offset cstart)
				  :end (+ sen-offset cend)
				  :data (list cui cname)
				  :head head?
				  :sw-ver (gsw-ver 'gmetamap)
				  :setting (gsetting 'gmetamap))))
	  (add-annotation doc ca)
	  (add-annotation sen ca))))))

(defmethod metamap ((doc document))
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (metamap sen))
  (add-analysis doc :ganalysis 'gmetamap))

(defmethod metamap ((corp corpus))
  (dolist (docid (documents corp))
    (let* ((doc (document docid)))
      (when (and ;; (ganalysis 'gmetamap)
	     (not (analyzedp doc :ganalysis 'gmetamap)))
	(format t "~&metamap document ~a (~a)~%" 
		(name doc) (late::doc-ann-cnt doc))
	(time-profiling (mm::metamap doc))
	(save doc)))))

(defmethod metamap ((senstr string))
  (load-metamap)
  (let* ((jl-res (mmwrapper.MetaMapMEDG:processSen *metamap* senstr))
	 (jnegs (mmwrapper.MetaMapMEDG:getNegations jl-res))
	 (jcons (mmwrapper.MetaMapMEDG:getConcepts jl-res))
	 (neg-len (mmwrapper.MetaMapMEDG:negationsLength jnegs))
	 (con-len (mmwrapper.MetaMapMEDG:conceptsLength jcons)))
    (format t "~&Negations:~%")
    (dotimes (i neg-len)
      (let* ((neg (mmwrapper.MetaMapMEDG:getNegation jnegs i))
	     (nstart (mmwrapper.MetaMapMEDG:getNegStart neg))
	     (nend (mmwrapper.MetaMapMEDG:getNegEnd neg)))
	(format t "[~a-~a]~%" nstart nend)))

    (format t "~&Concepts:~%")
    (dotimes (i con-len)
      (let* ((con (mmwrapper.MetaMapMEDG:getConcept jcons i))
	     (cstart (mmwrapper.MetaMapMEDG:getConceptStart con))
	     (cend (mmwrapper.MetaMapMEDG:getConceptEnd con))
	     (head? (mmwrapper.MetaMapMEDG:conceptIsHead con))
	     (cname (mmwrapper.MetaMapMEDG:getConceptName con)))
	(format t "[~a-~a] {head? ~a} ~a~%" cstart cend head? cname)))))

(defun write-unmasked-sen (doc &key dir)
  (let* (senstr output f)
    (dolist (sen (annotations doc :type 'sentence-annotation))
      (setf output nil)
      (setf senstr (outstr-init))
      (unless (maskedp sen)
	(dolist (ta (annotations-spec sen :type (gtype 'gtoken-type)))
	  (format senstr "~a " (content ta))
	  (setf output t))
	(when output
	  (setf f (open-new (format nil "~a~a_~a.sen" dir (name doc) (id sen))))
	  (format f "~a" senstr)
	  (close f))))))

(defun write-unmasked-corpus (corpn)
  (let* ((corp (corpus corpn))
	 (dirn (format nil "data:;~a_unmask;" corpn))
	 (cnt 0)
	 doc)
    (create-dir (namestring (translate-logical-pathname dirn)))
    (cmd-verbose (format nil "rm -rf *") :dir dirn)
    (dolist (docid (documents corp))
      (incf cnt)
      (when (= 0 (mod cnt 500))
	(format t "~&processed ~a docs ~%" cnt))
      (setf doc (document docid))
      (write-unmasked-sen doc :dir dirn))))



