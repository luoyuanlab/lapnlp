;;; -*- Mode: Lisp; Package: late -*-
#| 
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 09/01/2010 creation 
|#

(defpackage :late
  (:use :common-lisp :util :umlsize)
  (:export 
   "*debug-mask*" 
   "filter-cui-mention"
   "filter-tui-mention"
   "mask"
   "maskedp"
   "not-masked"
   "sen-mask-diff"))

(in-package :late)

(defparameter *debug-mask* nil)

(defmethod filter-cui-mention ((doc document)
			       (exclude-cui-hash hash-table)) 
  " input: document, hash table of CUIs to be excluded
output: a mask list whose entry is a conse of (start . end) marking
regions not to be looked at.
"
  (let* (mask-list)
    ;; filter out document
    (dolist (cui (annotations-spec doc :type 'cui-annotation))
      (when (gethash (format nil "~a" (data cui)) exclude-cui-hash)
	(pushnew (cons (start cui) (end cui)) mask-list)))
    ;; sort and merge
    (setf mask-list (sort mask-list #'(lambda (x y)
					(or (< (car x) (car y))
					    (and (= (car x) (car y))
						 (> (cdr x) (cdr y)))))))
    (if *debug-mask*
	(format t "~&old mask is: ~a~%" mask-list))
    (let* (cleaned-mask-list elt-i smark emark)
      (do* ((i 0 (incf i)))
	  ((= i (length mask-list)))
	(setf elt-i (elt mask-list i))
	
	(cond
	 ((null smark)
	  (setf smark (or smark (car elt-i))
		emark (or emark (cdr elt-i))))
	 ;; if contain or overlap or touch
	 ((<= (car elt-i) emark)
	  (cond 
	   ;; if contained, ignore i
	   ((<= (cdr elt-i) emark))
	   ;; if overlap or touch, expand window
	   (t
	    (setf emark (cdr elt-i)))))
	 ((> (car elt-i) emark)
	  (pushnew (cons smark emark) cleaned-mask-list)
	  (setf smark (car elt-i)
		emark (cdr elt-i)))))
      (when smark
	(pushnew (cons smark emark) cleaned-mask-list))
      (setf mask-list (nreverse cleaned-mask-list)))

    (when *debug-mask*
      ;; output document with masked parts marked.
      ;; union section annotation and sentence annotation
      ;; for section annotation, simply output
      ;; for sentence annotation, put braces around masked parts
      (format t "~&Report: ~a~%" (name doc))
      (let* ((bio "o")
	     (secs (annotations doc :type 'section-head-annotation))
	     (sents (annotations doc :type 'sentence-annotation))
	     (secs-sents (sort (append secs sents) #'annotation-lessp))
	     (dup-mask-list mask-list)
	     (mask (or (pop dup-mask-list)
		       (cons (size doc) (size doc)))))
	(format t "~&mask is: ~a~%" dup-mask-list)
	(dolist (ann secs-sents)
	  (cond
	   ((typep ann 'section-head-annotation)
	    (format t "~&~%~a~%" (content ann)))
	   ((typep ann 'sentence-annotation)
	    (dolist (tok (annotations-spec 
			  ann 
			  :type (gtype 'gtoken-type)))
	      (cond
	       ((and (<= (car mask) (start tok) (end tok) (cdr mask))
		     (equal bio "o"))
		(format t " {~a" (content tok))
		(setf bio "i"))
	       ((and (<= (cdr mask) (start tok))
		     (equal bio "i"))
		(format t "} ~a" (content tok))
		(setf bio "o")
		(setf mask (or (pop dup-mask-list) mask)))
	       (t
		(format t " ~a" (content tok)))))
	    (format t "~%"))
	   (t
	    (error "unexpected annotation ~a" ann))))))
    mask-list))

(defmethod filter-tui-mention ((doc document)
			       (exclude-tuis list)) 
  " input: document, list of TUIs to be excluded
output: a mask list whose entry is a conse of (start . end) marking
regions not to be looked at.
"
  (let* (mask-list)
    ;; filter out document
    (dolist (tui (annotations-spec doc :type 'tui-annotation))
      (when (member (format nil "~a" (data tui)) exclude-tuis :test #'equalp)
	(pushnew (cons (start tui) (end tui)) mask-list)))
    ;; sort
    (setf mask-list (sort mask-list #'(lambda (x y)
					(or (< (car x) (car y))
					    (and (= (car x) (car y))
						 (> (cdr x) (cdr y)))))))
    (if *debug-mask*
	(format t "~&old mask is: ~a~%" mask-list))
    
    ;; and merge
    (let* (cleaned-mask-list elt-i smark emark)
      (do* ((i 0 (incf i)))
	  ((= i (length mask-list)))
	(setf elt-i (elt mask-list i))
	
	(cond
	 ((null smark)
	  (setf smark (or smark (car elt-i))
		emark (or emark (cdr elt-i))))
	 ;; if contain or overlap or touch
	 ((<= (car elt-i) emark)
	  (cond 
	   ;; if contained, ignore i
	   ((<= (cdr elt-i) emark))
	   ;; if overlap or touch, expand window
	   (t
	    (setf emark (cdr elt-i)))))
	 ((> (car elt-i) emark)
	  (pushnew (cons smark emark) cleaned-mask-list)
	  (setf smark (car elt-i)
		emark (cdr elt-i)))))
      (when smark
	(pushnew (cons smark emark) cleaned-mask-list))
      (setf mask-list (nreverse cleaned-mask-list)))

    (when *debug-mask*
      ;; output document with masked parts marked.
      ;; union section annotation and sentence annotation
      ;; for section annotation, simply output
      ;; for sentence annotation, put braces around masked parts
      (format t "~&Report: ~a~%" (name doc))
      (let* ((bio "o")
	     (secs (annotations doc :type 'section-head-annotation))
	     (sents (annotations doc :type 'sentence-annotation))
	     (secs-sents (sort (append secs sents) #'annotation-lessp))
	     (dup-mask-list mask-list)
	     (mask (or (pop dup-mask-list)
		       (cons (size doc) (size doc)))))
	(format t "~&mask is: ~a~%" dup-mask-list)
	(dolist (ann secs-sents)
	  (cond
	   ((typep ann 'section-head-annotation)
	    (format t "~&~%~a~%" (content ann)))
	   ((typep ann 'sentence-annotation)
	    (dolist (tok (annotations-spec 
			  ann 
			  :type (gtype 'gtoken-type)))
	      (cond
	       ((and (<= (car mask) (start tok) (end tok) (cdr mask))
		     (equal bio "o"))
		(format t " {~a" (content tok))
		(setf bio "i"))
	       ((and (<= (cdr mask) (start tok))
		     (equal bio "i"))
		(format t "} ~a" (content tok))
		(setf bio "o")
		(setf mask (or (pop dup-mask-list) mask)))
	       (t
		(format t " ~a" (content tok)))))
	    (format t "~%")
	    )
	   (t
	    (error "unexpected annotation ~a" ann))))))
    mask-list))

(defmethod mask ((doc document)
		 (mask-l list)
		 (mask-type string))
  (dolist (mask-c mask-l)
    (let* ((m-toks (annotations 
		    doc 
		    :type (gtype 'gtoken-type)
		    :filter #'(lambda (a) 
				(and (>= (end a) (car mask-c))
				     (<= (start a) (cdr mask-c))
				     (not (typep a 'alp-token))))))
	   (mask (make-instance 'mask-annotation
				:document doc
				:start (car mask-c)
				:end (cdr mask-c)
				:data mask-type
				:h-down m-toks)))
      (add-annotation doc mask)))
  (add-analysis doc :ganalysis 'gmask)
  (save doc))

(defun sen-mask-diff (corpn)
  (let* (doc ans)
    (dolist (docid (documents (corpus corpn)))
      (setf doc (document docid))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(when (annotations-spec sen :type (gtype 'gtoken-type))
	  (when (and (maskedp sen)
		     (not (maskedp sen)))
	    (push sen ans)))))
    ans))

(defmethod maskedp ((sent sentence-annotation))
  (maskedp (content sent)))


(defmethod maskedp ((tui tui-annotation))
  (equalp (data tui) "T191"))

(defmethod maskedp ((str string))
  ;; The purpose is to avoid looking into the text that may indicate the ground truth
  (or (match-re "(burkit|burket)" str :case-fold t)
      (match-re "\\bBL\\b" str)
      (match-re "\\bDLBCL\\b" str)
      (match-re "(?is)(follicular|follicle).*(type|origin)" str :case-fold t)
      (match-re "\\bFL\\b" str)
      (match-re "(?i)\\b(nlphl|nlphd|hl|hd)\\b" str)
      (match-re "(?is)(ghsg|bnli|ihsg|german.*hodgkin.*study.*group)" str)
      (match-re "\\bNHL\\b" str)
      (match-re "(?i)hodgkin" str)
      (match-re "(?i)lymphoma" str)
      (match-re "(?i)leukemia" str)
      (match-re "(?i)carcinoma" str)
      (match-re "(?i)adenoma" str)
      (match-re "(?is)diffuse.*large.*b.*cell" str)
      (match-re "(?i)sarcoma" str)
      (match-re "(?i)T/HRBCL" str)
      (match-re "(?i)\\bSCC\\b" str)
      (match-re "\\bALL\\b" str)
      (match-re "(?is)(nodular\\s+sclerosis|mixed\\s+cellularity|lymphocyte-rich.*type|lymphocyte\\s+predominant)" str)
      (match-re "(?i)(history|support|diagnosis|grade)" str)
      ))


(defmethod maskedp ((pnode parse-node))
  (maskedp (content pnode)))

(defmethod maskedp ((ph phrase-annotation))
  (maskedp (content ph)))


(defmethod maskedp ((tok token-annotation))
  "Needs revisit to use -spec"
  (maskedp (content tok)))

(defun not-masked (a)
  "Strict, no overlapping"
  (not (annotations-share a :type 'phrase-annotation
			  :filter #'maskedp)))
