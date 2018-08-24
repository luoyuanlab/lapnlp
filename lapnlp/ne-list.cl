;;; -*- Mode: Lisp; Package: late -*-
#| 
yluo - 08/16/2018 clean and reorganization
yluo - 06/10/2015 rewrite using asdf framework
yluo - 06/11/2013 creation 

Finds the named entity list among records, save them as annotations. Currently 
the pattern are based on specific observations for immunologic factors, I defer the generalization as later steps
|#

(defpackage :late
  (:use :common-lisp :util :umlsize)
  (:export 
   "if-list"
   "if-adj"
   "if-list-status"
   "if-adj-status"))

(in-package :late)



;; @@ captures the NNS preceding the list, ## is the actual list
;; \\s*? doesn't do what it should do ...
(defparameter *if-lst-pat* "(?i)(@+\\s+)?\\(?#+\\)?(,?(\\s+and|\\s+or)?(^|\\s+)\\(?#+\\)?){1,}")
(defparameter *if-lst-paren-pat* "(?i)@+\\s+\\(((and|or|#+),?\\s*)+\\)")
(defparameter *if-adj-pat* "(@+\\s+)*#+\\s+(@+\\s+)*=+")



(defmethod if-adj ((sen sentence-annotation))
  (let* ((senstr (content sen))
	 (slen (length senstr))
	 (if-tuis (if-tuis sen))
	 (iflists (annotations sen :type 'if-list))
	 (sstart (start sen))
	 (doc (document sen))
	 mstart mend)
    (setf senstr (replace-re senstr "[#@=]" "_"))

    (dolist (adv-pos (adv-ptags sen))
      (let* ((pstart (- (start adv-pos) sstart))
	     (pend (- (end adv-pos) sstart))
	     (rstr (make-array (- pend pstart)
			       :element-type 'character
			       :initial-element #\@)))
	(setf senstr (replace senstr rstr :start1 pstart :end1 pend))))
    
    (dolist (adj-pos (adj-ptags sen))
      (let* ((pstart (- (start adj-pos) sstart))
	     (pend (- (end adj-pos) sstart))
	     (rstr (make-array (- pend pstart)
			       :element-type 'character
			       :initial-element #\=)))
	(when (match-re "(?i)^(positive|negative|bright|dim|strong|weak|bright)$" (content adj-pos))
	  (setf senstr (replace senstr rstr :start1 pstart :end1 pend)))))

    (dolist (iflist iflists)
      (let* ((istart (- (start iflist) sstart))
	     (iend (- (end iflist) sstart))
	     (rstr (make-array (- iend istart)
			       :element-type 'character
			       :initial-element #\#)))
	(setf senstr (replace senstr rstr :start1 istart :end1 iend))))

    (dolist (if-tui if-tuis)
      (let* ((istart (- (start if-tui) sstart))
	     (iend (- (end if-tui) sstart))
	     (rstr (make-array (- iend istart)
			       :element-type 'character
			       :initial-element #\#)))
	(unless (match-re "(\\+|-|bright|dim)$" (content if-tui))
	  (setf senstr (replace senstr rstr :start1 istart :end1 iend)))))
    
    (do* ((probe 0 mend))
	((>= probe slen))
      (multiple-value-bind (m? whole)
	  (match-re *if-adj-pat* senstr :start probe :return :index)
	(cond
	 (m?
	  (setf mstart (car whole)
		mend (cdr whole))
	  (format t  "~&[if adj] ~a(~a): ~a-~a [~a]~%"
		  (name doc) (id sen) mstart mend 
		  (subseq (content sen) mstart mend))
	  (let* ((ia (make-instance 'if-adj
				    :document doc
				    :start (+ sstart mstart)
				    :end (+ sstart mend)
				    :sw-ver (gsw-ver 'gif-adj)
				    :setting (gsetting 'gif-adj))))
	    (add-annotation doc ia)
	    (add-annotation sen ia)))
	 (t
	  (setf mend slen)))))))

(defun add-if-list (sen mstart mend)
  (let* ((sstart (start sen))
	 (doc (document sen))
	 (ia (make-instance 'if-list
			    :document doc
			    :start (+ sstart mstart)
			    :end (+ sstart mend)
			    :sw-ver (gsw-ver 'gif-list)
			    :setting (gsetting  'gif-list)))
	 ia-str)
    (add-annotation doc ia)
    (add-annotation sen ia)
    (setf ia-str (content ia))
    (let* ((m (match-re "^\\s*\\(?(?<g1>[^(]*?)\\)$" ia-str :return :match))
	   (old-start (start ia))
	   g1)
      (when m
	(setf g1 (re-submatch m nil nil "g1" :type :index))
	(setf (start ia) (+ old-start (car g1))
	      (end ia) (+ old-start (cdr g1)))))

    (dolist (ptag (annotations-spec ia :type (gtype 'gtag-type)))
      (when (ptb-pl-nounp ptag)
	(let* ((ph (car (annotations-spanning ptag :type 'phrase-annotation))))
	  (when (< (start ph) (start ia))
	    (setf (start ia) (start ph))))))
    
    (format t "~&[if list] ~a(~a): ~a-~a [~a]~%" 
	    (name doc) (id sen) (start ia) (end ia) (content ia))))

(defmethod if-list ((sen sentence-annotation))
  (let* ((senstr (content sen))
	 (slen (length senstr))
	 (if-tuis (if-tuis sen))
	 (sstart (start sen))
	 (hrep (make-hash-table :test #'equalp))
	 mstart mend)


    (dolist (if-tui if-tuis)
      (let* ((mstart (- (start if-tui) sstart))
	     (mend (- (end if-tui) sstart)))
	(unless (match-re "(\\+|-|bright|dim)$" (content if-tui))
	  (setf (gethash (cons mstart mend) hrep) 1))))
    
    (setf senstr (replace-re senstr "[#@]" "_"))

    (dolist (position (hash-keys hrep))
      (setf mstart (car position))
      (setf mend (cdr position))
      ;; include () after if
      (multiple-value-bind (pm? whole)
	  (match-re "^\\s+\\([^()#]+\\)" senstr :start mend :return :index)
	(when pm?
	  (setf mend (cdr whole))))

      (let* ((rstr (make-array (- mend mstart) 
			       :element-type 'character
			       :initial-element #\#)))
	(setf senstr (replace senstr rstr :start1 mstart :end1 mend))))
    (format t "[if list sen] ~a~%" senstr)

    (dolist (ptag (annotations-spec sen :type (gtype 'gtag-type)))
      
      (let* ((pstart (- (start ptag) sstart))
	     (pend (- (end ptag) sstart))
	     (rstr (make-array (- pend pstart) 
			       :element-type 'character
			       :initial-element #\@)))
	(when (and (ptb-pl-nounp ptag)
		   (not (match-re "^#+$" (subseq senstr pstart pend))))
	  (setf senstr (replace senstr rstr :start1 pstart :end1 pend)))))
    
    
    (do* ((probe 0 mend))
	((>= probe slen))
      (multiple-value-bind (pm? whole)
	  (match-re *if-lst-paren-pat* senstr :start probe :return :index)
	(cond
	 (pm?				
	  (setf mstart (car whole)
		mend (cdr whole))
	  (add-if-list sen mstart mend)
	  (let* ((rstr (make-array (- mend mstart) 
				   :element-type 'character
				   :initial-element #\_)))
	    (setf senstr (replace senstr rstr :start1 mstart :end1 mend))))
	 (t
	  (setf mend slen)))))

    (do* ((probe 0 mend))
	((>= probe slen))
      (multiple-value-bind (m? whole)
	  (match-re *if-lst-pat* senstr :start probe :return :index)
	(cond
	 (m? 
	  (setf mstart (car whole)
		mend (cdr whole))
	  (add-if-list sen mstart mend))
	 (t
	  (setf mend slen)))))))

(defmethod if-list ((doc document))
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (if-list sen))
  (add-analysis doc :ganalysis 'gif-list))

(defmethod if-adj ((doc document))
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (if-adj sen))
  (add-analysis doc :ganalysis 'gif-adj))

(defmethod if-list ((corp corpus))
  (dolist (docid (documents corp))
    (let* ((doc (document docid)))
      (when (and (ganalysis 'gif-list)
		 ;; (not (analyzedp doc :ganalysis 'gcoded-text))
		 )
	(format t "~&~a document ~a~%" (ganalysis 'gif-list) (name doc))
	(time-profiling (if-list doc))
	(save doc)))))

(defun if-list-status (a)
  (if (annotations-spanning a :type 'if-list)
      "part"
    nil))

(defun if-adj-status (a)
  (if (annotations-spanning a :type 'if-adj)
      "part"
    nil))
