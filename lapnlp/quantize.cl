;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/02/2015 rewrite using asdf framework
yluo - 05/21/2012 creation

Utility for capturing quantitative expression in documents, such as:

A 200-cell differential count
reveals: 53% myeloid precursors, 10% erythroid precursors, 17% lymphocytes, 1%
monocytes, 0% eosinophils, 4% promyelocytes, and 15% plasma cells.

Previously such information is captured in the .xml section specification file
However, as later discovered, their presence does not necessarily qualify as
sections; moreover, they can appear under arbitrary sections. Therefore, I 
decide to make their processing a separate work flow component.

|#

(defpackage :late
  (:use :common-lisp :util)
  (:export 
   "quantize"))

(in-package :late)

(defparameter *quant-diff-combine-pat* 
  (compile-re "(?<=\\s)(A\\s+[^.,]*?count.*?(reveal\\w*|show\\w*|yield\\w*|demonstrate\\w*)|\\(?[A-Z][\\w-\\s\\(\\)]*differential[^\\.]*?|Differential)\\s*:?(?<sub_count>(\\s+(and\\s+)?(?<num>[\\d\\.]*\\s*%)\\s+(?<item>[\\w\\s]+)[,\\.])+)" :single-line t))

(defparameter *sub-diff-combine-pat* 
  (compile-re "(?<num>[\\d\\.]*\\s*%)\\s+(?<item>[\\w\\s]+)[,\\.]"))

(defparameter *quant-cell-count-pat* 
  (compile-re "(A\\s+[^.,]*?count.*?(reveal\\w*|show\\w*|yield\\w*|demonstrate\\w*)):?(?<sub_count>.*?[A-Za-z]\\.)" :single-line t))

(defparameter *sub-cell-count-pat*
  (compile-re "(?<num>[\\d\\.]+%)\\s+(?<item>[\\w\\s]+)[,\\.]"))

(defparameter *quant-cbc-res-pat* 
  (compile-re "\\bCBC\\s+results\\s+from.*?(are\\s+as\\s+follows|show)\\s*:(?<sub_count>(\\s+((\\w){1,4}[=:]?)\\s*([\\d\\.]+)[#%]?\\s*?[;,\\.])+)"))

(defparameter *sub-cbc-res-pat*
  (compile-re "\\s+(?<item>(\\w){1,4})[=:]?\\s*(?<num>[\\d\\.]+[#%]?)\\s*?[;,\\.]"))

(defparameter *quant-differential-pat* 
  (compile-re "\\b([^\\.]*differential[^\\.]*?):(?<sub_count>.*?[A-Za-z]\\.)" :single-line t))

(defparameter *sub-differential-pat* 
  (compile-re "(?<num>[\\d\\.]+%)\\s+(?<item>[\\w\\s]+)[,\\.]"))

(defun sub-quantize (sub-pat txt)
  "Parse the sub-structures."
  (let* ((slen (length txt))
	 sub-counts mend)
    (do* ((probe 0 mend))
	((>= probe slen))
      (let* ((m (match-re sub-pat txt :start probe :return :match)))
	(cond
	 (m
	  (let* ((whole (re-submatch m nil nil 0 :type :index))
		 (num (re-submatch m nil nil "num" :type :string))
		 (item (re-submatch m nil nil "item" :type :string)))
	    (setf mend (cdr whole))
	    (push (cons item num) sub-counts)))
	 (t
	  (setf mend slen)))))
    (nreverse sub-counts)))

(defun pat-quantize (doc pat sub-pat &aux txt tlen mstart mend)
  (setf txt (content doc))
  (setf tlen (length txt))
  (do* ((probe 0 mend))
      ((>= probe tlen))
    (let* ((m (match-re pat txt :start probe :return :match)))
      ;; (format t "probe: ~a; match: ~a~%" probe m)
      (cond 
       (m
	(let* ((whole (re-submatch m nil nil 0 :type :index))
	       (sub-count (re-submatch m nil nil "sub_count" :type :string))
	       (sub-counts (sub-quantize sub-pat sub-count))
	       quant)
	  (setf mstart (car whole))
	  (setf mend (cdr whole))
	  (setf quant (make-instance (gtype 'gquant-type)
				     :document doc
				     :start mstart
				     :end mend
				     :sub-counts sub-counts
				     :sw-ver (gsw-ver 'gquantize)
				     :setting (gsetting 'gquantize)))
	  (add-annotation doc quant)))
       (t
	(setf mend tlen))))))

(defmethod quantize ((doc document))
  "Add quantization annotations to document. This should be performed before
sentencizer, as quantitative expression may mess up the sentence breaker.
Input
======
doc: document to be cross ref analyzed."
  (assert (gtype 'gquant-type)
	  ()
	  "No quantization type specified!")
  
  
  ;; cell count
  ;; (pat-quantize doc *quant-cell-count-pat* *sub-cell-count-pat*)
  
  ;; cbc results
  (pat-quantize doc *quant-cbc-res-pat* *sub-cbc-res-pat*)
  
  ;; differential (auto/manual) and cell count combined
  (pat-quantize doc *quant-diff-combine-pat* *sub-diff-combine-pat*)
  
  ;; differential (auto/manual)
  ;; (pat-quantize doc *quant-differential-pat* *sub-differential-pat*)
  ;; cell count and differential patterns collapse
  ;; seems parser doesn't handle mini-table (: delimited) well, but for 20% lymphs well

  (format t "~%quants:~%~{~a~%~}" (annotations doc :type (gtype 'gquant-type)))
  (add-analysis doc :ganalysis 'gquantize))

(defmethod quantize ((corp corpus))
  (dolist (docid (documents corp))
    (let* ((doc (document docid)))
      (format t "~&quantizing: ~a~%" (name doc))
      (quantize doc)
      (save doc))))
