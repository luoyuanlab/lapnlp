;;; -*- Mode: Lisp; Package: late -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 04/23/2012 added karyotype-doc-locator
yluo - 09/16/2010 creation 
|#
(defpackage :late
  (:use :common-lisp :util :cg)
  (:export 
   "*debug-icp*" 
   "*h-xml-struct-sec*"
   "*l-dir-struct-sec*"
   "*l-tuple-struct-sec*"
   "ic-pairedp"
   "item-content-pair"
   "karyotype-doc-locator"
   "perc->dec"
   "resolve-category"
   "str->dec"
   ))

(in-package :late)

(defparameter *debug-icp* nil)

;; list of tuple structured section, use configuration file later.
;; sections whose inner structure has been captured by XML spec.
;; when finally output as features, better concatenate upper section names into
;; feature name
;; immunotype is in outside consultation, needs further processing
;; how to understand cytogenetics coding.
;; I am not sure of how to find coded features in gross description
;; consult doctors on interpretation of karyotype
(defparameter *l-dir-struct-sec* (list "Specimen" 
				       "White_Blood_Cell_Count"
				       "Lymphocytes"
				       "Monocytes"
				       "Granulocytes"
				       "Debris"
				       "Blasts"
				       "Nucleated_Red_Cells"
				       ;; "Light_Scatter_Gate_Analyzed"
				       "Cell_Origin"
				       ;; "Karyotype" ;; needs further processing
				       "Analyzed"
				       "Scored"
				       "Banding"
				       "Number_of_Cells"
				       "Results"
				       ))
(defparameter *h-xml-struct-sec* (make-hash-table :test #'equalp))
(setf (gethash "Cell_Count" *h-xml-struct-sec*)
      (list "myel_prec_pct"
	    "eryt_prec_pct"
	    "lymphs_pct"
	    "monos_pct"
	    "eos_pct"
	    "basos_pct"
	    "pro_myel_pct"
	    "blasts_pct"
	    "plasma_pct"))
(setf (gethash "Differential" *h-xml-struct-sec*)
      (list "polys_pct"
	    "bands_pct"
	    "lymphs_pct"
	    "monos_pct"
	    "eos_pct"
	    "basos_pct"
	    "blasts_pct"))

(defparameter *l-tuple-struct-sec* (list "CBC_Results"))

;;; (defparameter *l-tuple-var-struct-sec* (list ))

(defun ic-pair-get-version ()
  "1.0")

(defun ic-pairedp (doc)
  (member (format nil "ic-paired-~a" (ic-pair-get-version)) (analyses doc) :test #'equalp))

(defun str->dec (s)
  "Read a string convert it into decimal number, 
Percentage format is converted into decimal format "
  (setf s (replace-re s "(^\\s+|\\s+$)" ""))
  (multiple-value-bind (match? whole sub1) 
      (match-re "^([-,\\d\\.]+)\\s*%\\s*$" s)
    (declare (ignore whole))
    (if match?
	(/ (read-from-string sub1) 100)
      (if (match-re "^([-,\\d\\.]+)$" s)
	  (read-from-string s)
	nil))))

(defun resolve-category (val)
  "Deprecated."
  (let* (category)
    (cond 
     ((match-re "^[<>=\\d\\.\\,; \\t-%]+$" val)
      (cond 
       ((match-re "^<" val)
	(setf category "quantity_<")
	(setf val (str->dec (replace-re val "<" ""))))
       ((match-re "^>" val)
	(setf category "quantity_>")
	(setf val (str->dec (replace-re val ">" ""))))
       (t
	(setf category "quantity_=")
	(setf val (str->dec val)))))
     ((> (length val) 0)
      (setf category "quality"))
     (t 
      (format t "~&Warning: unresolved category for val |~a|" val)))
    (values category val)))

(defun resolve-numeric (val)
  (let* (category)
    (cond
     ((match-re "^[<>=\\d\\.\\,; \\t-%]+$" val)
      (cond 
       ((match-re "^<" val)
	(setf category "quantity_<")
	(setf val (str->dec (replace-re val "<" ""))))
       ((match-re "^>" val)
	(setf category "quantity_>")
	(setf val (str->dec (replace-re val ">" ""))))
       (t
	(setf category "quantity_=")
	(setf val (str->dec val)))))
     (t
      (format t "~&Warning: unresolved numeric for val |~a|" val)))
    (values category val)))


(defun decoder-input (sa type)
  (let* ((sa-str (content sa)))
    (setf sa-str (replace-re sa-str "(^\\s+|\\s+$)" ""))
    (latesql "INSERT INTO decoder(ann_id, code, type) VALUES(~a, ~a, ~a)"
	     (sq (id sa)) (sq sa-str) (sq type))))


(defun decoder-annotation-input (sa type)
  (let* ((sa-str (content sa))
	 (sa-str (replace-re sa-str "(^\\s+|\\s+$)" ""))
	 (doc (document sa))
	 (ann (make-instance type
			     :document doc
			     :start (start sa)
			     :end (end sa)
			     :data sa-str)))
    (add-annotation doc ann)))

(defun item-content-pair (doc &aux proc procs k v cat)
  "for every quantitative expression, allocate three feature vector element,
corresponding to =, <, > respectively. Does simple calculation to translate all
expression into decimal format.
Haven't explore the column structures of antigens sections
The orders of the returned features keys should satisfy the restrictions that 
k=, k< and k> are grouped together.
what if the item appears more than once in a record, use upper hierarchy section
 names to disgintuish."
  (dolist (sa (annotations doc :type 'section-annotation
			   :filter #'(lambda (a) (and (processor a)
						      (not (typep a 'section-head-annotation))))))
    (setf proc (processor sa))
    (setf procs (split-re "\\|" proc))
    (setf k (format nil "~a" (data sa)))
    (cond 
     ((equalp "numeric" proc)
      (if (typep sa 'pattern-section-annotation)
	  (setf v (cdr (assoc "num" (bindings sa) :test #'equalp)))
	(setf v (content sa)))
      (multiple-value-setq (cat v) (resolve-numeric v))
      (when (and v cat)
	(add-annotation
	 doc
	 (make-instance 'item-content-pair
			:document doc
			:start (start sa) 
			:end (end sa)
			:data (replace-re (format nil "~a_~a" k cat) "\\s+" "_")
			:content v))))
     
     ((equalp "enumerate" proc)
      (dolist (ph (annotations sa :type 'phrase-annotation))
	(nconc v (phrase-longest-umlss ph :type 'cui-annotation :persist nil)))
      (if v
	  (add-annotation
	   doc
	   (make-instance 'item-content-pair
			  :document doc
			  :start (start sa) 
			  :end (end sa)
			  :data (replace-re (format nil "~a_quality" k) "\\s+" "_")
			  :content (format nil "~{~a~^ ~}" v)))))
     
     ((search "coded" proc)
      ;; user needs to provide the decoder.
      (multiple-value-bind (match? whole type)
	  (match-re "^coded:(.*)$" proc)
	(declare (ignore match? whole))
	(setf type (concatenate 'string "decoder-" type))
	(decoder-annotation-input sa (intern type))))
     
     ((not (set-difference (list "coded" "narrative") procs))
      ;; actually not sure how to best deal with this 
      )))
  (add-analysis doc :ganalysis 'gic-pair))

(defun karyotype-clean ()
  (let* ((l-id-code (latesql "SELECT ann_id, code FROM decoder")))
    (dolist (id-code l-id-code)
      (let* ((id (first id-code))
	     (code (second id-code)))
	(setf code (replace-re code "METAPHASES.*$" "" :single-line t))
	(setf code (replace-re code ",ish" ".ish"))
	(setf code (replace-re code "(^\\s+|\\s+$)" ""))
	(cond 
	 ((or (search "Canceled" code)
	      (search "Not obtained" code)
	      (search "See interpretation" code))
	  (latesql "DELETE FROM decoder WHERE ann_id=~a" (sq id)))
	 (t
	  (latesql "UPDATE decoder SET code=~a WHERE ann_id=~a"
		   (sq code) (sq id))))))))

(defun contain-karyotype-sec? (sa)
  (annotations sa :type 'section-annotation
	       :filter #'(lambda (a) 
			   (equalp "karyotype" 
				   (format nil "~a" (data a))))))

(defun karyotype-doc-locator (corpn out-fn)
  ;; Sample usage:
  ;; (karyotype-doc-locator "process_list" "late:;docs_with_uncaught_karyotypes")
  (with-open-file
   (out-f out-fn
	  :direction :output
	  :if-exists :supersede
	  :if-does-not-exist :create)
   (dolist (docid (documents (corpus corpn)))
     (unless (= 66245 docid)
       (let* ((doc (document docid))
	      mark?)
	 (format t "~&scanning ~a~%" (name doc))
	 (do* ((sas (annotations doc :type 'section-annotation) (cdr sas))
	       (sa (car sas) (car sas)))
	     ((or (null sas)
		  mark?))
	   (unless nil			; (contain-karyotype-sec? sa)
	     (when (match-re "(^|\\b)(ace|add|cen|chi|chr|cht|cp|cx|del\\(|de novo|der|dia|dic|dip|dir|dis|dit|dmin|dup|fem|fis|fra|hsr|idem|ider|idic|inc|ins|inv|lep|mal|mar|mat|oom|pac|pat|pcc|pcd|prx|psu|pvz|qdp|qr|rcp|rea|rec|rob|sce|sct|sdl|spm|stk|tas|tel|ter|tr|trc|trp|upd|xma|zyg|amp|con|dim\\(|enh|fib ish|ish|mv|nuc ish|pcp|rev ish|sep|sp|st|wcp)(\\b|$)"
			     (content sa))
	       (setf mark? t)
	       (format t "caught!~%"))))
	 (when mark?
	   (format out-f "~&~a~%" (name doc))))))))
