;;; -*- Mode: Lisp; Package: i2b2 -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 03/09/2012 added concept-eval
yluo - 03/08/2012 creation
|#
(defpackage :i2b2
  (:use :late :excl :excl.osi :common-lisp :util :norm)
  (:export
   "load-corpora"
   "panalyze-corpora"
   "load-concepts"
   "arff-inst-gen"
   "train-rel-arff-gen"
   "test-rel-arff-gen"
   "train-test-rel-gen"
   "rel-arff-gen"
   "concept-eval"
   ))

(in-package :i2b2)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :osi))

(defparameter *debug-i2b2-va-exp* nil)
(defparameter *semicolon-weight* 10)


(defun load-corpora (mode &aux corpn)
  (setf corpn (format nil "beth_train_~a" mode))
  (unless (corpus corpn)
    (format t "~%importing corpus ~a~%" corpn)
    (import-corpus "i2b2-va-train:;beth;txtc;" 
		   :name corpn
		   :spec "late:;i2b2-va.xml"
		   :mode mode))
  (setf corpn (format nil "partners_train_~a" mode))
  (unless (corpus corpn)
    (format t "~%importing corpus ~a~%" corpn)
    (import-corpus "i2b2-va-train:;partners;txtc;" 
		   :name corpn
		   :spec "late:;i2b2-va.xml"
		   :mode mode))

  (setf corpn (format nil "upmcd_train_~a" mode))
  (unless (corpus corpn)
    (format t "~%importing corpus ~a~%" corpn)
    (import-corpus "i2b2-va-train:;upmcd;txtc;" 
		   :name corpn
		   :spec "late:;i2b2-va.xml"
		   :mode mode))

  (setf corpn (format nil "upmcp_train_~a" mode))
  (unless (corpus corpn)
    (format t "~%importing corpus ~a~%" corpn)
    (import-corpus "i2b2-va-train:;upmcp;txtc;" 
		   :name corpn
		   :spec "late:;i2b2-va.xml"
		   :mode mode))
  
  (setf corpn (format nil "i2b2_test_~a" mode))
  (unless (corpus corpn)
    (format t "~%importing corpus ~a~%" corpn)
    (import-corpus "i2b2-va-test:;txtc;"
		   :name corpn
		   :spec "late:;i2b2-va.xml"
		   :mode mode)))

(defun panalyze-corpora (&key (pre-lp? t))
  (mapcar #'(lambda (corpn) (panalyze-corpus corpn :pre-lp? pre-lp?
					     :fn-pcfg "late:;i2b2-pconfig.cl"
					     :fn-flow-spec "late:;i2b2-flow.spec"
					     :fn-exp "late:;i2b2-va-exp.cl"))
	  '("beth_train_no_spl" "partners_train_no_spl" "upmcd_train_no_spl"
	    "upmcp_train_no_spl" "i2b2_test_no_spl")))

(defmethod load-persist-ann ((doc document))
  "Implements the generic interface."
  (load-concepts doc)
  (add-analysis doc :ganalysis 'gload-persist-ann))

(defmethod load-concepts ((doc document) 
			  &aux fncon)
  "load concept boundaries into a key sorted hash hpos (key: start, val: end), 
exclude those with PP"
  
  (setf fncon (replace-re (source doc) ";txtc;" ";conceptc;"))
  (setf fncon (concatenate 'string fncon ".conc"))
  (with-open-file (fcon fncon :direction :input)
		  (let* (ln 
			 (lnum 0))
		    (loop (unless (setf ln (read-line fcon nil nil)) (return))
			  (incf lnum)
			  (multiple-value-bind (match? whole phrase start end contype)
			      (match-re "^c=\"([^\"]+)\" +(\\d+) +(\\d+)\\|\\|t=\"([^\"]+)\"" ln)
			    (declare (ignore whole phrase))
			    (cond 
			     (match?
			      (let* ((ctag (make-instance 'concept-tag
							  :document doc
							  :start (parse-integer start)
							  :end (parse-integer end)
							  :data contype))
				     sa)
				(add-annotation doc ctag)
				;;(break)
				(setf sa (car (annotations-spanning ctag 
								    :type 'sentence-annotation)))
				(add-annotation sa ctag)
				;;(break)
				))
			     (t (format t "~&Warning: problematic line ~a at ~a" lnum fncon)))))
		    (save doc))))

(defmethod load-concepts ((corp corpus))
  "dncon has to be the logical dir where the concepts files reside."
  ;; sample usage 
  ;; (load-concepts (corpus "beth_train_no_spl") "i2b2-va-train:;beth;conceptc;")
  (dolist (doc-id (documents corp))
    (let* ((doc (document doc-id))
	   (docn (name doc)))
      (unless (concepts-loadedp doc)
	(time-profiling
	 (format t "~&loading concepts for ~a~%" docn)
	 (load-concepts doc))))))

(defparameter *h-sp* (make-hash-table :test #'equalp)
  "key is ordered cons of (sen-id . (i . j)) specifying source destination pair, value is shortest path syntactic tag string, serves for caching purpose.")
(defparameter *h-sp-words* (make-hash-table :test #'equalp)
  "key is ordered cons of (sen-id . (i . j)), specifying source destination pair, value is shortest path words string, serves for caching purpose.")
(defparameter *rel-con-filter* 
  (list (cons "TrIP" (cons "treatment" "problem"))
	(cons "TrWP" (cons "treatment" "problem"))
	(cons "TrCP" (cons "treatment" "problem"))
	(cons "TrAP" (cons "treatment" "problem"))
	(cons "TrNAP" (cons "treatment" "problem"))
	(cons "TeRP" (cons "test" "problem"))
	(cons "TeCP" (cons "test" "problem"))
	(cons "PIP" (cons "problem" "problem"))))

(defparameter *con-rel-filter* (make-hash-table :test #'equalp))

(setf (gethash (cons "treatment" "problem") *con-rel-filter*)
      '("TrIP" "TrWP" "TrCP" "TrAP" "TrNAP" "None"))
(setf (gethash (cons "test" "problem") *con-rel-filter*)
      '("TeRP" "TeCP" "None"))
(setf (gethash (cons "problem" "problem") *con-rel-filter*)
      '("PIP" "None"))

(defun i2b2-norm (str &aux ans)
  (setf ans (safe-norm str))
  (when ans
    (setf ans (replace-re ans "\\s+" "_"))
    (setf ans (replace-re ans "\\.+" "_"))
    (when (not (match-re "[A-Za-z]" ans))
      (setf ans nil)))
  ans)

(defun arff-inst-gen-pnode (sa c1tag c2tag rtype docn 
			       &aux c1pns c2pns frst frend backst backend)
  "First determine the head noun (token or abstract token), then computes the 
features.
Input
======
c1tag - the first concept annotation
c2tag - the second concept annotation
rtype - the relation type"
  ;; sample usage: 
  ;; (arff-inst-gen-pnode sa1 ctag11 ctag12 "TrAP" "record-15.spl")
;;;  (setf pns (annotations-spec sa :type (gtype 'gparse-node-type)))
  (setf c1pns (annotations-spec c1tag :type (gtype 'gparse-node-type)))
  (setf c2pns (annotations-spec c2tag :type (gtype 'gparse-node-type)))
  
  (setf frst (min (start c1tag) (start c2tag)))
  (setf frend (min (end c1tag) (end c2tag)))
  
  (setf backst (max (start c1tag) (start c2tag)))
  (setf backend (max (end c1tag) (end c2tag)))
  (unless (and c1pns c2pns)
    (return-from arff-inst-gen-pnode nil))
  (let* (;;(c1head (car (last (remove-if-not #'noun-tokenp c1pns))))
	 ;; c1head (car (last c1pns)))
	 ;;(c2head (car (last (remove-if-not #'noun-tokenp c2pns))))
	 ;; c2head (car (last c2pns)))
	 ;; is there a better way?
	 ;; (c1idx (- (length pns) (length (member c1head pns))))
	 ;; (c2idx (- (length pns) (length (member c2head pns))))
	 (con1-unigrams (i2b2-extract-ngram 1 sa :start (start c1tag) 
					    :end (end c1tag)))
	 (con1-bigrams (i2b2-extract-ngram 2 sa :start (start c1tag)
					   :end (end c1tag)))
	 (con1-trigrams (i2b2-extract-ngram 3 sa :start (start c1tag)
					    :end (end c1tag)))
	 
	 (con2-unigrams (i2b2-extract-ngram 1 sa :start (start c2tag) 
					    :end (end c2tag)))
	 (con2-bigrams (i2b2-extract-ngram 2 sa :start (start c2tag)
					   :end (end c2tag)))
	 (con2-trigrams (i2b2-extract-ngram 3 sa :start (start c2tag)
					    :end (end c2tag)))	
	 
	 (prec-unigrams (i2b2-extract-ngram 1 sa :end frst))
	 (prec-bigrams (i2b2-extract-ngram 2 sa :end frst))
	 (prec-trigrams (i2b2-extract-ngram 3 sa :end frst))
	 
	 (mid-unigrams (i2b2-extract-ngram 1 sa :start frend :end backst))
	 (mid-bigrams (i2b2-extract-ngram 2 sa :start frend :end backst))
	 (mid-trigrams (i2b2-extract-ngram 3 sa :start frend :end backst))
	 
	 (succ-unigrams (i2b2-extract-ngram 1 sa :start backend))
	 (succ-bigrams (i2b2-extract-ngram 2 sa :start backend))
	 (succ-trigrams (i2b2-extract-ngram 3 sa :start backend))
	 
	 ;; (pns-before (remove-if-not #'(lambda (a) (int< a c1tag)) pns))
	 ;; (pns-after (remove-if-not #'(lambda (a) (int< c2tag a)) pns))
	 ;; (pns-between (remove-if-not #'(lambda (a) (and (int< c1tag a)
	 ;;						(int< a c2tag))) 
	 ;;			     pns))

	 (arff-str (outstr-init)))
    ;; relation type
    (format arff-str "~a, " rtype)
    ;; instance detail for back tracking 
    (format arff-str "\"~a:|~a|(~a-~a) |~a|(~a-~a)\", " 
	    docn (content c1tag) (start c1tag) (end c1tag) 
	    (content c2tag) (start c2tag) (end c2tag))
    ;; unigrams for concept 1
    (setf con1-unigrams (mapcar #'weka-str con1-unigrams))
    (setf con1-unigrams (prepend-list "conc1_unigram" con1-unigrams))
    (format arff-str "\"~{~a~^ ~}\", " con1-unigrams)
    ;; bigrams for concept 1
    (setf con1-bigrams (mapcar #'weka-str con1-bigrams))
    (setf con1-bigrams (prepend-list "conc1_bigram" con1-bigrams))
    (format arff-str "\"~{~a~^ ~}\", " con1-bigrams)	
    ;; trigrams for concept 1
    (setf con1-trigrams (mapcar #'weka-str con1-trigrams))
    (setf con1-trigrams (prepend-list "conc1_trigram" con1-trigrams))
    (format arff-str "\"~{~a~^ ~}\", " con1-trigrams)
    
    ;; unigrams for concept 2
    (setf con2-unigrams (mapcar #'weka-str con2-unigrams))
    (setf con2-unigrams (prepend-list "conc2_unigram" con2-unigrams))
    (format arff-str "\"~{~a~^ ~}\", " con2-unigrams)
    ;; bigrams for concept 2
    (setf con2-bigrams (mapcar #'weka-str con2-bigrams))
    (setf con2-bigrams (prepend-list "conc2_bigram" con2-bigrams))
    (format arff-str "\"~{~a~^ ~}\", " con2-bigrams)
    ;; trigrams for concept 2
    (setf con2-trigrams (mapcar #'weka-str con2-trigrams))
    (setf con2-trigrams (prepend-list "conc2_trigram" con2-trigrams))
    (format arff-str "\"~{~a~^ ~}\", " con2-trigrams)	
    
    
    ;; unigrams before
    (setf prec-unigrams (mapcar #'weka-str prec-unigrams))
    (setf prec-unigrams (prepend-list "prec_unigram" prec-unigrams))
    (format arff-str "\"~{~a~^ ~}\", " prec-unigrams)
    ;; bigrams before
    (setf prec-bigrams (mapcar #'weka-str prec-bigrams))
    (setf prec-bigrams (prepend-list "prec_bigram" prec-bigrams))
    (format arff-str "\"~{~a~^ ~}\", " prec-bigrams)	
    ;; trigrams before
    (setf prec-trigrams (mapcar #'weka-str prec-trigrams))
    (setf prec-trigrams (prepend-list "prec_trigram" prec-trigrams))
    (format arff-str "\"~{~a~^ ~}\", " prec-trigrams)
    
    
    ;; middle unigrams 
    (setf mid-unigrams (mapcar #'weka-str mid-unigrams))
    (setf mid-unigrams (prepend-list "mid_unigram" mid-unigrams))
    (format arff-str "\"~{~a~^ ~}\", " mid-unigrams)
    ;; middle bigrams
    (setf mid-bigrams (mapcar #'weka-str mid-bigrams))
    (setf mid-bigrams (prepend-list "mid_bigram" mid-bigrams))
    (format arff-str "\"~{~a~^ ~}\", " mid-bigrams)	
    ;; middle bigrams
    (setf mid-trigrams (mapcar #'weka-str mid-trigrams))
    (setf mid-trigrams (prepend-list "mid_bigram" mid-trigrams))
    (format arff-str "\"~{~a~^ ~}\", " mid-trigrams)	
    
    
    ;; succeeding unigrams
    (setf succ-unigrams (mapcar #'weka-str succ-unigrams))
    (setf succ-unigrams (prepend-list "succ_unigram" succ-unigrams))
    (format arff-str "\"~{~a~^ ~}\", " succ-unigrams)
    ;; succeeding bigrams
    (setf succ-bigrams (mapcar #'weka-str succ-bigrams))
    (setf succ-bigrams (prepend-list "succ_bigram" succ-bigrams))
    (format arff-str "\"~{~a~^ ~}\", " succ-bigrams)
    ;; succeeding trigrams
    (setf succ-trigrams (mapcar #'weka-str succ-trigrams))
    (setf succ-trigrams (prepend-list "succ_trigram" succ-trigrams))
    (format arff-str "\"~{~a~^ ~}\"" succ-trigrams)
    
    arff-str))

(defun fts2libsvm (fts ftype hinst hftid
		       &key (scale nil) (rdir nil)
		       &aux hloc-ftid)
  "hinst stores the features for a instance
hftid index features"
  (setf fts (mapcar #'weka-str fts))
  (setf fts (prepend-list ftype fts))
  (setf hloc-ftid (make-hash-table :test #'equalp))
  (dolist (ft fts)
    (let* ((ftid (cnt-gethash ft hftid)))
      (incf (gethash ftid hinst 0))
      (setf (gethash ftid hloc-ftid) 1)))
  ;; scaling the features
  (when scale
    (dolist (kv (hash-table-val-ascd-alist hinst))
      (let* ((k (car kv))
	     (v (cdr kv)))
	(when (gethash k hloc-ftid) ;; only scales features of this type
	  (setf (gethash k hinst) (/ v scale))))))

  (when rdir
    (dolist (kv (hash-table-val-ascd-alist hinst))
      (let* ((k (car kv))
	     (v (cdr kv)))
	(when (gethash k hloc-ftid) ;; only dir features of this type
	  (setf (gethash k hinst) (* v rdir)))))))

(defun fts2sda (fts ftype hinst hftid)
  "hinst stores the features for a instance, note that it's in reverse order
hftid index features"
  (setf fts (mapcar #'weka-str fts))
  (dolist (ft fts)
    (let* ((ftid (cnt-gethash ft hftid)))
      (push ftid (gethash ftype hinst)))))

(defun libsvm-inst-gen-pnode (sa c1tag c2tag str-rtype rtype docn h-fts h-voc
				 &aux c1pns c2pns)
  "First determine the head noun (token or abstract token), then computes the 
features.
Input
======
c1tag - the first concept annotation
c2tag - the second concept annotation
rtype - the numeric relation type"
  ;; sample usage: 
  ;; (libsvm-inst-gen-pnode sa1 ctag11 ctag12 "TrAP" "record-15.spl" h-fts)
  (setf c1pns (annotations-spec c1tag :type (gtype 'gparse-node-type)))
  (setf c2pns (annotations-spec c2tag :type (gtype 'gparse-node-type)))

  (unless (and c1pns c2pns)
    (return-from libsvm-inst-gen-pnode nil))
  (let* ((s1 (start c1tag))
	 (e1 (end c1tag))
	 (s2 (start c2tag))
	 (e2 (end c2tag))
	 ;; (ss (start sa))
	 (frst (min s1 s2))
	 (frend (min e1 e2))
	 (backst (max s1 s2))
	 (backend (max e1 e2))
	 (ugs (i2b2-extract-ngram 1 sa))
	 (c1ug (i2b2-extract-ngram 1 sa :start s1 :end e1))
	 ;; (c1bg (i2b2-extract-ngram 2 sa :start s1 :end e1))
	 ;; (c1tg (i2b2-extract-ngram 3 sa :start s1 :end e1))
	 
	 (c2ug (i2b2-extract-ngram 1 sa :start s2 :end e2))
	 ;; (c2bg (i2b2-extract-ngram 2 sa :start s2 :end e2))
	 ;; (c2tg (i2b2-extract-ngram 3 sa :start s2 :end e2))	
	 
	 (prec-ug (i2b2-extract-ngram 1 sa :end frst))
	 ;; (prec-bg (i2b2-extract-ngram 2 sa :end frst))
	 ;; (prec-tg (i2b2-extract-ngram 3 sa :end frst))
	 
	 (mid-ug (i2b2-extract-ngram 1 sa :start frend :end backst))
	 ;; (mid-bg (i2b2-extract-ngram 2 sa :start frend :end backst))
	 ;; (mid-tg (i2b2-extract-ngram 3 sa :start frend :end backst))
	 
	 ;; mid-str
	 
	 
	 (succ-ug (i2b2-extract-ngram 1 sa :start backend))
	 ;; (succ-bg (i2b2-extract-ngram 2 sa :start backend))
	 ;; (succ-tg (i2b2-extract-ngram 3 sa :start backend))

	 (istr (outstr-init))
	 (idstr (outstr-init))
	 (hinst (make-hash-table :test #'equalp))
	 ;; (scale (length mid-ug))
	 )
    ;; update voc count
    (dolist (ug ugs)
      (incf (gethash ug h-voc 0)))

    (when (> frend backst)
      (format t "~a~%~a~%" c1tag c2tag))
    ;; (setf mid-str (subseq (content sa) (- frend ss) (- backst ss)))
    ;; (when (search ";" mid-str)
    ;;   (incf scale *semicolon-weight*))

    ;; relation type
    (format istr "~a " rtype)
    ;; instance detail for back tracking 
    (format idstr "~a:|~a|(~a-~a)|~a|~a|(~a-~a)" 
	    docn (content c1tag) s1 e1 str-rtype (content c2tag) s2 e2)
    ;; (when (= scale 0)
    ;;   (format t "scale 0 at ~a~%" idstr)
    ;;   (setf scale 1))
    ;; ngrams for concept 1
    (fts2libsvm c1ug "c1ug" hinst h-fts)
    ;; (fts2libsvm c1bg "c1bg" hinst h-fts)
    ;; (fts2libsvm c1tg "c1tg" hinst h-fts)
    
    ;; ngrams for concept 2
    (fts2libsvm c2ug "c2ug" hinst h-fts)
    ;; (fts2libsvm c2bg "c2bg" hinst h-fts)
    ;; (fts2libsvm c2tg "c2tg" hinst h-fts)	
    
    ;; ngrams before  :scale scale
    (fts2libsvm prec-ug "precug" hinst h-fts)
    ;; (fts2libsvm prec-bg "prec_bg" hinst h-fts)
    ;; (fts2libsvm prec-tg "prec_tg" hinst h-fts)
    
    ;; middle ngrams  :rdir rdir :scale scale
    (fts2libsvm mid-ug "midug" hinst h-fts)
    ;; (fts2libsvm mid-bg "mid_bg" hinst h-fts)
    ;; (fts2libsvm mid-tg "mid_tg" hinst h-fts)
    
    ;; succeeding ug :scale scale
    (fts2libsvm succ-ug "succug" hinst h-fts)
    ;; (fts2libsvm succ-bg "succ_bg" hinst h-fts)
    ;; (fts2libsvm succ-tg "succ_tg" hinst h-fts)

    (dolist (kv (hash-table-key-ascd-alist hinst))
      (format istr "~a:~,3f " (car kv) (cdr kv)))
    (format istr "~%")
    (list istr idstr)))


(defun arff-inst-gen (sa c1tag c2tag rtype ttype docn &aux tas c1tas c2tas)
  "First determine the head noun (token or abstract token), then computes the 
features. This is an older version, kept just to show how to extract syntactic
bigrams
Input
======
c1tag - the first concept annotation
c2tag - the second concept annotation
rtype - the relation type
ttype - the token annotation type: alp-token or lp-token "
  ;; sample usage: 
  ;; (arff-inst-gen sa1 ctag11 ctag12 "TrAP" 'lp-token "record-15.spl")
  (setf tas (annotations sa :type ttype))
  (setf c1tas (cond
	       ((equalp 'lp-token ttype)
		(annotations c1tag :type ttype))
	       ((equalp 'alp-token ttype)
		(remove-if-not #'(lambda (a) 
				   (or (and (< (start a) (end c1tag))
					    (> (end a) (start c1tag)))
				       (and (ph-start a) (ph-end a)
					    (< (ph-start a) (end c1tag))
					    (> (ph-end a) (start c1tag))))) 
			       tas))))
  (setf c2tas (cond
	       ((equalp 'lp-token ttype)
		(annotations c2tag :type ttype))
	       ((equalp 'alp-token ttype)
		(remove-if-not #'(lambda (a) 
				   (or (and (< (start a) (end c2tag))
					    (> (end a) (start c2tag)))
				       (and (ph-start a) (ph-end a)
					    (< (ph-start a) (end c2tag))
					    (> (ph-end a) (start c2tag))))) 
			       tas))))
  (unless (and c1tas c2tas)
    (return-from arff-inst-gen nil))
  (let* (;;(c1head (car (last (remove-if-not #'noun-tokenp c1tas))))
	 (c1head (car (last c1tas)))
	 ;;(c2head (car (last (remove-if-not #'noun-tokenp c2tas))))
	 (c2head (car (last c2tas)))
	 ;; is there a better way?
	 (c1idx (- (length tas) (length (member c1head tas))))
	 (c2idx (- (length tas) (length (member c2head tas))))
	 (tas-before (remove-if-not #'(lambda (a) (int< a c1tag)) tas))
	 (tas-after (remove-if-not #'(lambda (a) (int< c2tag a)) tas))
	 (tas-between (remove-if-not #'(lambda (a) (and (int< c1tag a)
							(int< a c2tag))) 
				     tas))
	 (left-syn-bigram1 (extract-left-syn-bigram c1head tas))
	 (right-syn-bigram1 (extract-right-syn-bigram c1head tas))
	 (left-syn-bigram2 (extract-left-syn-bigram c2head tas))
	 (right-syn-bigram2 (extract-right-syn-bigram c2head tas))
	 (arff-str (make-array 0 :element-type 'character :adjustable t
			       :fill-pointer 0))
	 )
    ;; relation type
    (format arff-str "~a, " rtype)
    ;; instance detail for back tracking 
    (format arff-str "\"~a:(~a-~a) (~a-~a)\", " 
	    docn (start c1tag) (end c1tag) (start c2tag) (end c2tag))
    ;; bag of words for concept 1
    (format arff-str "\"~{~a~^ ~}\", "
	    (mapcar #'(lambda (a) (or (car (norm (content a)))
				      (string-downcase (content a)))) 
		    c1tas))
    ;; bag of words for concept 2
    (format arff-str "\"~{~a~^ ~}\", "
	    (mapcar #'(lambda (a) (or (car (norm (content a)))
				      (string-downcase (content a)))) 
		    c2tas))
    ;; bag of words before
    (format arff-str "\"~{~a~^ ~}\", "
	    (mapcar #'(lambda (a) (or (car (norm (content a)))
				      (string-downcase (content a)))) 
		    tas-before))
    ;; bag of words between
    (format arff-str "\"~{~a~^ ~}\", "
	    (mapcar #'(lambda (a) (or (car (norm (content a)))
				      (string-downcase (content a)))) 
		    tas-between))
    ;; bag of words after
    (format arff-str "\"~{~a~^ ~}\", "
	    (mapcar #'(lambda (a) (or (car (norm (content a)))
				      (string-downcase (content a)))) 
		    tas-after))
    
    (format arff-str "\"~{~a~^ ~}\", " left-syn-bigram1)
    
    (format arff-str "\"~{~a~^ ~}\", " right-syn-bigram1)
    
    (format arff-str "\"~{~a~^ ~}\", " left-syn-bigram2)
    
    (format arff-str "\"~{~a~^ ~}\", " right-syn-bigram2)
    
    (format arff-str "\"~{~a~^ ~}\", " 
	    (mapcar #'(lambda (a) (string-downcase (content a))) 
		    (remove-if-not #'verb-tokenp tas-between)))
    
    (format arff-str "\"~a\", " 
	    (or (car (last (mapcar #'(lambda (a) (string-downcase (content a))) 
				   (remove-if-not #'verb-tokenp tas-before))))
		""))
    
    (format arff-str "\"~a\", " 
	    (or (first (mapcar #'(lambda (a) (string-downcase (content a))) 
			       (remove-if-not #'verb-tokenp tas-after)))
		""))


    (unless (gethash (cons (id sa) (cons c1idx c2idx)) *h-sp*)
      (dolist (sp (extract-shortest-syntactic-paths-from c1idx sa))
	(setf (gethash (cons (id sa) (car sp)) *h-sp*) (cdr sp))))
    (unless (gethash (cons (id sa) (cons c1idx c2idx)) *h-sp-words*)
      (dolist (sp (extract-shortest-paths-words-from c1idx sa))
	(setf (gethash (cons (id sa) (car sp)) *h-sp-words*) (cdr sp))))
    
    (format arff-str "\"~a\", " 
	    (replace-re (gethash (cons (id sa) (cons c1idx c2idx)) *h-sp*)
			"-" "_"))
    (format arff-str "\"~a\"" 
	    (replace-re (gethash (cons (id sa) (cons c1idx c2idx)) *h-sp-words*)
			"-" " "))
    arff-str))


(defun train-rel-arff-gen (mode ttype &aux fnarff)
  "This works across multiple corpora"
  (let* ((h-arff-insts (make-hash-table :test #'equalp)))
    (with-open-file (flog (format nil "late:;rel_sentences") 
			  :direction :output :if-exists :supersede
			  :if-does-not-exist :create)
		    (rel-arff-gen (corpus (format nil "beth_train_~a" mode))
				  "i2b2-va-train:;beth;relc;"
				  ttype
				  h-arff-insts
				  :flog flog)
		    (rel-arff-gen (corpus (format nil "partners_train_~a" mode))
				  "i2b2-va-train:;partners;relc;"
				  ttype
				  h-arff-insts
				  :flog flog)
		    (rel-arff-gen (corpus (format nil "upmcd_train_~a" mode))
				  "i2b2-va-train:;upmcd;relc;"
				  ttype
				  h-arff-insts
				  :flog flog)
		    (rel-arff-gen (corpus (format nil "upmcp_train_~a" mode))
				  "i2b2-va-train:;upmcp;relc;"
				  ttype
				  h-arff-insts
				  :flog flog))
    
    (maphash #'(lambda (con-pair l-inst)
		 (setf fnarff (format nil "~a_~a_~a_~a_train.arff" 
				      (car con-pair) (cdr con-pair) mode ttype))
		 (setf fnarff (replace-re fnarff "-" "_"))
		 (with-open-file
		  (farff (format nil "data:;arff;i2b2;~a" fnarff)
			 :direction :output 
			 :if-exists :supersede 
			 :if-does-not-exist :create)
		  (format farff "@relation ~a~%" fnarff)
		  (format farff "@attribute class {~{~a,~}}~%" 
			  (gethash con-pair *con-rel-filter*))
		  (format farff "@attribute ID string~%")
		  (format farff "@attribute con1_unigram string~%")
		  (format farff "@attribute con1_bigram string~%")
		  (format farff "@attribute con1_trigram string~%")
		  (format farff "@attribute con2_unigram string~%")
		  (format farff "@attribute con2_bigram string~%")
		  (format farff "@attribute con2_trigram string~%")
		  (format farff "@attribute prec_unigram string~%")
		  (format farff "@attribute prec_bigram string~%")
		  (format farff "@attribute prec_trigram string~%")
		  (format farff "@attribute mid_unigram string~%")
		  (format farff "@attribute mid_bigram string~%")
		  (format farff "@attribute mid_trigram string~%")
		  (format farff "@attribute succ_unigram string~%")
		  (format farff "@attribute succ_bigram string~%")
		  (format farff "@attribute succ_trigram string~%")
		  ;; may optionally want to include those features
		  ;; (format farff "@attribute LeftSynBigramCon1 string~%")
		  ;; (format farff "@attribute RightSynBigramCon1 string~%")
		  ;; (format farff "@attribute LeftSynBigramCon2 string~%")
		  ;; (format farff "@attribute RightSynBigramCon2 string~%")
		  ;; (format farff "@attribute MidVerb string~%")
		  ;; (format farff "@attribute PrecVerb string~%")
		  ;; (format farff "@attribute SuccVerb string~%")
		  ;; (format farff "@attribute SynPath string~%")
		  ;; (format farff "@attribute SynWords string~%")
		  (format farff "@data~%")
		  (dolist (inst-str l-inst)
		    (format farff "~a~%" inst-str))))
	     h-arff-insts)))


(defun train-test-rel-gen (mode ttype inst-gen-pnode t-rel-gen &aux fn)
  "This works across multiple corpora"
  (let* ((h-insts-tr (make-hash-table :test #'equalp))
	 (h-insts-te (make-hash-table :test #'equalp))
	 (flog (open-new (format nil "late:;rel_sentences")))
	 (h-fts (make-hash-table :test #'equalp))
	 (h-voc (make-hash-table :test #'equalp))
	 (h-ids-tr (make-hash-table :test #'equalp))
	 (h-ids-te (make-hash-table :test #'equalp)))
    (rel-gen (corpus (format nil "beth_train_~a" mode))
	     "i2b2-va-train:;beth;relc;"
	     ttype h-insts-tr h-fts h-voc h-ids-tr inst-gen-pnode :flog flog)
    (rel-gen (corpus (format nil "partners_train_~a" mode))
	     "i2b2-va-train:;partners;relc;"
	     ttype h-insts-tr h-fts h-voc h-ids-tr inst-gen-pnode :flog flog)
    (rel-gen (corpus (format nil "upmcd_train_~a" mode))
	     "i2b2-va-train:;upmcd;relc;"
	     ttype h-insts-tr h-fts h-voc h-ids-tr inst-gen-pnode :flog flog)
    (rel-gen (corpus (format nil "upmcp_train_~a" mode))
	     "i2b2-va-train:;upmcp;relc;"
	     ttype h-insts-tr h-fts h-voc h-ids-tr inst-gen-pnode :flog flog)
    (rel-gen (corpus (format nil "i2b2_test_~a" mode))
	     "i2b2-va-test:;relc;"
	     ttype h-insts-te h-fts h-voc h-ids-te inst-gen-pnode :flog flog)
    
    (maphash #'(lambda (con-pair l-inst)
		 (setf fn (format nil "~a_~a_~a_~a_train.~a" (car con-pair) 
				  (cdr con-pair) mode ttype t-rel-gen))
		 (setf fn (replace-re fn "-" "_"))
		 (let* ((f (open-new (format nil "data:;~a;i2b2;~a" t-rel-gen fn))))
		   (dolist (inst-str l-inst)
		     (format f "~a~%" inst-str))
		   (close f)))
	     h-insts-tr)

    (maphash #'(lambda (con-pair l-id)
		 (setf fn (format nil "~a_~a_~a_~a_train.ids" 
				  (car con-pair) (cdr con-pair) mode ttype))
		 (setf fn (replace-re fn "-" "_"))
		 (let* ((f (open-new (format nil "data:;~a;i2b2;~a" t-rel-gen fn))))
		   (dolist (id-str l-id)
		     (format f "~a~%" id-str))
		   (close f)))
	     h-ids-tr)

    (maphash #'(lambda (con-pair l-inst)
		 (setf fn (format nil "~a_~a_~a_~a_test.~a" (car con-pair) 
				  (cdr con-pair) mode ttype t-rel-gen))
		 (setf fn (replace-re fn "-" "_"))
		 (let* ((f (open-new (format nil "data:;~a;i2b2;~a" t-rel-gen fn))))
		   (dolist (inst-str l-inst)
		     (format f "~a~%" inst-str))
		   (close f)))
	     h-insts-te)

    (maphash #'(lambda (con-pair l-id)
		 (setf fn (format nil "~a_~a_~a_~a_test.ids" 
				  (car con-pair) (cdr con-pair) mode ttype))
		 (setf fn (replace-re fn "-" "_"))
		 (let* ((f (open-new (format nil "data:;~a;i2b2;~a" t-rel-gen fn))))
		   (dolist (id-str l-id)
		     (format f "~a~%" id-str))
		   (close f)))
	     h-ids-te)

    (maphash #'(lambda (con-pair hftcp)
		 (setf fn (format nil "~a_~a_~a_~a.fts" 
				  (car con-pair) (cdr con-pair) mode ttype))
		 (setf fn (replace-re fn "-" "_"))
		 (let* ((f (open-new (format nil "data:;~a;i2b2;~a" t-rel-gen fn))))
		   (dolist (ftkv (hash-table-val-ascd-alist hftcp))
		     (format f "~a~%" (car ftkv)))
		   (close f)))
	     h-fts)

    (setf fn (format nil "~a_~a.voccnt" mode ttype))
    (setf fn (replace-re fn "-" "_"))
    (let* ((f (open-new (format nil "data:;~a;i2b2;~a" t-rel-gen fn))))
      (dolist (ftkv (hash-table-val-ascd-alist h-voc))
	(format f "~a ~a~%" (car ftkv) (cdr ftkv)))
      (close f))
    ;; output instances
    ;; output features
    (close flog)))




;; generate the sparse matrices for stacked denoising auto-encoder
;; an alternative is to follow:
;; Daojian Zeng et al., Relation Classification via Convolutional Deep Neural Network
;; 5 matrices combined: con1, con2, before, middle, after
;; row - instance; col - position; val - word id
;; con1, con2, before right aligned, middle is mid aligned, after is left aligned
(defun sda-inst-gen-pnode (sa c1tag c2tag str-rtype rtype docn h-fts h-voc
			      &aux c1pns c2pns)
  "First determine the head noun (token or abstract token), then computes the 
features. Note that feature index starts from 1.
Input
======
c1tag - the first concept annotation
c2tag - the second concept annotation
rtype - the numeric relation type"
  ;; sample usage: 
  ;; (sda-inst-gen-pnode sa1 ctag11 ctag12 "TrAP" "record-15.spl" h-fts)
  (setf c1pns (annotations-spec c1tag :type (gtype 'gparse-node-type)))
  (setf c2pns (annotations-spec c2tag :type (gtype 'gparse-node-type)))

  (unless (and c1pns c2pns)
    (return-from sda-inst-gen-pnode nil))
  (let* ((s1 (start c1tag))
	 (e1 (end c1tag))
	 (s2 (start c2tag))
	 (e2 (end c2tag))
	 (ugs (i2b2-extract-ngram 1 sa))
	 (c1ug (i2b2-extract-ngram 1 sa :start s1 :end e1))
	 (c2ug (i2b2-extract-ngram 1 sa :start s2 :end e2))
	 (bc1-ug (i2b2-extract-ngram 1 sa :start (and (< e2 s1) e2) :end s1 :filter #'out-concept))
	 (ac1-ug (i2b2-extract-ngram 1 sa :start e1 :end (and (< e1 s2) s2) :filter #'out-concept))
	 (bc2-ug (i2b2-extract-ngram 1 sa :start (and (< e1 s2) e1) :end s2 :filter #'out-concept))
	 (ac2-ug (i2b2-extract-ngram 1 sa :start e2 :end (and (< e2 s1) s1) :filter #'out-concept))
	 (istr (outstr-init))
	 (idstr (outstr-init))
	 (hinst (make-hash-table :test #'equalp)))
    ;; update voc count
    (dolist (ug ugs)
      (incf (gethash ug h-voc 0)))


    ;; relation type
    (format istr "~a " rtype)
    ;; instance detail for back tracking 
    (format idstr "~a:|~a|(~a-~a)|~a|~a|(~a-~a)|~a" 
	    docn (content c1tag) s1 e1 str-rtype (content c2tag) s2 e2 (content sa))
    
    ;; ngrams for concept 1
    (fts2sda c1ug "c1ug" hinst h-fts)
    
    ;; ngrams for concept 2
    (fts2sda c2ug "c2ug" hinst h-fts)	
    
    ;; c1 context ngrams 
    (fts2sda bc1-ug "bc1ug" hinst h-fts)
    (fts2sda ac1-ug "ac1ug" hinst h-fts)

    ;; c2 context ngrams
    (fts2sda bc2-ug "bc2ug" hinst h-fts)
    (fts2sda ac2-ug "ac2ug" hinst h-fts)
    

    (dolist (kv (hash-table-alist hinst))
      (let* ((ftype (car kv))
	     (fts (cdr kv))
	     (i 0))
	(cond 
	 ((member ftype '("c1ug" "c2ug" "bc1ug" "bc2ug") :test #'equalp)
	  (dolist (ft fts)
	    (format istr "~a_~a:~d " ftype i ft)
	    (decf i)))
	 (t
	  (setf fts (nreverse fts))
	  (dolist (ft fts)
	    (format istr "~a_~a:~d " ftype i ft)
	    (incf i))))))
    (list istr idstr)))

(defun test-rel-arff-gen (mode ttype &aux fnarff)
  "This works across multiple corpora"
  (let* ((h-arff-insts (make-hash-table :test #'equalp)))
    (rel-arff-gen (corpus (format nil "i2b2_test_~a" mode))
		  "i2b2-va-test:;relc;"
		  ttype
		  h-arff-insts)
    
    
    (maphash #'(lambda (con-pair l-inst)
		 (setf fnarff (format nil "~a_~a_~a_~a_test.arff" (car con-pair) (cdr con-pair) mode ttype))
		 (setf fnarff (replace-re fnarff "-" "_"))
		 (with-open-file
		  (farff (format nil "data:;arff;i2b2;~a" fnarff)
			 :direction :output 
			 :if-exists :supersede 
			 :if-does-not-exist :create)
		  (format farff "@relation ~a~%" fnarff)
		  (format farff "@attribute class {~{~a,~}}~%" 
			  (gethash con-pair *con-rel-filter*))
		  (format farff "@attribute ID string~%")
		  (format farff "@attribute con1_unigram string~%")
		  (format farff "@attribute con1_bigram string~%")
		  (format farff "@attribute con1_trigram string~%")
		  (format farff "@attribute con2_unigram string~%")
		  (format farff "@attribute con2_bigram string~%")
		  (format farff "@attribute con2_trigram string~%")
		  (format farff "@attribute prec_unigram string~%")
		  (format farff "@attribute prec_bigram string~%")
		  (format farff "@attribute prec_trigram string~%")
		  (format farff "@attribute mid_unigram string~%")
		  (format farff "@attribute mid_bigram string~%")
		  (format farff "@attribute mid_trigram string~%")
		  (format farff "@attribute succ_unigram string~%")
		  (format farff "@attribute succ_bigram string~%")
		  (format farff "@attribute succ_trigram string~%")
		  ;; may optionally want to include these features
		  ;; (format farff "@attribute LeftSynBigramCon1 string~%")
		  ;; (format farff "@attribute RightSynBigramCon1 string~%")
		  ;; (format farff "@attribute LeftSynBigramCon2 string~%")
		  ;; (format farff "@attribute RightSynBigramCon2 string~%")
		  ;; (format farff "@attribute MidVerb string~%")
		  ;; (format farff "@attribute PrecVerb string~%")
		  ;; (format farff "@attribute SuccVerb string~%")
		  ;; (format farff "@attribute SynPath string~%")
		  ;; (format farff "@attribute SynWords string~%")
		  (format farff "@data~%")
		  (dolist (inst-str l-inst)
		    (format farff "~a~%" inst-str))))
	     h-arff-insts)))

(defmethod rel-arff-gen ((corp corpus)
			 (dnrel string)
			 (ttype t)
			 (h-arff-insts hash-table)
			 &key (flog nil)) ; a log  file handle
  (dolist (docid (documents corp))
    (let* ((doc (document docid))
	   (docn (name doc))
	   (fnrel (format nil "~a~a" dnrel (replace-re docn "\\..*$" ".relc"))))
      (rel-arff-gen doc fnrel ttype h-arff-insts :flog flog))))

(defmethod rel-gen ((corp corpus)
		    (dnrel string)
		    (ttype t)
		    (h-insts hash-table)
		    (h-fts hash-table)
		    (h-voc hash-table)
		    (h-ids hash-table)
		    (inst-gen-pnode function)
		    &key (flog nil)) ; a log  file handle
  (dolist (docid (documents corp))
    (let* ((doc (document docid))
	   (docn (name doc))
	   (fnrel (format nil "~a~a" dnrel (replace-re docn "\\..*$" ".relc"))))
      (rel-gen doc fnrel ttype h-insts h-fts h-voc h-ids inst-gen-pnode :flog flog))))


(defmethod rel-arff-gen ((doc document) 
			 (fnrel string) 
			 (ttype t) 
			 (h-arff-insts hash-table)
			 &key (flog nil) ; a log file handle
			 &aux (h-rel (make-hash-table :test #'equalp)))
  "Generate arff entries for each relation instances in the file
Features to use:
- bag of words for concept 1, concept 2, before, between, after
- the left/right syntactic bigrams of concept 1 and 2
- the verbs occuring between the candidate concepts
- up to one verb occuring before and after the candidate concepts
- the syntactic path between concepts
- the link path words
Input
======
h-arff-insts - hash table where key is relation, value is list of strings each
of which is one arff instance"
  ;; sample usage:
  ;; (rel-arff-gen (document "record-15.spl") "i2b2-va-train:;beth;relc;record-15.relc" 'lp-token h)
  (let* ((docn (name doc)))
    (format t "~&rel-arff-gen on doc ~a~%" docn)
    (with-open-file
     (frel fnrel :direction :input)
     (let* (ln 
	    (lnum 0))
       (loop (unless (setf ln (read-line frel nil nil)) (return))
	     (incf lnum)
	     (multiple-value-bind (m? whole ph1 start1 end1 rtype ph2 start2 end2)
		 (match-re "^c=\"([^\"]+)\" +(\\d+) +(\\d+)\\|\\|r=\"([^\"]+)\"\\|\\|c=\"([^\"]+)\" +(\\d+) +(\\d+)" ln)
	       (declare (ignore whole ph1 ph2))
	       (cond 
		(m?
		 (let* ((s1 (parse-integer start1))
			(e1 (parse-integer end1))
			(s2 (parse-integer start2))
			(e2 (parse-integer end2))
			(c1tag (make-instance 'concept-tag
					      :document doc
					      :start s1
					      :end e1))
			(c2tag (make-instance 'concept-tag
					      :document doc
					      :start s2
					      :end e2)))
		   ;; now finds the actual c1tag and c2tag 
		   (setf c1tag (car (annotations-on c1tag :type 'concept-tag))
			 c2tag (car (annotations-on c2tag :type 'concept-tag)))
		   (assert (and c1tag c2tag)
			   ()
			   "concept tags missing:~%~a~%~a~%at line ~a~%" c1tag c2tag ln)
		   (setf (gethash (list (list s1 e1) 
					(list s2 e2)) h-rel)
			 rtype)))
		(t 
		 (format t "~&Warning: problematic line ~a at ~a" lnum fnrel)))))))
    
    (dolist (sa (annotations doc :type 'sentence-annotation))
      (let* ((ctags (annotations sa :type 'concept-tag))
	     c1type c2type rtype)
	(dolist (con-pair (hash-keys *con-rel-filter*))
	  (setf c1type (car con-pair)
		c2type (cdr con-pair))
	  (dolist (c1tag ctags)
	    (dolist (c2tag ctags)
	      ;; we don't want to include spurious relations that could be 
	      ;; filtered out by concept checking.
	      (when (and (/= (id c1tag) (id c2tag))
			 (equalp c1type (format nil "~a" (data c1tag)))
			 (equalp c2type (format nil "~a" (data c2tag))))
		(setf rtype (or (gethash (list (list (start c1tag) (end c1tag))
					       (list (start c2tag) (end c2tag)))
					 h-rel)
				"None"))
		(unless (equalp rtype "None")
		  (format flog "~a~%" (id sa)))
		
		;; if alp-token parses, good; if not, use lp-token
		(let* ((arff-inst (arff-inst-gen-pnode sa c1tag c2tag rtype docn)))
		  (when arff-inst 
		    (push arff-inst (gethash con-pair h-arff-insts))))))))))))


(defmethod rel-gen ((doc document) 
		    (fnrel string) 
		    (ttype t) 
		    (h-insts hash-table)
		    (h-fts hash-table)
		    (h-voc hash-table)
		    (h-ids hash-table)
		    (inst-gen-pnode function) ;; it's supposed to be a function
		    &key (flog nil)	; a log file handle
		    &aux (h-rel (make-hash-table :test #'equalp)))
  "Generate libsvm entries for each relation instances in the file
Features to use:
- bag of words for concept 1, concept 2, before, between, after
- the left/right syntactic bigrams of concept 1 and 2
- the verbs occuring between the candidate concepts
- up to one verb occuring before and after the candidate concepts
- the syntactic path between concepts
- the link path words
Input
======
h-insts - hash table where key is relation, value is list of strings each
of which is one arff instance
h-fts - hash table for features, key is feature type, value is again a hash table whose key is feature label, and value is feature id
"
  ;; sample usage:
  ;; (rel-gen (document "record-15.spl") "i2b2-va-train:;beth;relc;record-15.relc" 'lp-token h)
  (let* ((docn (name doc)))
    (format t "~&rel-gen on doc ~a~%" docn)
    (with-open-file
     (frel fnrel :direction :input)
     (let* (ln 
	    (lnum 0))
       (loop (unless (setf ln (read-line frel nil nil)) (return))
	     (incf lnum)
	     (multiple-value-bind (m? whole ph1 start1 end1 rtype ph2 start2 end2)
		 (match-re "^c=\"([^\"]+)\" +(\\d+) +(\\d+)\\|\\|r=\"([^\"]+)\"\\|\\|c=\"([^\"]+)\" +(\\d+) +(\\d+)" ln)
	       (declare (ignore whole ph1 ph2))
	       (cond 
		(m?
		 (let* ((s1 (parse-integer start1))
			(e1 (parse-integer end1))
			(s2 (parse-integer start2))
			(e2 (parse-integer end2))
			(c1tag (make-instance 'concept-tag
					      :document doc
					      :start s1
					      :end e1))
			(c2tag (make-instance 'concept-tag
					      :document doc
					      :start s2
					      :end e2)))
		   ;; now finds the actual c1tag and c2tag 
		   (setf c1tag (car (annotations-on c1tag :type 'concept-tag))
			 c2tag (car (annotations-on c2tag :type 'concept-tag)))
		   (assert (and c1tag c2tag)
			   ()
			   "concept tags missing:~%~a~%~a~%at line ~a~%" c1tag c2tag ln)
		   (setf (gethash (list (list s1 e1) (list s2 e2)) h-rel)
			 rtype)))
		(t 
		 (format t "~&Warning: problematic line ~a at ~a" lnum fnrel)))))))
    
    (dolist (sa (annotations doc :type 'sentence-annotation))
      (let* ((ctags (annotations sa :type 'concept-tag))
	     c1type c2type rtype hftcp s1 e1 s2 e2)
	(dolist (con-pair (hash-keys *con-rel-filter*))
	  (setf hftcp (gethash con-pair h-fts))
	  (unless hftcp
	    (setf hftcp (make-hash-table :test #'equalp))
	    (setf (gethash con-pair h-fts) hftcp))
	  (setf c1type (car con-pair)
		c2type (cdr con-pair))
	  (dolist (c1tag ctags)
	    (dolist (c2tag ctags)
	      (setf s1 (start c1tag) e1 (end c1tag))
	      (setf s2 (start c2tag) e2 (end c2tag))
	      ;; we don't want to include spurious relations that could be 
	      ;; filtered out by concept checking.
	      (when (and (/= (id c1tag) (id c2tag))
			 (equalp c1type (format nil "~a" (data c1tag)))
			 (equalp c2type (format nil "~a" (data c2tag)))
			 ;; if both problem, must obey groundtruth direction, if relation actually holds
			 (if (and (equalp c1type "problem")
				  (equalp c2type "problem"))
			     (or (gethash (list (list s1 e1) (list s2 e2)) h-rel)
				 (and (not (gethash (list (list s2 e2) (list s1 e1)) h-rel))
				      (< s1 s2)))
			   t)
			 
			 ;; concepts must not overlap
			 (not (member (id c1tag) (mapcar #'id (annotations-share c2tag :type 'concept-tag)))))
		;; (if (< s1 s2)
		;;     (setf rdir 1)
		;;     (setf rdir -1))
		(setf rtype (or (gethash (list (list s1 e1) (list s2 e2)) h-rel)
				"None"))
		(unless (equalp rtype "None")
		  (format flog "~a~%" (id sa)))
		
		;; if alp-token parses, good; if not, use lp-token
		(let* ((rlist (gethash con-pair *con-rel-filter*))
		       (nrtype (position rtype rlist :test #'equalp))
		       
		       (inst-id (funcall inst-gen-pnode sa c1tag c2tag rtype nrtype docn hftcp h-voc))
		       (inst (first inst-id))
		       (id (second inst-id)))
		  (when inst 
		    (push inst (gethash con-pair h-insts))
		    (push id (gethash con-pair h-ids))))))))))))


(defun concepts-loadedp (doc)
  (member "concept-gt" (analyses doc) :test #'equalp))

(defun np-on (ann)
  (annotations ann :type 'phrase-annotation :relation ':=
	       :filter #'(lambda (a) (equalp "NP" (format nil "~a" (data a))))))

(defun np-uo (ann)
  (union
   (annotations ann :type 'phrase-annotation :relation ':o
		:filter #'(lambda (a) (equalp "NP" (format nil "~a" (data a)))))
   (annotations ann :type 'phrase-annotation :relation ':oi
		:filter #'(lambda (a) (equalp "NP" (format nil "~a" (data a)))))))

(defun np-within (ann)
  (annotations ann :type 'phrase-annotation :relation ':in
	       :filter #'(lambda (a) (equalp "NP" (format nil "~a" (data a))))))

(defun np-spanning (ann)
  (annotations ann :type 'phrase-annotation :relation ':ar
	       :filter #'(lambda (a) (equalp "NP" (format nil "~a" (data a))))))


(defmethod concept-eval ((doc document) 
			 (logic-dir string))
  (let* ((docn (name doc)) (match 0) (partial-match 0)
	 ctags rel-nps (num-ctags 0) target-nps)
    (unless (concepts-loadedp doc) 
      (load-concepts docn))
    (dolist (sa (annotations doc :type 'sentence-annotation))
      (setf ctags (annotations sa :type 'concept-tag))
      (incf num-ctags (length ctags))
      (dolist (ctag ctags)
	(cond ;; note that "on" is contained in "within" and "spanning"
	 ((setf target-nps (np-on ctag))
	  (incf match (length target-nps))
	  (incf partial-match (length target-nps)))
	 ((setf target-nps (np-uo ctag))
	  (incf partial-match (length target-nps)))
	 ((setf target-nps (np-within ctag))
	  (incf partial-match (length target-nps)))
	 ((setf target-nps (np-spanning ctag))
	  ;; count np surrounding tag a match
	  (incf match (length target-nps))))
	(setf rel-nps (union rel-nps target-nps))))
    (list match partial-match num-ctags (length rel-nps))))

(defmethod concept-eval ((corp corpus) 
			 (logic-dir string))
  (let* ((match 0) (partial-match 0) (num-ctags 0) (num-rel-nps 0)
	 (pre 0) (rec 0) (f 0) doc-eval-cnt)
    (dolist (doc-id (documents corp))
      (time-profiling
       (format t "~&Evaluate counting ~a: " (name (document doc-id)))
       (setf doc-eval-cnt (concept-eval (document doc-id) logic-dir)))
      (incf match (first doc-eval-cnt))
      (incf partial-match (second doc-eval-cnt))
      (incf num-ctags (third doc-eval-cnt))
      (incf num-rel-nps (fourth doc-eval-cnt)))
    (setf pre (/ match num-rel-nps)
	  rec (/ match num-ctags)
	  f (/ (* 2 pre rec) (+ pre rec)))
    (format t "~&precision: ~f4.2~%recall: ~f4.2~%f-measure: ~f4.2~%" 
	    pre rec f)))
