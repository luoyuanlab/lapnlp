;; -*- Mode: Lisp; Package: late -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 11/29/2011 creation
responsible for feature vector generation and output to different formats
|#

(defpackage :late
  (:use :common-lisp :excl :util :opennlp :cg :mnegex 
	:javatools.jlinker :jc)
  (:export 
   "*max-ft-length*"
   "convert-feature-vector"
   "convert-feature-vector-db"
   "docs-convert-feature-vector-db"
   "docs-convert-feature-vector-hash"
   "generate-train-test"
   "generate-train-test-db"
   "get-feature-values"
   "get-feature-class-values"
   "class-count"
   "class-dist"
   "feature-count"
   "feature-entropy"
   "feature-dist"
   "feature-class-joint-count"
   "feature-class-joint-dist"
   "mutual-information"
   "cg-class-mut-inf"
   "all-cg-class-mut-inf"
   "output-arff-nom-as-str"
   "output-cost-matrix"
   "save-inst-feature"
   "save-file-inst-feature"
   "weka-arff-gen"
   "weka-arff-gen-db"
   "weka-str"
   "set-equalp"
   "set-val-equalp"
   "prepend-list"
   "l-cg-class-mut-inf-db"
   "output-mrns"
   "generate-train-test-spmat-db"
   "boostexter-train-test-gen"
   ))
(in-package :late)


(defparameter *cg-conf* nil
  "The configuration for concept graph features extraction used in machine learning steps, read from ml-spec.cl")

(defparameter *max-ft-length* 65536
  "The feature length limit, should be consistent with feature_list table in
persistence.cl")

(defparameter *min-cg-class-mi* 0.95)

(defparameter *max-cg-class-ent* 0.95)

(defparameter *h-cg-count* (make-hash-table :test #'equalp))

(defparameter *h-metamap-count* (make-hash-table :test #'equalp))

(defparameter *h-fmetamap-count* (make-hash-table :test #'equalp))


(defun set-equalp (a b)
  (null (set-exclusive-or a b :test #'equalp)))

(defun set-val-equalp (a b)
  "Assumes set-class: (list set class)"
  (and (set-equalp (car a) (car b))
       (equalp (cadr a) (cadr b))))

(defun get-feature-values (var class)
  "Considers only training data."
  (let* ((pos-train-iset-name (format nil "~a_positive_train" class))
	 (neg-train-iset-name (format nil "~a_negative_train" class))
	 (l-instn-ptr (mapcar #'cadr (instances pos-train-iset-name)))
	 (l-instn-ntr (mapcar #'cadr (instances neg-train-iset-name)))
	 (l-instn-tr (union l-instn-ptr l-instn-ntr :test #'equalp))
	 (vals (latesql "SELECT feature, instname FROM feature_list 
                         WHERE type=~a" 
			(sq var))))
    (setf vals (remove-if-not #'(lambda (a) (member (cadr a) l-instn-tr :test #'equalp)) vals))
    (setf vals (mapcar #'car vals))
    (setf vals (mapcar #'unsq-read vals))))

(defun get-feature-values-all (var)
  "Considers both training and testing data."
  (let* ((vals (latesql "SELECT feature FROM feature_list WHERE type=~a" 
			(sq var))))
    ;; (setf vals (mapcar #'car vals))
    (setf vals (mapcar #'unsq-read vals))))

(defun get-feature-class-values (var class)
  "Consider only training data."
  (let* ((pos-train-iset-name (format nil "~a_positive_train" class))
	 (neg-train-iset-name (format nil "~a_negative_train" class))
	 (l-instn-ptr (mapcar #'cadr (instances pos-train-iset-name)))
	 (l-instn-ntr (mapcar #'cadr (instances neg-train-iset-name)))
	 (l-pos-instn l-instn-ptr)
	 (l-neg-instn l-instn-ntr)
	 (vals (latesql "SELECT feature, instname FROM feature_list 
                         WHERE type=~a" 
			(sq var)))
	 class ans feature instname)
    (dolist (val vals)
      (setf feature (unsq-read (car val)))
      (setf instname (cadr val))
      (cond 
       ((member instname l-pos-instn :test #'equalp)
	(setf class 1)
	(push (list feature class) ans))
       ((member instname l-neg-instn :test #'equalp)
	(setf class 0)
	(push (list feature class) ans))))
    (nreverse ans)))

(defun get-feature-class-values-all (var class)
  "Consider both training and testing data."
  (let* ((pos-train-iset-name (format nil "~a_positive_train" class))
	 (pos-test-iset-name (format nil "~a_positive_test" class))
	 (l-instn-ptr (mapcar #'cadr (instances pos-train-iset-name)))
	 (l-instn-pte (mapcar #'cadr (instances pos-test-iset-name)))
	 (l-pos-instn (union l-instn-ptr l-instn-pte :test #'equalp))
	 (vals (latesql "SELECT feature, instname FROM feature_list 
                         WHERE type=~a" 
			(sq var)))
	 class ans feature instname)
    (dolist (val vals)
      (setf feature (unsq-read (car val)))
      (setf instname (cadr val))
      (cond 
       ((member instname l-pos-instn :test #'equalp)
	(setf class 1))
       (t
	(setf class 0)))
      (push (list feature class) ans))
    (nreverse ans)))

(defun class-count (class)
  "Count class in only training"
  (let* ((pos-train-iset-name (format nil "~a_positive_train" class))
	 (neg-train-iset-name (format nil "~a_negative_train" class))
	 (l-inst-ptr (instances pos-train-iset-name))
	 (l-inst-ntr (instances neg-train-iset-name))
	 (h-class (make-hash-table :test #'equalp)))
    (setf (gethash 1 h-class) (length l-inst-ptr))
    ;; (format t "~a positive: ~a~%" class (gethash 1 h-class))
    (setf (gethash 0 h-class) (length l-inst-ntr))
    ;; (format t "~a negative: ~a~%" class (gethash 0 h-class))
    h-class))

(defun class-count-all (class)
  "Count class in both training and testing."
  (let* ((pos-train-iset-name (format nil "~a_positive_train" class))
	 (neg-train-iset-name (format nil "~a_negative_train" class))
	 (pos-test-iset-name (format nil "~a_positive_test" class))
	 (neg-test-iset-name (format nil "~a_negative_test" class))
	 (l-inst-ptr (instances pos-train-iset-name))
	 (l-inst-ntr (instances neg-train-iset-name))
	 (l-inst-pte (instances pos-test-iset-name))
	 (l-inst-nte (instances neg-test-iset-name))
	 (h-class (make-hash-table :test #'equalp)))
    (setf (gethash 1 h-class) (+ (length l-inst-ptr) (length l-inst-pte)))
    ;; (format t "~a positive: ~a~%" class (gethash 1 h-class))
    (setf (gethash 0 h-class) (+ (length l-inst-ntr) (length l-inst-nte)))
    ;; (format t "~a negative: ~a~%" class (gethash 0 h-class))
    h-class))

(defun class-dist (class)
  (normalize-hash-val (class-count class)))

(defun class-dist-all (class)
  (normalize-hash-val (class-count-all class)))

(defun normalize-hash-val (h)
  (let* ((sum 0))
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (incf sum v)) 
	     h)
    (maphash #'(lambda (k v)
		 (setf (gethash k h) (/ v sum)))
	     h)
    h))


(defun feature-count (var class)
  "Returns a hash table that stores distribution, value => frequency"
  (let* ((h-dist (make-hash-table :test 'equalp))
	 (vals (get-feature-values var class)))
    (dolist (val vals)
      (incf (gethash val h-dist 0)))
    h-dist))

(defun feature-count-all (var)
  "Returns a hash table that stores distribution, value => frequency"
  (let* ((h-dist (make-hash-table :test 'set-equalp))
	 (vals (get-feature-values-all var)))
    (dolist (val vals)
      (incf (gethash val h-dist 0)))
    h-dist))

(defun feature-dist (var class)
  (normalize-hash-val (feature-count var class)))

(defun feature-dist-all (var)
  (normalize-hash-val (feature-count-all var)))

(defun feature-entropy (var class)
  (let* ((hcnt (feature-count var class))
	 (lcnt (hash-table-alist hcnt)))
    (entropy (mapcar #'cdr lcnt))))

(defun feature-entropy-all (var)
  (let* ((hcnt (feature-count-all var))
	 (lcnt (hash-table-alist hcnt)))
    (entropy (mapcar #'cdr lcnt))))

(defun feature-class-joint-count (var class)
  (let* ((h-jdist (make-hash-table :test 'equalp))
	 (set-vals (get-feature-class-values var class)))
    (dolist (set-val set-vals)
      (incf (gethash set-val h-jdist 0)))
    h-jdist))

(defun feature-class-joint-count-all (var class)
  (let* ((h-jdist (make-hash-table :test 'set-val-equalp))
	 (set-vals (get-feature-class-values-all var class)))
    (dolist (set-val set-vals)
      (incf (gethash set-val h-jdist 0)))
    h-jdist))

(defun feature-class-joint-dist (var class)
  (normalize-hash-val (feature-class-joint-count var class)))

(defun feature-class-joint-dist-all (var class)
  (normalize-hash-val (feature-class-joint-count-all var class)))

(defun mutual-information (dv1 dv2 dvj &aux (mi 0))
  (maphash #'(lambda (v1 p1)
	       (maphash #'(lambda (v2 p2)
			    (let* ((vj (list v1 v2))
				   (pj (gethash vj dvj)))
			      ;; when pj is nil, by definition, no increse
			      (when pj
				(incf mi (* pj (log (/ pj (* p1 p2)) 2))))))
			dv2))
	   dv1)
  mi)

(defun sqr (a)
  (* a a))

(defun cg-class-mut-inf2 (cg-id class)
  "cg-id is also sigsub-id"
  (let* ((h-mut-inf (make-hash-table :test #'equalp))
	 (cg-size (caar (latesql "SELECT count(*) FROM sig_subgraph 
                                  WHERE type='node' AND sub_id=~a" 
				 (sq cg-id))))
	 (slot-vars '("cui_unigram" "lex_unigram" "link_cui_ngram" 
		      "link_lex_ngram"))
	 efv d-efv d-class d-efv-class slot h-mut-inf-slot mi mi-slot mi-slots)
    (dotimes (i cg-size)
      (setf slot (format nil "slot~a" i))
      (setf h-mut-inf-slot (make-hash-table :test #'equalp))
      (setf (gethash slot h-mut-inf) h-mut-inf-slot)
      (setf d-class (class-dist class))
      
      (setf mi-slot nil)
      (dolist (slot-var slot-vars)
	(setf efv (format nil "CG_~a_slot~a_~a" cg-id i slot-var))
	(setf d-efv (feature-dist efv class))
	(setf d-efv-class (feature-class-joint-dist efv class))
	(setf mi (mutual-information d-efv d-class d-efv-class))
	(setf (gethash slot-var h-mut-inf-slot) mi)
	(cond
	 (mi-slot
	  (setf mi-slot (max mi-slot mi)))
	 (t
	  (setf mi-slot mi))))
      (push mi-slot mi-slots))
    
    (sqrt (/ (reduce #'+ (mapcar #'sqr mi-slots)) (length mi-slots)))))

(defun cg-class-mut-inf (cg-id class)
  "cg-id is also sigsub-id"
  (let* (efv d-efv d-class d-efv-class)
    
    (setf efv (format nil "CG_~a" cg-id))
    (setf d-efv (feature-dist efv class))
    (setf d-class (class-dist class))
    (setf d-efv-class (feature-class-joint-dist efv class))
    (mutual-information d-efv d-class d-efv-class)))

(defun cg-class-mut-inf-db (cg-id class)
  (caar (latesql "SELECT mutual_information FROM sigsub_class_mutual_information WHERE sub_id=~a AND class=~a" (sq cg-id) (sq class))))

(defun l-cg-class-mut-inf-db (cg-ids class)
  (let* ((h (make-hash-table :test #'equalp)))
    (dolist (cg-id cg-ids)
      (setf (gethash cg-id h) (cg-class-mut-inf-db cg-id class)))
    (hash-table-val-desc-alist h)))

(defun all-cg-class-mut-inf (class 
			     &key (sigsub-type "sig_subgraph|hier")
			     &aux l-efid-mutinf)
  (setf l-efid-mutinf (latesql "SELECT sub_id, mutual_information FROM sigsub_class_mutual_information WHERE class=~a" (sq class)))
  (unless l-efid-mutinf
    (let* ((l-efid (mapcar #'car (latesql "SELECT gid FROM graph 
                                         WHERE gtype=~a" (sq sigsub-type))))
	   (l-class (make-list (length l-efid) :initial-element class))
	   (l-mutinf (mapcar #'cg-class-mut-inf l-efid l-class)))
      (setf l-efid-mutinf (mapcar #'(lambda (a b) (list a b)) l-efid l-mutinf))
      (dolist (efid-mutinf l-efid-mutinf)
	(let* ((efid (car efid-mutinf))
	       (mutinf (cadr efid-mutinf)))
	  (latesql "INSERT INTO sigsub_class_mutual_information (sub_id,class,mutual_information) VALUES (~a, ~a, ~a)" (sq efid) (sq class) (sq mutinf))))))
  (sort l-efid-mutinf #'> :key #'cadr))

;;; The design here is to leave as much details as possible to user subclassing
(defgeneric convert-feature-vector ((inst t) (label t) &key train? mode))

(defgeneric convert-feature-vector-db ((inst t) &key mode))

(defstruct (ft-unit
	    (:type vector)
	    (:conc-name ftu-))
  name val)

(defparameter *bulk-insert-ft-cmd* nil
  "bulk insert command for features.")

(defun insert-feature (type feature docname instname)
  "Insert feature into feature list DB table"
  (assert (< (length (sq feature)) *max-ft-length*)
	  ()
	  "Feature length exceeds limit of ~a~%(~a)~%" *max-ft-length* feature)
  (cond
   (*output-sql*
    (format *ft-sql* "~a|~a|~a|~a||~%" (sq type) (sq feature) (sq docname) 
	    (sq instname)))
   ((null *bulk-insert-ft-cmd*)
    (setf *bulk-insert-ft-cmd* (outstr-init))
    (format *bulk-insert-ft-cmd* "INSERT INTO feature_list (type, feature, docname, instname) VALUES (~a, ~a, ~a, ~a)" 
	    (sq type) (sq feature) (sq docname) (sq instname)))
   (t
    (format *bulk-insert-ft-cmd* ",~% (~a,~a,~a,~a)"
	    (sq type) (sq feature) (sq docname) (sq instname))
    
    (when (> (length *bulk-insert-ft-cmd*) *max-packet*)
      (setf *bulk-insert-ft-cmd* (replace-re *bulk-insert-ft-cmd* "~" "~~"))
      (latesql *bulk-insert-ft-cmd*)
      (setf *bulk-insert-ft-cmd* nil)))))

(defun add-cg-ftus (ef fv docn instname)
  (cond
   ((getf *cg-conf* 'loose)
    (add-cg-ftus-loose ef fv docn instname))
   
   ((getf *cg-conf* 'tight)
    (add-cg-ftus-tight ef fv docn instname))
   
   ((getf *cg-conf* 'digest)
    (add-cg-ftus-digest ef fv docn instname))

   ((getf *cg-conf* 'suppressed)
    (add-cg-ftus-suppressed ef fv docn instname))
   
   (t
    (error "Unspecified concept graph feature mode!"))))

(defmemo mi-pct (class)
  (let* ((pcts (latesql "select mutual_information 
                         from sigsub_class_mutual_information where class=~a
                         order by mutual_information desc" (sq class)))
	 (rsum 0)
	 sum)
    (setf pcts (mapcar #'car pcts))
    (setf sum (reduce #'+ pcts))
    (dolist (pct pcts)
      (setf rsum (+ rsum pct))
      (when (> rsum (* *min-cg-class-mi* sum))
	;; (format t "sum: ~a; rsum: ~a~%" sum rsum)
	(return-from mi-pct pct)))))


(defun add-cg-ftus-loose (ef fv docn instname)
  "treat each concept graph as having bag of slots and treat each slot as having 
bag of words."
  (let* ((sigsub-id (eff-sigsub-id ef))
	 (l-cg-slot-ft (eff-sigsub-fv ef))
	 ;; (sigsub-entropy (get-sigsub-entropy sigsub-id))
	 ;; (cg-class-mi (cg-class-mut-inf-db sigsub-id))
	 (i 0)
	 cui-unigrams lex-unigrams link-lex-ngrams link-cui-ngrams k ftu)
    (when (and t ;; (> sigsub-entropy 0) ;; (> cg-class-mi *min-cg-class-mi*)
	       )
      (dolist (cg-slot-ft l-cg-slot-ft)
	(setf cui-unigrams (efsf-cui-unigrams cg-slot-ft)
	      lex-unigrams (efsf-lex-unigrams cg-slot-ft)
	      link-lex-ngrams (efsf-link-lex-ngrams cg-slot-ft)
	      link-cui-ngrams (efsf-link-cui-ngrams cg-slot-ft))
	(setf k (format nil "CG_~a_slot~a_cui_unigram" sigsub-id i))
	(setf ftu (make-ft-unit :name k :val cui-unigrams))
	(insert-feature k cui-unigrams docn instname)
	;; update fv, (list ftu) is necessary to maintain fv as a list
	(nconc fv (list ftu))
	;; (push ftu fv)
	
	(setf k (format nil "CG_~a_slot~a_lex_unigram" sigsub-id i))
	(setf ftu (make-ft-unit :name k :val lex-unigrams))
	(insert-feature k lex-unigrams docn instname)	  
	(nconc fv (list ftu))

	(setf k (format nil "CG_~a_slot~a_link_lex_ngram" sigsub-id i))
	(setf ftu (make-ft-unit :name k :val link-lex-ngrams))
	(insert-feature k link-lex-ngrams docn instname)	  
	(nconc fv (list ftu))
	
	(setf k (format nil "CG_~a_slot~a_link_cui_ngram" sigsub-id i))
	(setf ftu (make-ft-unit :name k :val link-cui-ngrams))
	(insert-feature k link-cui-ngrams docn instname)	  
	(nconc fv (list ftu))
	(incf i)))))


(defun add-cg-ftus-suppressed (ef fv docn instname)
  "treat each concept graph as having bag of slots and treat each slot as having bag of words."
  (let* ((sigsub-id (eff-sigsub-id ef))
	 k ftu)
    (setf k (format nil "CG_~a" sigsub-id))
    (insert-feature k k docn instname)
    (setf ftu (make-ft-unit :name k :val k))
    (nconc fv (list ftu))
    fv))


(defun add-cg-ftus-tight (ef fv docn instname)
  "This might not be the ultimate solution, but anyway."
  (let* ((sigsub-id (eff-sigsub-id ef))
	 (l-cg-slot-ft (eff-sigsub-fv ef))
	 ;; (sigsub-entropy (get-sigsub-entropy sigsub-id))
	 ;; (cg-class-mi (cg-class-mut-inf-db sigsub-id))
	 (i 0)
	 (trivial? t)
	 cui-unigrams lex-unigrams ftk ftv ftu )
    (setf ftk (format nil "CG_~a" sigsub-id))
    (setf ftv (outstr-init))
    (dolist (cg-slot-ft l-cg-slot-ft)
      (when (> i 0)
	(format ftv "||"))
      (format ftv "slot~a_" i)
      (setf cui-unigrams (sort (efsf-cui-unigrams cg-slot-ft) 'string-lessp)
	    lex-unigrams (sort (efsf-lex-unigrams cg-slot-ft) 'string-lessp))
      ;; if we have real content, then non-trivial
      (when (or cui-unigrams lex-unigrams)
	(setf trivial? nil))
      (format ftv "cui_~{~a~^_~}|" cui-unigrams)
      (format ftv "lex_~{~a~^_~}" lex-unigrams)
      
      (incf i))
    (unless trivial?
      (setf ftu (make-ft-unit :name ftk :val ftv))
      (insert-feature ftk ftv docn instname)
      (nconc fv (list ftu)))))

(defun lex-orderlessp (lex-unigrams)
  (some #'(lambda (str) (match-re "(dim|\\+|-)$" str)) lex-unigrams))

(defun lex-order-blow (lex-unigrams)
  "Assumes that lex-unigrams are in reverse appearing order in document"
  (let* ((ans lex-unigrams)
	 item)

    (dolist (lu lex-unigrams)
      (push lu item)
      ;; the first item might overlap with bag of words
      (pushnew (format nil "~{~a~^_~}" item) ans :test #'equalp))
    ans))

(defun append-slot-lex (ftv lex)
  (concatenate 'string ftv "lex_" lex))

(defun digest-cg-slot (l-ftv cui-unigrams lex-unigrams i)
  "append each ftv in l-ftv with digested slot features"
  (when (> i 0)
    (setf l-ftv (mapcar #'(lambda (ftv) (concatenate 'string ftv "||")) l-ftv)))
  
  (setf l-ftv (mapcar #'(lambda (ftv) (format nil "~aslot~a_" ftv i)) 
		      l-ftv))
  
  (unless (lex-orderlessp lex-unigrams)
    (setf lex-unigrams (lex-order-blow lex-unigrams)))
  
  ;; insert empty string intentionally, has to be after blown
  ;; (pushnew "" lex-unigrams :test #'equalp)
  
  (setf l-ftv (mapcar #'(lambda (ftv)
			  (format nil "~acui_~{~a~^_~}|" ftv cui-unigrams))
		      l-ftv))
  
  ;; cartesian product, pay attention as follow not to nullify l-ftv
  (setf l-ftv (mapcan #'(lambda (ftv)
			  (or (mapcar #'(lambda (lex)
					  (append-slot-lex ftv lex)) 
				      lex-unigrams)
			      (list (concatenate 'string ftv "lex_")))) 
		      l-ftv)))

(defun singletonp (sub-id)
  (latesql "select * from graph where gid=~a and gname like 'singleton%'" 
	   sub-id))

(defun add-cg-ftus-digest (ef fv docn instname)
  "This differs from add-cg-ftus-tight in that we don't treat the slots simply
as is, instead, we digest the lex-unigrams to either blow modifiers 
into bag of words (when order doesn't matter), 
or into bag of words *plus* backward progression (when order matters)."
  (let* ((sigsub-id (eff-sigsub-id ef))
	 (l-cg-slot-ft (eff-sigsub-fv ef))
	 ;; (sigsub-entropy (get-sigsub-entropy sigsub-id))
	 
	 (i 0)
	 (trivial? t)
	 (ftv-nil (outstr-init))
	 cui-unigrams lex-unigrams ftk ftu l-ftv )
    (setf ftk (format nil "CG_~a" sigsub-id))
    
    (setf l-ftv '(""))
    
    (dolist (cg-slot-ft l-cg-slot-ft)
      (setf cui-unigrams (sort (efsf-cui-unigrams cg-slot-ft) 'string-lessp)
	    lex-unigrams (efsf-lex-unigrams cg-slot-ft))
      
      ;; if we have real content, then non-trivial
      (when (or cui-unigrams lex-unigrams)
	(setf trivial? nil))
      
      (setf l-ftv (digest-cg-slot l-ftv cui-unigrams lex-unigrams i))
      (when (> i 0)
	(format ftv-nil "||"))
      (format ftv-nil "slot~a_cui_|lex_" i)
      (incf i))
    
    (unless trivial?
      (dolist (ftv l-ftv)
	(unless (equalp ftv-nil ftv)
	  (setf ftu (make-ft-unit :name ftk :val ftv))
	  ;; (format t "~&ftu: ~a~%" ftu)
	  (insert-feature ftk ftv docn instname)
	  (nconc fv (list ftu)))))
    fv))

(defmemo get-sigsub-slotname (sub_id i)
  (caar (latesql "SELECT lab FROM sig_subgraph 
                  WHERE type='node' AND sub_id=~a AND n1=~a" 
		 (sq sub_id) (sq i))))

(defun digest-cg-slot-minimal (l-ftv cui-unigrams lex-unigrams slotname)
  "append each ftv in l-ftv with digested slot features
only digest slot if has real content
Experimental"
  (when (or cui-unigrams lex-unigrams)
    (unless (equalp l-ftv '(""))
      (setf l-ftv (mapcar #'(lambda (ftv) (concatenate 'string ftv "||")) 
			  l-ftv)))
    (setf l-ftv (mapcar #'(lambda (ftv) (format nil "~aslot_~a|" ftv slotname)) 
			l-ftv))
    
    (unless (lex-orderlessp lex-unigrams)
      (setf lex-unigrams (lex-order-blow lex-unigrams)))
    
    ;; insert empty string intentionally, has to be after blown
    (pushnew "" lex-unigrams :test #'equalp)
    
    (setf l-ftv (mapcar #'(lambda (ftv)
			    (format nil "~acui_~{~a~^_~}|" ftv cui-unigrams))
			l-ftv))
    
    ;; cartesian product, pay attention not to nullify l-ftv
    (setf l-ftv (mapcan #'(lambda (ftv)
			    (or (mapcar #'(lambda (lex)
					    (append-slot-lex ftv lex)) 
					lex-unigrams)
				(list (concatenate 'string ftv "lex_")))) 
			l-ftv))))

(defun add-cg-ftus-digest-unify (ef fv docn instname)
  "This differs from add-cg-ftus-tight in that we don't treat the slots simply
as is, instead, we digest the lex-unigrams to either blow modifiers 
into bag of words (when order doesn't matter), 
or into bag of words *plus* backward progression (when order matters).
This is not well thought yet."
  (let* ((sigsub-id (eff-sigsub-id ef))
	 (l-cg-slot-ft (eff-sigsub-fv ef))
	 ;; (sigsub-entropy (get-sigsub-entropy sigsub-id))
	 ;; (cg-class-mi (cg-class-mut-inf-db sigsub-id))
	 (i 0)
	 (trivial? t)
	 (ftv-nil (outstr-init))
	 cui-unigrams lex-unigrams ftk ftu l-ftv )
    (when (and t ;; (> sigsub-entropy 0)
	       ;; (> cg-class-mi *min-cg-class-mi*)
	       )
      (setf ftk (format nil "" sigsub-id))
      (setf l-ftv '(""))
      
      (dolist (cg-slot-ft l-cg-slot-ft)
	(setf cui-unigrams (sort (efsf-cui-unigrams cg-slot-ft) 'string-lessp)
	      lex-unigrams (efsf-lex-unigrams cg-slot-ft))
	
	;; if we have real content, then non-trivial
	(when (or cui-unigrams lex-unigrams)
	  (setf trivial? nil))
	
	(setf l-ftv (digest-cg-slot l-ftv cui-unigrams lex-unigrams i))
	(when (> i 0)
	  (format ftv-nil "||"))
	(format ftv-nil "slot_~a|cui_|lex_" (get-sigsub-slotname sigsub-id i))
	(incf i))
      
      (unless trivial?
	(dolist (ftv l-ftv)
	  (unless (equalp ftv-nil ftv)
	    (setf ftu (make-ft-unit :name ftk :val ftv))
	    ;; (format t "~&ftu: ~a~%" ftu)
	    (insert-feature ftk ftv docn instname)
	    (nconc fv (list ftu)))))
      fv)))

(defun docs-convert-feature-vector-db
    (docs 
     &key 
     (mask-secs nil) (instname nil) 
     &aux fv docn)
  "Generate feature vector, feature list is managed by DB tables."
  (push (make-ft-unit :name "inst_name" :val instname) fv)
  (dolist (doc docs)
    (setf docn (name doc))
    (dolist (sen (annotations doc :type 'sentence-annotation))
      (let* ((secs (annotations-spanning sen :type 'section-annotation))
	     (nsecs (mapcar #'(lambda (a) (data a)) secs))
	     ftu)	
	;; (notany #'(lambda (r) (match-re r (content sen))) mask-regexs)
	(when (and (not (intersection mask-secs nsecs :test #'equalp)))
	  
	  
	  (when (getf *ml-features* 'unigram)
	    (mapcar #'(lambda (x) 
			(setf ftu (make-ft-unit :name "unigram" :val x))
			(insert-feature "unigram" x docn instname)
			(push ftu fv))
		    (extract-ngram 1 sen)))
	  
	  (when (getf *ml-features* 'bigram)
	    (mapcar #'(lambda (x)
			(setf ftu (make-ft-unit :name "bigram" :val x))
			(insert-feature "bigram" x docn instname)
			(push ftu fv))
		    (extract-ngram 2 sen)))
	  
	  (when (getf *ml-features* 'trigram)
	    (mapcar #'(lambda (x) 
			(setf ftu (make-ft-unit :name "trigram" :val x))
			(insert-feature "trigram" x docn instname)
			(push ftu fv))
		    (extract-ngram 3 sen)))	  
	  
	  (when (getf *ml-features* 'np)
	    (mapcar #'(lambda (x)
			(setf ftu (make-ft-unit :name "NP" :val x))
			(insert-feature "NP" x docn instname)
			(push ftu fv))
		    (extract-NP sen)))

	  (when (getf *ml-features* 'metamap-con)
	    (mapcar #'(lambda (x)
			(setf ftu (make-ft-unit :name "metamap" :val x))
			(insert-feature "metamap" x docn instname)
			(push ftu fv))
		    (extract-metamap sen)))

	  (when (getf *ml-features* 'fmetamap-con)
	    (mapcar #'(lambda (x)
			(setf ftu (make-ft-unit :name "fmetamap" :val x))
			(insert-feature "fmetamap" x docn instname)
			(push ftu fv))
		    (extract-filtered-metamap sen)))
	  
	  ;; concept graph features
	  (when (getf *ml-features* 'cg)
	    (mapcar #'(lambda (ef) (add-cg-ftus ef fv docn instname)) 
		    (extract-concept-graph 
		     sen 
		     :closure? (getf *cg-conf* 'closure))))))))
  
  (when (getf *ml-features* 'icp)
    (dolist (doc docs)
      (mapcar #'(lambda (x) 
		  (let* ((ftu (make-ft-unit :name (car x) :val (cdr x))))
		    (insert-feature (car x) (cdr x) docn instname)
		    (push ftu fv)))
	      (extract-item-content-pair doc :mask-secs mask-secs))))
  
  (when (getf *ml-features* 'karyotype)
    (dolist (doc docs)
      (mapcar #'(lambda (x)
		  (let* ((ftu (make-ft-unit :name "KARYOTYPE" :val x)))
		    (insert-feature "KARYOTYPE" x docn instname)
		    (push ftu fv)))
	      (extract-karyotype doc))))
  
  (when (getf *ml-features* 'ic)
    (dolist (doc docs)
      (dolist (ic (annotations doc :type 'immuchem-annotation))
	(let* ((secs (annotations-spanning ic :type 'section-annotation))
	       (nsecs (mapcar #'(lambda (a) (format nil "~a" (data a))) secs)))
	  (when (intersection mask-secs nsecs :test #'equalp)
	    (go end1)))			
	
	(let* ((indicator (data ic))
	       (positivity (positivity ic))
	       (ic-name (concatenate 'string "IC_"  indicator))
	       ftu)
	  (when (and positivity (gethash indicator *h-immuchem*))
	    (setf ftu (make-ft-unit :name ic-name :val positivity))
	    (insert-feature ic-name positivity docn instname)
	    (push ftu fv)))
	end1)))
  
  (when *bulk-insert-ft-cmd*
    (setf *bulk-insert-ft-cmd* (replace-re *bulk-insert-ft-cmd* "~" "~~"))
    (latesql *bulk-insert-ft-cmd*)
    (setf *bulk-insert-ft-cmd* nil))
  
  fv
  ;; no labels so we can reuse instances in db
  ;; (push label fv)
  )

(defun docs-convert-feature-vector-hash
    (docs label 
	  &key 
	  (train? nil) (mask-secs nil) (instname nil) 
	  &aux fv)
  "Generate feature vector, feature list is mangaged by hash list associated
with individual types"
  (dolist (doc docs)
    (dolist (sen (annotations doc :type 'sentence-annotation))
      (let* ((secs (annotations-spanning sen :type 'section-annotation))
	     (nsecs (mapcar #'(lambda (a) (data a)) secs))
	     ftu)	
	;; (notany #'(lambda (r) (match-re r (content sen))) mask-regexs)
	(when (and (not (intersection mask-secs nsecs :test #'equalp)))
	  
	  
	  (when (getf *ml-features* 'unigram)
	    (mapcar #'(lambda (x) 
			(setf ftu (make-ft-unit :name "unigram" :val x))
			(pushnew ftu fv :test #'equalp))
		    (extract-ngram-id 1 *h-unigrams* *hr-unigrams* 
				      *h-unigram-count* sen)))
	  
	  (when (getf *ml-features* 'bigram)
	    (mapcar #'(lambda (x)
			(setf ftu (make-ft-unit :name "bigram" :val x))
			(pushnew ftu fv :test #'equalp))
		    (extract-ngram-id 2 *h-bigrams* *hr-bigrams*
				      *h-bigram-count* sen)))
	  
	  (when (getf *ml-features* 'trigram)
	    (mapcar #'(lambda (x) 
			(setf ftu (make-ft-unit :name "trigram" :val x))
			(pushnew ftu fv :test #'equalp))
		    (extract-ngram-id 3 *h-trigrams* *hr-trigrams*
				      *h-trigram-count* sen)))	  
	  (when (getf *ml-features* 'np)
	    (mapcar #'(lambda (x)
			(setf ftu (make-ft-unit :name "NP" :val x))
			(pushnew ftu fv :test #'equalp))
		    (extract-NP-id sen)))

	  (when (getf *ml-features* 'metamap-con)
	    (mapcar #'(lambda (x)
			(setf ftu (make-ft-unit :name "metamap" :val x))
			(pushnew ftu fv :test #'equalp))
		    (extract-metamap-id sen)))
	  
	  ;; concept graph features
	  (when (getf *ml-features* 'cg)
	    (mapcar #'(lambda (x)  
			(let* ((k (format nil "CG_~d" (eff-sigsub-id x)))
			       (v (eff-sigsub-fv x)))
			  (setf ftu (make-ft-unit :name k :val v))
			  ;; pushnew, but might have multiple matching
			  (pushnew ftu fv :test #'equalp))) 
		    (extract-concept-graph 
		     sen 
		     :closure? (getf *cg-conf* 'closure))))))))
  (when (getf *ml-features* 'icp)
    (dolist (doc docs)
      (mapcar #'(lambda (x) 
		  (let* ((ftu (make-ft-unit :name (car x) :val (cdr x))))
		    (pushnew ftu fv :test #'equalp)))
	      (extract-item-content-pair doc train? :mask-secs mask-secs))))
  
  ;; todo add karyotype
  
  (when (getf *ml-features* 'ic)
    (dolist (doc docs)
      (dolist (ic (annotations doc :type 'immuchem-annotation))
	(let* ((secs (annotations-spanning ic :type 'section-annotation))
	       (nsecs (mapcar #'(lambda (a) (format nil "~a" (data a))) secs)))
	  (when (intersection mask-secs nsecs :test #'equalp)
	    (go end1)))			
	
	(let* ((indicator (data ic))
	       (positivity (positivity ic))
	       (ic-name (concatenate 'string "IC_"  indicator))
	       ftu)
	  (when (and positivity (gethash indicator *h-immuchem*))
	    (setf ftu (make-ft-unit :name ic-name :val positivity))
	    (pushnew ftu fv :test #'equalp)))
	end1)))
  
  (push (make-ft-unit :name "inst_name" :val instname) fv)
  (push label fv))

(defun get-db-insts-fv (insts &aux res)
  (setf res (latesql "SELECT features from instances_features WHERE inst~a" 
		     (sql-matcher insts)))
  (setf res (mapcar #'car res))
  (setf res (mapcar #'unsq-read res)))

(defun get-insts-fv (insts &aux fv res)
  (dolist (inst insts)
    ;; (format t "~&reading ~a.sql~%" inst)
    (setf fv (read-file (format nil "late:;tmp_inst_sql;~a.sql" inst)))
    (setf fv (replace-re fv "^\\d+\\|\\|\\|" ""))
    (setf fv (replace-re fv "\\|{4}\\n.*$" ""))
    (setf fv (replace-re fv "(^'|'$)" ""))
    (push (unsq-read fv) res))
  (nreverse res))

(defun append-label (fv label)
  (append (list label) fv))

(defun append-plabel (fv)
  (append-label fv 1))

(defun append-nlabel (fv)
  (append-label fv 0))

(defun repeat (l n)
  (if (zerop n)
      nil
    (append l (repeat l (1- n)))))

(defun generate-train-test-db (pos-train-corpus-name
			       neg-train-corpus-name
			       train-corpus-name
			       pos-test-corpus-name
			       neg-test-corpus-name
			       test-corpus-name
			       &key 
			       (excl-insts nil))
  (let* ((l-inst-id-ptr (mapcar #'car (instances pos-train-corpus-name)))
	 (l-inst-id-ntr (mapcar #'car (instances neg-train-corpus-name)))
	 (l-inst-id-pte (mapcar #'car (instances pos-test-corpus-name)))
	 (l-inst-id-nte (mapcar #'car (instances neg-test-corpus-name)))
	 (l-excl-inst-id (mapcar #'car excl-insts))
	 (l-inst-id-ptr (set-difference l-inst-id-ptr l-excl-inst-id))
	 (l-inst-id-ntr (set-difference l-inst-id-ntr l-excl-inst-id))
	 (l-inst-id-pte (set-difference l-inst-id-pte l-excl-inst-id))
	 (l-inst-id-nte (set-difference l-inst-id-nte l-excl-inst-id))
	 (l-fv-ptr (mapcar #'append-plabel (get-insts-fv l-inst-id-ptr)))
	 (l-fv-ntr (mapcar #'append-nlabel (get-insts-fv l-inst-id-ntr)))
	 (l-fv-pte (mapcar #'append-plabel (get-insts-fv l-inst-id-pte)))
	 (l-fv-nte (mapcar #'append-nlabel (get-insts-fv l-inst-id-nte)))
	 l-fv-tr class)
    
    (format t "~&l-fv-ptr: ~a~%l-fv-ntr: ~a~%l-fv-pte: ~a~%l-fv-nte: ~a~%"
	    (length l-fv-ptr) (length l-fv-ntr) 
	    (length l-fv-pte) (length l-fv-nte))
    (cond 
     ((match-re "burkitts" pos-train-corpus-name)
      (setf class "burkitts"))
     ((match-re "dlbcl" pos-train-corpus-name)
      (setf class "dlbcl"))
     ((match-re "follicular" pos-train-corpus-name)
      (setf class "follicular"))
     ((match-re "hodgkins" pos-train-corpus-name)
      (setf class "hodgkins")))
    
    (setf l-fv-tr (append l-fv-ptr l-fv-ntr))
    (format t "~&after repeating~%l-fv-ptr: ~a~%l-fv-ntr: ~a~%"
	    (length l-fv-ptr) (length l-fv-ntr))
    (count-ft (append l-fv-ptr l-fv-ntr)) ; 

    (weka-arff-gen-db 
     l-fv-tr class
     :out-fn (concatenate 'string "data:;arff;" train-corpus-name ".arff"))
    (jl-free :flush)
    
    (weka-arff-gen-db
     (append l-fv-pte l-fv-nte) class
     :out-fn (concatenate 'string "data:;arff;" test-corpus-name ".arff"))
    (jl-free :flush)))

(defun boostexter-train-test-gen (pos-train-corpus-name
				  neg-train-corpus-name
				  pos-test-corpus-name
				  neg-test-corpus-name
				  stem-name)
  (let* ((mrns-ptr (mrns pos-train-corpus-name))
	 (mrns-ntr (mrns neg-train-corpus-name))
	 (mrns-pte (mrns pos-test-corpus-name))
	 (mrns-nte (mrns neg-test-corpus-name))
	 (hclass (make-hash-table :test #'equalp))
	 (fn-name (format nil "data:;boostexter;~a.names" stem-name))
	 (fn-tr (format nil "data:;boostexter;~a.data" stem-name))
	 (fn-te (format nil "data:;boostexter;~a.test" stem-name))
	 mrns-tr mrns-te)
    
    (format t "~&mrns-ptr: ~a~%mrns-ntr: ~a~%mrns-pte: ~a~%mrns-nte: ~a~%"
	    (length mrns-ptr) (length mrns-ntr) 
	    (length mrns-pte) (length mrns-nte))

    (boostexter:assign-class mrns-ptr "p" hclass)
    (boostexter:assign-class mrns-ntr "n" hclass)
    (boostexter:assign-class mrns-pte "p" hclass)
    (boostexter:assign-class mrns-nte "n" hclass)

    (setf mrns-tr (append mrns-ptr mrns-ntr))
    (setf mrns-te (append mrns-pte mrns-nte))
    
    (boostexter:write-names fn-name)
    (format t "~&writing boostexter train~%")
    (boostexter:ngram-gen mrns-tr hclass fn-tr)
    (format t "~&writing boostexter test~%")
    (boostexter:ngram-gen mrns-te hclass fn-te)))


(defun generate-train-test-spmat-db (pos-train-corpus-name
				     neg-train-corpus-name
				     train-corpus-name
				     pos-test-corpus-name
				     neg-test-corpus-name
				     test-corpus-name
				     &key 
				     (excl-insts nil))
  (let* ((l-inst-id-ptr (mapcar #'car (instances pos-train-corpus-name)))
	 (l-inst-id-ntr (mapcar #'car (instances neg-train-corpus-name)))
	 (l-inst-id-pte (mapcar #'car (instances pos-test-corpus-name)))
	 (l-inst-id-nte (mapcar #'car (instances neg-test-corpus-name)))
	 (l-excl-inst-id (mapcar #'car excl-insts))
	 (l-inst-id-ptr (set-difference l-inst-id-ptr l-excl-inst-id))
	 (l-inst-id-ntr (set-difference l-inst-id-ntr l-excl-inst-id))
	 (l-inst-id-pte (set-difference l-inst-id-pte l-excl-inst-id))
	 (l-inst-id-nte (set-difference l-inst-id-nte l-excl-inst-id))
	 (l-fv-ptr (mapcar #'append-plabel (get-insts-fv l-inst-id-ptr)))
	 (l-fv-ntr (mapcar #'append-nlabel (get-insts-fv l-inst-id-ntr)))
	 (l-fv-pte (mapcar #'append-plabel (get-insts-fv l-inst-id-pte)))
	 (l-fv-nte (mapcar #'append-nlabel (get-insts-fv l-inst-id-nte)))
	 (hmrn (load-mrns "data:;spmat;all_mrn"))
	 fnsp-tr fnsp-te l-fv-tr fnft class)
    
    (format t "~&l-fv-ptr: ~a~%l-fv-ntr: ~a~%l-fv-pte: ~a~%l-fv-nte: ~a~%"
	    (length l-fv-ptr) (length l-fv-ntr) 
	    (length l-fv-pte) (length l-fv-nte))
    (cond 
     ((match-re "burkitts" pos-train-corpus-name)
      (setf l-fv-tr (append (repeat l-fv-ptr 10) l-fv-ntr))
      (setf class "burkitts")
      (setf fnft "data:;spmat;burkitts.ft"))
     ((match-re "dlbcl" pos-train-corpus-name)
      (setf l-fv-tr (append l-fv-ptr (repeat l-fv-ntr 2)))
      (setf class "dlbcl")
      (setf fnft "data:;spmat;dlbcl.ft"))
     ((match-re "follicular" pos-train-corpus-name)
      (setf l-fv-tr (append (repeat l-fv-ptr 5) l-fv-ntr))
      (setf class "follicular")
      (setf fnft "data:;spmat;follicular.ft"))
     ((match-re "hodgkins" pos-train-corpus-name)
      (setf l-fv-tr (append (repeat l-fv-ptr 7) l-fv-ntr))
      (setf class "hodgkins")
      (setf fnft "data:;spmat;hodgkins.ft")))
    (setf l-fv-tr (append l-fv-ptr l-fv-ntr))
    (count-ft (append l-fv-ptr l-fv-ntr)) ; 

    (let* ((cgs (cg-filter (h-get-cg-feature-list) class))
	   (cgs (sort-cg-size cgs))
	   (ugs (count-filter (get-ft-val-list "unigram" "UG") 
			      *h-unigram-count*))
	   (bgs (count-filter (get-ft-val-list "bigram" "BG") 
			      *h-bigram-count*))
	   (tgs (count-filter (get-ft-val-list "trigram" "TG") 
			      *h-trigram-count*))
	   (mms (count-filter (get-ft-val-list "metamap" "MM") 
			      *h-metamap-count*))
	   (fmms (count-filter (get-ft-val-list "fmetamap" "FMM") 
			       *h-fmetamap-count*))
	   (hds '("class" "inst_name"))
	   (hft (ft-layout (append hds cgs ugs bgs tgs mms fmms))))
      (setf fnsp-tr (format nil "data:;spmat;~a.spmat" train-corpus-name))
      (sp-mat-gen-db l-fv-tr fnsp-tr hmrn hft class)
      (jl-free :flush)
      
      (setf fnsp-te (format nil "data:;spmat;~a.spmat" test-corpus-name))
      (sp-mat-gen-db (append l-fv-pte l-fv-nte) fnsp-te hmrn hft class)
      (jl-free :flush)
      
      (write-idx-hash hft fnft))))

(defun generate-train-test (pos-train-corpus-name
			    neg-train-corpus-name
			    train-corpus-name
			    pos-test-corpus-name
			    neg-test-corpus-name
			    test-corpus-name
			    &key (mode nil) 
			    (sigsub-type "sig_subgraph|hier"))
  "corpus really means instance set here."
  (clrhash *h-item-content-pair*)
  (clrhash *hr-item-content-pair*)
  (format t "~&filter train test: ~a, ~a~%" train-corpus-name test-corpus-name)
  ;; g means grouped feature list
  (let* (pos-train-fv-l neg-train-fv-l
			pos-test-fv-l neg-test-fv-l)
    (dolist (inst (mapcar #'second (instances pos-train-corpus-name)))
      (let* ((fv (convert-feature-vector inst 1 :train? t :mode mode)))
	(if fv
	    (pushnew fv pos-train-fv-l :test #'equalp)
	  (error "no fv for ~a: ~a" mode inst))))
    (setf pos-train-fv-l (nreverse pos-train-fv-l))

    (dolist (inst (mapcar #'second (instances neg-train-corpus-name)))
      (let* ((fv (convert-feature-vector inst 0 :train? t :mode mode)))
	(if fv
	    (pushnew fv neg-train-fv-l :test #'equalp)
	  (error "no fv for ~a: ~a" mode inst))))
    (setf neg-train-fv-l (nreverse neg-train-fv-l))

    
    (weka-arff-gen 
     (append pos-train-fv-l neg-train-fv-l)
     :out-fn (concatenate 'string "data:;arff;" train-corpus-name ".arff")
     :sigsub-type sigsub-type)
    
    (jl-free :flush)
    
    (dolist (inst (mapcar #'second (instances pos-test-corpus-name)))
      (let* ((fv (convert-feature-vector inst 1 :train? nil :mode mode)))
	(if fv
	    (pushnew fv pos-test-fv-l :test #'equalp)
	  (error t "no fv for ~a: ~a" mode inst))))
    (setf pos-test-fv-l (nreverse pos-test-fv-l))

    (dolist (inst (mapcar #'second (instances neg-test-corpus-name)))
      (let* ((fv (convert-feature-vector inst 0 :train? nil :mode mode)))
	(if fv
	    (pushnew fv neg-test-fv-l :test #'equalp)
	  (error t "no fv for ~a: ~a" mode inst))))
    (setf neg-test-fv-l (nreverse neg-test-fv-l))    
    
    (weka-arff-gen 
     (append pos-test-fv-l neg-test-fv-l)
     :out-fn (concatenate 'string "data:;arff;" test-corpus-name ".arff")
     :sigsub-type sigsub-type)

    (jl-free :flush)))

(defun inst-featuregen-p (inst-id)
  (latesql "SELECT * FROM instances_features WHERE inst=~a" (sq inst-id)))

(defun save-inst-feature (inst-id fv)
  ;; do not insert duplicate instances
  ;; This only works when you insert lymphoma class one by one
  (unless nil ;; (inst-featuregen-p inst-id)
    (latesql "INSERT INTO instances_features (inst, features) VALUES (~a, ~a)"
	     (sq inst-id) (sq fv))))

(defun save-file-inst-feature (inst-id fv)
  (with-open-file (f (format nil "late:;tmp_inst_sql;~a.sql" inst-id)
		     :direction :output :if-does-not-exist :create
		     :if-exists :supersede)
		  (format f "~&~a|||~a||||~%" (sq inst-id) (sq fv))))

(defun output-arff-nom-as-str-cg-separate (y x 
					     &key 
					     (out-fn nil) (ics nil) (cgs nil) (icps nil)
					     &aux
					     (ft-id))
  "Replace nominal values as string for ease of converting to liblinear format"
  (format t "~&writing to file ~s~%" out-fn)
  (with-open-file
   (s out-fn :direction :output :if-exists :supersede)
   
   (format s "@relation ~a~%" out-fn)
   (format s "@attribute class {0,1}~%")
   
   (setf ft-id 2)
   (dolist (cg cgs)
     (format s "@attribute ~a string~%" cg))
   (format t "~&cg features: ~a-~a~%" ft-id (1- (incf ft-id (length cgs))))
   
   (dolist (ic ics)
     (format s "@attribute ~a string~%" (replace-re ic "\\s+" "_")))
   (format t "~&ic features: ~a-~a~%" ft-id (1- (incf ft-id (length ics))))

   

   
   (dolist (icp icps)
     ;; need to distinguish quantity value from string value.
     (cond 
      ((match-re "^ICP_.*_quantity_(<|=|>)?" icp)
       (format s "@attribute ~a numeric~%" icp))
      ((match-re "^ICP_.*_quality" icp)
       (format s "@attribute ~a string~%" icp))
      (t
       (format t "~&icp ~a not properly formatted~%" icp))))
   (format t "~&icp features: ~a-~a~%" ft-id (1- (incf ft-id (length icps))))
   
   (when (getf *ml-features* 'karyotype) 
     (format s "@attribute KARYOTYPE string~%")
     (format t "~&karyotype features: ~a-~a~%" ft-id (1- (incf ft-id))))
   
   (when (getf *ml-features* 'unigram) 
     (format s "@attribute unigram string~%")
     (format t "~&unigram features: ~a-~a~%" ft-id (1- (incf ft-id))))
   (when (getf *ml-features* 'bigram)
     (format s "@attribute bigram string~%")
     (format t "~&bigram features: ~a-~a~%" ft-id (1- (incf ft-id))))
   (when (getf *ml-features* 'trigram)
     (format s "@attribute trigram string~%")
     (format t "~&trigram features: ~a-~a~%" ft-id (1- (incf ft-id))))
   (when (getf *ml-features* 'np) 
     (format s "@attribute NP string~%")
     (format t "~&np features: ~a-~a~%" ft-id (1- (incf ft-id))))
   (when (getf *ml-features* 'metamap-con) 
     (format s "@attribute metamap string~%")
     (format t "~&metamap features: ~a-~a~%" ft-id (1- (incf ft-id))))
   (when (getf *ml-features* 'fmetamap-con) 
     (format s "@attribute fmetamap string~%")
     (format t "~&fmetamap features: ~a-~a~%" ft-id (1- (incf ft-id))))
   
   
   (format s "@attribute inst_name string~%")
   (format s "@data~%")
   
   (do* ((ly y (cdr ly))
	 (lx x (cdr lx))
	 (ey (car ly) (car ly))
	 (ex (car lx) (car lx)))
       ((null ly))
     ;; (format t "~&output inst: ~a~%" (getf ex 'inst_name))
     (format s "~a, " ey)
     
     (format s "~{\"~a\", ~}" 
	     (mapcar #'(lambda (cg) (getf ex (intern cg))) cgs))
     
     (dolist (ic ics)
       (let* ((ic-val (getf ex (intern ic))))
	 (cond 
	  ((null ic-val)
	   (error "ic-val for ~a shouldn't be null" ic))
	  ((equalp "" ic-val)
	   (format s "\"\", "))
	  (t
	   (format s "\"~a_~a\", " ic ic-val)))))
     
     
     
     (dolist (icp icps)
       (let* ((icpc (getf ex (intern icp))))
	 (cond 
	  ;; need to recheck if uncomment
	  ((match-re "^ICP_.*_quantity_(<|=|>)?" icp)
	   (cond
	    ((typep icpc 'number)
	     (format s "~f, " icpc))
	    (t
	     (unless (equalp icpc "?")
	       (format t "warning: ~a: ~a not numeric or ?" icp icpc))
	     (format s "?, "))))
	  
	  ((match-re "^ICP_.*_quality" icp)
	   (assert (stringp icpc)
		   ()
		   "~a's val ~a is not of string type~%" icp icpc)
	   (format s "\"~a\", " (getf ex (intern icp))))
	  
	  (t
	   (format t "~&icp content ~a not properly formatted~%" icp)))))
     
     (when (getf *ml-features* 'karyotype)
       (format s "\"~a\", " (getf ex 'karyotype)))
     
     (when (getf *ml-features* 'unigram)
       (format s "\"~a\", " (getf ex 'unigram)))
     (when (getf *ml-features* 'bigram)
       (format s "\"~a\", " (getf ex 'bigram)))
     (when (getf *ml-features* 'trigram)
       (format s "\"~a\", " (getf ex 'trigram)))
     (when (getf *ml-features* 'np)
       (format s "\"~a\", " (getf ex 'NP)))
     (when (getf *ml-features* 'metamap-con)
       (format s "\"~a\", " (getf ex 'metamap-con)))
     (when (getf *ml-features* 'fmetamap-con)
       (format s "\"~a\", " (getf ex 'fmetamap-con)))
     (format s "\"~a\"" (getf ex 'inst_name))
     (format s "~%"))))

(defun output-arff-nom-as-str (y x 
				 &key 
				 (out-fn nil) (ics nil) (icps nil)
				 &aux
				 (ft-id))
  "Replace nominal values as string for ease of converting to liblinear format"
  (format t "~&writing to file ~s~%" out-fn)
  (with-open-file
   (s out-fn :direction :output :if-exists :supersede)
   
   (format s "@relation ~a~%" out-fn)
   (format s "@attribute class {0,1}~%")
   
   (setf ft-id 2)
   (format s "@attribute cg string~%")
   (format t "~&cg features: ~a-~a~%" ft-id (1- (incf ft-id)))
   
   (dolist (ic ics)
     (format s "@attribute ~a string~%" (replace-re ic "\\s+" "_")))
   (format t "~&ic features: ~a-~a~%" ft-id (1- (incf ft-id (length ics))))

   

   
   (dolist (icp icps)
     ;; need to distinguish quantity value from string value.
     (cond 
      ((match-re "^ICP_.*_quantity_(<|=|>)?" icp)
       (format s "@attribute ~a numeric~%" icp))
      ((match-re "^ICP_.*_quality" icp)
       (format s "@attribute ~a string~%" icp))
      (t
       (format t "~&icp ~a not properly formatted~%" icp))))
   (format t "~&icp features: ~a-~a~%" ft-id (1- (incf ft-id (length icps))))
   
   (when (getf *ml-features* 'karyotype) 
     (format s "@attribute KARYOTYPE string~%")
     (format t "~&karyotype features: ~a-~a~%" ft-id (1- (incf ft-id))))
   
   (when (getf *ml-features* 'unigram) 
     (format s "@attribute unigram string~%")
     (format t "~&unigram features: ~a-~a~%" ft-id (1- (incf ft-id))))
   (when (getf *ml-features* 'bigram)
     (format s "@attribute bigram string~%")
     (format t "~&bigram features: ~a-~a~%" ft-id (1- (incf ft-id))))
   (when (getf *ml-features* 'trigram)
     (format s "@attribute trigram string~%")
     (format t "~&trigram features: ~a-~a~%" ft-id (1- (incf ft-id))))
   (when (getf *ml-features* 'np) 
     (format s "@attribute NP string~%")
     (format t "~&np features: ~a-~a~%" ft-id (1- (incf ft-id))))
   (when (getf *ml-features* 'metamap-con) 
     (format s "@attribute metamap string~%")
     (format t "~&metamap features: ~a-~a~%" ft-id (1- (incf ft-id))))
   (when (getf *ml-features* 'fmetamap-con) 
     (format s "@attribute fmetamap string~%")
     (format t "~&fmetamap features: ~a-~a~%" ft-id (1- (incf ft-id))))
   
   
   (format s "@attribute inst_name string~%")
   (format s "@data~%")
   
   (do* ((ly y (cdr ly))
	 (lx x (cdr lx))
	 (ey (car ly) (car ly))
	 (ex (car lx) (car lx)))
       ((null ly))
     ;; (format t "~&output inst: ~a~%" (getf ex 'inst_name))
     (format s "~a, " ey)
     
     (format s "\"~a\", " (getf ex 'cg))
     
     (dolist (ic ics)
       (let* ((ic-val (getf ex (intern ic))))
	 (cond 
	  ((null ic-val)
	   (error "ic-val for ~a shouldn't be null" ic))
	  ((equalp "" ic-val)
	   (format s "\"\", "))
	  (t
	   (format s "\"~a_~a\", " ic ic-val)))))
     
     
     
     (dolist (icp icps)
       (let* ((icpc (getf ex (intern icp))))
	 (cond 
	  ;; need to recheck if uncomment
	  ((match-re "^ICP_.*_quantity_(<|=|>)?" icp)
	   (cond
	    ((typep icpc 'number)
	     (format s "~f, " icpc))
	    (t
	     (unless (equalp icpc "?")
	       (format t "warning: ~a: ~a not numeric or ?" icp icpc))
	     (format s "?, "))))
	  
	  ((match-re "^ICP_.*_quality" icp)
	   (assert (stringp icpc)
		   ()
		   "~a's val ~a is not of string type~%" icp icpc)
	   (format s "\"~a\", " (getf ex (intern icp))))
	  
	  (t
	   (format t "~&icp content ~a not properly formatted~%" icp)))))
     
     (when (getf *ml-features* 'karyotype)
       (format s "\"~a\", " (getf ex 'karyotype)))
     
     (when (getf *ml-features* 'unigram)
       (format s "\"~a\", " (getf ex 'unigram)))
     (when (getf *ml-features* 'bigram)
       (format s "\"~a\", " (getf ex 'bigram)))
     (when (getf *ml-features* 'trigram)
       (format s "\"~a\", " (getf ex 'trigram)))
     (when (getf *ml-features* 'np)
       (format s "\"~a\", " (getf ex 'NP)))
     (when (getf *ml-features* 'metamap-con)
       (format s "\"~a\", " (getf ex 'metamap-con)))
     (when (getf *ml-features* 'fmetamap-con)
       (format s "\"~a\", " (getf ex 'fmetamap-con)))
     (format s "\"~a\"" (getf ex 'inst_name))
     (format s "~%"))))


(defun weka-str (str &aux term)
  (setf term (replace-re str "\\s+" "_"))
  (setf term (replace-re term "(\\:|\\;|\\,|%)" ""))
  (setf term (replace-re term "(\"|\\'|\\?)" ""))
  (setf term (replace-re term "[\\(\\[\\{]" ""))
  (setf term (replace-re term "[\\)\\]\\}]" ""))
  (setf term (replace-re term "\\." "p")))

(defun prepend-list (pref list-in &aux psize prefs)
  "prepend each element of list-in with pref followed by an _"
  (setf psize (length list-in))
  (setf prefs (make-list psize :initial-element pref))
  (mapcar #'(lambda (a b) (format nil "~a_~a" a b)) prefs list-in))

(defun get-feature-list (type)
  (mapcar #'car (latesql "SELECT DISTINCT type FROM feature_list 
                          WHERE type like '~a%' ORDER BY feature" 
			 type)))
(defparameter *h-ftt* (make-hash-table :test #'equalp))

(defun h-get-feature-list (type)
  (db-load-feature-list)
  (let* ((l-type (hash-keys *h-ftt*)))
    (remove-if-not #'(lambda (a) 
		       (and (search type a) (= 0 (search type a)))) 
		   l-type)))

(defun load-feature-list (&key 
			  (dir (make-pathname 
				:host "late" 
				:directory '(:relative "tmp_ft_sql") 
				:name :wild)) 
			  &aux ln)
  (when (= 0 (hash-table-count *h-ftt*))
    (format t "~&loading feature list~%")
    (time-profiling
     (dolist (fn (directory dir))
       (with-open-file (f (probe-file fn) :direction :input)
		       (loop (unless (setf ln (read-line f nil nil)) (return))
			     (setf ln (replace-re ln "\\|\\|$" ""))
			     (destructuring-bind (type ft docn instn)
				 (split-re "'\\|'" ln)
			       (declare (ignorable ft docn instn))
			       (setf type (replace-re type "(^'|'$)" ""))
			       (incf (gethash type *h-ftt* 0)))))))))


(defun db-load-feature-list ()
  (when (= 0 (hash-table-count *h-ftt*))
    (format t "~&loading feature list from db~%")
    (time-profiling
     (mapcar #'(lambda (row)
		 (setf (gethash (car row) *h-ftt*) 1))  
	     (latesql "select distinct type from feature_list")))))

(defun get-cg-feature-list (class)
  (let* ((efs (mapcar #'car (latesql "SELECT DISTINCT type FROM feature_list 
                          WHERE type like 'CG\\_%' ORDER BY feature")))
	 sub-id ans)
    (dolist (ef efs)
      (setf sub-id (parse-integer (second (split-re "_" ef))))
      (when (>= (cg-class-mut-inf-db sub-id class) (mi-pct class))
	(pushnew ef ans)))
    ans))

(defmemo h-get-cg-feature-list ()
  (let* ((cgs (h-get-feature-list "CG_"))
	 (hcg-size (make-hash-table :test #'equalp))
	 (cnt 0)
	 (lev-size 0)
	 ans)
    (format t "~&[h-get-cg-feature-list]~%")
    (dolist (cg cgs)
      (let* ((cg-id (parse-integer (second (split-re "_" cg))))
	     (cg-size (sigsub-size cg-id)))
	(setf (gethash cg hcg-size) cg-size)))
    (dolist (kv (hash-table-val-ascd-alist hcg-size))
      (let* ((cg (car kv))
	     (cg-size (cdr kv)))
	(incf cnt)
	(when (> cg-size lev-size)
	  (setf lev-size cg-size)
	  (format t "~&cg cnt: ~a; size: ~a~%" cnt cg-size))
	(push cg ans)))
    (reverse ans)))

(defun sort-cg-size (cgs)
  (let* ((hcg-size (make-hash-table :test #'equalp))
	 (cnt 0)
	 (lev-size 0)
	 ans)
    (format t "~&[sort-cg-size]~%")
    (dolist (cg cgs)
      (let* ((cg-id (parse-integer (second (split-re "_" cg))))
	     (cg-size (sigsub-size cg-id)))
	(setf (gethash cg hcg-size) cg-size)))
    (dolist (kv (hash-table-val-ascd-alist hcg-size))
      (let* ((cg (car kv))
	     (cg-size (cdr kv)))
	(incf cnt)
	(when (> cg-size lev-size)
	  (setf lev-size cg-size)
	  (format t "~&cg cnt: ~a; size: ~a~%" cnt cg-size))
	(push cg ans)))
    (reverse ans)))

(defun get-ft-val-list (type pref)
  (let* ((fts (latesql "SELECT DISTINCT feature FROM feature_list 
                          WHERE type='~a' ORDER BY feature" type))
	 (fts (mapcar #'car fts)))
    (prepend-list pref fts)))


(defun count-ft (fv-l)
  (clrhash *h-unigram-count*)
  (clrhash *h-bigram-count*)
  (clrhash *h-trigram-count*)
  (clrhash *h-cg-count*)
  (clrhash *h-metamap-count*)
  (clrhash *h-fmetamap-count*)
  (let* (term ft mrn)
    (dolist (fv fv-l)
      (setf ft (cdr fv))
      (dolist (ft-u ft)
	(when (string= (ftu-name ft-u) "inst_name")
	  (setf mrn (ftu-val ft-u))))
      (dolist (ft-u ft)
	(when (string= (ftu-name ft-u) "unigram")
	  (setf term (weka-str (concatenate 'string "UG_" (ftu-val ft-u))))
	  ;; (incf (gethash term *h-unigram-count* 0))
	  (pushnew mrn (gethash term *h-unigram-count*) :test #'equalp))

	(when (string= (ftu-name ft-u) "bigram")
	  (setf term (weka-str (concatenate 'string "BG_" (ftu-val ft-u))))
	  ;; (incf (gethash term *h-bigram-count* 0))
	  (pushnew mrn (gethash term *h-bigram-count*) :test #'equalp))

	(when (string= (ftu-name ft-u) "trigram")
	  (setf term (weka-str (concatenate 'string "TG_" (ftu-val ft-u))))
	  ;; (incf (gethash term *h-trigram-count* 0))
	  (pushnew mrn (gethash term *h-trigram-count*) :test #'equalp))

	(when (string= (ftu-name ft-u) "metamap")
	  (setf term (weka-str (concatenate 'string "MM_" (ftu-val ft-u))))
	  ;; (incf (gethash term *h-trigram-count* 0))
	  (pushnew mrn (gethash term *h-metamap-count*) :test #'equalp))

	(when (string= (ftu-name ft-u) "fmetamap")
	  (setf term (weka-str (concatenate 'string "FMM_" (ftu-val ft-u))))
	  ;; (incf (gethash term *h-trigram-count* 0))
	  (pushnew mrn (gethash term *h-fmetamap-count*) :test #'equalp))

	(when (match-re "^CG_" (ftu-name ft-u))
	  (setf term (weka-str (ftu-name ft-u)))
	  (pushnew mrn (gethash term *h-cg-count*) :test #'equalp))))))

(defun count-pass? (term h)
  (let* ((c (length (gethash term h)))
	 (lb 2))
    ;; (cond
    ;;   ((equalp "burkitts" class)
    ;;    (setf lb 10))
    ;;   ((equalp "dlbcl" class)
    ;;    (setf lb 2))
    ;;   ((equalp "follicular" class)
    ;;    (setf lb 5))
    ;;   ((equalp "hodgkins" class)
    ;;    (setf lb 7)))
    (and c
	 (>= c lb))))

(defun entropy-pass? (subid class)
  (when (stringp subid)
    (setf subid (parse-integer subid)))
  (let* ((ent (get-sigsub-entropy subid class)))
    (< ent *max-cg-class-ent*)))

(defun cg-pass? (term class)
  (declare (ignorable class))
  (let* ((subid (replace-re term "^CG_" "")))
    (and (count-pass? term *h-cg-count*)
	 (not (sigsub-trivialp subid))
	 ;; (entropy-pass? subid class)
	 )))

(defun count-filter (l h)
  (remove-if-not #'(lambda (a) (count-pass? a h)) l))

(defun cg-filter (l class)
  (remove-if-not #'(lambda (a) (cg-pass? a class)) l))

(defun ablation (h r)
  (let* ((oblh (make-hash-table :test #'equalp))
	 obl)
    (dolist (k (hash-keys h))
      (when (count-pass? k h)
	(push k obl)))
    (setf obl (rnd-permu obl))
    (dolist (k (subseq obl (floor (* r (length obl)))))
      (setf (gethash k oblh) 1))
    oblh))

(defun ablation-pass? (term h)
  (gethash term h))




(defun weka-arff-gen-db (fv-l
			 class
			 &key (out-fn nil))
  "Generates the WEKA style arff file, use switches such as ic? to control 
whether to output certain feature groups. But given that weka can arbitrarilly
slice feature vector, one might just want to write all features."
  (let* ((ics (mapcar #'weka-str (h-get-feature-list "IC_")))
	 (icps (mapcar #'weka-str (h-get-feature-list "ICP_")))
	 x y)
    
    (dolist (fv fv-l)
      (let* ((label (car fv))
	     (ft (cdr fv))			 ;; list combo form feature
	     (unigram "")
	     (bigram "")
	     (trigram "")
	     (metamap-con "")
	     (fmetamap-con "")
	     (cg "")
	     (NP "")
	     (karyotypes "")
	     fn-l term)					; feature node list

	(push label y)

	(dolist (ic ics)
	  (push "" fn-l)
	  (push (intern ic) fn-l))

	;; put dummy stub for item-content pair feature
	(dolist (icp icps)
	  (if (match-re "_quantity_" icp)
	      ;; default missing values
	      (push "?" fn-l)
	    (push "" fn-l))
	  (push (intern icp) fn-l))
	
	(dolist (ft-u ft)
	  (when (string= (ftu-name ft-u) "inst_name")
	    (push (ftu-val ft-u) fn-l)
	    (push 'inst_name fn-l))
	  
	  (when (string= (ftu-name ft-u) "unigram")
	    (setf term (weka-str (concatenate 'string "UG_" (ftu-val ft-u))))
	    (when (count-pass? term *h-unigram-count* )
	      (setf unigram (concatenate 'string unigram " " term))))
	  
	  (when (string= (ftu-name ft-u) "bigram")
	    (setf term (weka-str (concatenate 'string "BG_" (ftu-val ft-u))))
	    (when (count-pass? term *h-bigram-count* )
	      (setq bigram (concatenate 'string bigram " " term))))
	  
	  (when (string= (ftu-name ft-u) "trigram")
	    (setf term (weka-str (concatenate 'string "TG_" (ftu-val ft-u))))
	    (when (count-pass? term *h-trigram-count* )
	      (setq trigram (concatenate 'string trigram " " term))))
	  
	  (when (string= (ftu-name ft-u) "NP")
	    (setf term (concatenate 'string "NP_" (ftu-val ft-u)))
	    (setq NP (concatenate 'string NP " " (weka-str term))))

	  (when (string= (ftu-name ft-u) "metamap")
	    (setf term (concatenate 'string "MM_" (ftu-val ft-u)))
	    (setf term (weka-str term))
	    (when (count-pass? term *h-metamap-count* )
	      (setq metamap-con (concatenate 'string metamap-con " " term))))

	  (when (string= (ftu-name ft-u) "fmetamap")
	    (setf term (concatenate 'string "FMM_" (ftu-val ft-u)))
	    (setf term (weka-str term))
	    (when (count-pass? term *h-fmetamap-count* )
	      (setq fmetamap-con (concatenate 'string fmetamap-con " " term))))
	  
	  (when (string= (ftu-name ft-u) "KARYOTYPE")
	    (setf term (concatenate 'string "KARYOTYPE_" (ftu-val ft-u)))
	    (setf karyotypes (concatenate 'string karyotypes " " (weka-str term))))

	  (when (match-re "^CG_" (ftu-name ft-u))
	    (setf term (weka-str (ftu-name ft-u)))
	    (when (cg-pass? term class)
	      (setf cg (concatenate 'string cg " " term))))
	  
	  
	  (when (match-re "^ICP_" (ftu-name ft-u))
	    (setf (getf fn-l (intern (ftu-name ft-u))) 
		  (or (if (stringp (ftu-val ft-u))
			  (replace-re (ftu-val ft-u) "\\s+" " ")
			(ftu-val ft-u))
		      (getf fn-l (intern (ftu-name ft-u))))))
	  
	  (when (match-re "^IC_" (ftu-name ft-u))
	    (let* ((ic (ftu-name ft-u)) 
		   (positivity (gethash (ftu-val ft-u) *hr-chem-tags*)))
	      (assert (not (null (getf fn-l (intern ic)))) 
		      ()
		      "~a not found in fn-l" ic)
	      (assert (not (null positivity))
		      ()
		      "null positivity in ~a" ic)
	      (setf (getf fn-l (intern ic)) positivity))))
	
	(when (getf *ml-features* 'unigram) 
	  (push unigram fn-l)
	  (push 'unigram fn-l))
	(when (getf *ml-features* 'bigram)
	  (push bigram fn-l)
	  (push 'bigram fn-l))
	(when (getf *ml-features* 'trigram)
	  (push trigram fn-l)
	  (push 'trigram fn-l))
	(when (getf *ml-features* 'np)
	  (push NP fn-l)
	  (push 'NP fn-l))
	(when (getf *ml-features* 'metamap-con)
	  (push metamap-con fn-l)
	  (push 'metamap-con fn-l))
	(when (getf *ml-features* 'fmetamap-con)
	  (push fmetamap-con fn-l)
	  (push 'fmetamap-con fn-l))
	(when (getf *ml-features* 'karyotype)
	  (push karyotypes fn-l)
	  (push 'karyotype fn-l))
	(push cg fn-l)
	(push 'cg fn-l)

	(push fn-l x)))
    (format t "~&x: ~a~%y: ~a~%" (length x) (length y))
    (when out-fn
      (output-arff-nom-as-str y x :out-fn out-fn :ics ics :icps icps))))

(defun output-mrns (fn)
  (let* ((mrns (mrns "MGH MRN instance set"))
	 (mrns (stable-sort mrns #'string-lessp))
	 (f (open-new fn)))
    (dolist (mrn mrns)
      (format f "~&~a~%" mrn))
    (close f)))

(defun load-mrns (fn)
  (let* ((f (open fn :direction :input))
	 (h (make-hash-table :test #'equalp))
	 (cnt 0)
	 ln)
    (loop (unless (setf ln (read-line f nil nil)) (return))
	  (setf (gethash ln h) (incf cnt)))
    h))

(defun ft-layout (l)
  (let* ((h (make-hash-table :test #'equalp)))
    (dolist (e l)
      (cnt-gethash e h))
    h))

(defun sp-mat-gen-db (fv-l fnmat hmrn hft class)
  "Generates the sparse matrix file for matlab."
  (let* ((hspm (make-hash-table :test #'equalp))
	 (ri 0))
    
    (dolist (fv fv-l)
      (let* ((label (car fv))
	     (ft (cdr fv))
	     ft-n ft-v term mrni ci)		   ;; feature node list
	(incf ri)
	(setf ci (gethash "class" hft))
	(matrix-set hspm ri ci label)

	(dolist (ft-u ft)
	  (setf ft-n (ftu-name ft-u))
	  (setf ft-v (ftu-val ft-u))
	  (when (string= ft-n "inst_name")
	    (setf ci (gethash "inst_name" hft))
	    (setf mrni (gethash ft-v hmrn))
	    (matrix-set hspm ri ci mrni))
	  
	  (when (string= ft-n "unigram")
	    (setf term (weka-str (concatenate 'string "UG_" ft-v)))
	    (when (count-pass? term *h-unigram-count*)
	      (setf ci (gethash term hft))
	      (matrix-add hspm ri ci)))
	  
	  (when (string= (ftu-name ft-u) "bigram")
	    (setf term (weka-str (concatenate 'string "BG_" ft-v)))
	    (when (count-pass? term *h-bigram-count* )
	      (setf ci (gethash term hft))
	      (matrix-add hspm ri ci)))
	  
	  (when (string= (ftu-name ft-u) "trigram")
	    (setf term (weka-str (concatenate 'string "TG_" ft-v)))
	    (when (count-pass? term *h-trigram-count* )
	      (setf ci (gethash term hft))
	      (matrix-add hspm ri ci)))

	  (when (string= (ftu-name ft-u) "metamap")
	    (setf term (weka-str (concatenate 'string "MM_" ft-v)))
	    (when (count-pass? term *h-metamap-count*)
	      (setf ci (gethash term hft))
	      (matrix-add hspm ri ci)))

	  (when (string= (ftu-name ft-u) "fmetamap")
	    (setf term (weka-str (concatenate 'string "FMM_" ft-v)))
	    (when (count-pass? term *h-fmetamap-count*)
	      (setf ci (gethash term hft))
	      (matrix-add hspm ri ci)))
	  

	  (when (match-re "^CG_" (ftu-name ft-u))
	    (setf term (weka-str (ftu-name ft-u)))
	    (when (cg-pass? term class)
	      (setf ci (gethash term hft))
	      (matrix-add hspm ri ci))))))
    
    (write-matrix fnmat hspm)))

(defun weka-arff-gen (fv-l &key (out-fn nil) 
			   (sigsub-type "sig_subgraph|hier"))
  "Generates the WEKA style arff file, use switches such as ic? to control 
whether to output certain feature groups. But given that weka can arbitrarilly
slice feature vector, one might just want to write all features."
  (let* (ics x y ic-keys cgs icps)
    ;; (break "begin constructing weka problem")
    (when (getf *ml-features* 'ic)
      (maphash #'(lambda (key val) (declare (ignore val)) (push key ic-keys)) 
	       *h-chem-tags*)
      (maphash #'(lambda (key val) (declare (ignore val)) 
		   (push (concatenate 'string "IC_" key) ics)) 
	       *h-immuchem*))

    (when (getf *ml-features* 'cg)
      (mapcar #'(lambda (sigsub-id) 
		  (dotimes (i (sigsub-size sigsub-id))
		    (push (format nil "CG_~a_slot~a_cui_unigram" sigsub-id i) 
			  cgs)
		    (push (format nil "CG_~a_slot~a_lex_unigram" sigsub-id i) 
			  cgs)
		    (push (format nil "CG_~a_slot~a_link_lex_ngram" sigsub-id i)
			  cgs)
		    (push (format nil "CG_~a_slot~a_link_cui_ngram" sigsub-id i)
			  cgs))) 
	      (get-sigsub-ids sigsub-type)))
    
    (when (getf *ml-features* 'icp)
      (maphash #'(lambda (key val)
		   (declare (ignore val))
		   (push (format nil "ICP_~a" 
				 (replace-re
				  (replace-re 
				   (replace-re 
				    (replace-re key "[\\(\\[\\{]" "_LB_") 
				    "[\\)\\]\\}]" "_RB_")
				   "%" "_pct_")
				  "," ""))
			 icps))
	       *h-item-content-pair*))
    
    (dolist (fv fv-l)
      (let* ((label (car fv))
	     (ft (cdr fv))			 ;; list combo form feature
	     (unigram "")
	     (bigram "")
	     (trigram "")
	     (metamap-con "")
	     (fmetamap-con "")
	     (NP "")
	     fn-l term)					; feature node list

	(push label y)

	(dolist (ic ics)
	  (push "" fn-l)
	  (push (intern ic) fn-l))

	(dolist (cg cgs)
	  (push "" fn-l)
	  (push (intern cg) fn-l))
	
	;; put dummy stub for item-content pair feature
	(dolist (icp icps)
	  (if (match-re "_quantity_" icp)
	      (push -1 fn-l)
	    (push "" fn-l))
	  (push (intern icp) fn-l))
	
	
	(dolist (ft-u ft)
	  (when (string= (ftu-name ft-u) "inst_name")
	    (push (ftu-val ft-u) fn-l)
	    (push 'inst_name fn-l))
	  
	  (when (and (getf *ml-features* 'unigram)
		     (string= (ftu-name ft-u) "unigram"))
	    (let* ((unigram-id (ftu-val ft-u))) 
	      (unless (<= (gethash unigram-id *h-unigram-count*) 1)
		(setf term 
		      (format nil "UG_~a" (gethash unigram-id *hr-unigrams*)))
		(setq unigram 
		      (concatenate 'string unigram " " (weka-str term))))))
	  
	  (when (and (getf *ml-features* 'bigram)
		     (string= (ftu-name ft-u) "bigram"))
	    (let* ((bigram-id (ftu-val ft-u)))
	      (unless (<= (gethash bigram-id *h-bigram-count*) 1)
		(setf term 
		      (format nil "BG_~a" (gethash bigram-id *hr-bigrams*)))
		(setq bigram 
		      (concatenate 'string bigram " " (weka-str term))))))
	  
	  (when (and (getf *ml-features* 'trigram)
		     (string= (ftu-name ft-u) "trigram"))
	    (let* ((trigram-id (ftu-val ft-u)))
	      (unless (<= (gethash trigram-id *h-trigram-count*) 1)
		(setf term 
		      (format nil "TG_~a" (gethash trigram-id *hr-trigrams*)))
		(setq trigram 
		      (concatenate 'string trigram " " (weka-str term))))))
	  
	  (when (and (getf *ml-features* 'np)
		     (string= (ftu-name ft-u) "NP"))
	    (let* ((NP-id (ftu-val ft-u)))
	      (unless (<= (gethash NP-id *h-NP-count*) 1)
		(setf term 
		      (format nil "NP_~a"(gethash NP-id *hr-NPs*)))
		(setq NP (concatenate 'string NP " " (weka-str term))))))

	  (when (and (getf *ml-features* 'metamap-con)
	  			 (string= (ftu-name ft-u) "metamap"))
	  	(let* ((metamap-id (ftu-val ft-u)))
	  	  (unless (<= (gethash metamap-id *h-metamap-count*) 1)
	  		(setf term (format nil "MM_~a"(gethash metamap-id *hr-metamaps*)))
	  		(setq metamap (concatenate 'string metamap " " (weka-str term))))))

	  (when (and (getf *ml-features* 'cg)
		     (match-re "CG_" (ftu-name ft-u)))
	    ;; i iterates through all the slots in an concept graph
	    (let* ((l-cg-slot-ft (ftu-val ft-u))
		   (cg-name (replace-re (ftu-name ft-u) "-" "_"))
		   (i 0)
		   cui-unigrams lex-unigrams link-lex-ngrams link-cui-ngrams
		   key)
	      (dolist (cg-slot-ft l-cg-slot-ft) 
		(setf cui-unigrams (efsf-cui-unigrams cg-slot-ft)
		      lex-unigrams (efsf-lex-unigrams cg-slot-ft)
		      link-lex-ngrams (efsf-link-lex-ngrams cg-slot-ft)
		      link-cui-ngrams (efsf-link-cui-ngrams cg-slot-ft))

		;; i don't think it hurts if core has duplicates
		(setf key (format nil "~a_slot~a_cui_unigram" cg-name i))
		;; TODO: handle multiple mappings, yluo
		(setf (getf fn-l (intern key)) 
		      (format nil "~{~a~^ ~}" (prepend-list key cui-unigrams)))
		
		(setf key (format nil "~a_slot~a_lex_unigram" cg-name i))
		(setf (getf fn-l (intern key)) 
		      (format nil "~{~a~^ ~}" (prepend-list key lex-unigrams)))
		
		(setf key (format nil "~a_slot~a_link_lex_ngram" cg-name i))
		(setf (getf fn-l (intern key)) 
		      (format nil "~{~a~^ ~}" (prepend-list key link-lex-ngrams)))
		
		(setf key (format nil "~a_slot~a_link_cui_ngram" cg-name i))
		(setf (getf fn-l (intern key)) 
		      (format nil "~{~a~^ ~}" (prepend-list key link-cui-ngrams)))
		(incf i))))
	  
	  (when (and (getf *ml-features* 'icp)
		     (match-re "ICP_" (ftu-name ft-u)))
	    (setf (getf fn-l (intern (ftu-name ft-u))) 
		  (or (if (stringp (ftu-val ft-u))
			  (replace-re (ftu-val ft-u) "[\\n\\s]+" " ")
			(ftu-val ft-u))
		      (getf fn-l (intern (ftu-name ft-u))))))

	  (when (and (getf *ml-features* 'ic)
		     (match-re "^IC_(.*)" (ftu-name ft-u)))
	    (let* ((ic (ftu-name ft-u)) 
		   (positivity (gethash (ftu-val ft-u) *hr-chem-tags*)))
	      (assert (not (null (getf fn-l (intern ic)))) 
		      ()
		      "~a not found in fn-l" ic)
	      (assert (not (null positivity))
		      ()
		      "null positivity in ~a" ic)
	      (setf (getf fn-l (intern ic)) positivity))))
	
	(when (getf *ml-features* 'unigram) 
	  (push unigram fn-l)
	  (push 'unigram fn-l))
	(when (getf *ml-features* 'bigram)
	  (push bigram fn-l)
	  (push 'bigram fn-l))
	(when (getf *ml-features* 'trigram)
	  (push trigram fn-l)
	  (push 'trigram fn-l))
	(when (getf *ml-features* 'np)
	  (push NP fn-l)
	  (push 'NP fn-l))
	(when (getf *ml-features* 'metamap-con)
	  (push metamap-con fn-l)
	  (push 'metamap-con fn-l))
	(when (getf *ml-features* 'fmetamap-con)
	  (push fmetamap-con fn-l)
	  (push 'fmetamap-con fn-l))
	(push fn-l x)))
    
    (when out-fn
      (output-arff-nom-as-str y x :out-fn out-fn :ics ics :cgs cgs :icps icps))))

(defun output-cost-matrix (lin fn)
  "Generate cost matrix based on the number of instances in classes.
Input
======
lin - list of (instance . number)
matrix row as first dimension is true label, column as second dimension is 
machine label "
  (with-open-file (f fn :direction :output :if-exists :supersede 
		     :if-does-not-exist :create)
		  (format f "% Rows~TColumns~%")
		  (format f "~a~T~a~%" (length lin) (length lin))
		  (format f "% Matrix elements~%")
		  (dolist (inst-num-t lin)
		    (dolist (inst-num-m lin)
		      (let* ((inst-t (car inst-num-t))
			     (num-t (cdr inst-num-t))
			     (inst-m (car inst-num-m))
			     (num-m (cdr inst-num-m))
			     (cost (sqrt (/ num-m num-t))))
			(cond
			 ((equalp inst-t inst-m)
			  (format f "0~T"))
			 (t
			  (format f "~,2f~T" cost)))))
		    (format f "~%"))))
