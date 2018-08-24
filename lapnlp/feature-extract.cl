;;; -*- Mode: Lisp; Package: late -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 03/13/2012 rewrote extract-left-link extract-right-link
extract-left-syn-bigram extract-right-syn-bigram, added
dijkstra, extract-shortest-syntactic-paths-from, and
extract-shortest-paths-words-from
yluo - 10/24/2010 changed ngram to not use stem
yluo - 04/04/2010 changed some equal to equalp
psz  - 08/24/2009 prev-anns, next-anns, r-link and l-link need to be
updated to work with the revised interval-tree storage of
annotations.  See implementation of prev-anns and next-anns in
annotations.cl, or comments there suggesting alternatives. 
yluo - 08/01/2009 add extract-immunochem and find-percentage 
yluo - 10/01/2008 creation 
load different component system
making a convention here: 
extract-... extracts the hash value of that annotation
define required data structures
|#

(defpackage :late
  (:use :common-lisp :excl :util :opennlp :cg :mnegex :norm)
  (:export 
   "*dates*"
   "*debug-feature-extract*"
   "*dict-home*"
   "*h-NP-count*"
   "*h-NPs*"
   "*h-bigram-count*"
   "*h-bigrams*"
   "*h-ctag*"
   "*h-ctag-count*"
   "*h-immuchem*"
   "*h-item-content-pair*"
   "*h-item-count*"
   "*h-label-count*"
   "*h-labels*"
   "*h-namer*"
   "*h-pos*"
   "*h-section-count*"
   "*h-sections*"
   "*h-sens*"
   "*h-trigram-count*"
   "*h-trigrams*"
   "*h-unigram-count*"
   "*h-unigrams*"
   "*h-word-count*"
   "*h-words*"
   "*hg-immuchem*"
   "*hr-NPs*"
   "*hr-bigrams*"
   "*hr-ctag*"
   "*hr-item-content-pair*"
   "*hr-labels*"
   "*hr-pos*"
   "*hr-trigrams*"
   "*hr-unigrams*"
   "*hr-words*"
   "*low-threshold*"
   "check-in-dict"
   "contains-punc"
   "dijkstra"
   "explode-link-cloud"
   "extract-NP"
   "extract-metamap"
   "extract-clinical-tag"
   "extract-concept-graph"
   "extract-factor-concept-graph"
   "extract-immun-histchem"
   "extract-item-content-pair"
   "extract-left-lexical-bigram"
   "extract-left-link"
   "extract-left-syn-bigram"
   "extract-link-lex-cloud"
   "extract-link-cui-cloud"
   "extract-link-envelope"
   "extract-ngram"
   "i2b2-extract-ngram"
   "extract-phi-tag"
   "extract-pos"
   "extract-right-lexical-bigram"
   "extract-right-link"
   "extract-right-syn-bigram"
   "extract-shortest-paths-words-from"
   "extract-shortest-syntactic-paths-from"
   "find-percentage"
   "first-cap"
   "immuchem-annotatedp"
   "is-number"
   "match-immun-histchem"
   "match-sigm"
   "populate-dict"
   "return-index"
   "rewrite"
   "self-assert-immun-histchem"
   "unify-ic"
   "transitive-closure-sigsubs"
   "get-karyotype-aberrations"
   "extract-karyotype"
   "get-affiliated-pnodes"
   "out-concept"
   ))

(in-package :late)

(defparameter *no-letter-pat* "^[^a-zA-Z]+$")

(defstruct (linkseg
	    (:type vector)
	    (:conc-name ls-))
  pnode elab)

(defparameter *debug-feature-extract* nil)
(defparameter *dict-home* nil
  "Location in the file system of the needed DICT files")

(defparameter *dates* '("JAN" "JANUARY" "FEB" "FEBRUARY" "MARCH" "MAR" "APRIL" "APR" "MAY" "JUN" "JUNE" "JULY" "JUL" "AUGUST" "AUG" "SEPTEMBER" "SEPT" "OCTOBER" "OCT" "NOVEMBER" "NOV" "DECEMBER" "DEC" "MONDAY" "TUESDAY" "WEDNESDAY" "THURSDAY" "FRIDAY" "SATURDAY" "SUNDAY"))


(defparameter *low-threshold* 2)

;; This is a specific example on how one can define their customized list of features and store them in the Common Data Model tables (not used in concept graph mining though)
;; key is the desginated name of ic feature
;; val is the regex used to match against the text
;; not found in mesh: annexin1,
(defparameter *h-immuchem* (make-hash-table :test #'equalp))
;; Somewhat more elegantly... --psz
(do ((names '("CD2" "CD3" "CD4" "CD5" "CD7" "CD8" "CD10" "CD15" "CD19" "CD20" 
	      "CD21" "CD22" "CD23" "CD25" "CD30" "CD56" "CD68" "CD79A" "CD103" 
	      "CD123" "CD138"
	      "CYCLIN_D1"
	      "KI67"
	      "BCL2" "BCL6" 
	      "t(11;14)" "t(14;18)"
	      "ANNEXIN1" "IRF4" "MUM1" "MYC" "ALK1" "ALK" "EBV_EBER" "HHV8" 
	      "EMA" "FDC" "PERFORIN" "TIA1" "HTLV" "TCL1" "HLA_DR" "PD1"
	      "IGA_LAMBDA" "IGA_KAPPA" "IGG_LAMBDA" "IGG_KAPPA" "GRANZYME_B"
	      "CYTOPLASMIC_IGM" "IG_LIGHT_CHAINS" "IG_HEAVY_CHAINS")
	    (cdr names))
     (i 1 (1+ i)))
    ((null names))
  (setf (gethash (car names) *h-immuchem*) i))

;; grouped immuchem features as expressed in text
;; changed equal to equalp
(defparameter *hg-immuchem* (make-hash-table :test #'equalp))

;; equal -> equalp
(defparameter *h-namer* (make-hash-table :test #'equalp))

;; hash table of stemmed words
;; equal -> equalp

(defparameter *h-item-content-pair* (make-hash-table :test #'equalp))
(defparameter *hr-item-content-pair* (make-hash-table :test #'equalp))
(defparameter *h-item-count* (make-hash-table :test #'equalp))


(defparameter *h-words* (make-hash-table :test #'equalp))
(setf (gethash 'none *h-words*) 1)
(setf (gethash 'low *h-words*) 2)

(defparameter *hr-words* (make-hash-table :test #'equalp))
(setf (gethash 1 *hr-words*) 'none)
(setf (gethash 2 *hr-words*) 'low)

;; equal -> equalp
(defparameter *h-ctag* (make-hash-table :test #'equalp))
(setf (gethash 'none *h-ctag*) 1)
(setf (gethash 'low *h-ctag*) 2)

(defparameter *hr-ctag* (make-hash-table :test #'equalp))
(setf (gethash 1 *hr-ctag*) 'none)
(setf (gethash 2 *hr-ctag*) 'low)

;; equal -> equalp
(defparameter *h-labels* (make-hash-table :test #'equalp))
(setf (gethash 'none *h-labels*) 1)
(setf (gethash 'low *h-labels*) 2)

(defparameter *hr-labels* (make-hash-table :test #'equalp))
(setf (gethash 1 *hr-labels*) 'none)
(setf (gethash 2 *hr-labels*) 'low)

;; corresponds to brill in pl script
;; equal -> equalp
(defparameter *h-pos* (make-hash-table :test #'equalp))
(setf (gethash 'none *h-pos*) 1)
(setf (gethash 'low *h-pos*) 2)

(defparameter *hr-pos* (make-hash-table :test #'equalp))
(setf (gethash 1 *hr-pos*) 'none)
(setf (gethash 2 *hr-pos*) 'low)

;; equal -> equalp
(defparameter *h-sections* (make-hash-table :test #'equalp))
(setf (gethash 'none *h-sections*) 1)
(setf (gethash 'low *h-sections*) 2)

;; equal -> equalp
(defparameter *h-sens* (make-hash-table :test #'equalp))
(setf (gethash 'patient *h-sens*) 2)
(setf (gethash 'doctor *h-sens*) 3)
(setf (gethash 'location *h-sens*) 4)
(setf (gethash 'hospital *h-sens*) 5)
(setf (gethash 'date *h-sens*) 6)
(setf (gethash 'id *h-sens*) 7)
(setf (gethash 'phone *h-sens*) 8)

;; currently vanilla n-gram
;; equal -> equalp
(defparameter *h-trigrams* (make-hash-table :test #'equalp))
(setf (gethash 'none *h-trigrams*) 1)
(setf (gethash 'low *h-trigrams*) 2)

(defparameter *hr-trigrams* (make-hash-table :test #'equalp))
(setf (gethash 1 *hr-trigrams*) 'none)
(setf (gethash 2 *hr-trigrams*) 'low)

;; equal -> equalp
(defparameter *h-bigrams* (make-hash-table :test #'equalp))
(setf (gethash 'none *h-bigrams*) 1)
(setf (gethash 'low *h-bigrams*) 2)

(defparameter *hr-bigrams* (make-hash-table :test #'equalp))
(setf (gethash 1 *hr-bigrams*) 'none)
(setf (gethash 2 *hr-bigrams*) 'low)

;; equal-> equalp
(defparameter *h-unigrams* (make-hash-table :test #'equalp))
(setf (gethash 'none *h-unigrams*) 1)
(setf (gethash 'low *h-unigrams*) 2)


(defparameter *hr-unigrams* (make-hash-table :test #'equalp))
(setf (gethash 1 *hr-unigrams*) 'none)
(setf (gethash 2 *hr-unigrams*) 'low)


;; equal -> equalp
(defparameter *h-NPs* (make-hash-table :test #'equalp))
(setf (gethash 'none *h-NPs*) 1)
(setf (gethash 'low *h-NPs*) 2)

(defparameter *hr-NPs* (make-hash-table :test #'equalp))
(setf (gethash 1 *hr-NPs*) 'none)
(setf (gethash 2 *hr-NPs*) 'low)

;; count stemmed words
(defparameter *h-word-count* (make-hash-table :test #'equalp))
(setf (gethash 1 *h-word-count*) 3)

(defparameter *h-section-count* (make-hash-table :test #'equalp))
(setf (gethash 1 *h-section-count*) 3)

(defparameter *h-ctag-count* (make-hash-table :test #'equalp))
(setf (gethash 1 *h-ctag-count*) 3)

(defparameter *h-label-count* (make-hash-table :test #'equalp))
(setf (gethash 1 *h-label-count*) 3)

(defparameter *h-trigram-count* (make-hash-table :test #'equalp))

(defparameter *h-bigram-count* (make-hash-table :test #'equalp))

(defparameter *h-unigram-count* (make-hash-table :test #'equalp))

(defparameter *h-NP-count* (make-hash-table :test #'equalp))
;; we need 6 pieces of information here
;;  stem, pos-tag, role, mesh, original, link

;; (defparameter *names* nil)
;; (defparameter *locations* nil)
;; (defparameter *hospitals* nil)
;; (defparameter *stoplist* nil)

(defmethod extract-NP ((sa sentence-annotation))
  (unless (maskedp sa)
    (let* (NPs NP Pa tas tacs
	       (pas (annotations 
		     sa 
		     :type (gtype 'gphrase-type) 
		     :filter 
		     #'(lambda (a)
			 (not (or (annotations 
				   a :type 'mask-annotation :relation ':ar)
				  (annotations
				   a :type 'mask-annotation :relation ':in)
				  (annotations 
				   a :type 'mask-annotation :relation ':o)
				  (annotations
				   a :type 'mask-annotation :relation ':oi)))))))

      (dotimes (i (length pas))			;make sure of that
	(setf Pa (elt pas i))
	(when (and *debug-feature-extract*
		   (match-re "burkitt" (content Pa) :case-fold t))
	  (format t "~&Cheat words in ~a in ~a in ~a~%" Pa sa (document Pa)))
	(setf tas (annotations-spec Pa :type (gtype 'gtoken-type)))
	(setf tacs (mapcar 
		    #'(lambda (a) 
			(or ;; (stem a) 
			 (car (norm (content a))) (content a)))
		    tas))
	;; disable stoplist
	;; (setf tacs (remove-if #'in-stoplist? tacs))
	
	(when (and (string= "NP" (format nil "~a" (data Pa)))
		   tacs
		   (every #'(lambda (x) (match-re "[A-Za-z]" x)) tacs))
	  (setq NP (format nil "~{~a~^_~}" tacs))
	  (push NP NPs)))
      ;; (format t "~&NPs to return: ~a~%" NPs)

      (nreverse NPs))))

(defmethod extract-metamap ((sa sentence-annotation))
  ;; (unless (maskedp sa)
  (let* ((metamap-cons (annotations sa 
				    :type 'metamap-con
				    :filter #'not-masked))
	 constr constrs cui-name)
    (dolist (metamap-con metamap-cons)		
      (setf cui-name (unsq-read (data metamap-con)))
      (setf constr (second cui-name))
      (setf constr (replace-re constr "\\s+" "_"))
      (setf constr (replace-re constr "[^\\w-_]" ""))
      (when (annotations-on metamap-con :type 'metamap-neg)
	(setf constr (concatenate 'string "neg_" constr)))
      (push constr constrs))

    (nreverse constrs)))


;; this list is from Ira's BMC paper
(defparameter *diseases* '("Acquired Abnormality" "Anatomical Abnormality" "Bacterium" "Congenital Abnormality" "Cell or Molecular Dysfunction" "Disease or Syndrome" "Fungus" "Hazardous or Poisonous Substance" "Injury or Poisoning" "Mental or Behavioral Dysfunction" "Neoplastic Process" "Pathologic Function" "Virus"))

(defparameter *symptoms* '("Clinical Attribute" "Diagnostic Procedure" "Finding"
			   "Laboratory Procedure" "Laboratory or Test Result" "Sign or Symptom"))

(defun cui-pass? (cui)
  (let* ((tuis (cui->tui cui))
	 (stns (mapcar #'tui->stn tuis))
	 (stys (mapcar #'stn->sty-rl stns)))
    (or (intersection stys *diseases* :test #'equalp)
	(intersection stys *symptoms* :test #'equalp)
	(some #'(lambda (a) (search (sty-rl->stn "Chemical") a)) stns)
	(some #'(lambda (a) (search (sty-rl->stn "Anatomical Structure") a)) stns))))

(defmethod extract-filtered-metamap ((sa sentence-annotation))
  ;; (unless (maskedp sa)
  (let* ((metamap-cons (annotations sa 
				    :type 'metamap-con
				    :filter #'not-masked))
	 constr constrs cui-name cui)
    (dolist (metamap-con metamap-cons)
      (setf cui-name (unsq-read (data metamap-con)))
      (setf cui (first cui-name))
      (when (cui-pass? cui)
	(setf constr (second cui-name))
	(setf constr (replace-re constr "\\s+" "_"))
	(setf constr (replace-re constr "[^\\w-_]" ""))
	(when (annotations-on metamap-con :type 'metamap-neg)
	  (setf constr (concatenate 'string "neg_" constr)))
	(push constr constrs)))

    (nreverse constrs)))

(defmethod extract-NP-id ((sa sentence-annotation))
  (let* (NPs NP NP-id Pa tas tacs
	     (pas (annotations 
		   sa 
		   :type (gtype 'gphrase-type) 
		   :filter #'not-masked)))
    (dotimes (i (length pas))			;make sure of that
      (setf Pa (elt pas i))
      (when (and *debug-feature-extract*
		 (match-re "burkitt" (content Pa) :case-fold t))
	(format t "~&Cheat words in ~a in ~a in ~a~%" Pa sa (document Pa)))
      (setf tas (annotations-spec Pa :type (gtype 'gtoken-type)))
      (setf tacs (mapcar 
		  #'(lambda (a) 
		      (or  (car (norm (content a))) (content a)))
		  tas))
      (setf tacs
	    (remove-if #'in-stoplist? tacs))
      
      (when (and (string= "NP" (format nil "~a" (data Pa)))
		 tacs
		 (every #'(lambda (x) (match-re "[A-Za-z]" x)) tacs))
	(setq NP (format nil "~{~a~^_~}" tacs))
	(setq NP-id (return-index *h-NPs* *hr-NPs* (string-downcase NP)))
	(push NP-id NPs)
	(incf (gethash NP-id *h-NP-count* 0))))
    (nreverse NPs)))

(defmethod extract-factor-concept-graph ((sa sentence-annotation))
  "Assume that lg sigsub mapping is a list of conses whose car is lg node id
   and whose cdr is sigsub node id.
   represents the concept graph as a list of lists"
  (format t "~&Extracting factor concept graph for sentence ~%~a~%" sa)
  (unless (annotations-spec sa :type (gtype 'gparse-node-type))
    (return-from extract-factor-concept-graph nil))
  (let* ((lgid (get-linkage-graph sa "linkage_factor_graph"))
	 concept-graph)
    (when lgid
      (format t "~&linkage graph id ~a~%" lgid)
      (maphash 
       #'(lambda (sub-map-ids lg-sigsub-mapping)
	   (format t "~&sub map id ~a~%" sub-map-ids)
	   (let* (sigsub-fv
		  sigsub-id
		  )
	     (multiple-value-bind (match? whole sigsub-ids map-ids)
		 (match-re "(\\d+)-(\\s+)" sub-map-ids)
	       (declare (ignorable whole match? map-ids))
	       (setf sigsub-id (parse-integer sigsub-ids)
		     ))
	     (dotimes (sub-nid (sigsub-size sigsub-id))
	       (let* (lgnids core precm succm)
		 ;; only need link_node
		 (mapcar #'(lambda (p) 
			     (if (and (= (cdr p) sub-nid)
				      )
				 (pushnew (car p) lgnids :test #'=)))
			 lg-sigsub-mapping)
		 (dolist (lgnid lgnids)
		   (when (equalp "link_node" (get-lg-node-type lgnid lgid))
		     (dolist (lgnid2 (get-lg-adj-nodes lgnid lgid))
		       (unless (member lgnid2 (mapcar #'car lg-sigsub-mapping) 
				       :test #'equalp)
			 (let* ((pn (find-annotation 
				     (lgnid->pnid lgnid2 lgid) 
				     (document sa)))
				(cui-or-head (pnode-longest-umlss pn :type 'cui-annotation)))
			   ;; core
			   (cond 
			    ((typep cui-or-head 'cui-annotation)
			     (pushnew (format nil "~a" (data cui-or-head)) core 
				      :test #'equalp))
			    ((typep cui-or-head 'token-annotation)
			     (pushnew (content cui-or-head) core 
				      :test #'equalp))
			    (t
			     (error "Neither cui nor head!")))
			   
			   ;; precm
			   (mapcar 
			    #'(lambda (x) 
				(let* ((cui (car (annotations-on-spec 
						  x :type 'cui-annotation))))
				  (pushnew (if cui
					       (format nil "~a" (data cui))
					     (content x))
					   precm :test #'equalp)))
			    (token-modifiers pn cui-or-head "prec"))

			   ;; succm
			   (mapcar 
			    #'(lambda (x) 
				(let* ((cui (car (annotations-on-spec 
						  x :type 'cui-annotation))))
				  (pushnew (if cui
					       (format nil "~a" (data cui))
					     (content x))
					   succm :test #'equalp)))
			    (token-modifiers pn cui-or-head "succ")))))))
		 
		 (push (list core precm succm) sigsub-fv)))
	     (push (cons sigsub-id (nreverse sigsub-fv)) concept-graph)))
       (get-sigsubs-of-lg lgid)))
    concept-graph))

(defun no-cui-cover? (a)
  (not (annotations-spanning-spec a :type 'cui-annotation)))


;; features associated with a significant subgraph's slot
;; colloquially, ef (event frame) and cg (concept graph) are used exchangeably
(defstruct (ef-slot-feature
	    (:type vector)
	    (:conc-name efsf-))
  cui-unigrams lex-unigrams link-lex-ngrams link-cui-ngrams)

(defstruct (ef-feature
	    (:type vector)
	    (:conc-name eff-))
  sigsub-id map-id sigsub-fv)

(defun ef-feature-gen (sub-map-id sub-lg-map sen lg-type
				  &key (closure? nil))
  (if closure?
      (ef-feature-gen-closure sub-map-id sub-lg-map sen lg-type)
    (ef-feature-gen-strict sub-map-id sub-lg-map sen lg-type)))

(defparameter *meaningful-dets* '("no" "some" "any" "much" "many" "little" "few" "more" "most" "less" "fewer" "least" "fewest" "all" "both" "enough" "sufficient" "neither" "either" "each" "every" "same" "other" "certain" "different" "only")"This list is obtained from the following link:
http://en.wikipedia.org/wiki/English_determiners")

(defun get-affiliated-pnodes (pnode)
  "For a pnode, returns the pnodes having det and appos connection with it, plus itself include nn here as OpenNLP phrase chunks sometimes treat NP as part of ADJP"
  (let* ((gov-deps (gov-deps pnode))
	 (doc (document pnode))
	 ans rel apnode)
    (dolist (gdep gov-deps)
      (setf rel (sp::d-rel gdep))
      (setf apnode (find-annotation (sp::d-depid gdep) doc))
      (cond
       ((member rel '("nn") :test #'equalp) ; "appos" 
	(push apnode ans))
       ((and (equalp rel "det")
	     (member (content apnode) *meaningful-dets* :test #'equalp))
	(push apnode ans))))
    ans))

(defun ef-feature-gen-closure (sub-map-id sub-lg-map sen lg-type
					  &aux sigsub-fv sigsub-id concept-graph doc lg-id)
  "Note: the lex-unigrams are returned in reverse appearing order in the 
document."
  (setf doc (document sen))
  (setf lg-id (get-linkage-graph sen lg-type))
  (when *debug-feature-extract*
    (format t "~&sub map id ~a~%" sub-map-id))
  (multiple-value-bind (match? whole sigsub-ids map-ids)
      ;; first is the string form of subgraph id, second the string of map id
      (match-re "(\\d+)-(\\d+)" sub-map-id)
    (declare (ignorable whole match? map-ids))
    (setf sigsub-id (parse-integer sigsub-ids)))       

  (dotimes (sub-nid (sigsub-size sigsub-id))
    (let* ((lg-nid (gethash sub-nid sub-lg-map))
	   (lgrow (and lg-nid (lgnid->row lg-nid lg-id)))
	   (pnid (first lgrow))
	   (mnlab (second lgrow))
	   (lgn-type (third lgrow))
	   (pnode (and lg-nid pnid (find-annotation pnid doc)))
	   ;; (lcui (annotations-spec pnode :type 'cui-annotation))
	   ;; only use the longest cui as cui
	   (lcui (and pnode (pnode-longest-umlss pnode :type 'cui-annotation)))
	   
	   
	   ;; lcui
	   (link-lex-ngrams (and (typep pnode (gtype 'gparse-node-type))
				 (extract-link-lex-cloud pnode lg-type)))
	   (link-cui-ngrams (and (typep pnode (gtype 'gparse-node-type))
				 (extract-link-cui-cloud pnode lg-type))) 
	   cui-unigrams lex-unigrams tok-str cui-start cui-end ltok apnodes
	   apnodes-before apnodes-after altok-before altok-after)
      (when (and (= 1 (hash-table-count sub-lg-map))
		 (not (search "sternberg" (content pnode) :test #'equalp)))
	(return-from ef-feature-gen-closure nil)) 
      
      
      (cond
       ((equalp "node-modifier" lgn-type)
	(setf lcui nil)
	(cond 
	 ((and (equalp "positivity" mnlab)
	       (match-re "\\w\\+/-$" (content pnode)))
	  (setf mnlab "+/-"))
	 ((and (equalp "negativity" mnlab)
	       (match-re "\\w-/\\+$" (content pnode)))
	  (setf mnlab "-/+")))
	(setf ltok (list mnlab)))
       (lcui
	(setf cui-start (reduce #'min (mapcar #'start lcui)))
	(setf cui-end (reduce #'max (mapcar #'end lcui)))
	(setf ltok (and pnode
			(annotations-spec pnode 
					  :type (gtype 'gtoken-type)
					  :filter #'(lambda (a) ;#'no-cui-cover?
						      (not (allenr-ar cui-start
								      cui-end
								      (start a)
								      (end a))))))))
       (t
	(setf ltok (and pnode
			(annotations-spec pnode 
					  :type (gtype 'gtoken-type))))))
      
      (when (typep pnode (gtype 'gparse-node-type))
	;; add affiliated pnodes' content, in order
	(setf apnodes (get-affiliated-pnodes pnode))
	(setf apnodes (sort apnodes #'annotation-lessp))
	(setf apnodes-before (remove-if #'(lambda (a) 
					    (annotation-lessp pnode a)) 
					apnodes))
	(setf apnodes-after (remove-if #'(lambda (a) 
					   (annotation-lessp a pnode))
				       apnodes))
	(setf altok-before (mapcan #'tokens apnodes-before))
	(setf altok-after (mapcan #'tokens apnodes-after))
	
	
	(setf ltok (concatenate 'list altok-before ltok altok-after)))
      
      ;; this is important!
      (when (and pnode (maskedp pnode))
	(setf lcui nil
	      ltok nil))
      
      (when *debug-feature-extract*
	;; (format t "ls-cloud:~%~a~%lc-cloud:~%~a~%" ls-cloud lc-cloud)
	(format t "link-lex-ngrams:~%~a~%link-cui-ngrams:~%~a~%"
		link-lex-ngrams link-cui-ngrams))
      ;; cuis
      (dolist (cui lcui)
	(pushnew (data cui) cui-unigrams :test #'equalp))
      
      (setf cui-unigrams (mapcar #'cui-pname cui-unigrams))
      (setf cui-unigrams (remove-if #'null cui-unigrams))
      ;; token unigrams not covered by cuis
      (dolist (tok ltok)
	(setf tok-str (tok-ft-str tok))
	(when (> (length tok-str) 0)
	  (pushnew tok-str lex-unigrams :test #'equalp)))
      
      (push (make-ef-slot-feature :cui-unigrams cui-unigrams
				  :lex-unigrams lex-unigrams
				  :link-lex-ngrams link-lex-ngrams
				  :link-cui-ngrams link-cui-ngrams) 
	    sigsub-fv)))
  (push (make-ef-feature :sigsub-id sigsub-id 
			 :map-id sub-map-id
			 :sigsub-fv (nreverse sigsub-fv))
	concept-graph)
  
  (when *debug-feature-extract* 
    (format t "ef:~%~a~%" concept-graph))
  (nreverse concept-graph))

(defun ef-feature-gen-strict (sub-map-id sub-lg-map sen lg-type
					 &aux sigsub-fv sigsub-id concept-graph doc lg-id)
  (setf doc (document sen))
  (setf lg-id (get-linkage-graph sen lg-type))
  (when *debug-feature-extract*
    (format t "~&sub map id ~a~%" sub-map-id))
  (multiple-value-bind (match? whole sigsub-ids map-ids)
      ;; first is the string form of subgraph id, second the string of map id
      (match-re "(\\d+)-(\\d+)" sub-map-id)
    (declare (ignorable whole match? map-ids))
    (setf sigsub-id (parse-integer sigsub-ids)))       

  (dotimes (sub-nid (sigsub-size sigsub-id))
    (let* ((lg-nid (gethash sub-nid sub-lg-map) )
	   (pnode (find-annotation (lgnid->pnid lg-nid lg-id) doc))
	   ;; (lcui (annotations-spec pnode :type 'cui-annotation))
	   ;; only use the longest cui as cui
	   (lcui (pnode-longest-umlss pnode :type 'cui-annotation))
;;;		   (ls-cloud (extract-link-lex-cloud pnode lg-type))
;;;		   (lc-cloud (extract-link-cui-cloud pnode lg-type))
;;;		   (link-lex-ngrams (explode-link-cloud ls-cloud))
;;;		   (link-cui-ngrams (explode-link-cloud lc-cloud))
	   ;; commented out part is for chained flavor		   
	   (link-lex-ngrams (extract-link-lex-cloud pnode lg-type))
	   (link-cui-ngrams (extract-link-cui-cloud pnode lg-type))		   
	   cui-unigrams lex-unigrams tok-str cui-start cui-end ltok)

      (cond
       (lcui
	(setf cui-start (reduce #'min (mapcar #'start lcui)))
	(setf cui-end (reduce #'max (mapcar #'end lcui)))
	(setf ltok (annotations-spec pnode 
				     :type (gtype 'gtoken-type)
				     :filter #'(lambda (a) ;#'no-cui-cover?
						 (not (allenr-ar cui-start
								 cui-end
								 (start a)
								 (end a)))))))
       (t
	(setf ltok (annotations-spec pnode 
				     :type (gtype 'gtoken-type)))))
      
      ;; this is important!
      (when (maskedp pnode)
	(setf lcui nil
	      ltok nil))
      
      (when *debug-feature-extract*
	;; (format t "ls-cloud:~%~a~%lc-cloud:~%~a~%" ls-cloud lc-cloud)
	(format t "link-lex-ngrams:~%~a~%link-cui-ngrams:~%~a~%"
		link-lex-ngrams link-cui-ngrams))
      ;; cuis
      (dolist (cui lcui)
	(pushnew (data cui) cui-unigrams :test #'equalp))
      
      (setf cui-unigrams (mapcar #'cui-pname cui-unigrams))
      (setf cui-unigrams (remove-if #'null cui-unigrams))
      ;; token unigrams not covered by cuis
      (dolist (tok ltok)
	(setf tok-str (tok-ft-str tok))
	(when (> (length tok-str) 0)
	  (pushnew tok-str lex-unigrams :test #'equalp)))
      
      (push (make-ef-slot-feature :cui-unigrams cui-unigrams
				  :lex-unigrams lex-unigrams
				  :link-lex-ngrams link-lex-ngrams
				  :link-cui-ngrams link-cui-ngrams) 
	    sigsub-fv)))
  (push (make-ef-feature :sigsub-id sigsub-id 
			 :map-id sub-map-id
			 :sigsub-fv (nreverse sigsub-fv))
	concept-graph)
  
  (when *debug-feature-extract* 
    (format t "ef:~%~a~%" concept-graph))
  (nreverse concept-graph))

(defun map-subsumedp (m1 m2 &aux m1-vals m2-vals)
  "Returns true is mapping m1 is subsumed in m2.
Note: proper subsumption"
  (setf m1-vals (hash-vals m1))
  (setf m2-vals (hash-vals m2))
  (and (< (length m1-vals) (length m2-vals))
       (subsetp m1-vals m2-vals :test #'equalp)))

(defun get-ss-map (sub1-id sub2-id map-id)
  "Returns the hash table corresponding to the map-id'th mapping between sub1-id
 and sub2-id"
  (let* ((h-mapping (make-hash-table :test #'equalp))
	 (rows (latesql "SELECT sub1_node, sub2_node FROM sigsub_sigsub 
                         WHERE sub1_id=~a AND sub2_id=~a AND mapping_i=~a"
			(sq sub1-id) (sq sub2-id) (sq map-id))))
    (dolist (row rows)
      (destructuring-bind (sub1n sub2n)
	  row
	(setf (gethash sub2n h-mapping) sub1n)))
    h-mapping))

(defun map-cascade (m1 m2)
  "Return m from m1's key to m2's val"
  (let* ((m (make-hash-table :test #'equalp)))
    (maphash #'(lambda (k v)
		 (setf (gethash k m) (gethash v m2)))
	     m1)
    m))

(defun add-rl-map (h-rt-lg rl-map1 rt-id1 &aux h-rl-map l-rl-map)
  "rl: root subgraph - linkage graph; sl: subgraph - linkage graph
Note:
Although we may also remove some maps, we used map as hash keys so that we don't
need to worry about holes in the map ids."
  ;; only need to worry about same root subgraph ids
  (unless (gethash rt-id1 h-rt-lg)
    (setf (gethash rt-id1 h-rt-lg) (make-hash-table :test #'equalp)))
  (setf h-rl-map (gethash rt-id1 h-rt-lg))
  
  (setf l-rl-map (hash-keys h-rl-map))
  (dolist (rl-map l-rl-map)
    (cond
     ;; rl-map1 and rl-map of same size
     ;; if exact same match, terminate
     ((equalp rl-map1 rl-map)
      (return-from add-rl-map nil))
     ;; if permutation, may add rl-map1
     ;; if not covering same set of lg nodes, may add rl-map1

     ;; rl-map1 size is smaller than rl-map size
     ;; if already has larger match, terminate
     ((map-subsumedp rl-map1 rl-map)
      (return-from add-rl-map nil))
     ;; if rl-map is not larger match, may add rl-map1
     
     ;; if rl-map1 size is larger than rl-map size, may add rl-map1
     ;; if rl-map1 is not larger match, can keep rl-map
     ;; if rl-map1 is larger match, remove rl-map
     ((map-subsumedp rl-map rl-map1)
      (remhash rl-map h-rl-map))))
  
  ;; add rl-map1
  (setf (gethash rl-map1 h-rl-map) 1)
  t)

(defun transitive-semi-closure-sigsubs (h-sigsub-lg)
  "Takes in the output of get-sigsubs-of-lg, which is a hash table whose key is a conjunction of subgraph ids and mapping ids and whose value is a hash table from subgraph node to linkage graph node.
semi - for sigsubs having multiple roots, we don't do transitive closure
Naming abbr:
sl: sig subgraph - linkage graph
rl: root sig subgraph - linkage graph
rs: root sig subgraph - sig subgraph
map-key is a combination of subgraph id and map id."
  (let* ((sl-map-keys (hash-keys h-sigsub-lg))
	 (sub-ids (mapcar #'(lambda (a) (car (split-re "-" a))) sl-map-keys))
	 (sub-ids (mapcar #'parse-integer sub-ids))
	 (h-rt-lg (make-hash-table :test #'equalp))
	 (h-ans (make-hash-table :test #'equalp))
	 sub-id rt-id sl-map)
    
    (dolist (sl-map-key sl-map-keys)
      (setf sub-id (car (split-re "-" sl-map-key)))
      (setf sub-id (parse-integer sub-id))
      
      (format t "~&sl-map-key: ~a~%" sl-map-key)
      ;; if good sigsub, no need to transitive closure, add it directly
      (cond
       ((good-sigsubp sub-id)
	(add-rl-map h-rt-lg (gethash sl-map-key h-sigsub-lg) sub-id))
       (t
	;; save a copy of original sub-lg-map
	(setf sl-map (gethash sl-map-key h-sigsub-lg))
	;; the non-good property says you only have one root here
	(setf rt-id (car (subisomorphic-roots sub-id)))
	
	;; if we already have root subgraph mapped, we can skip
	(unless (member rt-id sub-ids)
	  ;; mapping based on subgraph and root subgraph
	  (let* ((rs-map-cnt (caar (latesql "SELECT max(mapping_i)
                                             FROM sigsub_sigsub
                                             WHERE sub1_id=~a AND sub2_id=~a"
					    (sq sub-id) (sq rt-id))))
		 rs-map rl-map)
	    ;; insert new chained mapping
	    (do* ((rs-map-id 1 (1+ rs-map-id)))
		((> rs-map-id rs-map-cnt))
	      (setf rs-map (get-ss-map sub-id rt-id rs-map-id))
	      (setf rl-map (map-cascade rs-map sl-map))
	      (add-rl-map h-rt-lg rl-map rt-id))))))) 
    
    (maphash #'(lambda (rt-id h-rl-map &aux (i 0) rl-map-key)
		 (dolist (rl-map (hash-keys h-rl-map))
		   (incf i)
		   (setf rl-map-key (format nil "~a-~a" rt-id i))
		   (setf (gethash rl-map-key h-ans) rl-map)))
	     h-rt-lg)
    h-ans))

(defun transitive-closure-sigsubs (h-sigsub-lg)
  "Takes in the output of get-sigsubs-of-lg, which is a hash table whose key is a conjunction of subgraph ids and mapping ids and whose value is a hash table from subgraph node to linkage graph node.
Naming abbr:
sl: sig subgraph - linkage graph
rl: root sig subgraph - linkage graph
rs: root sig subgraph - sig subgraph
map-key is a combination of subgraph id and map id."
  (let* ((sl-map-keys (hash-keys h-sigsub-lg))
	 (sub-ids (mapcar #'(lambda (a) (car (split-re "-" a))) sl-map-keys))
	 (sub-ids (mapcar #'parse-integer sub-ids))
	 (h-rt-lg (make-hash-table :test #'equalp))
	 (h-ans (make-hash-table :test #'equalp))
	 sub-id rt-ids sl-map)
    
    (dolist (sl-map-key sl-map-keys)
      
      (setf sub-id (car (split-re "-" sl-map-key)))
      (setf sub-id (parse-integer sub-id))
      
      ;; (format t "~&sl-map-key: ~a~%" sl-map-key)
      
      ;; if good sigsub, no need to transitive closure, add it directly
      (cond
       ((root-sigsubp sub-id)
	(add-rl-map h-rt-lg (gethash sl-map-key h-sigsub-lg) sub-id))
       (t
	;; save a copy of original sub-lg-map
	(setf sl-map (gethash sl-map-key h-sigsub-lg))
	
	(setf rt-ids (subisomorphic-roots sub-id))
	
	(dolist (rt-id rt-ids)
	  ;; if we already have root subgraph mapped, we can skip
	  (unless (member rt-id sub-ids)
	    ;; mapping based on subgraph and root subgraph
	    (let* ((rs-map-cnt (caar (latesql "SELECT max(mapping_i)
                                               FROM sigsub_sigsub
                                               WHERE sub1_id=~a AND sub2_id=~a"
					      (sq sub-id) (sq rt-id))))
		   rs-map rl-map)
	      ;; insert new chained mapping
	      (do* ((rs-map-id 1 (1+ rs-map-id)))
		  ((> rs-map-id rs-map-cnt))
		(setf rs-map (get-ss-map sub-id rt-id rs-map-id))
		(setf rl-map (map-cascade rs-map sl-map))
		(add-rl-map h-rt-lg rl-map rt-id))))))))
    
    (maphash #'(lambda (rt-id h-rl-map &aux (i 0) rl-map-key)
		 (dolist (rl-map (hash-keys h-rl-map))
		   (incf i)
		   (setf rl-map-key (format nil "~a-~a" rt-id i))
		   (setf (gethash rl-map-key h-ans) rl-map)))
	     h-rt-lg)
    h-ans))

(defmethod extract-concept-graph ((sa sentence-annotation)
				  &key (hier? t) (closure? nil) 
				  &aux ef-type pnode-type lg-type)
  "Assume that lg sigsub mapping is a list of conses whose car is lg node id
   and whose cdr is sigsub node id.
   represents the concept graph as a list of lists"

  (cond
   (hier?
    (setf ef-type (gkey 'ghier-concept-graph 'graph-type))
    (setf pnode-type (gtype 'ghier-parse-node-type)))
   (t
    (setf ef-type (gkey 'gconcept-graph 'graph-type))
    (setf pnode-type (gtype 'gparse-node-type))))
  (setf lg-type (format nil "~a|~a" ef-type pnode-type))
  (when *debug-feature-extract*
    (format t "~&Extracting concept graph for sentence ~a~%: ~a~%" 
	    (id sa) (content sa)))
  
  (when (>= 1 (length (annotations sa :type (gtype 'ghier-parse-node-type))))
    (return-from extract-concept-graph nil))
  (let* ((lgid (get-linkage-graph sa lg-type))
	 h-sigsubs concept-graph)
    (when lgid
      (setf h-sigsubs (get-sigsubs-of-lg lgid))
      (when *debug-feature-extract*
	(format t "~&linkage graph id ~a~%" lgid))
      
      (when (eq closure? 'full)
	(format t "~&full transivitve closure concept graphs~%")
	(setf h-sigsubs (transitive-closure-sigsubs h-sigsubs)))
      
      (when (eq closure? 'semi)
	(format t "~&semi transitive closure concept graphs~%")
	(setf h-sigsubs (transitive-semi-closure-sigsubs h-sigsubs)))
      
      (maphash 
       #'(lambda (sub-map-id map)
	   (if concept-graph
	       (nconc concept-graph (ef-feature-gen sub-map-id map sa lg-type
						    :closure? closure?))
	     (setf concept-graph (ef-feature-gen sub-map-id map sa lg-type
						 :closure? closure?))))
       h-sigsubs))
    (when *debug-feature-extract*
      (format t "concept graph: ~a~%" concept-graph))
    concept-graph))

(defparameter *h-unigram-cg* (make-hash-table :test #'equalp))

(defun load-cg-unigram (&aux ln)
  (with-open-file (in-f "late:;cg_unigram.txt" :direction :input)
		  (loop (unless (setf ln (read-line in-f nil nil)) (return))
			(setf (gethash ln *h-unigram-cg*) 1))))

(defun h-in-cg? (wd)
  (unless (> (hash-table-count *h-unigram-cg*) 0)
    (load-cg-unigram))
  (gethash wd *h-unigram-cg*))

(defmethod extract-ngram ((num integer)
			  (sa sentence-annotation)
			  &key
			  (start nil)
			  (end nil))
  "num specifies whether we want to, use # as start symbol, ## as end symbol?
you need to filter out those stop words too."
  ;; (format t "~&sentence ~%~a~%" (content sa))
  (let* ((ngrams nil)
	 (ngram nil)
	 (start (or start (start sa)))
	 (end (or end (end sa)))
	 (toks (annotations-spec sa :type (gtype 'gtoken-type)
				 :filter #'(lambda (a) 
					     (and (not (maskedp a)) 
						  (<= start (start a))
						  (<= (end a) end)))))
	 (wds (mapcar #'tok-ft-str toks))
	 (wds (remove-if #'(lambda (a) (= 0 (length a))) wds))
	 )
    (dotimes (i (- (length wds) (1- num))) ;make sure of that
      (setq ngram (format nil "~{~a~^_~}" (subseq wds i (+ num i))))
      (push ngram ngrams))

    (nreverse ngrams)))

(defun tok-clean-str (tok)
  (let* (tok-str wds)
    (setf tok-str (string-downcase (content tok)))
    (setf wds (blow-word tok-str))
    ;; (setf tok-str (replace-re tok-str "[^A-Za-z0-9\\s]" ""))
    ;; (when (h-in-stoplist? tok-str)
    ;;   (setf tok-str ""))
    (remove-if #'(lambda (a) (match-re "^[0-9]+$" a)) wds)))

(defun blow-word (wd)
  (setf wd (replace-re wd "\\." ""))
  (split-re "[^A-Za-z0-9\\s]+" wd))

(defun out-concept (tok)
  (not (annotations-spanning tok :type 'concept-tag)))

(defun i2b2-extract-ngram (num sa
			       &key
			       (start nil)
			       (end nil)
			       (filter nil))
  (let* ((ngrams nil)
	 (ngram nil)
	 (start (or start (start sa)))
	 (end (or end (end sa)))
	 (toks (annotations-spec sa :type (gtype 'gtoken-type)
	 			 :filter #'(lambda (a) 
	 				     (and (if filter
						      (funcall filter a)
						    t)
						  (<= start (start a))
	 					  (<= (end a) end)))))
	 (wds (mapcan #'tok-clean-str toks))
	 (wds (remove-if #'(lambda (a) (= 0 (length a))) wds))
	 )
    (dotimes (i (- (length wds) (1- num))) ;make sure of that
      (setq ngram (format nil "~{~a~^_~}" (subseq wds i (+ num i))))
      (push ngram ngrams))
    (nreverse ngrams)))

(defmethod extract-pnode-ngram ((num integer)
				(sa sentence-annotation)
				&key
				(start nil)
				(end nil))
  "num specifies whether we want to, use # as start symbol, ## as end symbol?
you need to filter out those stop words too."
  ;; (format t "~&sentence ~%~a~%" (content sa))
  (let* ((ngrams nil)
	 (ngram nil)
	 (start (or start (start sa)))
	 (end (or end (end sa)))
	 (pns (annotations-spec sa :type (gtype 'ghier-parse-node-type)
				:filter #'(lambda (a) 
					    (and (not (maskedp a))
						 (<= start (start a))
						 (<= (end a) end)))))
	 (wds (mapcar #'tok-ft-str pns))
	 (wds (remove-if #'(lambda (a) (= 0 (length a))) wds)))
    
    (unless (maskedp sa)
      (dotimes (i (- (length wds) (1- num))) ;make sure of that
	(setq ngram (format nil "~{~a~^_~}" (subseq wds i (+ num i))))
	(push ngram ngrams))
      (nreverse ngrams))))

(defmethod extract-ngram-id ((num integer)
			     (h-ngram hash-table) 
			     (hr-ngram hash-table)
			     (h-ngram-count hash-table) 
			     (sa sentence-annotation))
  "num specifies whether we want to, use # as start symbol, ## as end symbol?
you need to filter out those stop words too."
  ;; (format t "~&sentence ~%~a~%" (content sa))
  (let* ((ngrams nil)
	 (ngram nil)
	 (ngram-id nil)
	 (wds (mapcar 
	       #'(lambda (ta) (or ;; (stem ta) 
			       (car (norm (content ta)))
			       (content ta)))
	       (annotations-spec 
		sa 
		:type (gtype 'gtoken-type)
		:filter 
		#'(lambda (a) 
		    (let* ((ac (or ;; (stem a) 
				(car (norm (content a)))
				(content a))))
		      (and 
		       (not (annotations-spanning-spec a 
						       :type 'mask-annotation))
		       (not (in-stoplist? ac)
			    ;;(member ac *stoplist* :test #'equalp)
			    )
		       (match-re "[A-Za-z]" ac))))))))
    
    (dotimes (i (- (length wds) (1- num))) ;make sure of that
      (setq ngram (format nil "~{~a~^_~}" (subseq wds i (+ num i))))
      (setq ngram-id (return-index h-ngram 
				   hr-ngram
				   (string-downcase ngram)))
      (pushnew ngram-id ngrams :test #'=)
      (incf (gethash ngram-id h-ngram-count 0)))
;;;    (format t "~&~agrams of are ~%~a~%" num
;;;	    (mapcar #'(lambda (x) (gethash x hr-ngram)) ngrams))

    (nreverse ngrams)))




#|

;;; psz: proposed reorganization, using the gazette instead of word files.
;;; To do this, we could add the names of the months, their abbreviations, and the days
;;; of the week to the gazette. Until then, the last select below won't work.
;;; There are three names in the current names data that are not in the current gazette of names:
;;; antoniochristopher
;;; jacky
;;; robinlinda
;;; This does not take care of the stoplist issues, but pretty clearly those should also be
;;; in the gazette. I have now added them.  I also added day-of-week abbreviations Mon Tue ...
;;;  

|#

(defmemo check-in-dict (tok)
  "Looks up token in gazette and returns a 4-list showing whether it occurs as a name, location,
   hospital or Month or Day of Week name."
  (let*
      ((qtokm (sq (concatenate 'string "%" tok "%")))
       (n (sql
	   (format
	    nil
	    "select id from gazette where type in ('last', 'male', 'female') and entry=~a limit 1"
	    (sq tok))
	   :db *gazette-db*))
       (l (sql (format
		nil
		"select id from gazette where type in ('city', 'county', 'tiger-city', 'extra-city') and entry like ~a limit 1"
		qtokm)
	       :db *gazette-db*))
       (h (sql (format
		nil
		"select id from gazette where type in ('hospital', 'ma-hospital') and entry like ~a limit 1"
		qtokm)
	       :db *gazette-db*))
       (d (sql (format
		nil
		"select id from gazette where type in ('day', 'month', 'month-abbrev', 'day-abbrev') and entry like ~a limit 1"
		qtokm)
	       :db *gazette-db*)))
    (list n l h d)))





;; The roles should be extracted directly from database
(defun extract-phi-tag (token)
  ;; start should be able to identify a token/tag annotation
  (let* ((phi (annotations-on token 'phi-tag)))
    (gethash (data phi) *h-sens*)
    )
  )

;; assume Lisp pass hash table by reference
(defun return-index (hash-ref rhash-ref key)
  (let* ((index (gethash key hash-ref)))
    (when (not index)
      (setq index (1+ (hash-table-count hash-ref)))
      (setf (gethash index rhash-ref) key)
      (setf (gethash key hash-ref) index)
      )
    index
    )
  )

(defun extract-clinical-tag (token)
  ;; start should be able to identify a token/tag annotation
  (let* ((ctag (annotations-on token 'clinical-tag))
	 (id-ctag (return-index *h-ctag* *hr-ctag* (data ctag))))
    (incf (gethash id-ctag *h-ctag-count* 0))
    id-ctag
    )
  )

(defun extract-pos (token)
  ;; start should be able to identify a token/tag annotation
  (let* ((pos (annotations-on-spec token :type (gtype 'gtag-type))))
    (return-index *h-pos* *hr-pos* (data pos))))

;; lexical bigram must be stemmed ones
;; return the indexes of names of bigram tokens
(defmethod extract-right-lexical-bigram ((token lp-token))
  ;; find token sequence within the same sentence
  (let* ((n-tok-l (next-anns token :type 'lp-token))
	 ;; or use stem
	 (n-sw (safe-norm (content  (elt n-tok-l 0))))
	 (nn-sw (safe-norm (content (elt n-tok-l 1)))))
    (if (null n-sw) (setq n-sw "none"))
    (if (null nn-sw) (setq nn-sw "none"))
    (setq n-sw (return-index *h-words* *hr-words* n-sw))
    (setq nn-sw (return-index *h-words* *hr-words* nn-sw))
    (incf (gethash n-sw *h-word-count* 0))
    (incf (gethash nn-sw *h-word-count* 0))
    (list n-sw nn-sw)))

(defmethod extract-left-lexical-bigram ((token lp-token))
  ;; find token sequence within the same sentence
  (let* ((p-tok-l (prev-anns token :type 'lp-token))
	 ;; or use stem 
	 (p-sw (safe-norm (content (elt p-tok-l 0))))
	 (pp-sw (safe-norm (content (elt p-tok-l 1)))))
    (if (null p-sw) (setq p-sw "none"))
    (if (null pp-sw) (setq pp-sw "none"))
    (setq p-sw (return-index *h-words* *hr-words* p-sw))
    (setq pp-sw (return-index *h-words* *hr-words* pp-sw))
    (incf (gethash p-sw *h-word-count* 0))
    (incf (gethash pp-sw *h-word-count* 0))
    (list pp-sw p-sw)))

(defun contains-punc (token)
  (match-re "[\.\,\?\:\;\[\]\{\}\(\)\*\`\!\@\#\$\%\^\&\/\\]" 
	    (content token))
  )

(defun is-number (token)
  (match-re "^\\d+(\\.\\d*)?$" (content token))
  )

(defun first-cap (token)
  (match-re "^[A-Z][a-z]+" (content token))
  )


(defmethod extract-left-link ((token token-annotation)
			      &optional tas)
  "Return the a list of syntactic links' (left-token left-label) of a token.
Input
======
token - token annotation
tas - a list of token annotations belonging to the sentence where the token is 
      at
Test
======
Tested on sentences on alp-token, see dijkstra."
  (let* ((l-link-l (left-links token))
	 (ret nil))
    (dolist (l-link-iter l-link-l)
      (let* ((lta (if (and (plusp (l-lindex l-link-iter))
			   (<= (l-lindex l-link-iter) (length tas)))
		      (elt tas (1- (l-lindex l-link-iter)))
		    (l-lword l-link-iter)))					; index!
	     (ll (l-llab l-link-iter)))
	(push (list lta ll) ret)))
    (nreverse ret)))

;; note that the token should be of type lp-token
(defmethod extract-right-link ((token token-annotation)
			       &optional tas)
  "Return the a list of syntactic links' (right-token right-label) of a token.
Input
======
token - token annotation
tas - a list of token annotations belonging to the sentence where the token is 
      at
Test
======
Tested on sentences on alp-token, see dijkstra."
  ;; token annotation
  (let* ((r-link-l (right-links token))
	 (ret nil))
    (dolist (r-link-iter r-link-l)
      (let* ((rta (if (and (plusp (l-rindex r-link-iter))
			   (<= (l-rindex r-link-iter) (length tas)))
		      (elt tas (1- (l-rindex r-link-iter)))
		    (l-rword r-link-iter)))
	     (rl (l-rlab r-link-iter)))
	(push (list rta rl) ret)))
    (setq ret (nreverse ret))))



(defmethod extract-left-link ((pnode parse-node-stanford)
			      &optional pnodes)
  "Return the a list of syntactic links' (left-pnode left-label) of a parse 
node.
Input
======
pnode - parse node annotation
pnodes - a list of parse node annotations belonging to the sentence where the
         parse node is at
Test
======
Tested on sentences on alp-token, see dijkstra.
"
  ;; token annotation
  (let* ((gov-dep-l (gov-deps pnode))
	 (dep-dep-l (dep-deps pnode))
	 (sen (car (annotations-spanning pnode :type 'sentence-annotation)))
	 (pnode-type (type-of pnode))
	 (ret nil))
    (unless pnodes 
      (setf pnodes (annotations sen :type pnode-type)))
    
    (dolist (gov-dep gov-dep-l)
      (let* ((govpn (find-annotation (sp::d-govid gov-dep) sen))
	     (deppn (find-annotation (sp::d-depid gov-dep) sen))
	     (ldep (sp::d-rel gov-dep)))
	(when (annotation-lessp deppn govpn)
	  (push (make-linkseg :pnode deppn :elab ldep) ret))))
    
    (dolist (dep-dep dep-dep-l)
      (let* ((govpn (find-annotation (sp::d-govid dep-dep) sen))
	     (deppn (find-annotation (sp::d-depid dep-dep) sen))
	     (ldep (sp::d-rel dep-dep)))
	(when (annotation-lessp govpn deppn)
	  (push (make-linkseg :pnode govpn :elab ldep) ret))))
    (nreverse ret)))

(defmethod extract-right-link ((pnode parse-node-stanford)
			       &optional pnodes)
  "Return the a list of syntactic links' (right-token right-label) of a parse node.
Input
======
token - token annotation
tas - a list of token annotations belonging to the sentence where the token is 
      at
Test
======
Tested on sentences on alp-token, see dijkstra."
  ;; token annotation
  (let* ((gov-dep-l (gov-deps pnode))
	 (dep-dep-l (dep-deps pnode))
	 (sen (car (annotations-spanning pnode :type 'sentence-annotation)))
	 (pnode-type (type-of pnode))
	 (ret nil))
    (unless pnodes 
      (setf pnodes (annotations sen :type pnode-type)))
    
    (dolist (gov-dep gov-dep-l)
      (let* ((govpn (find-annotation (sp::d-govid gov-dep) sen))
	     (deppn (find-annotation (sp::d-depid gov-dep) sen))
	     (rdep (sp::d-rel gov-dep)))
	(when (annotation-lessp govpn deppn)
	  (push (make-linkseg :pnode deppn :elab rdep) ret))))
    
    (dolist (dep-dep dep-dep-l)
      (let* ((govpn (find-annotation (sp::d-govid dep-dep) sen))
	     (deppn (find-annotation (sp::d-depid dep-dep) sen))
	     (rdep (sp::d-rel dep-dep)))
	(when (annotation-lessp deppn govpn)
	  (push (make-linkseg :pnode govpn :elab rdep) ret))))
    (nreverse ret)))



(defmethod extract-left-syn-bigram ((token token-annotation)
				    (tas list))
  "Returns the left syntactic bigram defined in Tawanda's thesis.
This works specifically on Link Grammar Parser.
Test
======
Tested on sentences on alp-token, see dijkstra."
  (let* ((layer1 (extract-left-link token tas))
	 ans)
    (dolist (l1-link layer1)
      ;; note that lw and ll are string now
      (let* ((lta (elt l1-link 0))
	     (lw (if (stringp lta) 
		     lta 
		   (or (car (norm (content lta)))
		       (string-downcase (content lta)))))
	     (ll (elt l1-link 1)))
	
	(unless (string= lw "LEFT-WALL")
	  (let* ((layer2 (extract-left-link lta tas)))
	    (if layer2					; this is ugly
		(dolist (l2-link layer2)
		  (let* ((lta2 (elt l2-link 0))
			 (lw2 (if (stringp lta2) 
				  lta2 
				(or (car (norm (content lta2)))
				    (string-downcase (content lta2)))))
			 (ll2 (elt l2-link 1)))
		    (pushnew (format nil "~a_~a_~a_~a" lw ll lw2 ll2) ans
			     :test #'string=)))
	      (pushnew (format nil "~a_~a" lw ll) ans :test #'string=))))))
    ans))

(defmethod extract-left-syn-bigram-layered ((token token-annotation)
					    (tas list))
  "Returns the left syntactic bigram defined in Tawanda's thesis.
Test
======
Tested on sentences on alp-token, see dijkstra."
  (let* ((layer1 (extract-left-link token tas))
	 (first-layer nil)
	 (second-layer nil))
    (dolist (l1-link layer1)
      ;; note that lw and ll are string now
      (let* ((lta (elt l1-link 0))
	     (lw (if (stringp lta) 
		     lta 
		   (or (car (norm (content lta)))
		       (string-downcase (content lta)))))
	     (ll (elt l1-link 1)))
	(pushnew (format nil "~a-~a" lw ll) first-layer :test #'string=)
	(unless (string= lw "LEFT-WALL")
	  (let* ((layer2 (extract-left-link lta tas)))
	    (dolist (l2-link layer2)
	      (let* ((lta2 (elt l2-link 0))
		     (lw2 (if (stringp lta2) 
			      lta2 
			    (or (car (norm (content lta2)))
				(string-downcase (content lta2)))))
		     (ll2 (elt l2-link 1)))
		(pushnew (format nil "~a-~a" lw2 ll2) second-layer 
			 :test #'string=)))))))
    (list second-layer first-layer)))

(defmethod extract-right-syn-bigram ((token token-annotation)
				     (tas list))
  "Returns the right syntactic bigram defined in Tawanda's thesis.
Test
======
Tested on sentences on alp-token, see dijkstra."
  (let* ((layer1 (extract-right-link token tas))
	 ans)
    (dolist (r1-link layer1)
      (let* ((rta (elt r1-link 0))
	     (rw (if (stringp rta) 
		     rta 
		   (or (car (norm (content rta)))
		       (string-downcase (content rta)))))
	     (rl (elt r1-link 1)))
	
	(unless (string= rw "RIGHT-WALL")
	  (let* ((layer2 (extract-right-link rta tas)))
	    (if layer2
		(dolist (r2-link layer2)
		  (let* ((rta2 (elt r2-link 0))
			 (rw2 (if (stringp rta2) 
				  rta2 
				(or (car (norm (content rta2)))
				    (string-downcase (content rta2)))))
			 (rl2 (elt r2-link 1)))
		    (pushnew (format nil "~a_~a_~a_~a" rw rl rw2 rl2) ans
			     :test #'string=)))
	      (pushnew (format nil "~a_~a" rw rl) ans :test #'string=))))))
    ans))

(defmethod extract-right-syn-bigram-layered ((token token-annotation)
					     (tas list))
  "Returns the right syntactic bigram defined in Tawanda's thesis.
Test
======
Tested on sentences on alp-token, see dijkstra."
  (let* ((layer1 (extract-right-link token tas))
	 (first-layer nil)
	 (second-layer nil))
    (dolist (r1-link layer1)
      (let* ((rta (elt r1-link 0))
	     (rw (if (stringp rta) 
		     rta 
		   (or (car (norm (content rta)))
		       (string-downcase (content rta)))))
	     (rl (elt r1-link 1)))
	(pushnew (format nil "~a-~a" rw rl) first-layer :test #'string=)
	(unless (string= rw "RIGHT-WALL")
	  (let* ((layer2 (extract-right-link rta tas)))
	    (dolist (r2-link layer2)
	      (let* ((rta2 (elt r2-link 0))
		     (rw2 (if (stringp rta2) 
			      rta2 
			    (or (car (norm (content rta2)))
				(string-downcase (content rta2)))))
		     (rl2 (elt r2-link 1)))
		(pushnew (format nil "~a-~a" rw2 rl2) second-layer
			 :test #'string=)))))))
    (list first-layer second-layer)))


(defmethod extract-link-envelope ((pnode parse-node-stanford) 
				  (covered-pnode-ids hash-table)
				  &aux l-envelope r-envelope)
  "Extracts dependencies as far as to any parse node that is not in any current concept graph (signficnat subgraph) mapping with the current linkage graph.
This works specifically for Stanford Parser."
  ;; calculate a set of uncovered parse nodes, be sure to include preps
  ;; look at both gov and dep links on this parse node


  (setf (gethash (id pnode) covered-pnode-ids) 1)
  (setf l-envelope (extract-left-link pnode))
  (setf l-envelope (remove-if #'(lambda (a) (gethash a covered-pnode-ids))
			      l-envelope
			      :key #'(lambda (a) (id (ls-pnode a)))))
  (setf r-envelope (extract-right-link pnode))
  (setf r-envelope (remove-if #'(lambda (a) (gethash a covered-pnode-ids))
			      r-envelope
			      :key #'(lambda (a) (id (ls-pnode a)))))
  (union l-envelope r-envelope
	 :test #'(lambda (a b) (and (= (id (ls-pnode a)) (id (ls-pnode b)))
				    (equalp (ls-elab a) (ls-elab b)))))
  
  
  ;; look at parse nodes in the Markov envelop, which are themselves not nodes
  ;; in the significant subgraph
  ;; return "_" concatenated chains of alternating dependency-pnode pair, 
  ;; distinguishing between dep link and gov link
  )


(defmethod extract-link-lex-cloud-chain ((pnode parse-node-stanford)
					 (lg-type string)
					 &optional covered-pnode-ids
					 &aux ans)
  "Recursive call extract-stanford-dep-envelope.
What if there is cycles? The sequence may be indeterministic. I think in ordering the envelopes elements, put right links before left links and put shorter links before longer links."
  (unless covered-pnode-ids
    (let* ((sen (car (annotations-spanning pnode :type 'sentence-annotation))))
      (setf covered-pnode-ids (sigsub-covered-pnode-ids sen lg-type))))
  (dolist (link-seg (extract-link-envelope pnode covered-pnode-ids))
    (let* ((apnode (ls-pnode link-seg))
	   (elab (ls-elab link-seg))
	   (link-paths (extract-link-lex-cloud-chain apnode lg-type covered-pnode-ids))
	   new-path pn-str)
      (setf pn-str (replace-re (content apnode) "\\s+" " "))
      (setf pn-str (replace-re pn-str "[,\"']" ""))
      (cond 
       ((match-re "(dim|\\+|-)$" pn-str)
	(setf pn-str (string-downcase pn-str)))
       (t
	(setf pn-str (safe-norm pn-str))))
      ;; replace space with _, not - to avoid confusion with minus sign
      (setf pn-str (replace-re pn-str "\\s+" "_"))
      ;; (setf pn-str (data apnode))
      (dolist (link-path link-paths)
	(setf new-path (format nil "~a|~a||~a" elab pn-str link-path))
	(when *debug-feature-extract*
	  (format t "new-path: ~a~%" new-path))
	(push new-path ans))
      (unless link-paths
	(setf new-path (format nil "~a|~a" elab pn-str))
	(when *debug-feature-extract*
	  (format t "new-path: ~a~%" new-path))
	(push new-path ans))))
  (nreverse ans))


(defmethod extract-link-cui-cloud-chain ((pnode parse-node-stanford)
					 (lg-type string)
					 &optional covered-pnode-ids
					 &aux ans)
  "Recursive call extract-stanford-dep-envelope.
What if there is cycles? The sequence may be indeterministic. I think in ordering the envelopes elements, put right links before left links and put shorter links before longer links."
  (unless covered-pnode-ids
    (let* ((sen (car (annotations-spanning pnode :type 'sentence-annotation))))
      (setf covered-pnode-ids (sigsub-covered-pnode-ids sen lg-type))))
  (dolist (link-seg (extract-link-envelope pnode covered-pnode-ids))
    (let* ((apnode (ls-pnode link-seg))
	   (elab (ls-elab link-seg))
	   (link-paths (extract-link-cui-cloud apnode lg-type covered-pnode-ids))
	   (cuis (annotations-spec apnode :type 'cui-annotation))
	   (cuis (sort (mapcar #'data cuis) #'string-lessp))
	   new-path)
      (dolist (link-path link-paths)
	(setf new-path (format nil "~a|~{~a~^-~}||~a" elab cuis link-path))
	(when *debug-feature-extract*
	  (format t "new-path: ~a~%" new-path))
	(push new-path ans))
      (unless link-paths
	(setf new-path (format nil "~a|~{~a~^-~}" elab cuis))
	(when *debug-feature-extract*
	  (format t "new-path: ~a~%" new-path))
	(push new-path ans))))
  (nreverse ans))

(defun repl-punc (str)
  (cond 
   ((equalp str ",") "COMMA")
   ((equalp str ":") "COLON")
   ((equalp str ".") "PERIOD")
   ((equalp str "%") "PERCENT")
   ((equalp str ";") "SEMICOLON")
   ((equalp str "#") "NUMBERSIGN")
   ((equalp str "/") "SLASH")
   ((equalp str "+") "PLUS")
   ((equalp str "-") "DASH")
   ((equalp str "\"") "DQ")
   ((equalp str "'") "QT")
   ((equalp str "(") "LB")
   ((equalp str ")") "RB")
   (t str)))

(defun tok-ft-str (tok)
  (let* (tok-str)
    (if (stringp tok)
	(setf tok-str tok)
      (setf tok-str (content tok)))

    (setf tok-str (replace-re tok-str "[,\"']" ""))
    ;; well, if there is a stand alone +, - etc. that's a data problem

    (setf tok-str (replace-re tok-str "\\+" "_plus_"))
    (setf tok-str (replace-re tok-str "/" "_slash_"))
    (setf tok-str (replace-re tok-str "-" "_minus_"))

    (cond 
     ((h-in-stoplist? tok-str)
      (setf tok-str ""))

     (t 
      (setf tok-str (or (string-downcase tok-str) "")))) 
    
    (setf tok-str (replace-re tok-str "\\s+" "_"))
    (unless (and (>= (length tok-str) 3)
		 (match-re "(?i)[a-z]" tok-str)
		 (not (match-re "[0-9]{4,}" tok-str)))
      (setf tok-str ""))

    (setf tok-str (replace-re tok-str "_minus_" "-"))
    (setf tok-str (replace-re tok-str "_+" "_"))

    (stemmer::stem tok-str)))

(defun cui-pname (cui)
  "append cui with pname"
  (let* ((pname (pname cui)))
    (setf pname (replace-re pname "[,\"']" ""))
    (setf pname (replace-re pname "\\s+" "_"))
    (cond 
     ((match-re *no-letter-pat* pname)
      nil)
     (t
      (format nil "~a_~a" cui pname)))))

(defmethod extract-link-lex-cloud ((pnode parse-node-stanford)
				   (lg-type string)
				   &optional covered-pnode-ids
				   &aux ans link-segs)
  "Recursive call extract-stanford-dep-envelope.
What if there is cycles? The sequence may be indeterministic. I think in ordering the envelopes elements, put right links before left links and put shorter links before longer links."
  (unless covered-pnode-ids
    (let* ((sen (car (annotations-spanning pnode :type 'sentence-annotation))))
      (setf covered-pnode-ids (sigsub-covered-pnode-ids sen lg-type))))
  (setf link-segs (extract-link-envelope pnode covered-pnode-ids))
  ;; (format t "~&pnode: ~a~%link-segs:~% ~{~a~^~%~}~%" pnode link-segs)
  (dolist (link-seg link-segs)
    
    (let* ((apnode (ls-pnode link-seg))
	   (elab (ls-elab link-seg))
	   (lcui (pnode-longest-umlss apnode :type 'cui-annotation))
	   (link-paths (extract-link-lex-cloud apnode lg-type covered-pnode-ids))
	   toks cui-start cui-end new-path tok-strs)
      (cond
       (lcui
	(setf cui-start (reduce #'min (mapcar #'start lcui)))
	(setf cui-end (reduce #'max (mapcar #'end lcui)))
	;; i think it should be apnode, right?
	(setf toks (annotations-spec apnode 
				     :type (gtype 'gtoken-type)
				     :filter #'(lambda (a) ;#'no-cui-cover?
						 (not (allenr-ar cui-start
								 cui-end
								 (start a)
								 (end a)))))))
       (t
	(setf toks (annotations-spec apnode 
				     :type (gtype 'gtoken-type)))))
      
      (dolist (link-path link-paths)
	(setf new-path (format nil "~a|~a" elab link-path))
	(push new-path ans))
      


      
      (setf tok-strs (mapcar #'tok-ft-str toks))
      (setf tok-strs (remove-if #'(lambda (a) (= 0 (length a))) tok-strs))
      (setf tok-strs (sort tok-strs #'string-lessp))
      
      (dolist (tok-str tok-strs)
	(setf new-path (format nil "~a|~a" elab tok-str))
	(when *debug-feature-extract*
	  (format t "new-path: ~a~%" new-path))
	(push new-path ans))
      ))
  (nreverse ans))

(defmethod extract-link-cui-cloud ((pnode parse-node-stanford)
				   (lg-type string)
				   &optional covered-pnode-ids
				   &aux ans)
  "Recursive call extract-stanford-dep-envelope.
What if there is cycles? The sequence may be indeterministic. I think in ordering the envelopes elements, put right links before left links and put shorter links before longer links.
bag of words flavor"
  (unless covered-pnode-ids
    (let* ((sen (car (annotations-spanning pnode :type 'sentence-annotation))))
      (setf covered-pnode-ids (sigsub-covered-pnode-ids sen lg-type))))
  (dolist (link-seg (extract-link-envelope pnode covered-pnode-ids))
    (let* ((apnode (ls-pnode link-seg))
	   (elab (ls-elab link-seg))
	   (link-paths (extract-link-cui-cloud apnode lg-type covered-pnode-ids))
	   (cuis (pnode-longest-umlss apnode :type 'cui-annotation))
	   (cuis (sort (mapcar #'data cuis) #'string-lessp))
	   new-path)
      (dolist (link-path link-paths)
	
	(setf new-path (format nil "~a|~a" elab link-path))
	(push new-path ans))
      
      (setf cuis (mapcar #'cui-pname cuis))
      (setf cuis (remove-if #'null cuis))

      (when cuis
	(setf new-path (format nil "~a|~{~a~^-~}" elab cuis))
	(when *debug-feature-extract*
	  (format t "new-path: ~a~%" new-path))
	(push new-path ans))))
  (nreverse ans))

(defun explode-link-cloud (cloud &aux explosion)
  "This is only needed when using extract-link-cui-cloud-chained (lex resp.)"
  (dolist (path cloud)
    (let* ((segs (split-re "\\|\\|" path))
	   subpath)
      (dolist (seg segs)
	(setf subpath (format nil "~@[~a||~]~a" subpath seg))
	(pushnew subpath explosion :test #'equalp)
	(pushnew seg explosion :test #'equalp))))
  explosion)

(defmethod dijkstra ((self integer)
		     (tas list)
		     &key 
		     (debug? nil))
  "Computes the shortest path (and cost) of self to all other nodes in the 
dependency graph, currently for link parsed dependency, to be generalized.
Input
======
self - integer denotes the token position
tas - a list of tokens of the same type
debug? - whether to print verbose debug information.
Output
======
h-paths - a hash table of the shortest path (edges) of self to others (strings)
h-nodes - a hash table of the shortest route (nodes) of self to others (strings)
Note
======
The cost itself is encoded in the path, just split against \"-\".
h-snb - a hash table of the shortest path neibor (currently) for a node whose
shortest path is yet determined.
Note that for path to itself and path to unreachable node, we return empty
string in both cases.
Test
======
Tested on the following sentence:
1)
hierarchically link parse sentence 552163:
The aspirate smear is paucicellular and diluted with peripheral
     blood.
sent str is 
The smear is paucicellular and diluted with blood .
poss seq is: 
DT, NN, VBZ, JJ, CC, VBN, IN, NN, .
Number of linkage is 2
Cost (UNUSED, DIS, NSUB, AND, PP-LEN): (0 0 -1 0 1)

    +------------------------------------Xp-----------------------------------
    |            +-------------Ss-------------+                               
    +-----Wd-----+      +--------VJlsi--------+--------MVp-------+            
    |      +--Ds-+      +----Pa---+           +--VJrsi--+        +--Ju--+     
    |      |     |      |         |           |         |        |      |     
LEFT-WALL the smear.n is.v paucicellular.a and.j-v diluted.v-d with blood.n-u 


+
|
|
|
|
. 
2)
hierarchically link parse sentence:
Additional studies that might help to provide a definite
     diagnosis include molecular genetic analysis of the
     immunoglobulin gene for B-cell clonality.
sent str is 
Studies that might help to provide a diagnosis include analysis of the gene for clonality .
poss seq is: 
NNS, WDT, MD, VB, TO, VB, DT, NN, VBP, NN, IN, DT, NN, IN, NN, .
Number of linkage is 236
Cost (UNUSED, DIS, NSUB, AND, PP-LEN): (0 0 -1 0 1)

    +--------------------------------------------------------------Xp----
    |         +------------------------------Sp--------------------------
    |         +--------Bp-------+                    +-----Os-----+      
    +----Wd---+----R----+---RS--+---I---+--TO-+---I--+     +--Ds--+      
    |         |         |       |       |     |      |     |      |      
LEFT-WALL studies.n that.j-r might.v help.v to.r provide.v a diagnosis.s 


---------------------------------------------------------+
----+                                                    |
    |                   +---Js---+                       |
    +-----Ou----+---Mp--+  +--Ds-+--Mp-+----Ju---+       |
    |           |       |  |     |     |         |       |
include.v analysis.n-u of the gene.n for.p clonality.n-u . 
"
  (let* ((h-paths (make-hash-table :test #'equalp))
	 (h-nodes (make-hash-table :test #'equalp))
	 (h-snb (make-hash-table :test #'equalp))
	 (infinity (* 2 (+ 3 (length tas))))
	 s ta lcost)
    ;; ? to be safe, I think (length tas) is enough to replace infinity
    (dotimes (i (length tas))
      (push (cons i infinity) lcost))
    (rplacd (assoc self lcost) 0)
    (if debug? (format t "lcost: ~a~%" lcost))
    (loop (unless (> (length lcost) 0) (return (list h-paths h-nodes)))
	  (setf lcost (sort lcost #'< :key #'cdr))
	  (if debug? (format t "lcost: ~a~%" lcost))
	  (let* ((vcost (cdar lcost))
		 (v (caar lcost)))
	    (setf ta (elt tas v))
	    (push v s)
	    (if debug? (format t "adding v: ~a, and S is: ~a~%" v s))
	    (if (< vcost infinity)
		(cond
		 ((= self v)
		  (setf (gethash v h-paths) ""
			(gethash v h-nodes) ""))
		 
		 (t
		  (setf (gethash v h-paths) 
			(replace-re
			 (format nil "~a-~a" 
				 (gethash (car (gethash v h-snb)) h-paths) 
				 (cdr (gethash v h-snb)))
			 "^-+" ""))
		  (setf (gethash v h-nodes) 
			(replace-re
			 (format nil "~a-~a" 
				 (gethash (car (gethash v h-snb)) h-nodes)
				 (let* ((ta (elt tas (car (gethash v h-snb)))))
				   (if (= self (car (gethash v h-snb)))
				       ""
				     (string-downcase (content ta)))))
			 "^-+" ""))))
	      
	      (setf (gethash v h-paths) ""
		    (gethash v h-nodes) ""))
	    
	    (when debug?
	      (format t "[~a-~a] shortest path: ~a~%" self v (gethash v h-paths))
	      (format t "[~a-~a] shortest route: ~a~%" self v (gethash v h-nodes)))
	    (dolist (l-link-iter (left-links ta))
	      (let* ((u (1- (l-lindex l-link-iter)))
		     (ta2 (if (>= u 0) (elt tas u)))
		     ;; every edge cost 1, cannot connect through left-wall
		     (dis (if ta2 1 infinity)) 
		     (lab (l-lab l-link-iter)))
		(when (and (assoc u lcost) 
			   (<= 0 u (1- (length tas)))
			   (> (cdr (assoc u lcost)) (+ vcost dis)))
		  (rplacd (assoc u lcost) (+ vcost dis))
		  (setf (gethash u h-snb) (cons v (format nil "~a" lab))))))
	    ;; iterate through right links
	    (dolist (r-link-iter (right-links ta))
	      (let* ((u (1- (l-rindex r-link-iter)))
		     (ta2 (if (< u (length tas)) (elt tas u)))
		     ;; every edge cost 1, cannot connect through right-wall
		     (dis (if ta2 1 infinity)) 
		     (lab (l-lab r-link-iter)))
		(when (and (assoc u lcost)
			   (<= 0 u (1- (length tas)))
			   (> (cdr (assoc u lcost)) (+ vcost dis)))
		  (rplacd (assoc u lcost) (+ vcost dis))
		  (setf (gethash u h-snb) (cons v (format nil "~a" lab))))))
	    (setf lcost (cdr lcost))))))

(defmethod extract-shortest-syntactic-paths-from ((i integer)
						  (sa sentence-annotation))
  (let* (ans
	 (tas (annotations-spec sa :type (gtype 'gtoken-type)))
	 (sp-res (dijkstra i tas))
	 (h-paths (first sp-res)))
    (dotimes (j (length tas))
      (push (cons (cons i j) (gethash j h-paths)) ans))
    (nreverse ans)))

(defmethod extract-shortest-paths-words-from ((i integer)
					      (sa sentence-annotation))
  (let* (ans
	 (tas (annotations sa :type (gtype 'gtoken-type)))
	 (sp-res (dijkstra i tas))
	 (h-nodes (second sp-res)))
    (dotimes (j (length tas))
      (push (cons (cons i j) (gethash j h-nodes)) ans))
    (nreverse ans)))


(defmethod match-cd-bcl-colon ((ta token-annotation))
  (match-re "^((CD|bcl)(\\d|\\+|\\/|\\w|\\-|,)*):" 
	    (content ta) 
	    :return :match :case-fold t))

;; This is a specific example on how one can define their customized list of regular expression patterns and store the matches in the Common Data Model tables (not used in concept graph mining though)
(defmethod match-immun-histchem ((sa sentence-annotation)
				 (start integer)
				 &optional
				 (colon? nil))
  "Assuming only CD and BCL may have compressed expressions like CD19+ 20+ 10+ 
Input
----------
start: relative position in the sentence"
  (let* ((sent (content sa))
	 re-cd-bcl re-other
	 ;; cd-bcl-match cd-bcl-whole
	 )
    (setf re-other (concatenate 'string "("
				"PAX[\\s-]*5" "|"
				"cyclin[\\s-]*D1" "|"
				"annexin[\\s-]*1" "|"
				"cytoplasmic[\\s-]*igm" "|"
				"ki[\\s-]*67" "|"
				"IRF[\\s-]*4" "|"
				"MUM[\\s-]*1" "|" ;?
				"ALK[\\s-]*1" "|"
				"MYC" "|"
				"EBV" "|" ;?
				"EBER" "|"
				"HHV[\\s-]*8" "|"
				"Ig\\s+light\\s+chains?" "|"
				"Ig\\s+heavy\\s+chains?" "|"
				"(IgA|IgG)[\\s-]+(lambda|kappa)" "|"
				"EMA" "|"
				"FDC" "|"
				"beta-F1" "|"
				"perforin" "|"
				"granzyme[\\s+-]*B" "|"
				"TIA[\\s-]*1" "|"
				"HTLV[\\S-]*1" "|"
				"TCL[\\s-]*1" "|"
				"HLA[\\s-]*DR" "|"
				"PD[\\s-]*1" ")\\s*"))
    (setf re-cd-bcl "(CD|bcl)(\\d|\\+|\\/|\\s|\\-|,)*")
    (if colon?
	(setf re-other (concatenate 'string "(" re-other ":)\\s*")
	      re-cd-bcl (concatenate 'string "(" re-cd-bcl ":)\\s*")))
    (multiple-value-bind (cd-bcl-match cd-bcl-whole)
	(match-re re-cd-bcl
		  sent 
		  :start start
		  :case-fold t 
		  :return :index)
      (declare (ignore cd-bcl-match))
      (or cd-bcl-whole
	  (multiple-value-bind (other-match other-whole)
	      (match-re re-other
			sent 
			:start start
			:case-fold t 
			:return :index)
	    (declare (ignore other-match))
	    other-whole)))))

;;; for "CD 10", an annotation with (start end) same as "10" will be inserted.
(defun match-cd-bcl (ta prev-ta)
  ;;  (format t "ta is ~a, prev-ta is ~a~%" ta prev-ta)
  (let* ((match nil))
    (cond 
     ((setq match (match-re "^((CD|bcl)(\\d|\\+|\\/|\\w|\\-|,)*)" 
			    (content ta) 
			    :return :match :case-fold t))
      (setf (data ta) (content ta))
      (return-from match-cd-bcl match)
      )

     ((and prev-ta
	   (match-re "^CD" (or (data prev-ta) "") :case-fold t)
	   (match-re "(\\w*\\d+(\\d+|\\+|\\/|\\w|\\-|,)*)" 
		     (content ta) 
		     :return :match :case-fold t))
      (setf (data ta) (concatenate 'string "CD" (content ta)))
      (setq match (match-re "^((CD|bcl)(\\d|\\+|\\/|\\w|\\-|,)*)" 
			    (data ta) 
			    :return :match :case-fold t))
      (return-from match-cd-bcl match)
      )
     
     ((and prev-ta
	   (match-re "^bcl" (or (data prev-ta) "") :case-fold t)
	   (match-re "(\\w*\\d+(\\d+|\\+|\\/|\\w|\\-|,)*)" 
		     (content ta) 
		     :return :match :case-fold t))
      (setf (data ta) (concatenate 'string "BCL" (content ta)))
      (setq match (match-re "^((CD|bcl)(\\d|\\+|\\/|\\w|\\-|,)*)" 
			    (data ta) 
			    :return :match :case-fold t))
      (return-from match-cd-bcl match)
      )
     )
    match
    )
  )



(defun match-sigm (ta prev-ta)
  (cond 
   ((match-re "^(sig|sigm|igm)$" 
	      (content ta) 
	      :case-fold t)
    (setf (data ta) "sIgM")
    (return-from match-sigm (start ta)))
   ((and (typep prev-ta 'token-annotation)
	 (match-re "^s(urface)?$"
		   (content prev-ta)
		   :case-fold t)
	 (match-re "immunoglobulin"
		   (content ta)
		   :case-fold t))
    (setf (data ta) "sIgM")
    (return-from match-sigm (start prev-ta)))
   (t nil)))

;;; rewrite into fully expanded terms, space separated.
(defmethod rewrite ((expression string))
  (setf expression (replace-re expression "PAX[\\s-]*5" "PAX5" :case-fold t))
  (setf expression (replace-re expression "(CD)[\\s-]*(\\d+)" "\\1\\2" 
			       :case-fold t))
  (setf expression (replace-re expression "annexin[\\s-]*1" "annexin1"
			       :case-fold t))
  (setf expression (replace-re expression "cytoplasmic[\\s-]*igm" 
			       "cytoplasmic_igm"
			       :case-fold t))
  (setf expression (replace-re expression "ki[\\s-]*67" "ki67" :case-fold t))
  (setf expression (replace-re expression "IRF[\\s-]*4" "IRF4" :case-fold t))
  (setf expression (replace-re expression "MUM[\\s-]*1" "MUM1" :case-fold t))
  (setf expression (replace-re expression "ALK[\\s-]*1" "ALK1" :case-fold t))
  (setf expression (replace-re expression "HHV[\\s-]*8" "HHV8" :case-fold t))
  (setf expression (replace-re expression "(Ig)\\s+(light|heavy)\\s+(chains?)"
			       "\\1_\\2_\\3" :case-fold t))
  (setf expression (replace-re expression "(IgA|IgG)[\\s-]+(lambda|kappa)"
			       "\\1_\\2" :case-fold t))
  (setf expression (replace-re expression "granzyme[\\s+-]*B" "granzyme_B"
			       :case-fold t))
  (setf expression (replace-re expression "TIA[\\s-]*1" "TIA1" :case-fold t))
  (setf expression (replace-re expression "HTLV[\\S-]*1" "HTLV1" :case-fold t))
  (setf expression (replace-re expression "TCL[\\s-]*1" "TCL1" :case-fold t))
  (setf expression (replace-re expression "HLA[\\s-]*DR" "HLA_DR" :case-fold t))
  (setf expression (replace-re expression "PD[\\s-]*1" "PD1" :case-fold t))
  
  (setq expression (replace-re expression 
			       "(\\+|\\+\\/\\-)(CD|BCL|\\d)" 
			       "\\1 \\2" :case-fold t))
  (setq expression (replace-re expression 
			       "(\\d\\-)(CD|BCL|\\d)" 
			       "\\1 \\2" :case-fold t))
  
  
  
  (let* ((words (split-re "(\\/(?!\\-)|\\s+|,)" expression))
	 (pref nil)
	 (ans nil))
    (dolist (w words)
      (cond 
       ((match-re "^CD" w :case-fold t)
	(setq pref "CD")
	(push w ans))
       
       ((match-re "^BCL" w :case-fold t)
	(setq pref "BCL")
	(push w ans))
       
       ((match-re "^sIgM" w)
	(setq pref "sIgM")
	(push w ans))
       
       (t 
	;; (assert pref () "expression is ~a, w is ~a~%" expression w)
	(if pref
	    (push (concatenate 'string pref w) ans)
	  (push w ans)))))
    (nreverse ans)))



;;; unify immunochemistry feature name
(defmethod unify-ic2 ((s string))
  (setf s (replace-re s "(\\+|\\-)" ""))
  (replace-re s "^(CD|BCL)\\D*(\\d+)\\D+$" "\\1\\2" :case-fold t)
  (string-upcase s))

(defmethod unify-ic ((s string))
  (setf s (replace-re s "(^\\s+|\\s+$)" ""))
  (if (match-re s "EBV[-\\s]+EBER")
      (setf s (replace-re s "EBV[-\\s]+EBER" "EBV_EBER" :case-fold t))
    (setf s (replace-re s "(EBV|EBER)" "EBV_EBER" :case-fold t)))
  (setf s (replace-re s "HLA[\\s-]+DR" "HLA_DR"))
  (setf s (replace-re s "(\\+|\\-)" ""))
  (setf s (replace-re s "^(CD|BCL|HTLV|ANNEXIN|HHV8)\\D*(\\d+)\\D+$" 
		      "\\1\\2" :case-fold t))
  (setf s (replace-re s "(strong|bright|dim|weak)" "" :case-fold t))

  (setf s (replace-re s "\\s+" "_"))
  (string-upcase s))

(defmethod self-assert-immun-histchem ((sa sentence-annotation)
				       (s-start integer)
				       (s-end integer))
  "Assume that s-start and s-end are relative to the sentence start position."
  (let* ((exp (subseq (content sa) s-start s-end))
	 (words (rewrite exp))
	 (doc (document sa))
	 (start (+ s-start (start sa)))
	 (end (+ s-end (start sa)))
	 flag)
    (dolist (w words)
      (cond
       ((match-re "(strong|bright)" w)
	(setq flag t)
	;; note that the start and end here is approximate to token level
	;; similarly hereinafter
	(add-annotation doc (make-instance 'immuchem-annotation
					   :document doc
					   :start start
					   :end end
					   :data (unify-ic w)
					   :positivity (gethash "strong" *h-chem-tags*))))
       
       ((match-re "(dim|weak)" w)
	(setq flag t)
	(add-annotation doc (make-instance 'immuchem-annotation
					   :document doc 
					   :start start
					   :end end
					   :data (unify-ic w)
					   :positivity (gethash "weak" *h-chem-tags*))))
       
       ((match-re "\\+$" w)
	(setq flag t)
	(add-annotation doc (make-instance 'immuchem-annotation
					   :document doc 
					   :start start
					   :end end
					   :data (unify-ic w)
					   :positivity (gethash "positive" *h-chem-tags*))))
       
       ((match-re "\\-$" w)
	(setq flag t)
	(add-annotation doc (make-instance 'immuchem-annotation
					   :document doc
					   :start start
					   :end end
					   :data (unify-ic w)
					   :positivity (gethash "negative" *h-chem-tags*))))))
    flag
    )
  )

(defun immuchem-annotatedp (doc)
  (member "immuchem-annotated" (analyses doc) :test #'equalp))



(defmethod extract-immun-histchem ((doc document))
  "extracting immunochemistry features
CD19	CD20	CD5	CD10	CD23	sIgM	Bcl-2	Bcl-6	FISH
what needs to be matched: (please refer to NCCN Practice Guidelines in Oncology 
- v.1.2010)
----------
B-cell antigens positive (CD19, CD20, CD79a, PAX5)
Small Cells: CD5, CD10, CD23, cyclin D1, BCL2, BCL6, CD25, CD103, t(11;14), 
t(14;18), annexin 1, Cytoplasmic Ig, morphology and clinical features
----------
B-cell antigens positive (CD19, CD20, CD79a, PAX5)
Medium Cells: CD5, CD10, BCL2, BCL6, cyclin D1, Ki67, IRF4/MUM1, MYC
----------
B-cell antigens positive (CD19, CD20, CD79a, PAX5)
Large Cells: CD5, CD10, BCL6, IRF4/MUM1, cyclin D1, CD20, PAX5, CD138, ALK1, 
CD30, CD15, EBV-EBER, HHV8, Ig light and heavy chains, MAL?, TRAF?, REL? (nuc), 
IgA lambda or kappa, EMA, IgG lambda or kappa
----------
B-cell antigens positive (CD19, CD20, CD79a, PAX5)
Cutaneous localization: CD10, BCL2, BCL6, IRF4/MUM1, CD21/23, FDC
----------
T-cell antigens positive (CD2, CD3, CD5, CD7) and B-cell antigens negative 
 (Pax5)
Anaplastic morphology: CD30, CD15, PAX5, ALK, EBV-EBER, CD25
----------
T-cell antigens positive (CD2, CD3, CD5, CD7) and B-cell antigens negative
 (Pax5)
Cutaneous localization (non-anaplastic morphology)
CD2, CD5, CD7, CD4, CD8, CD30, CD56, beta-F1?, cytotoxic granule proteins 
 (CGP = perforin - Perf, granzyme B - GRB, TIA1), EBV-EBER, CD25, HTLV, CD123, 
CD68, TCL1
----------
Extranodal, noncutaneous localization (non-anaplastic morphology): ALK1, CD5,
CD7, CD4, CD8, CD30, CD56, beta-F1, 
 (CGP = perforin - Perf, granzyme B - GRB, TIA1), EBV-EBER
----------
Nodal localization (non-anaplastic morphology): CD5, CD4, CD8, CD30, ALK1, CD10,
BCL6, PD1, CD21, CD23, EBV-EBER, CD2, CD7, CD25, CD56, HTLV
									      
implements the algorithm of immunochemistry feature extraction
"
  (let* ((sas (annotations doc :type 'sentence-annotation))
	 find-per item-span)
;;;    (format t "sas has ~a sentences ~%" (length sas))
    (do ((i 0 (+ i 1)))
	((= i (length sas)))
      ;;      (format t "sentence ~a~%" i)
      (let* ((sa (elt sas i))
	     (cursor 0)
	     sc
	     pmatch?)
	;; quantity expression of immunochem features
	(clrhash *h-cand-chem-ph*)
	(when find-per
	  (multiple-value-setq (pmatch? cursor)
	    (find-percentage sa item-span cursor))
	  (setq find-per nil))
	
	(loop
	 (unless (< cursor (- (end sa) (start sa))) (return))
	 (let* (span)
	   (cond 
	    ((setf item-span (match-immun-histchem sa cursor t))
	     (setq sc (concatenate 'string sc (subseq (content sa)
						      cursor (car item-span))))
	     (setf cursor (cdr item-span))
	     (cond 
	      ;; if reach the eol
	      ((= cursor (- (end sa) (start sa)))
	       (setq find-per t))
	      (t 
	       (multiple-value-setq (pmatch? cursor)
		 (find-percentage sa item-span cursor))
	       (cond 
		((not pmatch?)
		 (cond 
		  ;; for "CD 10", an annotation with (start end) same as 
		  ;; "10" will be inserted.
		  ((not (self-assert-immun-histchem sa
						    (car item-span)
						    (cdr item-span)))
		   (setq span (concatenate 'string "<" 
					   (write-to-string (car item-span)) 
					   "-" 
					   (write-to-string (cdr item-span)) 
					   "> "))
		   (setq sc (concatenate 'string sc span))
		   (setf (gethash span *h-cand-chem-ph*) 
			 (subseq (content sa) (car item-span) (cdr item-span)))
		   )))))))
	    ((setf item-span (match-immun-histchem sa cursor nil))
	     (setq sc (concatenate 'string sc (subseq (content sa)
						      cursor (car item-span))))
	     (setf cursor (cdr item-span))
	     (cond
	      ((not (self-assert-immun-histchem sa 
						(car item-span) 
						(cdr item-span)))
	       (setq span (concatenate 'string "<" 
				       (write-to-string (car item-span)) 
				       "-" 
				       (write-to-string (cdr item-span)) 
				       "> "))
	       (setq sc (concatenate 'string sc span))
	       (setf (gethash span *h-cand-chem-ph*) 
		     (subseq (content sa) (car item-span) (cdr item-span)))      
	       )))
	    (t 
	     (setq sc (concatenate 'string sc (subseq (content sa) cursor)))
	     (setf cursor (- (end sa) (start sa))))))
	 )
	(predict sc)
	;; used for assertion classification, defined in mnegex.cl
	(assert (= (hash-table-count *h-pred-chem-ph*)
		   (hash-table-count *h-cand-chem-ph*))
		()
		"Error: s(*h-pred-chem-ph*) ~a /= s(*h-cand-chem-ph*) ~a~%"
		(hash-table-count *h-pred-chem-ph*)
		(hash-table-count *h-cand-chem-ph*))
	
	(with-hash-table-iterator
	 (entry *h-pred-chem-ph*)
	 (labels ((try (got-one &optional key val)
		       (when got-one
			 (dolist (w (rewrite (gethash key *h-cand-chem-ph*)))
			   (let* ((match (match-re "(\\d+)-(\\d+)" key
						   :return :match))
				  (start (+ (start sa) 
					    (parse-integer 
					     (re-submatch match nil nil 1))))
				  (end (+ (start sa) 
					  (parse-integer
					   (re-submatch match nil nil 2))))
				  (ica (make-instance 'immuchem-annotation 
						      :document (document sa)
						      :start start
						      :end end
						      :data (unify-ic w)
						      :positivity val)))
			     (add-annotation doc ica)))
			 (multiple-value-call #'try (entry)))))
	   (multiple-value-call #'try (entry)))))))
  (add-analysis doc :ganalysis 'gimmuchem))

(defmethod find-percentage ((sa sentence-annotation)
			    (item-span cons)
			    (cursor integer))
  "At some point, you need to pay attention to the _or equal to_ expression.
Output
----------
Whether matched
The new cursor"
  (let* ((re (concatenate 'string 
			  "^\\s*(<=?|>=?|=|"
			  "more\\s*than|less\\s*than|greater\\s*than|smaller\\s*than)?"
			  "\\s*((\\d)+(\\.\\d+)?\\s*%?)"))
	 (sen (content sa))
	 (doc (document sa))
	 (sbase (start sa))
	 (item (replace-re (subseq (content doc) 
				   (car item-span) (cdr item-span))
			   "(^\\s+|\\s+$)" ""))
	 category sub1 sub2)
    (multiple-value-bind (match? iwhole isub1 isub2)
	(match-re re sen :start cursor :return :index)
      (declare (ignore iwhole))
      (setf sub1 (replace-re (subseq (content sa) (car isub1) (cdr isub1))
			     "(^\\s+|\\s+$)" "")
	    sub2 (replace-re (subseq (content sa) (car isub2) (cdr isub2))
			     "(^\\s+|\\s+$)" ""))
      (when match?
	(cond
	 ((match-re "(<=?|less\\s*than|smaller\\s*than)" sub1)
	  (setf category "quantity_<"))
	 ((match-re "(>=?|more\\s*than|greater\\s*than)" sub1)
	  (setf category "quantity_>"))
	 (t
	  (setf category "quantity_=")))
	(add-annotation doc
			(make-instance 'item-content-pair
				       :document doc
				       :start (+ sbase (car item-span))
				       :end (+ sbase (cdr isub2))
				       :data (replace-re (format nil "~a_~a" item category)
							 "\\s+" "_")
				       :content sub2)))
      (values match? (or (cdr isub2) cursor)))))


(defun extract-item-content-pair (doc &key (mask-secs nil))
  (let* (fv)
    (dolist (icp (annotations doc :type 'item-content-pair))
      (let* ((secs (annotations-spanning icp :type 'section-annotation))
	     (nsecs (mapcar #'(lambda (a) (data a)) secs)))
	(when (intersection mask-secs nsecs :test #'equalp)
	  (go end1)))
      (let* ((item (data icp))
	     (content (content icp)))
	
	(unless (match-re "quality$" item)
	  (pushnew (cons (concatenate 'string "ICP_" item) content) fv
		   :test #'equalp)))
      end1)
    fv))

(defun get-karyotype-aberrations (ka-id)
  (let* ((ab-str (caar (latesql "SELECT decoded FROM decoder 
                                 WHERE type='karyotype' AND ann_id=~a"
				(sq ka-id))))
	 (abs (and ab-str 
		   (not (match-re "(^NV|error)" ab-str))
		   (split-re "\\n+" ab-str))))
    (setf abs (mapcar #'(lambda (a) (replace-re a "(^\\s+|,?\\s+$)" "")) abs))
    (remove-if #'(lambda (a) (equalp "" a)) abs)))

(defun extract-karyotype (doc)
  (let* ((sas (annotations doc :type 'section-annotation
			   :filter #'(lambda (a) 
				       (equalp "karyotype" (data a)))))
	 ksa-id
	 abs)
    (dolist (sa sas)
      (setf ksa-id (id sa))
      (dolist (ab (get-karyotype-aberrations ksa-id))
	(pushnew ab abs :test #'equalp)))
    abs))



