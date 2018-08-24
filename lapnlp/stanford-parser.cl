;;; -*- Mode: Lisp; Package: stanford-parser; -*-
#||
yluo - 08/16/2018 clean and reorganization
yluo - 06/10/2015 rewrite using asdf framework
yluo - 04/01/2012 creation 

Note: I think it's in general a good idea to qualify symbols not in the main
package rather than use-package unless you are sure there won't be name 
conflict.
||#

(defpackage :stanford-parser
  (:nicknames :sp)
  (:use #+allegro :excl :common-lisp :javatools.jlinker :jc :umlsize)
  (:import-from :late 
		"accession-number-pattern"
		"add-analysis"
		"add-annotation"
		"analyses"
		"annotations"
		"annotations-spec"
		"content"
		"data"
		"date-pattern"
		"date-range-pattern"
		"dep-deps"
		"dirty"
		"document"
		"discrete-num"
		"discretize-pct-th"
		"maskedp"
		"end"
		"ganalysis"
		"gkey"
		"gov-deps"
		"gsetting"
		"gsw-ver"
		"gtype"
		"id"
		"in-no-token-area?"
		"annotations-share"
		"parse-annotation-stanford"
		"parse-annotation-stanford-hier-tagged"
		"parse-annotation-stanford-tagged"
		"parse-annotation-stanford-tokenized-constrained"
		"parse-node-stanford-tokenized-constrained"
		"phone-number-pattern"
		"pnode-from-token"
		"pnode-pos-tag"
		"pos-tag"
		"pos-tag-stanford"
		"pos-tag-stanford-umls"
		"range-of-ratios-pattern"
		"ratio-of-ranges-pattern"
		"sentence-annotation"
		"sp-token"
		"start"
		"time-pattern"
		"noun-ph?"
		"annotation-equal"
		"surface-pn"
		"coded-substitute-pn"
		"if-list-substitute-pn"
		"if-adj-substitute-pn"
		"coded-text"
		"if-list"
		"coded-pn"
		"if-list-pn"
		"if-adj-pn"
		"get-annotation"
		"find-annotation"
		"noun-pn?"
		"adj-pn?"
		"adv-pn?"
		"vbn-pn?"
		"non-stop?"
		"annotations-on-spec"
		"annotations-spanning-spec"
		"annotation-lessp"
		"substitute-pn"
		"raw-pns"
		"surface-pns"
		"range-adj-pns"
		"range-adj-substitute-pns"
		"cross-ref-annotation"
		"coded"
		"if-adj"
		"range-adj"
		"ptb-pl-nounp"
		"ptb-nounp"
		"ptb-adjp"
		)
  (:import-from :norm
		"medg-norm"))

;;; in-package has effect at both compile time and load time
(in-package :stanford-parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :stanford-parser-wrapper-jarlist "late:;stanford-parser-wrapper-jarlist"))

(defstruct (deprep
	    (:type vector)
	    (:conc-name d-))
  govid rel depid (copies 1))

(defun load-parser ()
  "Defer parser loading to when it's needed."
  (unless *parser*
    (setf *parser* (spwrapper.ParserMEDG:ParserMEDG *stanford-parser-grammar*))))

(defun deps-weak= (l1 l2)
  (and (= (d-govid l1) (d-govid l2))
       (= (d-depid l1) (d-depid l2))))

(defun insert-gov (pn gov)
  (assert (= (id pn) (d-govid gov))
	  ()
	  "gov id should agree")
  (pushnew gov (gov-deps pn) :test #'deps-weak=)
  (setf (dirty pn) t))

(defun insert-dep (pn dep)
  (assert (= (id pn) (d-depid dep))
	  ()
	  "dep id should agree")
  (pushnew dep (dep-deps pn) :test #'deps-weak=)
  (setf (dirty pn) t))

(defun delete-gov (pn gov)
  (setf (gov-deps pn) (remove-if #'(lambda (a) (equalp gov a)) (gov-deps pn)))
  (setf (dirty pn) t))

(defun delete-dep (pn dep)
  (setf (dep-deps pn) (remove-if #'(lambda (a) (equalp dep a)) (dep-deps pn)))
  (setf (dirty pn) t))

(defmethod tokenize ((sen sentence-annotation))
  "Tokenize a sentence string."
  
  (load-parser)
  (let* ((tokens (spwrapper.ParserMEDG:tokenize *parser* (content sen)))
	 (doc (document sen))
	 (offset (start sen))
	 segs)
    (dotimes (i (spwrapper.ParserMEDG:tokensLength tokens))
      (let* ((token (spwrapper.ParserMEDG:getToken tokens i)))
	(push (cons (+ offset (spwrapper.ParserMEDG:tokenStart token))
		    (+ offset (spwrapper.ParserMEDG:tokenEnd token)))
	      segs)))
    (setf segs (nreverse segs))
    (dolist (seg segs)
      (let ((ta (make-instance 'sp-token
			       :document doc
			       :start (car seg)
			       :end (cdr seg)
			       :sw-ver (gsw-ver 'gtokenize)
			       :setting (gsetting 'gtokenize))))
	(unless (in-no-token-area? ta)
	  (add-annotation doc ta)
	  (add-annotation sen ta))))))

(defun pnode->str (pnode)
  "Converts the pnode annotation into string that is ready to pass
to Stanford Parser."
  (let* ((pnode-str (data pnode)))
    (if (and (gkey 'gparse 'gallcap2normal)
	     (not (equalp "-LRB-" pnode-str))
	     (not (equalp "-RRB-" pnode-str)))
	(setf pnode-str	(string-downcase pnode-str)))
    pnode-str))



(defun pnodes->str-arr (pnodes)
  "Converts the token annotations list into string array that is ready to pass
to Stanford Parser."
  (let* ((pnode-str-arr (make-array (list (length pnodes))))
	 pnode-str)
    (do ((pnodes pnodes (cdr pnodes))
	 (i 0 (incf i)))
	((null pnodes))
      (setf pnode-str (pnode->str (car pnodes)))
      (setf (aref pnode-str-arr i) pnode-str))
    pnode-str-arr))

(defun tags->str-arr (tas)
  "Converts the tag annotations list into string array that is ready to pass
to Stanford Parser."
  (let* ((tag-str-arr (make-array (list (length tas)))))
    (do ((tas tas (cdr tas))
	 (i 0 (incf i)))
	((null tas))
      (setf (aref tag-str-arr i) (format nil "~a" (data (car tas)))))
    tag-str-arr))

(defmethod parse-tokenized ((sen sentence-annotation)
			    &key (tag-only? nil))
  "Assume that the sentence has already been tokenized.
Note
======
If you do not parse from original sentence text, getTaggedWordBegin will be 
meaningless, so does getTaggedWordEnd."
  ;; (format t "Parse sentence ~a:~%~a" (id sen) (content sen))
  (let* ((tas (annotations-spec sen :type (gtype 'gtoken-type))))
    (unless tas
      (format t "~&Warning: Empty token sentence ~a~%" sen)
      (return-from parse-tokenized nil)))
  (load-parser)
  (let* ((style "penn,typedDependenciesCollapsed") 
	 (doc (document sen))
	 (tokens (annotations-spec sen :type (gtype 'gtoken-type)))
	 (pnodes (mapcar #'pnode-from-token tokens))
	 (token-str-arr (pnodes->str-arr pnodes))

	 (parse (spwrapper.ParserMEDG:parseTokenized *parser* token-str-arr))
	 (tws (spwrapper.ParserMEDG:extractTags parse))
	 (tdl (spwrapper.ParserMEDG:extractDeps *parser* parse))
	 (res-text (spwrapper.ParserMEDG:resultText parse style))
	 (i 0))
    (dolist (pnode pnodes)
      (let* ((tw (spwrapper.ParserMEDG:getTaggedWord tws i))
	     (tag (make-instance 'pos-tag-stanford
				 :document doc
				 :start (start pnode)
				 :end (end pnode)
				 :data (spwrapper.ParserMEDG:getTagString tw)
				 :sw-ver (gsw-ver 'gparse)
				 :setting (gsetting 'gparse))))
	(unless tag-only?
	  (add-annotation doc pnode)
	  (add-annotation sen pnode))
	(add-annotation doc tag)
	(add-annotation sen tag)
	(incf i)))
    
    (unless tag-only?
      (dotimes (i (spwrapper.ParserMEDG:typedDepsLength tdl))
	(let* ((td (spwrapper.ParserMEDG:getTypedDep tdl i))
	       (rel (spwrapper.ParserMEDG:typedDepRel td))
	       (govid (- (spwrapper.ParserMEDG:typedDepGovInd td) 1))
	       (depid (- (spwrapper.ParserMEDG:typedDepDepInd td) 1))
	       (dep (make-deprep :govid govid :rel rel :depid depid)))

	  (when (>= govid 0)
	    (push dep (gov-deps (elt pnodes govid)))
	    (setf (dirty (elt pnodes govid)) t)
	    (push dep (dep-deps (elt pnodes depid)))
	    (setf (dirty (elt pnodes depid)) t))))
      
      (let* ((pann (make-instance (gtype 'gparse-type)
				  :document doc
				  :start (start sen)
				  :end (end sen)
				  :res-text res-text
				  :parsing (format nil "parse-tokenized-~a" 
						   *stanford-parser-version*)
				  :sw-ver (gsw-ver 'gparse)
				  :setting (gsetting 'gparse)
				  :style style)))
	(add-annotation doc pann)
	(add-annotation sen pann)))))

(defun tag-constraint (pnode &aux pnode-str)
  "Assign POS tag constranit on tokens for Stanford Parser."
  (setf pnode-str (pnode->str pnode))
  (cond 
   ;; count for CD19+ etc.
   ((match-re "^.+[\\+\\-]$" pnode-str)
    "JJ")
   ((equalp "'s" pnode-str)
    "")
   ((annotations pnode :type 'date-pattern)
    "CD")
   ((annotations pnode :type 'accession-number-pattern)
    "CD")
   ((annotations pnode :type 'time-pattern)
    "CD")
   ((annotations pnode :type 'date-range-pattern)
    "CD")
   ((annotations pnode :type 'phone-number-pattern)
    "CD")
   ((annotations pnode :type 'range-of-ratios-pattern)
    "CD")
   ((annotations pnode :type 'ratio-of-ranges-pattern)
    "CD")
   ;; as of opennlp 1.4.3, there is convertToken hack in 
   ;; TreebankParser.java 
   ((string-equal "{" pnode-str)
    "-LCB-")
   ((string-equal "[" pnode-str)
    "-LRB-")
   ((string-equal "(" pnode-str)
    "-LRB-")
   ((string-equal "}" pnode-str)
    "-RCB-")
   ((string-equal "]" pnode-str)
    "-RRB-")
   ((string-equal ")" pnode-str)
    "-RRB-")
   (t
    (umlsize::online-lookup pnode-str))))

(defmethod parse-tokenized-constrained ((sen sentence-annotation)
					&key (tag-only? nil))
  "Assume that the sentence has already been tokenized. We look up the UMLS
Specialist Lexicon to determine the possbile tags a word can have
Note
======
If you do not parse from original sentence text, getTaggedWordBegin will be 
meaningless, so does getTaggedWordEnd."
  ;; (format t "Parse sentence ~a:~%~a" (id sen) (content sen))
  (let* ((tas (annotations-spec sen :type (gtype 'gtoken-type))))
    (unless tas
      (format t "~&Warning: Empty token sentence ~a~%" sen)
      (return-from parse-tokenized-constrained nil)))
  (load-parser)
  (let* ((style "penn,typedDependenciesCollapsed") 
	 (doc (document sen))
	 (tokens (annotations-spec sen :type (gtype 'gtoken-type)))
	 (pnodes (or (annotations sen :type (gtype 'gparse-node-type))
		     (mapcar #'pnode-from-token tokens)))
	 (token-str-arr (pnodes->str-arr pnodes))
	 (i 0)
	 tokidxes tagsets parse tws tdl res-text)
    ;; run over token string array
    (dolist (pnode pnodes)
      (unless (id pnode)
	(add-annotation doc pnode)
	(add-annotation sen pnode))
      (let* ((tagset (tag-constraint pnode)))
	(unless (equalp "" tagset)
	  (push tagset tagsets)
	  (push i tokidxes)))
      (incf i))
    (format t "tokidxes ~a~%" tokidxes)
    (format t "tagsets ~a~%" tagsets)
    (setf tagsets (coerce (nreverse tagsets) 'array))
    (setf tokidxes (make-array (length tokidxes) 
			       :element-type '(signed-byte 32) 
			       :initial-contents (nreverse tokidxes)))
    (format t "tokidxes arr ~s~%" tokidxes)
    (format t "tagsets arr ~s~%" tagsets)
    (format t "token-str-arr ~s~%" token-str-arr)
    (setf i 0)

    
    (setf parse (spwrapper.ParserMEDG:parseTokenizedWithConstraints 
		 *parser* token-str-arr tokidxes tagsets))
    (setf tws (spwrapper.ParserMEDG:extractTags parse))
    (setf tdl (spwrapper.ParserMEDG:extractDeps *parser* parse))
    (setf res-text (spwrapper.ParserMEDG:resultText parse style))
    
    (dolist (pnode pnodes)
      (let* ((tw (spwrapper.ParserMEDG:getTaggedWord tws i))
	     (tag (make-instance 'pos-tag-stanford
				 :document doc
				 :start (start pnode)
				 :end (end pnode)
				 :data (spwrapper.ParserMEDG:getTagString tw)
				 :sw-ver (gsw-ver 'gparse)
				 :setting (gsetting 'gparse))))
	
	(add-annotation doc tag)
	(add-annotation sen tag)
	(incf i)))
    
    (unless tag-only?
      (dotimes (i (spwrapper.ParserMEDG:typedDepsLength tdl))
	(let* ((td (spwrapper.ParserMEDG:getTypedDep tdl i))
	       (rel (spwrapper.ParserMEDG:typedDepRel td))
	       (govid (- (spwrapper.ParserMEDG:typedDepGovInd td) 1))
	       (depid (- (spwrapper.ParserMEDG:typedDepDepInd td) 1))
	       (dep (make-deprep :govid govid :rel rel :depid depid)))

	  (when (>= govid 0)
	    (push dep (gov-deps (elt pnodes govid)))
	    (setf (dirty (elt pnodes govid)) t)
	    (push dep (dep-deps (elt pnodes depid)))
	    (setf (dirty (elt pnodes depid)) t))))
      
      (let* ((pann (make-instance (gtype 'gparse-type)
				  :document doc
				  :start (start sen)
				  :end (end sen)
				  :res-text res-text
				  :parsing (format nil "parse-tokenized-~a" 
						   *stanford-parser-version*)
				  :sw-ver (gsw-ver 'gparse)
				  :setting (gsetting 'gparse)
				  :style style)))
	(add-annotation doc pann)
	(add-annotation sen pann)))))


(defmethod augment-pos-tag ((sen sentence-annotation)
			    &aux sp-poses sp-tas)
  "Augment pos tags by looking up in the UMLS specialist lexicon"
  
  (setf sp-poses (annotations sen 
			      :type (gtype 'gtag-type)
			      :sw-ver (gsw-ver 'gtagize))
	sp-tas (annotations-spec sen :type (gtype 'gtoken-type)))
  (format t "~&sen ~a has ~a sp-poses~%" (id sen) (length sp-poses))
  (do ((sp-poses sp-poses (cdr sp-poses))
       (sp-tas sp-tas (cdr sp-tas)))
      ((null sp-poses))
    (let* ((sp-pos (car sp-poses))
	   (sp-ta (car sp-tas))
	   (ta-str (content sp-ta))
	   (doc (document sen))
	   aug-pos-str aug-pos)
      (cond
       ((equal (list "noun") (w->sp-pos ta-str))
	(let* ((agrs (w-sca->sp-agr ta-str "noun")))
	  (cond 
	   ((or (member "count(thr_plur)" agrs  :test #'string=)
		(member "count(fst_plur)" agrs  :test #'string=))
	    (setf aug-pos-str "NNS"))
	   (t
	    (setf aug-pos-str "NN")))))
       
       ((equal (list "adj") (w->sp-pos ta-str))
	(let* ((agrs (w-sca->sp-agr ta-str "adj")))
	  (cond 
	   ((member "comparative" agrs  :test #'string=)
	    (setf aug-pos-str "JJR"))
	   ((member "superlative" agrs  :test #'string=)
	    (setf aug-pos-str "JJS"))
	   (t
	    (setf aug-pos-str "JJ")))))
       
       ((equal (list "verb") (w->sp-pos ta-str))
	(let* ((agrs (w-sca->sp-agr ta-str "verb")))
	  (cond 
	   ((member "past_part" agrs :test #'string=)
	    (setf aug-pos-str "VBN"))
	   ((member "past" agrs :test #'string=)
	    (setf aug-pos-str "VBD"))
	   ((member "pres(thr_sing)" agrs :test #'string=)
	    (setf aug-pos-str "VBZ"))
	   ((member "pres_part" agrs :test #'string=)
	    (setf aug-pos-str "VBG"))
	   ((member "pres(fst_sing,fst_plur,thr_plur,second)" agrs 
		    :test #'string=)
	    (setf aug-pos-str "VBP"))
	   ((member "infinitive" agrs  :test #'string=)
	    (setf aug-pos-str "VB")))))
       
       ((equal (list "adv") (w->sp-pos ta-str))
	(let* ((agrs (w-sca->sp-agr ta-str "adv")))
	  (cond 
	   ((member "comparative" agrs  :test #'string=)
	    (setf aug-pos-str "RBR"))
	   ((member "superlative" agrs  :test #'string=)
	    (setf aug-pos-str "RBS"))
	   (t
	    (setf aug-pos-str "RB"))))))
      
      (cond 
       (aug-pos-str
	(format t "~&old: ~a; new: ~a~%" (data sp-pos) aug-pos-str))
       (t
	(setf aug-pos-str (format nil "~a" (data sp-pos)))))
      
      (setf aug-pos (make-instance 'pos-tag-stanford
				   :document doc
				   :start (start sp-pos)
				   :end (end sp-pos)
				   :data aug-pos-str
				   :sw-ver (gsw-ver 'gtagize)
				   :setting (gsetting 'gtagize)))
      (add-annotation doc aug-pos)
      (add-annotation sen aug-pos))))

(defmethod augment-pos-tag ((doc document))
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (augment-pos-tag sen)))

(defmethod parse-tagged ((sen sentence-annotation))
  "Assume that the sentence has already been tokenized and tagged."
  
  (let* ((tas (annotations-spec sen :type (gtype 'gtoken-type)))
	 (pos (annotations-spec sen :type (gtype 'gtag-type))))
    (unless (and tas pos)
      (format t "~&Warning: Empty token sentence ~a~%" sen)
      (return-from parse-tagged nil)))
  (load-parser)
  (let* ((doc (document sen))
	 (style "penn,typedDependenciesCollapsed")
	 (tokens (annotations-spec sen :type (gtype 'gtoken-type)))
	 (pnodes (mapcar #'pnode-from-token tokens))
	 (tags (annotations-spec sen :type (gtype 'gtag-type)))
	 (pnode-str-arr (pnodes->str-arr pnodes))
	 (tag-str-arr (tags->str-arr tags))
	 (sen-len (length pnodes))
	 parse tdl res-text)
;;;	(format t "~&sen-len: ~a~%" sen-len)
    (dolist (pnode pnodes)
      (add-annotation doc pnode)
      (add-annotation sen pnode))
    (when (< sen-len 100)
      (setf parse (spwrapper.ParserMEDG:parseTagged *parser* pnode-str-arr 
						    tag-str-arr))
      (setf tdl (spwrapper.ParserMEDG:extractDeps *parser* parse))
      (setf res-text (spwrapper.ParserMEDG:resultText parse style))
      (dotimes (i (spwrapper.ParserMEDG:typedDepsLength tdl))
	(let* ((td (spwrapper.ParserMEDG:getTypedDep tdl i))
	       (rel (spwrapper.ParserMEDG:typedDepRel td))
	       (govid (1- (spwrapper.ParserMEDG:typedDepGovInd td)))
	       (depid (1- (spwrapper.ParserMEDG:typedDepDepInd td)))
	       (dep (make-deprep :govid govid :rel rel :depid depid)))
	  (when (>= govid 0)
	    (push dep (gov-deps (elt pnodes govid)))
	    (setf (dirty (elt pnodes govid)) t)
	    (push dep (dep-deps (elt pnodes depid)))
	    (setf (dirty (elt pnodes depid)) t))))
      (let* ((pann (make-instance (gtype 'gparse-type)
				  :document doc
				  :start (start sen)
				  :end (end sen)
				  :res-text res-text
				  :parsing (format nil "parse-tagged-~a" 
						   *stanford-parser-version*)
				  :style style
				  :sw-ver (gsw-ver 'gparse)
				  :setting (gsetting 'gparse))))
	(add-annotation doc pann)
	(add-annotation sen pann)))))

(defmethod parse-hier-tokenized ((sen sentence-annotation)
				 &key (tag-only? nil))
  "Assume that the sentence has already been tokenized and tagged.
Note
======
If you do not parse from original sentence text, getTaggedWordBegin will be 
meaningless, so does getTaggedWordEnd."
  ;; (format t "Parse sentence ~a:~%~a" (id sen) (content sen))
  (let* ((tas (annotations-spec sen :type (gtype 'gtoken-type))))
    (unless tas
      (format t "~&Warning: Empty token sentence ~a~%" sen)
      (return-from parse-hier-tokenized nil)))
  (load-parser)
  (let* ((style "penn,typedDependenciesCollapsed") 
	 (doc (document sen))
	 (pnodes (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	 (pnode-str-arr (pnodes->str-arr pnodes))
	 (parse (spwrapper.ParserMEDG:parseTokenized *parser* pnode-str-arr))
	 (tws (spwrapper.ParserMEDG:extractTags parse))
	 (tdl (spwrapper.ParserMEDG:extractDeps *parser* parse))
	 (res-text (spwrapper.ParserMEDG:resultText parse style))
	 (i 0))
    
    (dolist (pnode pnodes)
      (let* ((tw (spwrapper.ParserMEDG:getTaggedWord tws i)))
	(setf (pnode-pos-tag pnode) (spwrapper.ParserMEDG:getTagString tw))
	(incf i)))
    
    (unless tag-only?
      (dotimes (i (spwrapper.ParserMEDG:typedDepsLength tdl))
	(let* ((td (spwrapper.ParserMEDG:getTypedDep tdl i))
	       (rel (spwrapper.ParserMEDG:typedDepRel td))
	       (govid (- (spwrapper.ParserMEDG:typedDepGovInd td) 1))
	       (depid (- (spwrapper.ParserMEDG:typedDepDepInd td) 1))
	       (dep (make-deprep :govid govid :rel rel :depid depid)))

	  (when (>= govid 0)
	    (push dep (gov-deps (elt pnodes govid)))
	    (setf (dirty (elt pnodes govid)) t)
	    (push dep (dep-deps (elt pnodes depid)))
	    (setf (dirty (elt pnodes depid)) t))))
      
      (let* ((pann (make-instance (gtype 'ghier-parse-type)
				  :document doc
				  :start (start sen)
				  :end (end sen)
				  :res-text res-text
				  :parsing (ganalysis 'ghier-parse)
				  :sw-ver (gsw-ver 'ghier-parse)
				  :setting (gsetting 'ghier-parse)
				  :style style)))
	(add-annotation doc pann)
	(add-annotation sen pann)))))



(defmethod parse-hier-tagged ((sen sentence-annotation))
  "Assume that the sentence has already been tokenized and tagged.
Use the database id as id in the dep"
  
  (let* ((tas (annotations-spec sen :type (gtype 'gtoken-type)))
	 (pos (annotations-spec sen :type (gtype 'gtag-type)))
	 (pnodes (annotations-spec sen :type (gtype 'ghier-parse-node-type))))
    (unless (and tas pos)
      (format t "~&Warning: Empty token sentence ~a~%" sen)
      (return-from parse-hier-tagged nil))
    (assert pnodes
	    ()
	    "no pnodes?")
    (when (= 1 (length pnodes))
      (let* ((pn (car pnodes)))
	(format t "[parse-hier-tagged] ~a ~a ~a ~a ~a" pn (coded pn) (if-list pn) (if-adj pn) (range-adj pn)))))
  (load-parser)
  (let* ((doc (document sen))
	 (style "penn,typedDependenciesCollapsed")
	 (pnodes (annotations-spec sen :type (gtype 'ghier-parse-node-type)
				   :filter #'surface-pn))
	 (tags (mapcar #'pnode-pos-tag pnodes))
	 (pnode-str-arr (pnodes->str-arr pnodes))
	 (tag-str-arr (coerce tags 'array))
	 (parse (spwrapper.ParserMEDG:parseTagged *parser* pnode-str-arr 
						  tag-str-arr))
	 (tdl (spwrapper.ParserMEDG:extractDeps *parser* parse))
	 (res-text (spwrapper.ParserMEDG:resultText parse style)))
    
    (dotimes (i (spwrapper.ParserMEDG:typedDepsLength tdl))
      (let* ((td (spwrapper.ParserMEDG:getTypedDep tdl i))
	     (rel (spwrapper.ParserMEDG:typedDepRel td))
	     (govid (1- (spwrapper.ParserMEDG:typedDepGovInd td)))
	     (depid (1- (spwrapper.ParserMEDG:typedDepDepInd td)))
	     govn depn dep)
	(when (>= govid 0)
	  (setf govn (elt pnodes govid))
	  (setf govid (id govn))
	  (setf depn (elt pnodes depid))
	  (setf depid (id depn))
	  (setf dep (make-deprep :govid govid :rel rel :depid depid))
	  (insert-gov govn dep)
	  (insert-dep depn dep))))
    (let* ((pann (make-instance (gtype 'ghier-parse-type)
				:document doc
				:start (start sen)
				:end (end sen)
				:res-text res-text
				:parsing (ganalysis 'ghier-parse)
				:style style
				:sw-ver (gsw-ver 'gparse)
				:setting (gsetting 'gparse))))
      (add-annotation doc pann)
      (add-annotation sen pann)))

  ;; post processing
  (pp-prep-of-parse-hier-tagged sen) ;; needs to be before handling +, - etc.
  (pp-coded-parse-hier-tagged sen)
  (pp-decode-adj-parse-hier-tagged sen)
  (pp-range-adj-parse-hier-tagged sen)
  (pp-if-adj-parse-hier-tagged sen)
  (pp-if-list-parse-hier-tagged sen)
  ;; (pp-noun-adj-parse-hier-tagged sen) ;; unifying
  
  
  (pp-noun-mod-parse-hier-tagged sen)
  (pp-nsubj-be-parse-hier-tagged sen)
  (pp-ki67-parse-hier-tagged sen)
  (pp-myc-parse-hier-tagged sen)
  (pp-express-parse-hier-tagged sen)
  )

(defun adjs-n? (pos-tags)
  (when pos-tags
    (let* ((len (length pos-tags))
	   (leads (subseq pos-tags 0 (1- len)))
	   (last (elt pos-tags (1- len))))
      (setf leads (remove-if #'(lambda (a) (match-re "^\\W$" a)) leads))
      (and (> (length leads) 1)
	   (every #'(lambda (a) (match-re "^JJ" a)) leads)
	   (match-re "^NN" last)))))






(defun output-deps (pn)
  (format t "~a~%" pn)
  (format t "dep-deps: ~{~a~^~%~}~%" (dep-deps pn))
  (format t "gov-deps: ~{~a~^~%~}~%" (gov-deps pn)))



(defun copy-links (sub-pn pn)
  (setf (dirty pn) t)
  
  (let* ((pnid (id pn))
	 (doc (document sub-pn))
	 new-dep new-gov govpn deppn new-rel)
    (dolist (dep (dep-deps sub-pn))
      (when (/= pnid (d-govid dep))
	(setf govpn (find-annotation (d-govid dep) doc))
	;; insert new dep to both gov and dep nodes
	(setf new-dep (make-deprep :govid (d-govid dep) :rel (d-rel dep) :depid pnid)) 
	
	(insert-dep pn new-dep)
	(insert-gov govpn new-dep)))
    (dolist (gov (gov-deps sub-pn))
      (when (/= pnid (d-depid gov))
	(setf deppn (find-annotation (d-depid gov) doc))
	;; insert new dep to both gov and dep nodes
	(setf new-rel (d-rel gov))
	(setf new-gov (make-deprep :govid pnid :rel new-rel :depid (d-depid gov)))
	
	(insert-gov pn new-gov)
	(insert-dep deppn new-gov)))

    ))

(defun transfer-links (sub-pn pn)
  (copy-links sub-pn pn)
  (setf (dirty sub-pn) t)
  (disconnect-pn sub-pn))

(defmethod pp-coded-parse-hier-tagged ((sen sentence-annotation))
  #||post processing for coded text after parse-hier-tagged.
  Note: this will invalidate the saved text format parse-result extracted 
  directly from Stanford Parser
  Note: you have to use find-annotation instead of get-annotation as find-annotation pulls up everything from memory while get-annotation reads everything from database.
  ||#
  
  (let* ((pntype (gtype 'ghier-parse-node-type))
	 (sub-pns (annotations-spec sen :type pntype 
				    :filter #'coded-substitute-pn))
	 pns)

    (dolist (sub-pn sub-pns)
      (format t "~&[pp-coded-parse-hier-tagged] sub-pn: ")
      (output-deps sub-pn)
      (setf pns (annotations-spec sub-pn :type pntype :filter #'coded-pn))
      (dolist (pn pns)
	;; scan the modifiers
	(when (and (match-re "\\w" (content pn))
		   (not (equalp "and" (content pn))))
	  (copy-links sub-pn pn)
	  (format t "pn: ")
	  (output-deps pn)))

      (disconnect-pn sub-pn))))





(defmethod pp-if-adj-parse-hier-tagged ((sen sentence-annotation))
  #||post processing for if adj text after parse-hier-tagged.
  Note: this will invalidate the saved text format parse-result extracted 
  directly from Stanford Parser
  Note: you have to use find-annotation instead of get-annotation as find-annotation pulls up everything from memory while get-annotation reads everything from database.
  ||#
  
  (let* ((doc (document sen))
	 (sub-pns (annotations-spec sen :type (gtype 'ghier-parse-node-type)
				    :filter #'if-adj-substitute-pn))
	 pns adj-pnid)
    (format t "~&sub-pns for sen ~a: ~a~%" (id sen) sub-pns)
    (dolist (sub-pn sub-pns)
      (format t "~&[pp-if-adj-parse-hier-tagged] sub-pn: ")
      (output-deps sub-pn)
      (setf pns (annotations-spec sub-pn :type (gtype 'ghier-parse-node-type)
				  :filter #'if-adj-pn))
      ;; must first encounter the adj pn which is at last
      (dolist (pn (nreverse pns))
	;; scan the modifiers
	(cond
	 ((adj-pn? pn) ; transfer all sub-pn's link to this adj pn
	  (setf adj-pnid (id pn))
	  (copy-links sub-pn pn)
	  (format t "adj pn: ")
	  (output-deps pn))
	 
	 ((adv-pn? pn)
	  (let* ((adj-pn (find-annotation adj-pnid doc))
		 (l (make-deprep :govid adj-pnid :rel "advmod" :depid (id pn))))
	    (insert-gov adj-pn l)
	    (insert-dep pn l)))
	 
	 ((noun-pn? pn)
	  (let* ((adj-pn (find-annotation adj-pnid doc))
		 (l (make-deprep :govid adj-pnid :rel "noun_adj" :depid (id pn))))
	    (insert-gov adj-pn l)
	    (insert-dep pn l)))))
      (format t "~&disconnecting ~a~%" sub-pn)
      (disconnect-pn sub-pn))))




(defmethod pp-range-adj-parse-hier-tagged ((sen sentence-annotation))
  #||post processing for range adj text after parse-hier-tagged.
  Note: this will invalidate the saved text format parse-result extracted 
  directly from Stanford Parser
  ||#
  
  (let* ((doc (document sen))
	 (sub-pns (range-adj-substitute-pns sen))
	 pns adj-pnid)
    ;; adj-pnid1 is before to, adj-pnid2 is after to
    (dolist (sub-pn sub-pns)
      (format t "~&[pp-range-adj-parse-hier-tagged] sub-pn: ")
      (output-deps sub-pn)
      (setf pns (range-adj-pns sub-pn))
      ;; must first encounter the adj pn which is at last
      (dolist (pn (nreverse pns))
	;; scan the modifiers
	(cond
	 ;; transfer all sub-pn's link to this adj pn
	 ((or (adj-pn? pn) (vbn-pn? pn))
	  (setf adj-pnid (id pn))
	  (copy-links sub-pn pn)
	  (output-deps pn))
	 
	 ((adv-pn? pn)
	  (let* ((adj-pn (find-annotation adj-pnid doc))
		 (l (make-deprep :govid adj-pnid :rel "advmod" :depid (id pn))))
	    (insert-gov adj-pn l)
	    (insert-dep pn l)))))
      ;; (setf pns (range-adj-pns sub-pn))
      ;; (dolist (pn1 pns)
      ;; 	(dolist (pn2 pns)
      ;; 	  (when (and (adj-pn? pn1) (adj-pn? pn2) 
      ;; 				 (annotation-lessp pn1 pn2)
      ;; 				 (not (connected? pn1 pn2)))
      ;; 		(let* ((l (make-deprep :govid (id pn2) 
      ;; 							   :rel "nn" 
      ;; 							   :depid (id pn1))))
      ;; 		  (insert-gov pn2 l)
      ;; 		  (insert-dep pn1 l)
      ;; 		  (setf (dirty pn1) t)
      ;; 		  (setf (dirty pn2) t)))))
      (disconnect-pn sub-pn))))


(defmethod pp-if-list-parse-hier-tagged ((sen sentence-annotation))
  #||post processing for if list text after parse-hier-tagged.
  Note: this will invalidate the saved text format parse-result extracted 
  directly from Stanford Parser
  Note: you have to use find-annotation instead of get-annotation as find-annotation pulls up everything from memory while get-annotation reads everything from database.
  ||#
  
  (let* ((sub-pns (annotations-spec sen :type (gtype 'ghier-parse-node-type)
				    :filter #'if-list-substitute-pn))
	 pns)
    (dolist (sub-pn sub-pns)
      (format t "~&[pp-if-list-parse-hier-tagged] sub-pn: ")
      (output-deps sub-pn)
      (setf pns (annotations-spec sub-pn :type (gtype 'ghier-parse-node-type)
				  :filter #'if-list-pn))
      (dolist (pn pns)
	;; scan the modifiers
	(when (and (match-re "\\w" (content pn))
		   (not (equalp "and" (content pn)))
		   (not (equalp "or" (content pn)))
		   ;; (not (ptb-pl-nounp (pnode-pos-tag pn)))
		   (if-tuis pn))
	  (format t "sub-pn: ")
	  (output-deps sub-pn)
	  (copy-links sub-pn pn)
	  (format t "pn: ")
	  (output-deps pn)))
      (disconnect-pn sub-pn))))

(defun disconnect-pns (pn1 pn2)
  (setf (dirty pn1) t)
  (setf (dirty pn2) t)
  (dolist (gov (gov-deps pn1))
    (when (= (d-depid gov) (id pn2))
      (delete-gov pn1 gov)
      (delete-dep pn2 gov)))
  (dolist (dep (dep-deps pn1))
    (when (= (d-govid dep) (id pn2))
      (delete-gov pn2 dep)
      (delete-dep pn1 dep))))

(defun disconnect-link (link sen)
  (let* ((govpn (find-annotation (d-govid link) sen))
	 (deppn (find-annotation (d-depid link) sen)))
    (delete-gov govpn link)
    (delete-dep deppn link)))

(defun connected? (pn1 pn2)
  (let* (ans)
    (dolist (gov (gov-deps pn1))
      (when (= (d-depid gov) (id pn2))
	(setf ans t)))
    (dolist (dep (dep-deps pn1))
      (when (= (d-govid dep) (id pn2))
	(setf ans t)))
    ans))

(defun disconnect-pn (pn1)
  (let* ((doc (document pn1))
	 pn2)
    (setf (dirty pn1) t)
    (dolist (gov (gov-deps pn1))
      (setf pn2 (find-annotation (d-depid gov) doc))
      (setf (dirty pn2) t)
      (delete-gov pn1 gov)
      (delete-dep pn2 gov))
    
    (dolist (dep (dep-deps pn1))
      (setf pn2 (find-annotation (d-govid dep) doc))
      (setf (dirty pn2) t)
      (delete-gov pn2 dep)
      (delete-dep pn1 dep))))

(defmethod pp-nsubj-be-parse-hier-tagged ((sen sentence-annotation))
  (let* ((pntype (gtype 'ghier-parse-node-type))
	 (pns (annotations-spec sen :type pntype))
	 subjid pnsubj)
    (dolist (pn pns)
      (setf subjid nil)
      (when (equalp "be" (medg-norm (content pn)))
	(dolist (gov (gov-deps pn))
	  (when (equalp "nsubj" (d-rel gov))
	    (setf subjid (d-depid gov))))
	(when subjid
	  (setf pnsubj (find-annotation subjid sen))
	  (format t "~&[pp-nsubj-be-parse-hier-tagged] pnsubj before: ")
	  (output-deps pnsubj)
	  (format t "~&[pp-nsubj-be-parse-hier-tagged] pn before: ")
	  (output-deps pn)
	  
	  (disconnect-pns pn pnsubj)
	  (transfer-links pn pnsubj)
	  (format t "~&[pp-nsubj-be-parse-hier-tagged] pnsubj after: ")
	  (output-deps pnsubj)
	  (format t "~&[pp-nsubj-be-parse-hier-tagged] pn after: ")
	  (output-deps pn))))))


(defmethod pp-noun-adj-parse-hier-tagged ((sen sentence-annotation))
  #||
  if the noun and adj are connected, unify the edge lable to be noun_adj
  make the changes in place, tested, it works.
  ||#
  (let* ((pns (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	 pn2)
    (dolist (pn pns)
      (dolist (dep (dep-deps pn))
	(setf pn2 (find-annotation (d-govid dep) sen))
	(when (or (and (noun-pn? pn2) (adj-pn? pn))
		  (and (adj-pn? pn2) (noun-pn? pn)))
	  (setf (d-rel dep) "noun_adj")))

      (dolist (gov (gov-deps pn))
	(setf pn2 (find-annotation (d-depid gov) sen))
	(when (or (and (noun-pn? pn2) (adj-pn? pn))
		  (and (adj-pn? pn2) (noun-pn? pn)))
	  (setf (d-rel gov) "noun_adj"))))))

(defmethod pp-prep-of-parse-hier-tagged ((sen sentence-annotation))
  #||
  changed prep-of to nn
  ||#
  (let* ((pns (surface-pns sen))
	 (pns (remove-if #'(lambda (a) (ptb-adjp (pnode-pos-tag a))) pns)))
    (do* ((pn-iter pns (cdr pn-iter))
	  (pn1 (first pn-iter) (first pn-iter))
	  (pn2 (second pn-iter) (second pn-iter))
	  (pn3 (third pn-iter) (third pn-iter)))
	((null pn3))
      (when (and (equalp "of" (data pn2))
		 (ptb-nounp (pnode-pos-tag pn1))
		 (ptb-nounp (pnode-pos-tag pn3)))
	(dolist (gov (gov-deps pn1))
	  (when (equalp "prep_of" (d-rel gov))
	    (disconnect-link gov sen)))
	(dolist (dep (dep-deps pn3))
	  (when (equalp "prep_of" (d-rel dep))
	    (disconnect-link dep sen)))
	(let* ((l (make-deprep :govid (id pn1) 
			       :rel "prep_of" 
			       :depid (id pn3))))
	  (insert-gov pn1 l)
	  (insert-dep pn3 l))

	(copy-links pn1 pn3)))
    
    (dolist (pn pns)
      (dolist (dep (dep-deps pn))
	(when (equalp "prep_of" (d-rel dep)) 
	  (setf (d-rel dep) "nn")))

      (dolist (gov (gov-deps pn))
	(when (equalp "prep_of" (d-rel gov))
	  (setf (d-rel gov) "nn"))))))


(defmethod pp-ki67-parse-hier-tagged ((sen sentence-annotation))
  (let* ((pns (raw-pns sen))
	 (doc (document sen))
	 (max-pct 0)
	 ki67-pn pct-pn m pct)
    (do* ((pn-iter pns (cdr pn-iter))
	  (pn (first pn-iter) (first pn-iter))
	  (next-pn (second pn-iter) (second pn-iter)))
	((null next-pn))
      (when (match-re "(?i)(ki67|mib.*1)" (content pn))
	(setf (data pn) "ki67")
	(setf ki67-pn pn))
      (cond
       ((and (setf m (match-re "^[~><]?(?<pct>\\d+)%?$" (content pn) :return :match))
	     (equalp "%" (content next-pn)))
	(setf pct (parse-integer (re-submatch m nil nil "pct")))
	(when (> pct max-pct)
	  (setf max-pct pct)
	  (setf pct-pn pn)))
       ((and (setf m (match-re "^\\d+%?-(?<pct>\\d+)%?$" (content pn) :return :match))
	     (equalp "%" (content next-pn)))
	(setf pct (parse-integer (re-submatch m nil nil "pct")))
	(when (> pct max-pct)
	  (setf max-pct pct)
	  (setf pct-pn pn)))))
    (format t "~&[pp-ki67-parse-hier-tagged] ki67: ~a pct: ~a~%" 
	    ki67-pn pct-pn)
    (when (and ki67-pn pct-pn)
      (dolist (th '(10 20 30 40 50 60 70 80 90))
	(let* ((dnum (discretize-pct-th max-pct th))
	       ki67-th-pn pct-th-pn	l)
	  (when dnum
	    (setf ki67-th-pn (make-instance (gtype 'ghier-parse-node-type)
					    :doc doc
					    :start (start ki67-pn)
					    :end (end ki67-pn)
					    :pnode-pos-tag "NN"
					    :data "ki67"
					    :sw-ver (gsw-ver 'ghier-parse)
					    :setting (gsetting 'ghier-parse)))
	    (setf pct-th-pn (make-instance (gtype 'ghier-parse-node-type)
					   :doc doc
					   :start (start pct-pn)
					   :end (end pct-pn)
					   :pnode-pos-tag "NN"
					   :data dnum
					   :sw-ver (gsw-ver 'ghier-parse)
					   :setting (gsetting 'ghier-parse)))
	    (add-annotation doc pct-th-pn)
	    (add-annotation sen pct-th-pn)
	    (add-annotation doc ki67-th-pn)
	    (add-annotation sen ki67-th-pn)
	    (setf l (make-deprep :govid (id ki67-th-pn) 
				 :rel "num" 
				 :depid (id pct-th-pn)))
	    (insert-gov ki67-th-pn l)
	    (insert-dep pct-th-pn l)))))))


(defun str-modifier? (pn gov str)
  (let* ((doc (document pn))
	 (deppn (find-annotation (d-depid gov) doc)))
    (and (equalp (d-rel gov) "noun_adj")
	 (equalp str (content deppn)))))

(defun no-str-modifier? (pn str)
  (let* ((govs (gov-deps pn)))
    (or (null govs)
	(notany #'(lambda (a) (str-modifier? pn a str)) govs))))

(defun neg-dep? (dep pn)
  (let* ((rel (d-rel dep))
	 (govid (d-govid dep))
	 (doc (document pn))
	 (govpn (find-annotation govid doc)))
    (or (equalp "conj_negcc" rel)
	(and (equalp "noun_adj" rel)
	     (member (content govpn) '("negative") :test #'equalp))
	(and (equalp "dobj" rel)
	     (member (content govpn) '("lack") :test #'equalp)))))

(defun neg-gov? (gov pn)
  (let* ((rel (d-rel gov))
	 (depid (d-depid gov))
	 (doc (document pn))
	 (deppn (find-annotation depid doc)))
    (or (and (equalp "det" rel)
	     (equalp "no" (content deppn)))
	(and (equalp "noun_adj" rel)
	     (member (content deppn) '("negative") :test #'equalp))
	(equalp "neg" rel))))

(defun neg-pn? (pn)
  (or (some #'(lambda (a) (neg-dep? a pn)) (dep-deps pn))
      (some #'(lambda (a) (neg-gov? a pn)) (gov-deps pn))))

(defmethod pp-express-parse-hier-tagged ((sen sentence-annotation))
  (let* ((pns (raw-pns sen))
	 (doc (document sen))
	 mpn npn l str)
    (dolist (pn pns)
      (when (match-re "(?i)(express|stain)" (data pn))
	(dolist (gov (gov-deps pn))
	  (when (member (d-rel gov) '("nn"		  ; cd10 expression
				      "dobj"	  ; coexpressing cd10
				      "prep_with" ; with cd30, cells stained
				      "prep_for") ; stain for cd10
			:test #'equalp) 
	    (setf mpn (find-annotation (d-depid gov) sen))
	    (when (if-tuis mpn)
	      (if (not (eq (neg-pn? mpn) (neg-pn? pn)))
		  (setf str "negative")
		(setf str "positive"))
	      (setf npn (make-instance (gtype 'ghier-parse-node-type)
				       :doc doc
				       :start (start pn)
				       :end (start pn)
				       :pnode-pos-tag "JJ"
				       :data str
				       :sw-ver (gsw-ver 'ghier-parse)
				       :setting (gsetting 'ghier-parse)))
	      (add-annotation doc npn)
	      (add-annotation sen npn)
	      (setf l (make-deprep :govid (id npn) 
				   :rel "noun_adj" 
				   :depid (id mpn)))
	      (insert-gov npn l)
	      (insert-dep mpn l)
	      (format t "~&[pp-express] mpn: ~a, pn: ~a~%" mpn npn))))))))

(defmethod pp-myc-parse-hier-tagged ((sen sentence-annotation))
  (let* ((pns (raw-pns sen))
	 (doc (document sen)))
    (dolist (pn pns)
      (when (match-re "(?i)myc" (content pn))
	(let* ((mpn1 (make-instance (gtype 'ghier-parse-node-type)
				    :doc doc
				    :start (start pn)
				    :end (start pn)
				    :pnode-pos-tag "NN"
				    :data "myc"
				    :sw-ver (gsw-ver 'ghier-parse)
				    :setting (gsetting 'ghier-parse)))
	       (mpn2 (make-instance (gtype 'ghier-parse-node-type)
				    :doc doc
				    :start (start pn)
				    :end (start pn)
				    :pnode-pos-tag "NN"
				    :data "myc"
				    :sw-ver (gsw-ver 'ghier-parse)
				    :setting (gsetting 'ghier-parse)))
	       l)
	  (add-annotation doc mpn1)
	  (add-annotation sen mpn1)
	  (add-annotation doc mpn2)
	  (add-annotation sen mpn2)
	  (setf l (make-deprep :govid (id mpn1) 
			       :rel "nn" 
			       :depid (id mpn2)))
	  (insert-gov mpn1 l)
	  (insert-dep mpn2 l))))))


(defmethod pp-decode-adj-parse-hier-tagged ((sen sentence-annotation))
  (let* ((pns (annotations-spec sen :type (gtype 'ghier-parse-node-type)
				:filter #'coded-pn))
	 (h-newpn (make-hash-table :test #'equalp))
	 (doc (document sen))
	 new-pn new-pid)
    (dolist (pn pns)
      (multiple-value-bind (m? pstr)
	  (match-re "(\\+/-|-/\\+|\\+|-|bright|dim|dim/-)$" (content pn))
	(when m?
	  (setf new-pid (gethash pstr h-newpn))
	  
	  (cond 
	   (new-pid
	    (setf new-pn (find-annotation new-pid sen)))
	   (t
	    (setf new-pn (make-instance (gtype 'ghier-parse-node-type)
					:doc doc
					:start (end pn)
					:end (end pn)
					:data pstr
					:adj-decode t
					:pnode-pos-tag "JJ"
					:sw-ver (gsw-ver 'ghier-parse)
					:setting (gsetting 'ghier-parse)))
	    ;; add-annotation gives you id
	    (add-annotation doc new-pn)
	    (add-annotation sen new-pn)
	    (setf new-pid (id new-pn))
	    ;; convert all pn's links to new-pn
	    (let* (new-dep govpn)
	      (dolist (dep (dep-deps pn))
		(setf govpn (find-annotation (d-govid dep) sen))
		(setf new-dep (make-deprep :govid (d-govid dep) 
					   :rel (d-rel dep) 
					   :depid new-pid))
		(insert-gov govpn new-dep)
		;; assume that every pn share the same deps
		(insert-dep new-pn new-dep)))
	    
	    (let* (new-gov deppn)
	      (dolist (gov (gov-deps pn))
		(setf deppn (find-annotation (d-depid gov) sen))
		(setf new-gov (make-deprep :govid new-pid 
					   :rel (d-rel gov) 
					   :depid (d-depid gov)))
		(insert-dep deppn new-gov)
		;; assume that every pn share the same deps
		(insert-gov new-pn new-gov)))

	    (setf (gethash pstr h-newpn) (id new-pn))))
	  
	  
	  (setf (pnode-pos-tag pn) "NN")
	  (setf (data pn) (replace-re (data pn) "(\\+/-|-/\\+|\\+|-|bright|dim|dim/-)$" ""))
	  (setf (dirty pn) t)
	  ;; regardless of new-pn created or not, these have to be deleted.
	  (dolist (dep (dep-deps pn))
	    (let* ((govpn (find-annotation (d-govid dep) sen)))
	      (delete-gov govpn dep)))
	  (dolist (gov (gov-deps pn))
	    (let* ((deppn (find-annotation (d-depid gov) sen)))
	      (delete-dep deppn gov)))

	  (setf (dep-deps pn) nil)
	  (setf (gov-deps pn) nil)
	  ;; link new-pn and pn
	  (let* ((pid (id pn))
		 (l (make-deprep :govid new-pid 
				 :rel "noun_adj" 
				 :depid pid)))
	    (insert-gov new-pn l)
	    (insert-dep pn l)))))))

(defun spec-mod? (ph)
  (let* ((phstr (content ph))
	 (tas (annotations-spec ph :type (gtype 'gtoken-type)))
	 (tas (remove-if-not #'non-stop? tas)))
    (and (noun-ph? ph)
	 (not (match-re "[,.;]" phstr))
	 (> (length tas) 2)
	 (null (annotations ph :type 'coded-text))
	 (null (annotations ph :type 'if-list)))))



(defmethod pp-noun-mod-parse-hier-tagged ((sen sentence-annotation))
  (let* ( ;; (doc (document sen))
	 (pns (raw-pns sen))
	 (pns (remove-if-not #'noun-pn? pns))
	 ph mod-pns)
    (dolist (pn pns)
      (setf ph (car (annotations-spanning-spec pn :type (gtype 'gphrase-type))))
      (when (and ph (spec-mod? ph) (not (maskedp ph)))
	(setf mod-pns (raw-pns ph))
	(setf mod-pns (remove-if-not #'(lambda (a) (annotation-lessp a pn)) mod-pns))
	(setf mod-pns (remove-if-not #'non-stop? mod-pns))


	(setf mod-pns (stable-sort mod-pns #'annotation-lessp))
	;; (setf mod-pns (concatenate 'list mod-pns (list pn)))
	(dolist (pn1 mod-pns)
	  (dolist (pn2 mod-pns)
	    (when (or (and (adj-pn? pn1) (noun-pn? pn2) 
			   (not (connected? pn1 pn2))))
	      (let* ((l (make-deprep :govid (id pn2) 
				     :rel "nn" 
				     :depid (id pn1))))
		
		(insert-gov pn2 l)
		(insert-dep pn1 l)
		(setf (dirty pn1) t)
		(setf (dirty pn2) t)))))))))


