;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/12/2015 rewrite using asdf framework
yluo - 07/16/2012 added special-of-np-hinge
yluo - 05/30/2012 added np-hinge-sp-lex
yluo - 4/12/2012 creation 

A parse method dispatcher.
|#
(defpackage :late
  (:use :common-lisp :util :norm :op :umlsize)
  (:export 
   "*debug-parse*"
   "*l-pnodes*"
   "*l-prep-in-pnode*"
   "ann->reg-str"
   "ann->tok-str"
   "np-hinge-sp-lex"
   "parse"
   "hierarchize"
   "surface-pn"
   "coded-pn"
   "coded-substitute-pn"
   "if-list-substitute-pn"
   "substitute-pn"
   "if-list-pn"
   "if-adj-pn"
   "range-adj-pn"
   "raw-pn"
   "raw-pns"
   "surface-pns"
   "sp-hier-tagged"
   "sp-tagged"
   "noun-pn?"
   "adj-pn?"
   "adv-pn?"
   "adj-pns"
   "adv-pns"))

(in-package :late)

(defparameter *debug-parse* nil)
(defparameter *l-pnodes* nil)
(defparameter *l-prep-in-pnode* nil)

(defun coded-substitute-pn (a)
  (equalp "substitute" (coded a)))

(defun if-list-substitute-pn (a)
  (equalp "substitute" (if-list a)))

(defun if-adj-substitute-pn (a)
  (equalp "substitute" (if-adj a)))

(defun range-adj-substitute-pn (a)
  (equalp "substitute" (range-adj a)))

(defun substitute-pn (a)
  (or (coded-substitute-pn a) 
      (if-list-substitute-pn a) 
      (if-adj-substitute-pn a)
      (range-adj-substitute-pn a)))

(defun coded-pn (a)
  (equalp "part" (coded a)))

(defun if-list-pn (a)
  (equalp "part" (if-list a)))

(defun if-adj-pn (a)
  (equalp "part" (if-adj a)))

(defun range-adj-pn (a)
  (equalp "part" (range-adj a)))

(defun surface-pn (a)
  (or (and (null (coded a)) (null (if-list a)) (null (if-adj a)) (null (range-adj a)))
      (equalp "substitute" (coded a))
      (equalp "substitute" (if-list a))
      (equalp "substitute" (if-adj a))
      (equalp "substitute" (range-adj a))))

(defun raw-pn (a)
  (or (and (null (coded a)) (null (if-list a)) (null (if-adj a)) (null (range-adj a)))
      (equalp "part" (coded a))
      (equalp "part" (if-list a))
      (equalp "part" (if-adj a))
      (equalp "part" (range-adj a))))


(defmethod parse ((doc document))
  (unless (analyzedp doc :ganalysis 'gsentencize)
    (format t "Warning: Cannot compute tags for tokens in ~a because ~
                 sentence annotations are not done." (name doc))
    (return-from parse nil))
  
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (parse sen))
  (add-analysis doc :ganalysis 'gparse))

(defmethod hier-parse ((doc document))
  (unless (analyzedp doc :ganalysis 'ghierarchize)
    (format t "Warning: Cannot hier-parse ~a because hierarchize is not done."
	    (name doc))
    (return-from hier-parse nil))
  
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (hier-parse sen))
  (add-analysis doc :ganalysis 'ghier-parse))

(defmethod parse ((sa sentence-annotation))
  (cond 
   ((equalp (ganalysis 'gparse) 'parse-stanford-tagged)
    (sp:parse-tagged sa))
   
   ((equalp (ganalysis 'gparse) 'parse-stanford-tokenized)
    (sp:parse-tokenized sa))
   
   ((equalp (ganalysis 'gparse) 'parse-stanford-tokenized-constrained)
    (sp:parse-tokenized-constrained sa))
   
   ((equalp (ganalysis 'gparse) 'parse-stanford-hier-tagged)
    (sp:parse-hier-tagged sa))
   
   ((equalp (ganalysis 'gparse) 'parse-link-biased)
    (biased-link-parse sa :best-only? t :pos-check? t :dup-rm? t))
   
   ((equalp (ganalysis 'gparse) 'parse-link-hier)
    (hier-link-parse sa :best-only? t :pos-check? t :dup-rm? t))))

(defmethod hier-parse ((sa sentence-annotation))
  (cond 
   ((equalp (ganalysis 'ghier-parse) 'parse-stanford-hier-tagged)
    (sp:parse-hier-tagged sa))
   
   ((equalp (ganalysis 'ghier-parse) 'parse-stanford-hier-tokenized)
    (sp:parse-hier-tokenized sa))
   
   ((equalp (ganalysis 'ghier-parse) 'parse-link-hier)
    (hier-link-parse sa :best-only? t :pos-check? t :dup-rm? t))))

(defmethod ann->tok-str ((ann annotation))
  ;; it's necessary to go through tokens as we want to exclude cross-ref etc.
  (let* ((str (format nil "~{~a~^ ~}" (mapcar #'content (annotations ann :type (gtype 'gtoken-type))))))
    (ann->tok-str str)))

(defmethod ann->tok-str ((str string))
  ;; (setf str (replace-re str "[^A-Za-z0-9\\-\\s\\+]" ""))
  (setf str (replace-re str "\\s+" "-"))
  (cond					 ;; according to Stanford Parser FAQ
   ((search str "([{")
    (setf str "-LRB-"))
   ((search str ")]}")
    (setf str "-RRB-"))
   ((equalp str "/")
    (setf str "\\/"))
   ((equalp str "*")
    (setf str "\\*"))
   (t
    str)))

(defun ann->reg-str (ann)
  (let* ((str (content ann)))
    ;; (setf str (replace-re str "[^A-Za-z0-9\\-\\s\\+]" ""))
    (setf str (replace-re str "\\s+" " "))
    (string-downcase str)))

(defun np->pnode (np)
  "Use - to concatenate NP's to tokens."
  (let* ((doc (document np))
	 (str (ann->tok-str np)))
    (make-instance (gtype 'gparse-node-type)
		   :doc doc 
		   :start (start np)
		   :end (end np)
		   :data str
		   :pnode-pos-tag "NN"
		   :sw-ver (gsw-ver 'gparse)
		   :setting (gsetting 'gparse))))

(defun add-phrase-pnodes (ph sen)
  (let* ((doc (document ph)))
    (cond
     ;; if NP
     ((equalp (data ph) "NP")
      (let* ((pnode (np->pnode ph)))
	(add-annotation doc pnode)
	(add-annotation sen pnode)
	(when *debug-parse*
	  (format t "~&pnode: ~a~%" pnode))))
     ;; if other phrase
     (t
      (dolist (tok (annotations-spec ph :type (gtype 'gtoken-type)))
	(let* ((pnode (pnode-from-token tok))
	       (pos (car (annotations-on-spec tok :type (gtype 'gtag-type)))))
	  (setf (pnode-pos-tag pnode) (data pos))
	  (add-annotation doc pnode)
	  (add-annotation sen pnode)
	  (when *debug-parse*
	    (format t "~&pnode: ~a~%" pnode))))))))

(defmethod hierarchize ((doc document))
  (cond
   ((equalp (ganalysis 'ghierarchize) 'hierarchize-syn-sem)
    (hierarchize-syn-sem doc))
   
   ((equalp (ganalysis 'ghierarchize) 'hierarchize-sem)
    (hierarchize-sem doc))))

(defmethod hierarchize-sem2 ((sen sentence-annotation)
			     &aux helped?)
  "This logic is soooo complicated ..."
  (assert (eq (gtype 'gparse-node-type) 'parse-node-stanford-hier-tagged)
	  ()
	  "Please ensure gparse-node-type is 'parse-node-stanford-hier-tagged")
  (let* ((umls-anns (umls-cover sen 'cui-annotation))
	 (phs (annotations-spec sen :type (gtype 'gphrase-type)))
	 (doc (document sen))
	 pnodes pnode cover covers ph)
    (setf umls-anns (stable-sort umls-anns 'annotation-lessp))
    
    ;; merge umls annotations to get cover annontations, linear scan
    (dolist (umls-ann umls-anns)
      (cond
       ((null cover)
	(setf cover (make-instance 'annotation
				   :doc doc
				   :start (start umls-ann)
				   :end (end umls-ann))))
       
       (t
	(cond 
	 ((member (allen cover umls-ann) '(:o))
	  (setf (end cover) (end umls-ann)))
	 
	 ((member (allen cover umls-ann) '(:<))
	  (push cover covers)
	  (setf cover (make-instance 'annotation
				     :doc doc
				     :start (start umls-ann)
				     :end (end umls-ann))))))))
    (when cover 
      (push cover covers))
    (setf covers (nreverse covers))
    (when *debug-parse* (format t "~&covers:~%~{~a~%~}" covers))
    
    (dolist (cover covers)
      ;; there must be a phrase start before cover end, or cover is useless
      (when (and phs (> (end cover) (start (car phs))))
	(let* (np1 np2)
	  (do ()
	      ((or (null phs) 
		   (and (equalp (data (car phs)) "NP")
			(> (end (car phs)) (start cover)))))
	    (setf ph (car phs))
	    (when *debug-parse* (format t "~&ph 1st step: ~a~%" ph))
	    (add-phrase-pnodes ph sen)
	    (setf phs (cdr phs)))	
	  (setf np1 (car phs))
	  
	  
	  (do ()
	      ((or (null phs) 
		   (and (equalp (data (car phs)) "NP")
			(<= (start (car phs)) (end cover) (end (car phs))))))
	    (setf phs (cdr phs)))
	  (setf np2 (car phs))
	  (setf phs (cdr phs))  

	  (cond
	   ((and np1 np2)
	    (setf pnode (make-instance (gtype 'gparse-node-type)
				       :doc doc
				       :start (start np1)
				       :end (end np2)
				       :sw-ver (gsw-ver 'gparse)
				       :setting (gsetting 'gparse))
		  helped? t)
	    (add-annotation doc pnode)
	    (setf (data pnode) (ann->tok-str pnode)
		  (pnode-pos-tag pnode) "NN")
	    (when *debug-parse* (format t "~&pnode: ~a~%" pnode))
	    
	    (let* ((str (replace-re (content pnode) "[\\s\\t\\n]+" " "))
		   (pnode-cons (assoc str *l-pnodes* :test #'equalp)))
	      (if pnode-cons
		  (incf (cdr pnode-cons))
		(setf *l-pnodes* (acons str 1 *l-pnodes*))))
	    
	    (dolist (pos (annotations-spec pnode :type (gtype 'gtag-type)
					   :filter #'ptb-prepp))
	      (let* ((pos-str (content pos))
		     (pos-cons (assoc pos-str *l-prep-in-pnode* :test #'equalp)))
		(if pos-cons
		    (incf (cdr pos-cons))
		  (setf *l-prep-in-pnode* (acons pos-str 1 *l-prep-in-pnode*)))))
	    
	    (push pnode pnodes))
	   
	   (t
	    (error "np1 and np2 should both exist!~%cover: ~a~%sent: ~a~%np1: ~a~%np2: ~a~%" cover sen np1 np2))))))
    
    
    (do ()
	((null phs))
      (setf ph (car phs))
      (when *debug-parse* (format t "~&ph 3rd step: ~a~%" ph))
      (add-phrase-pnodes ph sen)
      (setf phs (cdr phs)))

    (setf pnodes (nreverse pnodes))
    (when (and *debug-parse* helped?)
      (format t "~&>>>>>>~%~a|| ~a~%~%umls: ~{~a~%~}~%pnodes: ~{~a~%~}~%~%"
	      (id sen) (content sen) umls-anns pnodes))
    pnodes))

(defmethod np-hinge-sp-lex ((sen sentence-annotation))
  "Hinge Two NPs (and other phrases in middle) if they can be glued together
using a Specialist lexical element
Enforces np2 must be NP, otherwise, sentences like the following is messed up:
S96T10644: 
The findings are consistent with membranoproliferative
glomerulonephritis associated with chronic hepatitis B virus
infection, though the etiology is not proved.
"
  ;; :filter #'(lambda (a) (equalp "NP" (data a)))
  (let* ((orig-nps (annotations sen 
				:type (gtype 'gphrase-type)))
	 (doc (document sen)))
    (do* ((nps orig-nps (cdr nps))
	  (np1 (car nps) (car nps)))
	((null nps))
      (let* ((sp-lexes 
	      (union 
	       (annotations np1 :relation ':oi 
			    :type 'sp-lex-annotation
			    :filter #'(lambda (a) 
					(> (length-wo-punc a) 1)))
	       (annotations np1 :relation ':si 
			    :type 'sp-lex-annotation
			    :filter #'(lambda (a) 
					(> (length-wo-punc a) 1))))
;;;						(union 
;;;						 (annotations np1 :relation ':oi 
;;;									  :type 'cui-annotation
;;;									  :filter #'(lambda (a) 
;;;												  (> (length-wo-punc a) 1)))
;;;						 (annotations np1 :relation ':si 
;;;									  :type 'cui-annotation
;;;									  :filter #'(lambda (a) 
;;;												  (> (length-wo-punc a) 1))))
	      )
	     longest-sp-lex np2)
	(setf sp-lexes (stable-sort sp-lexes #'> :key #'length-wo-punc))
	(setf longest-sp-lex (car sp-lexes))
	(when longest-sp-lex
	  (setf np2 (union 
		     ;; :filter  #'(lambda (a) (not (equalp "PP" (data a))))
		     (annotations longest-sp-lex :relation ':oi
				  :type (gtype 'gphrase-type)
				  :filter #'(lambda (a) 
					      (equalp "NP" (data a))))
		     ;; :filter  #'(lambda (a) (not (equalp "PP" (data a))))
		     (annotations longest-sp-lex :relation ':f
				  :type (gtype 'gphrase-type)
				  :filter #'(lambda (a) 
					      (equalp "NP" (data a))))))
	  (when np2
	    (if (> (length np2) 1)
		(format t "~&Warning: more than one NP overlaps ~a~%"
			longest-sp-lex))
	    (setf np2 (car np2))
	    (setf nps (member np2 orig-nps))
	    (format t "~&~a: ~a~%~a joined ~a ~a~%~%" 
		    (id sen) (content sen) longest-sp-lex np1 np2)
	    (let* ((np1-ptags (annotations-spec np1 :type (gtype 'gtag-type)))
		   (second-ptag (second np1-ptags))
		   (first-ptag (first np1-ptags))
		   (pstart (start np1))
		   (pend (end np2))
		   pnode)
	      (when (equalp (data first-ptag) "DT")
		(cond
		 (second-ptag
		  (setf pstart (start second-ptag)))
		 (t
		  (format t "~&Wawrning: second ptag null in ~a~%" np1))))
	      (setf pnode (make-instance (gtype 'ghier-parse-node-type)
					 :doc doc
					 :start pstart
					 :end pend
					 :sw-ver (gsw-ver 'ghier-parse)
					 :setting (gsetting 'ghier-parse)))
	      (add-annotation doc pnode)
	      (add-annotation sen pnode)
	      (setf (data pnode) (ann->tok-str pnode))
	      (setf (pnode-pos-tag pnode) (np-pos pnode)))))))))

(defmethod np-hinge-sp-lex ((doc document))
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (np-hinge-sp-lex sen)))

(defmethod np-hinge-sp-lex ((corp corpus))
  (dolist (docid (documents corp))
    (let* ((doc (document docid)))
      (format t "~&~a~%" (name doc))
      (np-hinge-sp-lex doc))))

(defun np-pos (np &aux ptags first-ptag second-ptag nptags)
  (setf ptags (annotations-spec np :type (gtype 'gtag-type)))
  (setf nptags (annotations-spec np :type (gtype 'gtag-type) 
				 :filter #'ptb-nounp))
  (setf first-ptag (first ptags))
  (setf second-ptag (second ptags))
  (cond
   ((= (length ptags) 1)
    (data (car ptags)))
   ((and (= (length nptags) 1)
	 (member (data (car nptags)) '("NNS" "NNPS") :test #'equal))
    (data (car nptags)))
   ((and (= (length ptags) 2)
	 (equalp "DT" (data first-ptag)))
    (data second-ptag))
   ((equalp "DT" (data first-ptag))
    (data (car (last (or nptags ptags)))))
   (t
    (format t "~&weird np-pos on ~a [~a]~%" 
	    np (data (car (last (or nptags ptags)))))
    (data (car (last (or nptags ptags)))))))

(defun adjp-pos (adjp)
  (let* ((ptags (annotations-spec adjp :type (gtype 'gtag-type)))
	 (adj-ptags (adj-ptags adjp))
	 (last-ptag (car (last ptags)))
	 (last-adj-ptag (car (last adj-ptags)))
	 (ptag (or last-adj-ptag last-ptag)))
    
    (data ptag)))

(defun type-preserving-np? (ph)
  (and (equalp "NP" (data ph))
       (or (match-re "\\b(percent|amount|portions?|types?|parts?|populations?|forms?)$" (content ph) 
		     :case-fold t)
	   (match-re "%$" (content ph)))))

(defun special-of-np-hinge (sen &aux probe-ann doc pnodes spans)
  "Handles grouping of noun phrases such as 
[NP Small_JJ amount_NN ] [PP of_IN ] [NP the_DT tumor_NN cells_NNS ]
or
[NP Seventy-five_CD percent_NN (75%)_NN ] [PP of_IN ] [NP the_DT tumor_NN cells_NNS ]"
  (setf doc (document sen))
  (setf pnodes (annotations sen :type (gtype 'ghier-parse-node-type)))
  (setf spans (mapcan #'(lambda (a) (list (start a) (end a))) pnodes))
  (setf spans (concatenate 'list (list (start sen)) spans (list (end sen))))
  
  (do* ((spans spans (cddr spans))
	(spst (car spans) (car spans))
	(spe (cadr spans) (cadr spans)))
      ((null spans))

    (setf probe-ann (make-instance 'annotation :start spst :end spe))
    (add-annotation doc probe-ann)
    (let* ((phs (annotations probe-ann :type (gtype 'gphrase-type)
			     :filter #'(lambda (a) 
					 (not (equalp "PRN" (data a))))))
	   (ph-arr (coerce phs 'array))
	   ph1 ph2 ph3 pnode)
      (dotimes (i (- (length ph-arr) 3))
	(setf ph1 (aref ph-arr i)
	      ph2 (aref ph-arr (1+ i))
	      ph3 (aref ph-arr (+ 2 i)))
	(when (and (type-preserving-np? ph1)
		   (equalp (content ph2) "of")
		   (equalp "NP" (data ph3)))
	  (setf pnode (make-instance (gtype 'ghier-parse-node-type)
				     :doc doc
				     :start (start ph1)
				     :end (end ph3)
				     :sw-ver (gsw-ver 'ghier-parse)
				     :setting (gsetting 'ghier-parse)))
	  (add-annotation doc pnode)
	  (add-annotation sen pnode)
	  (setf (pnode-pos-tag pnode) (np-pos ph3))
	  (setf (data pnode) (ann->tok-str pnode)))))
    (tree-delete (annotations-tree doc) probe-ann)))

(defun proper-np? (a)
  (and (equalp "NP" (data a))
       (annotations-spec a :type (gtype 'gtag-type)
			 :filter #'(lambda (a) (search "NN" (data a))))
       (np-safep a)
       ))

(defun adjp-hinge (sen &aux probe-ann doc pnodes spans)
  (setf doc (document sen))
  (setf pnodes (annotations sen :type (gtype 'ghier-parse-node-type)))
  (setf spans (mapcan #'(lambda (a) (list (start a) (end a))) pnodes))
  (setf spans (concatenate 'list (list (start sen)) spans (list (end sen))))
  
  (do* ((spans spans (cddr spans))
	(spst (car spans) (car spans))
	(spe (cadr spans) (cadr spans)))
      ((null spans))

    (setf probe-ann (make-instance 'annotation :start spst :end spe))
    (add-annotation doc probe-ann)
    (dolist (adjp (annotations-spec probe-ann :type (gtype 'gphrase-type)
				    :filter #'(lambda (a) 
						(equalp "ADJP" (data a)))))
      (unless nil ;; (match-re "\\band\\b" (content adjp) :case-fold t)
	(let* ((pstart (start adjp))
	       (pend (end adjp))
	       pnode
	       pnode-pos
	       pnode-str)
	  (setf pnode (make-instance (gtype 'ghier-parse-node-type)
				     :doc doc
				     :start pstart
				     :end pend
				     :sw-ver (gsw-ver 'ghier-parse)
				     :setting (gsetting 'ghier-parse)))
	  (add-annotation doc pnode)
	  (add-annotation sen pnode)
	  
	  (setf pnode-pos (adjp-pos adjp))
	  (setf pnode-str (ann->tok-str pnode))
	  (setf (data pnode) pnode-str)
	  (setf (pnode-pos-tag pnode) pnode-pos))))
    (tree-delete (annotations-tree doc) probe-ann)))

(defun np-hinge (sen &aux probe-ann doc pnodes spans)
  (setf doc (document sen))
  (setf pnodes (annotations sen :type (gtype 'ghier-parse-node-type)))
  (setf spans (mapcan #'(lambda (a) (list (start a) (end a))) pnodes))
  (setf spans (concatenate 'list (list (start sen)) spans (list (end sen))))
  
  (do* ((spans spans (cddr spans))
	(spst (car spans) (car spans))
	(spe (cadr spans) (cadr spans)))
      ((null spans))

    (setf probe-ann (make-instance 'annotation :start spst :end spe))
    (add-annotation doc probe-ann)
    (dolist (np (annotations-spec probe-ann :type (gtype 'gphrase-type)
				  :filter #'proper-np?))
      (unless nil ;; (match-re "\\band\\b" (content np) :case-fold t)
	(let* ((ptags (annotations-spec np :type (gtype 'gtag-type)))
	       (first-ptag (first ptags))
	       (second-ptag (second ptags))
	       (pstart (start np))
	       (pend (end np))
	       pnode
	       pnode-pos
	       pnode-str)
	  (when (equalp "DT" (data first-ptag))
	    ;; ignore the DT, will be added in glean-tokens later
	    (cond
	     ((match-re "^a\\s+few" (content np) :case-fold t))
	     (second-ptag
	      (setf pstart (start second-ptag)))
	     (t
	      (format t "~&second-ptag null in ~a~%" np))))
;;;		(unless (and (= (length ptags) 2) 
;;;					 (member (data first-ptag) *ptb-adj-list* :test #'equal))
	  (setf pnode (make-instance (gtype 'ghier-parse-node-type)
				     :doc doc
				     :start pstart
				     :end pend
				     :sw-ver (gsw-ver 'ghier-parse)
				     :setting (gsetting 'ghier-parse)))
	  (add-annotation doc pnode)
	  (add-annotation sen pnode)
	  (setf pnode-pos (np-pos np))
	  (setf pnode-str (ann->tok-str pnode))
	  ;; you probably don't want this too naive plural handling
;;;		  (when (and (equal "NNS" pnode-pos)
;;;					 (not (match-re "s$" pnode-str :case-fold t)))
;;;			(setf pnode-str (format nil "~as" pnode-str)))
	  (setf (data pnode) pnode-str)
	  (setf (pnode-pos-tag pnode) pnode-pos))))
    (tree-delete (annotations-tree doc) probe-ann)))

(defun sp-lex-hinge (sen &aux probe-ann doc pnodes spans sp-lexes)
  (setf doc (document sen))
  (setf pnodes (annotations sen :type (gtype 'ghier-parse-node-type)))
  (setf spans (mapcan #'(lambda (a) (list (start a) (end a))) pnodes))
  (setf spans (concatenate 'list (list (start sen)) spans (list (end sen))))
  
  (do* ((spans spans (cddr spans))
	(spst (car spans) (car spans))
	(spe (cadr spans) (cadr spans)))
      ((null spans))

    (setf probe-ann (make-instance 'annotation :start spst :end spe))
    (add-annotation doc probe-ann)
    (setf sp-lexes (annotations probe-ann :type 'sp-lex-annotation
				:filter #'(lambda (a)
					    (> (length-wo-punc a) 1))))
    (setf sp-lexes (remove-duplicates sp-lexes :test #'annotation-equal))
    (dolist (sp-lex sp-lexes)
      (unless nil ;; (match-re "\\band\\b" (content np) :case-fold t)
	(let* ((ptags (annotations-spec sp-lex :type (gtype 'gtag-type)))
	       (first-ptag (first ptags))
	       (second-ptag (second ptags))
	       (pstart (start sp-lex))
	       (pend (end sp-lex))
	       pnode
	       pnode-pos
	       pnode-str)
	  (when (equalp "DT" (data first-ptag))
	    ;; ignore the DT, will be added in glean-tokens later
	    (cond
	     ((match-re "^a\\s+few" (content sp-lex) :case-fold t))
	     (second-ptag
	      (setf pstart (start second-ptag)))
	     (t
	      (format t "~&second-ptag null in ~a~%" sp-lex))))
	  ;; (unless (and (= (length ptags) 2) 
	  ;; 			 (member (data first-ptag) *ptb-adj-list* :test #'equal))
	  (setf pnode (make-instance (gtype 'ghier-parse-node-type)
				     :doc doc
				     :start pstart
				     :end pend
				     :sw-ver (gsw-ver 'ghier-parse)
				     :setting (gsetting 'ghier-parse)))
	  (add-annotation doc pnode)
	  (add-annotation sen pnode)
	  (setf pnode-pos (np-pos sp-lex))
	  (setf pnode-str (ann->tok-str pnode))
	  ;; you probably don't want this too naive plural handling
	  ;; (when (and (equal "NNS" pnode-pos)
	  ;; 			 (not (match-re "s$" pnode-str :case-fold t)))
	  ;; 	(setf pnode-str (format nil "~as" pnode-str)))
	  (setf (data pnode) pnode-str)
	  (setf (pnode-pos-tag pnode) pnode-pos))))
    (tree-delete (annotations-tree doc) probe-ann)))

(defun ext-adj-noun-cui? (cui)
  (let* ((ptags (annotations-spec cui :type (gtype 'gtag-type)))
	 (toks (annotations-spec cui :type (gtype 'gtoken-type)))
	 (len (length ptags))
	 (mod-ptags (subseq ptags 0 (1- len)))
	 (last-ptag (car (last ptags)))
	 (last-tok (car (last toks))))
    (or (and (ptb-nounp last-ptag)
	     (every #'ptb-ext-adjp mod-ptags))
	(member (content last-tok) '("cells" "cell") :test #'equalp))))

(defun cui-hinge (sen &aux probe-ann doc pnodes spans cuis)
  (setf doc (document sen))
  (setf pnodes (annotations sen :type (gtype 'ghier-parse-node-type)))
  (setf spans (mapcan #'(lambda (a) (list (start a) (end a))) pnodes))
  (setf spans (concatenate 'list (list (start sen)) spans (list (end sen))))
  
  (do* ((spans spans (cddr spans))
	(spst (car spans) (car spans))
	(spe (cadr spans) (cadr spans)))
      ((null spans))

    (setf probe-ann (make-instance 'annotation :start spst :end spe))
    (add-annotation doc probe-ann)
    (setf cuis (ann-coverage-cuis probe-ann))
    
    (setf cuis (remove-if #'(lambda (a) (< (length-wo-punc a) 2)) cuis))
    (setf cuis (remove-if #'ext-adj-noun-cui? cuis))
    (setf cuis (remove-duplicates cuis :test #'annotation-equal))

    (dolist (cui cuis)
      (let* ((pstart (start cui))
	     (pend (end cui))
	     pnode
	     pnode-pos
	     pnode-str)

	(setf pnode (make-instance (gtype 'ghier-parse-node-type)
				   :doc doc
				   :start pstart
				   :end pend
				   :coded (coded-status cui)
				   :if-list (if-list-status cui)
				   :if-adj (if-adj-status cui)
				   :range-adj (range-adj-status cui)
				   :sw-ver (gsw-ver 'ghier-parse)
				   :setting (gsetting 'ghier-parse)))
	(add-annotation doc pnode)
	(add-annotation sen pnode)
	(setf pnode-pos (np-pos cui))
	(setf pnode-str (ann->tok-str pnode))
	(setf (data pnode) pnode-str)
	(setf (pnode-pos-tag pnode) pnode-pos)))
    (tree-delete (annotations-tree doc) probe-ann)))

(defun prep-sp-lex (sen &aux probe-ann doc pnodes spans)
  (setf doc (document sen))
  (setf pnodes (annotations sen :type (gtype 'ghier-parse-node-type)))
  (setf spans (mapcan #'(lambda (a) (list (start a) (end a))) pnodes))
  (setf spans (concatenate 'list (list (start sen)) spans (list (end sen))))
  
  (do* ((spans spans (cddr spans))
	(spst (car spans) (car spans))
	(spe (cadr spans) (cadr spans)))
      ((null spans))

    (setf probe-ann (make-instance 'annotation :start spst :end spe))
    (add-annotation doc probe-ann)
    (dolist (sp-pos (annotations probe-ann :type 'sp-pos-annotation
				 :filter #'(lambda (a) 
					     (and (equalp "prep" (data a))
						  (> (length-wo-punc a) 1)))))
      (let* ((pnode (make-instance (gtype 'ghier-parse-node-type)
				   :doc doc
				   :start (start sp-pos)
				   :end (end sp-pos)
				   :coded (coded-status sp-pos)
				   :if-list (if-list-status sp-pos)
				   :if-adj (if-adj-status sp-pos)
				   :range-adj (range-adj-status sp-pos)
				   :pnode-pos-tag "IN"
				   :sw-ver (gsw-ver 'ghier-parse)
				   :setting (gsetting 'ghier-parse))))
	(add-annotation doc pnode)
	(add-annotation sen pnode)
	(setf (data pnode) (ann->tok-str pnode))))
    (tree-delete (annotations-tree doc) probe-ann)))

(defun glean-tokens (sen &aux probe-ann doc pnodes spans)
  (setf doc (document sen))
  (setf pnodes (annotations sen :type (gtype 'ghier-parse-node-type)))
  (setf spans (mapcan #'(lambda (a) (list (start a) (end a))) pnodes))
  (setf spans (concatenate 'list (list (start sen)) spans (list (end sen))))
  
  (do* ((spans spans (cddr spans))
	(spst (car spans) (car spans))
	(spe (cadr spans) (cadr spans)))
      ((null spans))

    (setf probe-ann (make-instance 'annotation :start spst :end spe))
    (add-annotation doc probe-ann)
    (dolist (ta (annotations probe-ann :type (gtype 'gtoken-type)))
      (let* ((pos-tag (car (annotations-on-spec ta 
						:type (gtype 'gtag-type))))
	     (tagstr (data pos-tag))
	     pnode)
	(when (and (equalp "VBN" tagstr) (in-noun-ph? pos-tag))
	  (setf tagstr "JJ"))
	;; (format t "~&[glean tokens] ~a if-list: ~a~%" ta (if-list-status ta))
	;; (format t "~&sen if list: ~{~a~%~}" (annotations sen :type 'if-list))
	(setf pnode (make-instance (gtype 'ghier-parse-node-type)
				   :doc doc
				   :start (start ta)
				   :end (end ta)
				   :coded (coded-status ta)
				   :if-list (if-list-status ta)
				   :if-adj (if-adj-status ta)
				   :range-adj (range-adj-status ta)
				   :data (ann->tok-str ta)
				   :pnode-pos-tag tagstr
				   :sw-ver (gsw-ver 'ghier-parse)
				   :setting (gsetting 'ghier-parse)))
	(add-annotation doc pnode)
	(add-annotation sen pnode)))
    (tree-delete (annotations-tree doc) probe-ann)))

(defun redundant-substitute? (ia pntype)
  (or (annotations-spanning-spec ia :type pntype :filter #'surface-pn)
      (annotations-overlap-spec ia :type pntype :filter #'surface-pn)
      (annotations-spanning-spec ia :type (gtype 'gcross-ref-type))))

(defun glean-coded (sen)
  (let* ((doc (document sen))
	 (pntype (gtype 'ghier-parse-node-type))
	 pn coded data)
    (dolist (ca (annotations sen :type 'coded-text))
      (unless (redundant-substitute? ca pntype)
	(setf coded "substitute")
	(if (= (start ca) (start sen))
	    (setf data "Atypical")
	  (setf data "atypical"))
	
	(setf pn (make-instance pntype
				:doc doc
				:start (start ca)
				:end (end ca)
				:coded coded
				:data data
				:pnode-pos-tag "JJ"
				:sw-ver (gsw-ver 'ghier-parse)
				:setting (gsetting 'ghier-parse)))
	(add-annotation doc pn)
	(add-annotation sen pn)))))



(defun glean-if-adj (sen)
  (let* ((doc (document sen))
	 (pntype (gtype 'ghier-parse-node-type))
	 pn if-adj data)
    (dolist (ia (annotations sen :type 'if-adj))
      (unless (redundant-substitute? ia pntype)
	(setf if-adj "substitute")
	(setf data (content (car (last (annotations-spec ia :type (gtype 'gtoken-type))))))
	(setf pn (make-instance pntype
				:doc doc
				:start (start ia)
				:end (end ia)
				:if-adj if-adj
				:data data
				:pnode-pos-tag "JJ"
				:sw-ver (gsw-ver 'ghier-parse)
				:setting (gsetting 'ghier-parse)))
	(add-annotation doc pn)
	(add-annotation sen pn)))))

(defun glean-range-adj (sen)
  (let* ((doc (document sen))
	 (pntype (gtype 'ghier-parse-node-type))
	 pn range-adj data)
    (dolist (ia (annotations sen :type 'range-adj))
      (unless (redundant-substitute? ia pntype)
	(setf range-adj "substitute")
	(if (= (start ia) (start sen))
	    (setf data "Atypical")
	  (setf data "atypical"))
	(setf pn (make-instance pntype
				:doc doc
				:start (start ia)
				:end (end ia)
				:range-adj range-adj
				:data data
				:pnode-pos-tag "JJ"
				:sw-ver (gsw-ver 'ghier-parse)
				:setting (gsetting 'ghier-parse)))
	(add-annotation doc pn)
	(add-annotation sen pn)))))

(defun glean-if-list (sen)
  (let* ((doc (document sen))
	 (pntype (gtype 'ghier-parse-node-type))
	 pn if-list data)
    (dolist (ia (annotations sen :type 'if-list))
      (unless (redundant-substitute? ia pntype)
	(setf if-list "substitute")
	(setf data "ATG")
	(setf pn (make-instance pntype
				:doc doc
				:start (start ia)
				:end (end ia)
				:if-list if-list
				:data data
				:pnode-pos-tag "NN"
				:sw-ver (gsw-ver 'ghier-parse)
				:setting (gsetting 'ghier-parse)))
	(add-annotation doc pn)
	(add-annotation sen pn)))))

(defmethod hierarchize-syn-sem ((sen sentence-annotation))
  "Generate hierarchize parse-nodes and pos-tags using both syntactic (NP chunkers, SP Lexicons) and semantic (CUIs) information."
  ;; (np-hinge-sp-lex sen)
  ;; you are toasted if you see 75% of lymph cells and blood cells
  ;; (special-of-np-hinge sen)
  ;; (np-hinge sen)
  ;; (adjp-hinge sen)
  ;; (sp-lex-hinge sen)
  (cui-hinge sen)
  (prep-sp-lex sen)
  (glean-tokens sen)
  (glean-coded sen)
  (glean-if-adj sen)
  (glean-if-list sen)
  (glean-range-adj sen))

(defmethod hierarchize-syn-sem ((doc document))
  "Generate hierarchize parse-nodes and pos-tags using both syntactic (NP chunkers, SP Lexicons) and semantic (CUIs) information."
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (hierarchize-syn-sem sen))
  (add-analysis doc :ganalysis 'ghierarchize))

(defmethod hierarchize-sem ((sen sentence-annotation))
  (assert (eq (gtype 'gparse-node-type) 'parse-node-stanford-hier-tagged)
	  ()
	  "Please ensure gparse-node-type is 'parse-node-stanford-hier-tagged")
  
  (let* ((tas (annotations-spec sen :type (gtype 'gtoken-type)))
	 (pos (annotations-spec sen :type (gtype 'gtag-type))))
    (unless (and tas pos)
      (format t "~&Warning: Empty token sentence ~a~%" sen)
      (return-from hierarchize-sem nil)))
  
  (let* ((umls-anns (umls-cover sen 'cui-annotation))
	 (toks (annotations-spec sen :type (gtype 'gtoken-type)))
	 (doc (document sen))
	 cover aux-pos covers umls-str)
    (setf umls-anns (stable-sort umls-anns 'annotation-lessp))
    
    ;; merge umls annotations to get cover annontations, linear scan
    (dolist (umls-ann umls-anns)
      (setf umls-str (ann->reg-str umls-ann))
      (setf aux-pos (words->umls-fn->ptb umls-str))
      (when (or (annotations-spec umls-ann :type (gtype 'gtag-type)
				  :filter #'ptb-nounp)
		aux-pos)
	(cond
	 ((null cover)
	  (setf cover (make-instance 'annotation
				     :doc doc
				     :start (start umls-ann)
				     :data (or aux-pos "NN")
				     :end (end umls-ann))))
	 
	 (t
	  (cond 
	   ((member (allen cover umls-ann) '(:o))
	    (setf (end cover) (end umls-ann)))
	   
	   ((member (allen cover umls-ann) '(:<))
	    (push cover covers)
	    (setf cover (make-instance 'annotation
				       :doc doc
				       :data (or aux-pos "NN")
				       :start (start umls-ann)
				       :end (end umls-ann)))))))))
    (when cover 
      (push cover covers))
    (setf covers (nreverse covers))
    (when *debug-parse* (format t "~&covers:~%~{~a~%~}" covers))
    
    (dolist (cover covers)
      ;; scan tokens before cover
      (do ()
	  ((or (null toks) 
	       (>= (start (car toks)) (start cover))))
	
	(let* ((tok (car toks)) 
	       (pnode (pnode-from-token tok))
	       (pos (car (annotations-on-spec tok :type (gtype 'gtag-type)))))
	  (when *debug-parse* (format t "~&tok 1st step: ~a~%" tok))
	  (setf (pnode-pos-tag pnode) (data pos))
	  (add-annotation doc pnode)
	  (add-annotation sen pnode)
	  (when *debug-parse* (format t "~&pnode: ~a~%" pnode)))
	(setf toks (cdr toks)))
      
      
      ;; ignore tokens within cover
      (do ()
	  ((or (null toks) 
	       (>= (start (car toks)) (end cover))))
	(setf toks (cdr toks)))

      ;; pnode of cover 
      (let* ((pnode (make-instance (gtype 'gparse-node-type)
				   :doc doc
				   :start (start cover)
				   :end (end cover)
				   :pnode-pos-tag (data cover)
				   :sw-ver (gsw-ver 'gparse)
				   :setting (gsetting 'gparse))))
	(add-annotation doc pnode)
	(add-annotation sen pnode)
	(setf (data pnode) (ann->tok-str pnode))
	
	(when *debug-parse* (format t "~&pnode: ~a~%" pnode))))
    
    ;; scan tokens after covers
    (dolist (tok toks)
      
      (when *debug-parse* (format t "~&tok 3rd step: ~a~%" tok))
      
      (let* ((pnode (pnode-from-token tok))
	     (pos (car (annotations-on-spec tok :type (gtype 'gtag-type)))))
	(setf (pnode-pos-tag pnode) (data pos))
	(add-annotation doc pnode)
	(add-annotation sen pnode)
	(when *debug-parse* (format t "~&pnode: ~a~%" pnode))))))

(defmethod hierarchize-sem ((doc document))
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (hierarchize-sem sen))
  (add-analysis doc :ganalysis 'ghierarchize))

(defmethod hierarchize-sem ((corp corpus))
  (dolist (docid (documents corp))
    (hierarchize-sem (document docid)))
  
  (setf *l-pnodes* (sort *l-pnodes* '> :key 'cdr))
  (setf *l-prep-in-pnode* (sort *l-prep-in-pnode* '> :key 'cdr))
  (when *debug-parse*
    (dolist (a-cons *l-pnodes*)
      (format t "~&~a: ~a~%" (cdr a-cons) (car a-cons)))
    (dolist (a-cons *l-prep-in-pnode*)
      (format t "~&~a: ~a~%" (cdr a-cons) (car a-cons)))))

(defmethod sp-hier-tagged ((sen sentence-annotation))
  (res-text (car (annotations sen :type 'parse-annotation-stanford-hier-tagged))))

(defmethod sp-tagged ((sen sentence-annotation))
  (res-text (car (annotations sen :type 'parse-annotation-stanford-tagged))))

(defun noun-pn? (pn)
  (ptb-nounp (pnode-pos-tag pn)))

(defun adj-pn? (pn)
  (ptb-adjp (pnode-pos-tag pn)))

(defun vbn-pn? (pn)
  (ptb-vbnp (pnode-pos-tag pn)))

(defun adv-pn? (pn)
  (ptb-advp (pnode-pos-tag pn)))

(defun adj-pns (sen)
  (annotations-spec sen :type (gtype 'ghier-parse-node-type) :filter #'adj-pn?))

(defun adv-pns (sen)
  (annotations-spec sen :type (gtype 'ghier-parse-node-type) :filter #'adv-pn?))

(defun raw-pns (sen)
  (annotations-spec sen :type (gtype 'ghier-parse-node-type) :filter #'raw-pn))

(defun surface-pns (sen)
  (annotations-spec sen :type (gtype 'ghier-parse-node-type) :filter #'surface-pn))

(defun range-adj-substitute-pns (sen)
  (annotations-spec sen :type (gtype 'ghier-parse-node-type)
		    :filter #'range-adj-substitute-pn))

(defun range-adj-pns (sub-pn)
  (annotations-spec sub-pn :type (gtype 'ghier-parse-node-type)
		    :filter #'range-adj-pn))
