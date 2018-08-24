;;; -*- Mode: Lisp; Package: concept-graph; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 05/01/2012 made use of results from number-recognize.cl
yluo - 04/22/2012 Added output-concept-graphs, flush-concept-graph,
       pnode-unit-stats
yluo - 04/18/2012 rewrite linkage->graph to parse->graph etc. 
yluo - 04/17/2012 add concept-graph-init
yluo - 09/28/2011 Moved alp-longest-cui from feature-extract.cl to here.
yluo - 06/30/2010 Creation.

Package to extract concept graph group from the Stanford Parser or Link Grammar Parser Output
|#

(defpackage :concept-graph
  (:nicknames :cg)
  (:use :common-lisp :late :link :wordnet :util :norm :umlsize 
		#+allegro :excl :dbi.mysql)
  (:export 
   "*hash-llab*"
   "*hid2lab*"
   "lg-in-training?"
   "parse->graph"
   "pnode->unit"
   "pnode-longest-umlss"
   "phrase-longest-umlss"
   "remove-parts-sents"
   "root-sigsubp"
   "sigsub-nontrivialp"
   "sigsub-roots"
   "sigsub-size"
   "sigsub-trivialp"
   "singleton-lg-sigsub"
   "singleton-sigsub-general-lg"
   "subisomorphic-roots"
   "persist-singleton-lg-sigsub"
   "persist-singleton-sigsub"
   "uninode-sg-map"
   ))

(in-package :concept-graph)
(defparameter *hash-llab* nil)
(defparameter *hid2lab* nil)

(defun concept-graph-init (&key (fn "late:;syn_deps") 
						 &aux (id 0)
							  (line nil))
  (assert (probe-file fn)
      ()
    "Please specify a valid syntax dependency file.")
  (unless *hash-llab*
    (setf *hash-llab* (make-hash-table :test #'equalp))
	(setf *hid2lab* (make-hash-table :test #'equalp))
    (with-open-file (f fn :direction :input)
      (loop
		(unless (setf line (read-line f nil nil)) (return))
		(setf line (replace-re line "(^\\s+|\\s+$)" ""))
		(setf (gethash line *hash-llab*) id)
		(setf (gethash id *hid2lab*) line)
		(incf id)))))

(defmethod phrase-head-synsenses ((ph phrase-annotation)
								  &key (persist t))
  (let* ((ph-head (car (ph-head ph)))
		 (ph-type (format nil "~a" (data ph)))
		 (hd-type (cond
					((equalp ph-type "ADJP") "a")
					((equalp ph-type "ADVP") "r")
					((equalp ph-type "VP") "v")))
		 (hd-str (content ph-head))
		 (synsenses (synsensedef-ff hd-str hd-type)))
    (when persist
      (dolist (synsense synsenses)
		(late-transaction
		 (let* ((s-freq (wn-synset-freq synsense hd-str hd-type)))
		   (if s-freq
			   (update-wn-synset synsense hd-str (1+ s-freq) hd-type)
			   (save-wn-synset synsense hd-str 1 hd-type))))))))

(defmethod phrase-longest-umlss ((ph phrase-annotation) 
								 &key (type 'umls-annotation)
									  (persist t))
  "find the longest phrase-head containing umls'es within a phrase annotation, stores them in a table so that we can calculate a distribution later on."
  (let* ((ph-head (car (ph-head ph)))
		 (umlss (stable-sort
				 (annotations-spec 
				  ph 
				  :type type 
				  :filter #'(lambda (a) 
							  (and (<= (start a) (start ph-head))
								   (<= (end ph-head) (end a)))))
				 #'(lambda (a b)
					 (>= (w-tok-len (lp-tokens a)) 
						 (w-tok-len (lp-tokens b))))))
		 (max-length (w-tok-len (lp-tokens (car umlss))))
		 (longest-umlss (remove-if 
						 #'(lambda (a) 
							 (< (w-tok-len (lp-tokens a)) max-length))
						 umlss)))
;;;    (unless (listp umlss)
;;;      (setf umlss (cons umlss nil)))
    (when persist
      (dotimes (i (length longest-umlss))
		(let* ((umls (elt longest-umlss i))
			   content)
		  ;; umls must contain alp-token
		  (cond 
		   ((equalp type 'mesh-annotation)
			(setf content (split-re "\\." (format nil "~a" (data umls))))
			(if (< *mesh-tree-depth* (length content))
				(error "mesh depth exceeding max: ~a (~a)" 
					   (length content) (data umls)))
			(dotimes (j *mesh-tree-depth*)
			  (save-phrase-longest-umls 
			   (id ph) (id umls) 
			   (format nil "~{~a~^.~}" (subseq content 0 (min (1+ j) (length content))))
			   (format nil "mesh-~a" (1+ j)))))
	 
		   ((equalp type 'tui-annotation)
			(setf content (split-re "\\." (tui->stn umls)))
			(if (< *stn-depth* (length content))
				(error "STN depth exceeding max: ~a (~a)" 
					   (length content) (tui->stn umls)))
			(dotimes (j *stn-depth*)
			  (save-phrase-longest-umls 
			   (id ph) (id umls) 
			   (format nil "~{~a~^.~}" (subseq content 0 (min (1+ j) (length content))))
			   (format nil "stn-~a" (1+ j)))))
	 
		   ((equalp type 'cui-annotation)
			(save-phrase-longest-umls (id ph) (id umls) (data umls) "cui"))))))
    longest-umlss))

(defun pnode-longest-umlss (pnode
							&key (type 'tui-annotation))
  "find the longest phrase-head containing umls'es within a phrase annotation, stores them in a table so that we can calculate a distribution later on.
forget about phrase head, procedure is noisy.
I want the most popular one, then
I only want the last one.
it should contain at least some noun?"
  (let* ((umlss (annotations-spec pnode :type type :filter #'ann-has-nounp))
		 (umlss (stable-sort umlss #'ann-w-tok-len-greaterp))
		 (max-length 0)
		 (last-start 0)
		 (humls-freq (make-hash-table :test #'equalp))
		 freq-umls-labs
		 max-freq)
    (when umlss
      (setf max-length (w-tok-len (car umlss))))
    (setf umlss (remove-if #'(lambda (a) (< (w-tok-len a) max-length)) umlss))
	
	;; sort according to frequency of umls label
	(mapcar #'(lambda (a) (incf (gethash (data a) humls-freq 0))) umlss)
	(setf freq-umls-labs (hash-table-val-desc-alist humls-freq))
	(setf max-freq (cdar freq-umls-labs))
	(setf freq-umls-labs (remove-if #'(lambda (a) (< (cdr a) max-freq))
									freq-umls-labs))
	(setf freq-umls-labs (mapcar #'car freq-umls-labs))
	(setf umlss (remove-if-not #'(lambda (a) 
								   (member (data a) freq-umls-labs 
										   :test #'equalp))
							   umlss))
	(setf umlss (nreverse (stable-sort umlss #'annotation-lessp)))
	(when umlss
	  (setf last-start (start (car umlss))))
	(remove-if #'(lambda (a) (< (start a) last-start)) umlss)))

(defun pnode-long-umlss (pnode
						 &key (type 'tui-annotation))
  "find the umls annotations in a parse node that are not subsumed by others"
  (let* ((umlss (annotations-spec pnode :type type))
		 (umlss (stable-sort umlss #'ann-w-tok-len-greaterp))
		 ans)
 	(dolist (umls umlss)
	  (when (and (notany #'(lambda (a) (subsumed? umls a)) ans)
				 (not (match-re "cd4\\S?\\s+and\\s+cd8" (content umls) :case-fold t)))
		(push umls ans)))
	ans))

;; This reflects alternatives to use nominalized words
(defun verb-adj-adv-pnode->unit (pnode pos-tag &key (use-wn-synset? nil))
  (cond
	(use-wn-synset?
	 (let* ((word (string-downcase (data pnode)))
			(pos (cond 
				   ((member pos-tag *ptb-adj-list* :test #'equalp)
					"a")
				   ((member pos-tag *ptb-adv-list* :test #'equalp)
					"r")
				   (t
					"v"))))
	   (or (popular-syns word pos)
		   (car (norm word))
		   word)))
	((member pos-tag '("VBZ" "VBP" "VBD") :test #'equalp)
	 "VBA")
	(t
	 pos-tag)))

(defstruct (nunit
			(:type vector)
			(:conc-name nu-))
  nlab satellite modifier satellite-type)

(defun blow-mesh (mesh &aux mstr ans mesh-heading)
  (setf mesh (data mesh))
  
  (dolist (lstr (split-re "\\." mesh))
	(cond
	 (mstr
	  (setf mstr (concatenate 'string mstr "." lstr)))
	 (t
	  (setf mstr lstr)))
	(setf mesh-heading (h-mesh-heading mstr))
	(when mesh-heading
	  (pushnew mesh-heading ans :test #'equalp)))
  ans)

(defun mesh-adjp (mesh)
  (let* ((l-pos (annotations mesh :type (gtype 'gtag-type)))
		 (pos (data (car (last l-pos)))))
	(cond
	 ((member pos '("JJ" "JJR" "JJS" "VBN") :test #'equalp)
	  pos)
	 (t
	  nil))))

(defun last-anns (anns &aux last-anns last-start)
  "Given a list of anns, find the last ones."
  (when anns
	(setf last-anns (stable-sort (copy-list anns) #'annotation-lessp))
	(setf last-anns (nreverse last-anns))
	(setf last-start (start (car last-anns)))
	(remove-if #'(lambda (a) (< (start a) last-start)) last-anns)))

(defun noun-pnode->mesh-unit (pnode)
  (let* ((meshs (pnode-long-umlss pnode :type 'mesh-annotation))
;;;		 (postags (annotations-spec pnode :type (gtype 'gtag-type)))
;;;		 (postag (car (last (remove-if-not #'ptb-nounp postags))))
		 ;; (tuis (pnode-longest-umlss pnode :type 'tui-annotation))
		 ;; (stns (trim-stns (mapcar #'tui->stn tuis)))
		 head-meshs headings nlab satellite modifier satellite-type)
	(cond
	 (meshs
	  (setf head-meshs (last-anns meshs))
	  (setf headings (remove-duplicates (mapcar #'h-mesh-heading head-meshs)
										:test #'equalp))
	  (cond
	   (headings
		(when (> (length headings) 1)
		  (format t "~&warning: ~a has ambiguous MeSH headings ~a~%"
				  pnode headings))
		(setf nlab (car headings)))
	   (t
		(setf nlab (safe-norm (content pnode)))))
	  ;; modifiiers
	  (dolist (mesh (set-difference meshs head-meshs :test #'equalp))
		(pushnew mesh modifier :test #'equalp))
	  ;; satellites
	  (dolist (mesh head-meshs)
		(setf satellite (union satellite (blow-mesh mesh) :test #'equalp)))
	  (setf satellite-type "node-satellite-mesh"))
	 (t
	  (setf nlab (safe-norm (content pnode)))))
	
	(make-nunit :nlab (string-downcase nlab)
				:satellite satellite :modifier modifier
				:satellite-type satellite-type)))

(defun noun-pnode->stn-unit (pnode)
  (let* ((tuis (pnode-long-umlss pnode :type 'tui-annotation))
		 (head-tuis (last-anns tuis))
		 (stns (trim-stns (mapcar #'tui->stn head-tuis)))
		 nlab modifier)
	
	(cond
	 ((has-karyotype pnode)
	  (setf nlab "Karyotype_Aberrance"))
	 (stns
	  (setf nlab (tuis->sty head-tuis))
	  ;; modifiiers
	  (dolist (tui (remove-if #'(lambda (a) 
								  (> (end a) (start (car head-tuis)))) 
							  tuis))
		(pushnew tui modifier :test #'equalp)))
	 (t
	  (setf nlab (safe-norm (content pnode)))))
	
	(make-nunit :nlab (string-downcase nlab) :modifier modifier)))



(defun noun-pnode->cui-unit (pnode)
  (let* ((cuis (pnode-long-umlss pnode :type 'cui-annotation))
		 (head-cuis (last-anns cuis))
		 (str (string-downcase (data pnode)))
		 nlab)
	
	(cond
	  ((has-karyotype pnode)
	   (setf nlab str)) 

	  ((= (start pnode) (end pnode))
	   (setf nlab (stemmer:stem str)))

	  ((not (match-re "\\s" (content pnode)))
	   (setf nlab (stemmer:stem str)))

	  (head-cuis
	   (setf nlab (pname (data (car head-cuis)))))

	  (t
	   (setf nlab (stemmer:stem str))))

	(setf nlab (reg-lab-str nlab))
	

	;; for specially inserted parse node
	(when (equalp "lambda" nlab)
	  (setf nlab "immunoglobulin_lambda-chains"))
	(when (equalp "kappa" nlab)
	  (setf nlab "immunoglobulin_kappa-chains"))
	(when (equalp "fdc" nlab)
	  (setf nlab "dendritic_cells,_follicular"))
	
	

	;; (when (sp::neg-pn? pnode)
	;;   (setf nlab (concatenate 'string "neg_" nlab)))
	(make-nunit :nlab (string-downcase nlab))))

(defun noun-pnode->unit (pnode)
  (cond
	((eq (gkey 'ghier-concept-graph 'umls) late::'cui)
	 (noun-pnode->cui-unit pnode))
	((eq (gkey 'ghier-concept-graph 'umls) late::'mesh)
	 (noun-pnode->mesh-unit pnode))
	((eq (gkey 'ghier-concept-graph 'umls) late::'stn)
	 (noun-pnode->stn-unit pnode))))

(defun numeric-pnode->unit (pnode &aux dnum)
  (setf dnum (car (annotations-on pnode :type 'discrete-num)))
  (cond 
	(dnum
	 (make-nunit :nlab (data dnum)))
	;; ((match-re "^\\d+[pq]\\d+$" (content pnode))
	;;  (make-nunit :nlab (content pnode)))
	((annotations pnode :type 'date-pattern)
	 (make-nunit :nlab "date"))
	((annotations pnode :type 'time-pattern)
	 (make-nunit :nlab "time"))
	((annotations pnode :type 'date-range-pattern)
	 (make-nunit :nlab "date_range"))
	((annotations pnode :type 'phone-number-pattern)
	 (make-nunit :nlab "phone_num"))
	((annotations pnode :type 'range-of-ratios-pattern)
	 (make-nunit :nlab "range_of_ratios"))
	((annotations pnode :type 'ratio-of-ranges-pattern)
	 (make-nunit :nlab "ratio_of_ranges"))
	((annotations pnode :type 'accession-number-pattern)
	 (make-nunit :nlab "accession_number"))
	(t							  ; do not use CD, as it's ambiguate.	
	 (make-nunit :nlab "general_number"))))

(defun function-pnode->unit (pnode)
  (make-nunit :nlab (string-downcase (data pnode))))

(defun adv-pnode->unit (pnode)
  (let* ((adv (data pnode))
		 ;; (adv (bl::get-lemma adv (pnode-pos-tag pnode)))
		 ;; (adv (or (h-nominalize adv "adj") adv))
		 (nlab (stemmer:stem (string-downcase adv))))
	(setf nlab (reg-lab-str nlab))
	(make-nunit :nlab nlab)))


(defun verb-pnode->unit (pnode)
  (let* ((verb (data pnode))
		 ;; (verb (bl::get-lemma verb (pnode-pos-tag pnode)))
		 ;; (verb (or (h-nominalize verb "verb") verb))
		 (nlab (stemmer::stem (safe-norm verb)))) ; changed from string-downcase to safe-norm
	;; (when (sp::neg-pn? pnode)
	;;   (setf nlab (concatenate 'string "neg_" nlab)))
	(setf nlab (reg-lab-str nlab))
	(make-nunit :nlab nlab)))

(defun decode-adj (str &aux ans)
  (cond 
	((equalp "+" str)
	 (setf ans "positive"))

	((equalp "bright" str)
	 (setf ans "positive")) ; 02242014 bright -> positive

	((equalp "dim" str)
	 (setf ans "positive"))
		 
	((equalp "-" str)
	 (setf ans "negative"))
		 
	((member str '("+/-" "dim/-") :test #'equalp)
	 (setf ans "positive_negative"))
		 
	((equalp "-/+" str)
	 (setf ans "negative_positive"))
	
	(t
	 (error "unknown adj str ~a" str)))
  (stemmer:stem ans))


(defun adj-pnode->unit (pnode)
  (let* ((str (string-downcase (data pnode)))
		 (cuis (pnode-long-umlss pnode :type 'cui-annotation))
		 (head-cuis (last-anns cuis))
		 nlab)
	(cond
	  ((has-karyotype pnode)
	   (setf nlab str))

	  ((adj-decode pnode)
	   (setf nlab (decode-adj str))
	   (setf str ""))

	  ((= (start pnode) (end pnode))
	   (setf nlab (stemmer:stem str)))

	  ((not (match-re "\\s" (content pnode)))
	   (setf nlab (stemmer:stem str)))

	  (head-cuis
	   (setf nlab (pname (data (car head-cuis)))))

	  (t
	   ;; (setf nlab (bl::get-lemma str (pnode-pos-tag pnode)))
	   ;; (setf nlab (or (h-nominalize nlab "adj") nlab))
	   (setf nlab (stemmer:stem str))))

	(cond 
	 ((match-re "\\w(\\+|-positive)$" str)
	  (setf nlab (format nil "~a_~a" nlab "positive"))
	  (format t "~&[adj-pnode->unit] wrong str: ~a~%" str))

	 ;; was (setf adj (list (h-nominalize "bright" "adj")))
	 ((match-re "\\wbright$" str)
	  (setf nlab (format nil "~a_~a" nlab "bright"))
	  (format t "~&[adj-pnode->unit] wrong str: ~a~%" str))

	 ((match-re "\\wdim$" str)
	  (setf nlab (format nil "~a_~a" nlab "dim"))
	  (format t "~&[adj-pnode->unit] wrong str: ~a~%" str))
		 
	 ((match-re "\\w(-|-negative)$" str)
	  (setf nlab (format nil "~a_~a" nlab "negative"))
	  (format t "~&[adj-pnode->unit] wrong str: ~a~%" str))
		 
	 ((match-re "\\w\\+/-$" str)
	  (setf nlab (format nil "~a_~a" nlab "dim")) ; "positive_negative"
	  (format t "~&[adj-pnode->unit] wrong str: ~a~%" str))
		 
	 ((match-re "\\w-/\\+$" str)
	  (setf nlab (format nil "~a_~a" nlab "negative_positive"))
	  (format t "~&[adj-pnode->unit] wrong str: ~a~%" str)))

	;; (when (equalp "lacunar" (content pnode))
	;;   (setf nlab "reed_sternberg"))
	;; (when (equalp "cleaved" (data pnode))
	;;   (setf nlab "cleaved"))
	;; (when (equalp "irregular" (data pnode))
	;;   (setf nlab "irregular"))

	(setf nlab (reg-lab-str nlab))
	
	(make-nunit :nlab (string-downcase nlab))))

(defun get-pnode-pos-tag (pnode)
  (cond
   ((typep pnode 'parse-node-stanford-hier-tagged)
	(pnode-pos-tag pnode))
   ((typep pnode 'parse-node-stanford-tagged)
	(let* ((pta (car (annotations-spec pnode :type (gtype 'gtag-type)))))
	  (data pta)))
   (t
	(error "unhandled pnode type ~a" (type-of pnode)))))

(defmethod pnode->unit ((pnode parse-node))
  ;;&key (use-wn-synset? nil)
  (let* ((pos-tag (get-pnode-pos-tag pnode)) 
		 ans)
    (cond
	 ;; function words
     ((ptb-functionp pos-tag)
      (setf ans (function-pnode->unit pnode)))
	 
	 ;; adverbs
	 ((ptb-advp pos-tag)
	  (setf ans (adv-pnode->unit pnode)))
	 
	 ;; the verbs  
     ((ptb-verbp pos-tag)
	  (setf ans (verb-pnode->unit pnode)))
     
	 ;; the adjectives
	 ((ptb-adjp pos-tag)
	  (setf ans (adj-pnode->unit pnode)))
	 
	 ;; nouns 
	 ((ptb-nounp pos-tag)
	  (setf ans (noun-pnode->unit pnode)))
	 
	 ;; pronouns
	 ((ptb-pronp pos-tag)
	  (setf ans (make-nunit :nlab pos-tag)))
	 
     ((ptb-nump pos-tag)
      (setf ans (numeric-pnode->unit pnode)))

	 ;; foreign words and symbols
     ((ptb-fsp pos-tag)
      (setf ans (make-nunit :nlab pos-tag)))

     (t
      (when (and (match-re "\\w" pos-tag) (not (match-re "-\\w" pos-tag)))
		(format t "~&Warning: ~a is not in Penn Treebank tags~%" pos-tag))
      (setf ans (make-nunit :nlab pos-tag))))
	ans))

(defun reg-lab-str (str &aux nstr)
  (setf nstr (replace-re str "\\s+" "_"))
  (setf nstr (replace-re nstr "#" "sharp_sign"))
  ;; (setf nstr (replace-re nstr "[+\\-/]+$" ""))
  (when (equalp "" nstr)
	(format t "~&Warning: empty str after regularize ~a~%" str))
  nstr)

;;; construct labeled graph from the Link Grammar Output
;;; return a string of graph representation 
;;; added factor graph representation, notice that it gets rid of the need for 
;;; global variable *hash-llab*
(defparameter *lg-min-size* 1)


(defmethod node-edges ((pnode parse-node-link))
  ;; as I am traversing all tokens, I only need left links
  (left-links pnode))

(defmethod node-edges ((pnode parse-node-stanford))
  (gov-deps pnode))

(defun edge-from-node (edge)
  (cond
	((member (ganalysis 'gparse) *l-link-parse*)
	 (1- (l-lindex edge)))
	((member (ganalysis 'gparse) *l-stanford-parse*)
	 (sp::d-govid edge))
	(t
	 (error "Unknown parse option ~a" (ganalysis 'gparse)))))

(defun edge-to-node (edge)
  (cond
	((member (ganalysis 'gparse) *l-link-parse*)
	 (1- (l-rindex edge)))
	((member (ganalysis 'gparse) *l-stanford-parse*)
	 (sp::d-depid edge))
	(t
	 (error "Unknown parse option ~a" (ganalysis 'gparse)))))

(defun edge-lab (edge)
  (cond 
	((member (ganalysis 'gparse) *l-link-parse*)
	 (multiple-value-bind (m? lab)
		 (match-re "^[A-Z]+" (l-lab edge))
	   (declare (ignorable m?))
	   (assert lab
			   ()
			   "Invalid link label input ~a." (l-lab edge))
	   lab))
	((member (ganalysis 'gparse) *l-stanford-parse*)
	 (sp::d-rel edge))
   
	(t
	 (error "Unkown parse option ~a" (ganalysis 'gparse)))))

(defun DT-pnode?2 (pnode)
  (let* ((pta (car (annotations-spec pnode :type (gtype 'gtag-type))))
		 (pos-tag (format nil "~a" (data pta))))
	(member pos-tag (list "DT" "WDT" "PDT") :test #'equalp)))

(defun DT-pnode? (pnode)
  (let* ((pn-str (content pnode)))
	(member (string-downcase pn-str) (list "a" "an" "the" "this" "that" "these" "those") :test #'equalp)))

(defun POS-pnode? (pnode)
	(member (pnode-pos-tag pnode) (list "POS") :test #'equalp))

(defun PRN-pnode? (pnode)
  (remove-if-not #'(lambda (a) (equalp "PRN" (data a)))
				 (annotations-spanning pnode :type (gtype 'gphrase-type))))

(defun mesh-adj-mod (mesh sent cnid efid vid &aux (add-noun t) str)
  (setf str (content mesh))
  (cond 
   ((match-re "\\w(\\+|-positive)$" str)
	(save-lg-node efid "node-modifier" cnid (id mesh) "positivity" (id sent))
	(save-lg-edge efid "edge" vid (id mesh) cnid (id mesh) 
				  "amod" (id sent)))
   ((match-re "\\w(-|-negative)$" str)
	(save-lg-node efid "node-modifier" cnid (id mesh) "negativity" (id sent))
	(save-lg-edge efid "edge" vid (id mesh) cnid (id mesh) 
				  "amod" (id sent)))
   ((match-re "\\w\\+/-$" str)
	;; +/- -> positivity
	(save-lg-node efid "node-modifier" cnid (id mesh) "positivity" (id sent))
	(save-lg-edge efid "edge" vid (id mesh) cnid (id mesh) 
				  "amod" (id sent)))
   ;; -/+ -> negativity
   ((match-re "\\w-/\\+$" str)
	(save-lg-node efid "node-modifier" cnid (id mesh) "negativity" (id sent))
	(save-lg-edge efid "edge" vid (id mesh) cnid (id mesh) 
				  "amod" (id sent)))
   (t
	(setf str (safe-norm str))
	(setf str (bl::get-lemma str "adj"))
	;; mesh modifier can be adjective or VBN
	(setf str (or (h-nominalize str "adj")
				  (h-nominalize str "verb")
				  str))
	(save-lg-node efid "node-modifier" cnid (id mesh)
				  (reg-lab-str str) (id sent))
	(save-lg-edge efid "edge" vid (id mesh) cnid (id mesh) 
				  "amod" (id sent))
	(setf add-noun nil)))
  add-noun)

(defun tui-nominalp (tui)
  (let* ((lpos (annotations-spec tui :type (gtype 'gtag-type)))
		 (pos (car (nreverse lpos))))
	(ptb-nounp pos)))

(defun tui-adj-mod (tui sent cnid efid vid &aux (add-noun t) str)
  (setf str (content tui))
  (cond 
   ((match-re "\\w(\\+|-positive)$" str :case-fold t)
	(save-lg-node efid "node-modifier" cnid (id tui) "positivity" (id sent))
	(save-lg-edge efid "edge" vid (id tui) cnid (id tui) 
				  "nsubj" (id sent))	; amod -> nsubj
	(incf cnid))									
   ((match-re "\\w(-|-negative)$" str :case-fold t)
	(save-lg-node efid "node-modifier" cnid (id tui) "negativity" (id sent))
	(save-lg-edge efid "edge" vid (id tui) cnid (id tui) 
				  "nsubj" (id sent))
	(incf cnid))						; amod -> nsubj
   ((match-re "\\w\\+/-$" str :case-fold t)
	;; +/- -> positivity
	(save-lg-node efid "node-modifier" cnid (id tui) "positivity" (id sent))
	(save-lg-edge efid "edge" vid (id tui) cnid (id tui) 
				  "nsubj" (id sent))
	(incf cnid))						; amod -> nsubj
   ;; -/+ -> negativity
   ((match-re "\\w-/\\+$" str :case-fold t)
	(save-lg-node efid "node-modifier" cnid (id tui) "negativity" (id sent))
	(save-lg-edge efid "edge" vid (id tui) cnid (id tui) 
				  "nsubj" (id sent))
	(incf cnid))						; amod -> nsubj
   
   ;; nsubj -> nn
   ((and (tui-nominalp tui)
		 (equalp (data tui) "T129"))
	(save-lg-node efid "node-modifier" cnid (id tui)
				  (replace-re (stn->sty-rl (tui->stn (data tui))) " +" "_") 
				  (id sent))
	(save-lg-edge efid "edge" vid (id tui) cnid (id tui) "prep_with" (id sent))
	(incf cnid)
	(setf add-noun nil))
   (t
;;;	(setf str (safe-norm str))
;;;	(setf str (bl::get-lemma str "adj"))
;;;	;; tui modifier can be adjective or VBN
;;;	(setf str (or (h-nominalize str "adj") (h-nominalize str "verb") str))
;;;	(save-lg-node efid "node-modifier" cnid (id tui)
;;;				  (reg-lab-str str) (id sent))
;;;	;; amod -> nsubj
;;;	(save-lg-edge efid "edge" vid (id tui) cnid (id tui) "nsubj" (id sent)) 
	(setf add-noun nil))
   )
  (list cnid add-noun))

(defun pnode-has-conj (pnode)
  (let* ((postags (annotations-spec pnode :type (gtype 'gtag-type)))
		 (postags (mapcar #'data postags)))
	(member "CC" postags :test #'equalp)))

(defun graph-syntax-rewriting (edge pnode pnodes efid eid cnid)
  "deprecated, need to adapt to edge-from-node returning database id"
  (let* ((fn (edge-from-node edge))
		 (tn (edge-to-node edge))
		 (lab (replace-re (edge-lab edge) "-+" "_")) ;; ? 
		 (fnid (id (elt pnodes fn)))
		 (tnann (elt pnodes tn))
		 (tnid (id tnann))
		 (tnlab (reg-lab-str (nu-nlab (pnode->unit tnann))))
		 (sent (car (annotations-spanning pnode :type 'sentence-annotation)))
		 fn2 fn2id skip-edge)
	(when (and (member lab '("prep_for" "prep_to") :test #'equalp)
			   (member (content pnode)
					   '("positive" "negative" "+/-" "-/+") 
					   :test #'equalp))
	  (setf lab "nsubj"))				; prep_for -> nsubj for imf
				
	(when (and (member lab '("dobj" "prep_of") :test #'equalp)
			   (search "express" (content pnode) :test #'equalp))
	  (setf lab "prep_with"))
				
	;; handles with antibodies to CD3, there is staining of cells.
	(when (and (member lab '("prep_with") :test #'equalp)
			   (or (member (content pnode) *copular* :test #'equalp)
				   (search "staining" (content pnode))))
	  (setf fn2 fn)
	  (setf fn2id fnid)
	  (dolist (edge2 (node-edges pnode))
		(let* ((tn2 (edge-to-node edge2))
			   (tn2ann (elt pnodes tn2))
			   (tn2id (id tn2ann)))
		  (cond 
		   ((search "staining" (content tn2ann) :test #'equalp)
			(setf fn2 tn2)
			(setf fn2id tn2id)))))
				  
	  (dolist (tedge (node-edges tnann))
		(let* ((telab (edge-lab tedge))
			   (ttn (edge-to-node tedge))
			   (ttnann (elt pnodes ttn))
			   (ttnid (id ttnann))
			   (ttnlab (reg-lab-str (nu-nlab (pnode->unit ttnann)))))
		  (format t "~&tnlab: ~a; telab: ~a~%" tnlab telab)
		  (when (and (member telab '("appos" "dep") :test #'equalp)
					 (equalp "Immunologic_Factor" tnlab))
			(format t "~&~a: [~a] prep_with [~a] dep/appos [~a]~%"
					sent (content pnode) (content tnann) 
					(content ttnann))
			(save-lg-edge efid "edge" fn fnid ttn ttnid "prep_with" (id sent))
			(incf eid)
						
			(dolist (ttedge (node-edges ttnann))
			  (let* ((ttelab (edge-lab ttedge))
					 (t3n (edge-to-node ttedge))
					 (t3nann (elt pnodes t3n))
					 (t3nid (id t3nann)))
				(when (equalp "appos" ttelab)
				  (format t "~&~a: [~a] prep_with [~a] dep/appos [~a] dep [~a]~%"
						  sent (content pnode) (content tnann) 
						  (content ttnann) (content t3nann))
				  (save-lg-edge efid "edge" fn fnid t3n t3nid "prep_with" (id sent))
				  (incf eid))))
			(setf skip-edge t))
					  
					  
		  (when (and (equalp "prep_to" telab)
					 (equalp "Immunologic_Factor" ttnlab))
			(when (pnode-has-conj ttnann)
			  (dolist (tui (annotations ttnann :type 'tui-annotation))
				(when (and (equalp (data tui) "T129")
						   (not (match-re "(antibod|antigen)" (content tui))))
				  (format t "~&~a: [~a] prep_with [~a] prep_to [~a] tui ~a~%"
						  sent (content pnode) (content tnann) (content ttnann) tui)
				  (save-lg-node efid "node-list-noun" cnid (id tui) 
								"Immunologic_Factor" (id sent))
							  
				  (save-lg-edge efid "edge" fn2 fn2id cnid (id tui) "prep_with" (id sent))
				  (incf cnid)
				  (incf eid))))
			(dolist (ttedge (node-edges ttnann))
			  (let* ((ttelab (edge-lab ttedge))
					 (t3n (edge-to-node ttedge))
					 (t3nann (elt pnodes t3n))
					 (t3nid (id t3nann)))
							
				(when (member ttelab '("dep" "appos") :test #'equalp)
				  (format t "~&~a: [~a] prep_with [~a] prep_to [~a] dep [~a]~%"
						  sent (content pnode) (content tnann) 
						  (content ttnann) (content t3nann))
				  (save-lg-edge efid "edge" fn2 fn2id t3n t3nid "prep_with" (id sent))
				  (incf eid)
							  
				  (dolist (t3edge (node-edges t3nann))
					(let* ((t3elab (edge-lab t3edge))
						   (t4n (edge-to-node t3edge))
						   (t4nann (elt pnodes t4n))
						   (t4nid (id t4nann)))
					  (when (equalp "appos" t3elab)
						(format t "~&~a: [~a] prep_with [~a] prep_to [~a] dep [~a] appos [~a]~%"
								sent (content pnode) (content tnann) 
								(content ttnann) (content t3nann)
								(content t4nann))
						(save-lg-edge efid "edge" fn2 fn2id t4n t4nid "prep_with" (id sent))
						(incf eid)))))))
			(setf skip-edge t)))))
				
	;; adding one nsubj, to cover With antibodies to BCL2, tumor cells are positive.
	(when (and (member lab '("prep_with") :test #'equalp)
			   (member (content pnode)
					   '("positive" "negative" "+/-" "-/+") 
					   :test #'equalp))
	  (dolist (tedge (node-edges tnann))
		(let* ((telab (edge-lab tedge))
			   (ttn (edge-to-node tedge))
			   (ttnann (elt pnodes ttn))
			   (ttnid (id ttnann)))
		  (when (equalp "prep_to" telab)
			(format t "~&~a: [~a] prep_with [~a] prep_to [~a]~%"
					sent (content pnode) (content tnann) 
					(content ttnann))
			(save-lg-edge efid "edge" fn fnid ttn ttnid "nsubj" (id sent))
			(incf eid)))))
				
				
	(when (and (member lab '("prep_with" "prep_for") :test #'equalp)
			   (member (safe-norm (content pnode)) '("stain") 
					   :test #'equalp))
	  (dolist (edge2 (node-edges pnode))
		(let* ((tn2 (edge-to-node edge2))
			   (tn2ann (elt pnodes tn2))
			   (tn2id (id tn2ann)))
		  (cond 
		   ((member (content tn2ann) '("positively" "negatively")
					:test #'equalp)
			(format t "~&~a: stains [~a] with/for [~a]~%"
					sent (content tn2ann) (content tnann))
			(save-lg-edge efid "edge" tn tnid tn2 tn2id "nsubj" (id sent))
			(incf eid))))))
	(list skip-edge cnid eid)))

;; "dep" "parataxis"
(defun elab-trivial? (lab)
  (or (member lab '("cop" "attr" "cc" "complm" "dep" "parataxis" "expl" "poss" "possessive") :test #'equalp)
	  (match-re "conj_(and|or|but)" lab)
	  (match-re "aux" lab)))

(defmethod parse->plain-graph ((sent sentence-annotation)
							   &key 
							   (hier? nil))
  ;; scan parse-nodes and construct graphs from dependencies
  (let* ((vid 0) ;; would be the # vertices at the end of loop
		 (eid 0) ;; would be the # edges at the end of loop
		 (ef-type "plain_graph")
		 (pnode-type (if hier? 
						 (gtype 'ghier-parse-node-type)
						 (gtype 'gparse-node-type)))
		 (pnodes (annotations-spec sent :type pnode-type :filter #'raw-pn))
		 (pnids (mapcar #'id pnodes))
		 ;; hlink prevents inserting same link multiple times bw/ nodes
		 (hlink (make-hash-table :test #'equalp))
		 (doc (document sent))
		 (graph-name (format nil "~a_~a" (id doc) (id sent)))
		 (graph-type (format nil "~a|~a" ef-type pnode-type))
		 efid
		 (cnid (length pnodes))) 
	(when pnodes
	  (setf efid (save-linkage-graph graph-name graph-type)))
	;; (format t "~&[parse->plain-graph] ~{~a~%~}" pnodes)
    (when efid 
      (dolist (pnode pnodes)
		;; include masked vertices in the output graph file but not links
		;; (unless (DT-pnode? pnode)
		(let* ((nunit (pnode->unit pnode))
			   (nlab (reg-lab-str (nu-nlab nunit)))
			   (satellite (nu-satellite nunit))
			   (satellite-type (nu-satellite-type nunit))
			   (mods (nu-modifier nunit))
			   htui mkey add-noun)
		  ;; well, for pnode whose content is "~", you need to be very careful in 
		  ;; choosing the ~a and ~s output options in order not 
		  ;; to get it omitted in the text output, but, I don't think it affects
		  ;; database storage
		  (save-lg-node efid "node" vid (id pnode) nlab (id sent))
		  
		  (unless (maskedp pnode)
			;; also filter out WALL links and adjust indexes
			(dolist (edge (node-edges pnode))
			  (let* ((fnid (edge-from-node edge))
					 (tnid (edge-to-node edge))
					 (lab (replace-re (edge-lab edge) "-+" "_")) ;; ? 
					 (fpn (find-annotation fnid sent))
					 (tpn (find-annotation tnid sent))
					 (fn (position fnid pnids))
					 (tn (position tnid pnids)))

				
				(when (and (>= fn 0)	
						   (not (maskedp fpn))
						   (not (maskedp tpn))
						   (not (DT-pnode? (elt pnodes fn)))
						   (not (POS-pnode? (elt pnodes fn)))
						   ;; (not (PRN-pnode? (elt pnodes fn)))
						   (not (DT-pnode? (elt pnodes tn)))
						   (not (POS-pnode? (elt pnodes tn)))
						   ;; (not (PRN-pnode? (elt pnodes tn)))
						   (not (elab-trivial? lab))
						   ;; (not skip-edge)
						   )
				  (let* ((edge-sig (format nil "~a-~a-~a" fn tn lab)))
					(unless (gethash edge-sig hlink)
					  (setf (gethash edge-sig hlink) 1)
					  (setf lab "nn") ;; 08/21 unifying
					  (save-lg-edge efid "edge" fn fnid tn tnid lab (id sent))
					  (incf eid)))))))
		  
		  (when (and  ;; (not (maskedp pnode))
					 (not (DT-pnode? pnode))) ; (not (PRN-pnode? pnode))
			(dolist (cnlab satellite)
			  (setf cnlab (reg-lab-str cnlab))
			  
			  (save-lg-node efid satellite-type cnid (id pnode) cnlab (id sent))
			  ;; concept -> nsubj ?
			  (save-lg-edge efid "edge" vid (id pnode) cnid (id pnode) 
							"concept" (id sent))
			  (incf cnid)
			  (incf eid))
			;; this two hash table ensure meshes with same spans are grouped
			(setf htui (make-hash-table :test #'equalp))
			;; (setf hmstr (make-hash-table :test #'equalp))
			(dolist (mod mods)
			  
			  (cond 
			   ((typep mod 'tui-annotation)
				(setf mkey (format nil "~a-~a" (start mod) (end mod)))
				(unless (gethash mkey htui)
				  ;; add adj modifiers
				  (destructuring-bind (r1 r2)
					  (tui-adj-mod mod sent cnid efid vid)
					(setf cnid r1
						  add-noun r2))
				  ;; well I suppose tui only refers to nouns
				  (when add-noun
					(save-lg-node efid "node-modifier-noun" cnid (id mod) 
								  (replace-re (stn->sty-rl (tui->stn mod)) " +" "_") 
								  (id sent))
					(save-lg-edge efid "edge" cnid (id mod) 
								  (1- cnid) (id mod) "nsubj" (id sent))
					(incf cnid)
					(incf eid))
										; prep_for -> nsubj
				  (setf (gethash mkey htui) cnid)))
			   ((typep mod 'string)
				
				(save-lg-node efid "node-modifier" cnid (id pnode) mod (id sent))
				(save-lg-edge efid "edge" vid (id pnode) cnid (id pnode) 
							  "nsubj" (id sent))
				(incf cnid)
				(incf eid))
			   (t
				(error "~&unhandled modifier ~a~%" mod)))))
		  (incf vid))))))




(defmethod parse->factor-graph ((sent sentence-annotation)
								&key
								(hier? nil))
  "deprecated, need to adapt to edge-from-node returning database id"
  ;; scan parse-nodes and construct graphs from dependencies
  (let* ((vertex-txt (outstr-init))
		 (edge-txt (outstr-init))
		 (corp-vertex-txt (outstr-init))
		 (corp-edge-txt (outstr-init))
		 (res-txt (outstr-init))
		 (corp-res-txt (outstr-init))
		 (vid 0) ;; would be the # vertices at the end of loop
		 (eid 0) ;; would be the # edges at the end of loop
		 (pnode-type (if hier? 
						 (gtype 'ghier-parse-node-type)
					   (gtype 'gparse-node-type)))
		 (pnodes (annotations-spec sent :type pnode-type)) 
		 (hlink (make-hash-table :test #'equalp))
		 (graph-name (format nil "~a_~a" (id (document sent)) (id sent)))
		 (graph-type (format nil "factor-graph|~a" pnode-type))
		 efid) 
	
	;; add a filter here to exclude cui mentioning tokens.
    (setf efid (save-linkage-graph graph-name graph-type))
    (when efid 
      (dolist (pnode pnodes)
		;; include masked vertices in the output graph file but not links
		(format vertex-txt "~&~a, ~a~%" (data pnode) (pnode->unit pnode))
		;; well, for pnode whose content is "~", you need to be very careful in 
		;; choosing the ~a and ~s output options in order not 
		;; to get it omitted in the text output, but, I don't think it affects
		;; database storage
		(format corp-vertex-txt "~&~a~%" (pnode->unit pnode))
		(save-lg-node efid "node" vid (id pnode) (pnode->unit pnode) (id sent))
		(incf vid))
      (dolist (pnode pnodes)
		(unless (maskedp pnode)      
		  ;; also filter out WALL links and adjust indexes
		  (dolist (edge (node-edges pnode))
			(let* ((fn (edge-from-node edge))
				   (tn (edge-to-node edge))
				   (lab (edge-lab edge))
				   (fnid (id (elt pnodes fn)))
				   (tnid (id (elt pnodes tn))))
			  (when (and (>= fn 0) (not (maskedp (elt pnodes fn))))
				(let* ((edge-sig (format nil "~a-~a-~a" fn tn lab)))
				  (unless (gethash edge-sig hlink)
					(setf (gethash edge-sig hlink) 1)
					(format vertex-txt "~&lg_~a~%" lab)		  
					(format corp-vertex-txt "~&lg_~a~%" lab)
					(save-lg-node efid "link_node" vid 0 
								  (format nil "lg_~a" lab) (id sent))
		      
					(format edge-txt "~&~a ~a 1~%" fn vid)
					(format corp-edge-txt "~&~a ~a 1~%" fn vid)
					(format edge-txt "~&~a ~a 1~%" vid tn)
					(format corp-edge-txt "~&~a ~a 1~%" vid tn)
					(save-lg-edge efid "edge" fn fnid vid 0 1 (id sent))
					(save-lg-edge efid "edge" vid 0 tn tnid 1 (id sent))
					(incf vid)
					(incf eid)
					(incf eid)))))))))
	;; docid, sentid, and graph (concept graph) ID
    (when (> eid 0) ;; filter out graph with no effective edges
      (format res-txt "~&#~a_~a_~a~%" (id (document sent)) (id sent) efid) 
      (format res-txt "~&~a~%" vid)		; number of vertices
      (format res-txt "~&~a~%" vertex-txt) ; label of each vertex
      (format res-txt "~&~a~%" eid)		; number of edges
      (format res-txt "~&~a~%" edge-txt) ; edge end points and label
      
      (format corp-res-txt "~&#~a_~a~%" (id (document sent)) (id sent))
      (format corp-res-txt "~&~a~%" vid) ; number of vertices
      (format corp-res-txt "~&~a~%" corp-vertex-txt) ; label of each vertex
      (format corp-res-txt "~&~a~%" eid) ; number of edges
      (format corp-res-txt "~&~a~%" corp-edge-txt)) ; edge end points and label
    
    (cons res-txt corp-res-txt)))

(defmethod parse->graph ((sen sentence-annotation)
						 &key (hier? nil))
  (cond
	((or (and hier? (equalp "plain_graph" (gkey 'ghier-concept-graph 'graph-type)))
		 (and (not hier?) (equalp "plain_graph" (gkey 'gconcept-graph 'graph-type))))
	 (parse->plain-graph sen :hier? hier?))
   
	((or (and hier? (equalp "factor_graph" (gkey 'ghier-concept-graph 'graph-type)))
		 (and (not hier?) (equalp "factor_graph" (gkey 'gconcept-graph 'graph-type))))
	 (parse->factor-graph sen :hier? hier?))
   
	(t
	 (error "Invalid concept graph type ~a!" (gkey 'gconcept-graph 'graph-type)))))

(defmethod parse->graph ((doc document)
						 &key
						 (hier? nil))
  (concept-graph-init)
  (let* ((doc-gids (outstr-init))
		 (corp-gdb-txt (outstr-init))
		 (corp-gid-txt (outstr-init)))
	;; another option is to look at sentence annotation
    (dolist (sent (annotations doc :type 'sentence-annotation))
;;;      (setf pnodes (annotations-spec sent :type (gtype 'gparse-node-type)))
;;;      (when (> (length pnodes) 1)
;;;		)
	  (let* ((res (parse->graph sent :hier? hier?)))
		  (when (> (length (car res)) 0)
			(format doc-gids "~&~a_~a~%" (id doc) (id sent))
			(format corp-gdb-txt "~&~a" (cdr res)))))

    (format corp-gid-txt "~&~a~%" doc-gids)
	(if hier?
		(add-analysis doc :ganalysis 'ghier-concept-graph)
	  (add-analysis doc :ganalysis 'gconcept-graph))
    (cons corp-gdb-txt corp-gid-txt)))


(defmethod parse->graph ((corp corpus)
						 &key 
						 (hier? nil))
  (format t "~&Extracting linkage graph for corpus ~a~%" (name corp))
  (let* ((corp-gdbs (outstr-init))
		 (corp-gids (outstr-init))
		 (dir (gkey 'gconcept-graph 'outdir))
		 res)
    (create-dir (namestring (translate-logical-pathname dir)))
    
    (dolist (doc-id (documents corp))
      (when (annotations (document doc-id) :type 'sentence-annotation) 
		(setf res (parse->graph (document doc-id) :hier? hier?))
		(format corp-gdbs "~a~%~%" (car res))
		(format corp-gids "~a~%~%" (cdr res))))
    
    (with-open-file (fgdb (concatenate 'string dir "lg_" (name corp))
					 :direction :output :if-exists :supersede
					 :if-does-not-exist :create)
	  ;; remember to use ~a, otherwise you loose ~
      (format fgdb "~a" corp-gdbs))
    (with-open-file (fgid (concatenate 'string dir "gid_" (name corp))
					 :direction :output :if-exists :supersede
					 :if-does-not-exist :create)
      (format fgid "~a" corp-gids))
    (cons corp-gdbs corp-gids)))

(defun persist-singleton-sigsub (lab cnt
								 &key (sigsub-type "sig_subgraph|hier")
								 &aux sigsub-id)
  (latesql "insert into graph (gname, gtype) values (~a, ~a)" 
		   (sq (format nil "singleton: ~a" lab)) (sq sigsub-type))
  (setf sigsub-id (mysql-insert-id *late-db*))
  (latesql "insert into sig_subgraph (sub_id, n1, lab, type, significance)
            values (~a, 0, ~a, ~a, ~a)"
		   (sq sigsub-id) (sq lab) (sq "node") (sq cnt))
  sigsub-id)

(defun persist-singleton-lg-sigsub (map-id lg-id sub-id lg-n1)
  (latesql "insert into lg_sigsub (mapping_i, lg_id, sub_id, lg_node, sub_node)
            values (~a, ~a, ~a, ~a, 0)" 
		   (sq map-id) (sq lg-id) (sq sub-id) (sq lg-n1)))

(defun lg-in-training? (lg-id)
  (let* ((sen-id (lgid->sen lg-id))
		 (doc-id (ann-doc-id sen-id)))
	(member doc-id (documents (corpus "lymphoma_train")))))

(defun multi-token-pn? (pnid)
  (let* ((pn (get-annotation pnid nil)))
	(> (length (annotations-spec pn :type (gtype 'gtoken-type))) 1)))

(defmemo mapped-labs ()
  (mapcar #'car (latesql "select distinct lab from sig_subgraph where type='node'")))

(defun mapped (lab)
  (member lab (mapped-labs) :test #'equalp))

(defun get-uninode-sg (thrshld corpn)
  (let* ((sen-ids (mapcar #'car (latesql "select a.id from annotations as a join corpora_documents as cd join corpora  as c on a.document_id=cd.document_id and cd.corpus_id=c.id where a.type='sentence-annotation' and c.name=~a" (sq corpn))))
		 ;; you need to ensure there is no lg for masked sentences
		 (labs (latesql "select lab, count(pn1), max(pn1) from linkage_graph where type='node' and sent~a group by lab having count(pn1)>=~a" (sql-matcher sen-ids) thrshld)))
	(setf labs (remove-if-not #'(lambda (a) (match-re "^[A-Za-z]" (first a))) 
							  labs))
	(setf labs (remove-if #'(lambda (a) (or (maskedp (first a))
											;; (mapped (first a))
											(h-in-stoplist? (first a)))) 
						  labs))
	;; (setf labs (remove-if-not #'(lambda (a) (multi-token-pn? (third a))) labs))
	(setf labs (remove-if #'(lambda (a) (h-in-stoplist? (first a))) labs))
	(setf labs (remove-if #'(lambda (a) (member (first a) (list "date" "percent" "show" "general_number" "POS" "PRP" "PRP$" "be") :test #'equalp)) labs)))) ;; add "be" 2/18/2014

(defun get-uninode-sg-lg-n (lab)
  (latesql "select lg_id, n1 from linkage_graph where type='node' and lab=~a" (sq lab)))

(defun get-all-uninode-sg-lg-n (labs)
  (latesql "select lg_id, n1, lab from linkage_graph where type='node' and lab~a" (sql-matcher labs)))

(defun multi-word? (str)
  (setf str (replace-re str "_-_.*$" ""))
  (setf str (replace-re str "\\(.*?\\)" ""))
  (setf str (replace-re str "\\[.*?\\]" ""))
  (setf str (replace-re str "(^_+|_+$)" ""))
  (let* ((lstr (split-re "_+" str)))
	(setf lstr (remove-if #'h-in-stoplist? lstr))
	(> (length lstr) 1)))

(defun uninode-sg-map (thrshld corpn)
  (let* ((sgs (get-uninode-sg thrshld corpn))
		 (hmap (make-hash-table :test #'equalp))
		 (hlab (make-hash-table :test #'equalp))
		 lab cnt sgid lgid nid fsql mapid)
	;; use corpn to ensure only selecting sgs from training
	;; (setf sgs (remove-if-not #'(lambda (a) (multi-word? (first a))) sgs))
	(dolist (sg sgs)
	  (setf lab (first sg)
			cnt (second sg))
	  (format t "~&~a~%" lab)
	  (setf sgid (persist-singleton-sigsub lab cnt))
	  (setf (gethash lab hlab) sgid))

	;; this also dumps for testing mapping, but it's ok
	(setf fsql (open-new "lg_sigsub.uninodesql"))
	(dolist (lgid-nid-lab (get-all-uninode-sg-lg-n (mapcar #'car sgs)))
	  (setf lgid (first lgid-nid-lab)
			nid (second lgid-nid-lab)
			lab (third lgid-nid-lab))
	  (setf sgid (gethash lab hlab))
	  (setf mapid (incf (gethash (format nil "~a-~a" sgid lgid) hmap 0)))
	  
	  (format fsql "~a|~a|~a|~a|~a||~%" sgid lgid nid 0 mapid))
	(close fsql)))



(defun singleton-lg-sigsub (thrshld)
  "Handles singleton significant subgraphs (of size 1) and their mappings to
singleton linkage graph"
  (let* ((slgs (get-singleton-lgs))
		 (hlabs (make-hash-table :test #'equalp))
		 lab sigsub-id lg-n1)
	(format t "~&scanning linkage graphs.~%")
	(dolist (lgid slgs)
	  (when (lg-in-training? lgid)
		(setf lab (get-singleton-lab lgid))
		(incf (gethash lab hlabs 0))))
	
	(format t "~&saving significant singleton subgraphs.~%")
	(maphash #'(lambda (lab cnt)
				 (cond
				  ((>= cnt thrshld)
				   (setf sigsub-id (persist-singleton-sigsub lab cnt))
				   (setf (gethash lab hlabs) sigsub-id))
				  (t
				   (setf (gethash lab hlabs) nil))))
			 hlabs)
	
	(format t "~&saving singleton mappings.~%")
	(dolist (lgid slgs)
	  (setf lab (get-singleton-lab lgid))
	  (setf lg-n1 (get-singleton-n1 lgid))
	  (setf sigsub-id (gethash lab hlabs))
	  (when sigsub-id
		(persist-singleton-lg-sigsub 1 lgid sigsub-id lg-n1)))))


(defun singleton-sigsub-general-lg (thrshld)
  "Handles singleton significant subgraphs (of size 1) and their mappings to
singleton linkage graph"
  (let* ((slgs (get-singleton-lgs))
		 (lgs (get-lgs))
		 (hlabs (make-hash-table :test #'equalp))
		 lab sigsub-id lg-n1)
	(format t "~&scanning linkage graphs.~%")
	(dolist (lgid slgs)
	  (when (lg-in-training? lgid)
		(setf lab (get-singleton-lab lgid))
		(incf (gethash lab hlabs 0))))
	
	(format t "~&saving significant singleton subgraphs.~%")
	(maphash #'(lambda (lab cnt)
				 (cond
				  ((>= cnt thrshld)
				   (setf sigsub-id (persist-singleton-sigsub lab cnt))
				   (setf (gethash lab hlabs) sigsub-id))
				  (t
				   (setf (gethash lab hlabs) nil))))
			 hlabs)
	
	(format t "~&saving singleton mappings with general lgs.~%")
	(dolist (lgid lgs)
	  (let* ((hmap (make-hash-table :test #'equalp))
			 map-id)
		(dolist (node (get-lg-nonpunc-nodes lgid))
		  (setf lab (second node))
		  (setf lg-n1 (first node))
		  (setf sigsub-id (gethash lab hlabs))
		  (when sigsub-id
			(setf map-id (incf (gethash sigsub-id hmap 0)))
			(persist-singleton-lg-sigsub map-id lgid sigsub-id lg-n1)))))))


(defun sigsub-mapped-lgid-name (sigsub-id)
  (latesql "SELECT DISTINCT lg_id, gname FROM lg_sigsub JOIN graph ON gid=lg_id WHERE sub_id=~a" (sq sigsub-id)))

(defun subisomorphic-roots (sub-id)
  (subisomorphic-roots-tracing sub-id))

(defparameter *h-sub-roots* (make-hash-table)
  "Cache the sigsub roots tracing")

(defmemo subisomorphic-roots-tracing (sub-id)
  (let* ((containers (get-container sub-id)))
	#||
	(dolist (c containers)
	  (format t "~&~a_~a, ~a_~a, s~%" 
			  c (sigsub-size c) sub-id (sigsub-size sub-id)))
	||#
	(if containers
		(reduce #'union (mapcar #'subisomorphic-roots-tracing containers))
	  (list sub-id))))

(defmemo good-sigsubp (sub-id)
  (let ((roots (subisomorphic-roots sub-id)))
	(or (equalp (list sub-id) roots) 
		(> (length roots) 1))))

(defmemo root-sigsubp (sub-id)
  (let ((roots (subisomorphic-roots sub-id)))
	(equalp (list sub-id) roots)))

(defun get-good-sigsubs (sigsubs)
  (remove-if-not #'good-sigsubp sigsubs))

(defmemo sigsub-nontrivialp (sub-id)
  "Needs to has a UMLS"
  (let* ((stys (get-stys))
		 (stys (mapcar #'(lambda (a) (replace-re a " +" "_")) stys))
		 (labs (latesql "select lab from sig_subgraph 
                         where sub_id=~a and type='node'" 
						(sq sub-id)))
		 (labs (mapcar #'car labs)))
	(or (= 1 (length labs))				; special singleton case
		(some #'(lambda (a) (intersection (split-re "-" a) stys :test #'equalp))
			  labs))))

(defmemo sigsub-be-trivial? (sub-id)
  (let* ((be-nodes (latesql "select n1 from sig_subgraph where sub_id=~a and type='node' and lab='be'" sub-id)))
	(and (= (length be-nodes) 1)
		 (= 1 (length (latesql "select * from sig_subgraph where sub_id=~a and type='edge' and (n1=~a or n2=~a)" sub-id (caar be-nodes) (caar be-nodes)))))))

(defmemo sigsub-trivialp (sub-id)
  (let* ((nlabs (latesql "select lab from sig_subgraph 
                         where sub_id=~a and type='node'" 
						 sub-id))
		 (elabs (latesql "select lab from sig_subgraph 
                         where sub_id=~a and type='edge'" 
		 				 sub-id))
		 ;; (freq (sg-freq sub-id))
		 ;; (support (caar (latesql "select count(distinct lg_id) from lg_sigsub
         ;;                    where sub_id=~a" (sq sub-id))))
		 
		 (nlabs (mapcar #'car nlabs))
		 ;; (elabs (mapcar #'car elabs))
		 (hnlab (make-hash-table :test #'equalp)))
	(dolist (nlab nlabs)
	  (incf (gethash nlab hnlab 0)))
	(or ; (< 2 (cdar (hash-table-val-desc-alist hnlab)))
		;; (> freq 1000)
		;; (member "dep" elabs :test #'equalp)
		;; 07/31 (sigsub-be-trivial? sub-id)
		;; prep_for|prep_of|nn||dobj|appos
		;; (null (remove-if #'(lambda (a)  
		;; 					 (match-re "^(aux|conj|expl)" a)) elabs))
	 ;; (< (length nlabs) 2)
		(member "general_number" nlabs :test #'equalp)
		
		(some #'(lambda (a) (match-re "percent" a)) nlabs)
		(member "PRP" nlabs :test #'equalp)
		(member "PRP$" nlabs :test #'equalp)
		;; (member "ar" nlabs :test #'equalp)
		;; (member "%" nlabs :test #'equalp)
		(some #'(lambda (a) (match-re "old_year" a)) nlabs)
		(equalp nlabs '("not"))
		(equalp nlabs '("no"))
	    (and nil
		(member "diagnosis" nlabs :test #'equalp)
		(member "show" nlabs :test #'equalp)
		(member "confirmation" nlabs :test #'equalp)
		(member "feature" nlabs :test #'equalp)
		(member "immunohistochemical" nlabs :test #'equalp)
		(member "scientific_study" nlabs :test #'equalp)
		(member "support" nlabs :test #'equalp)
		(member "result" nlabs :test #'equalp)
		(member "paraffin" nlabs :test #'equalp)
		(member "additional" nlabs :test #'equalp)
		(member "information" nlabs :test #'equalp)
		(member "section" nlabs :test #'equalp)
		(member "possibility" nlabs :test #'equalp)
		;; (member "grade" nlabs :test #'equalp) ;; x
		(member "levels_(qualifier_value)" nlabs :test #'equalp)
		(member "reticulosarcoma" nlabs :test #'equalp)
		(member "cell_malignant" nlabs :test #'equalp)
		(member "immunoperoxidase_techniques" nlabs :test #'equalp)
		(member "evidence_of" nlabs :test #'equalp)
		(member "fraction_of" nlabs :test #'equalp) ;; added 7/26/13
		(member "consistent_with" nlabs :test #'equalp)
		(member "immunophenotyping" nlabs :test #'equalp)
		(member "smear_test" nlabs :test #'equalp)
		(member "cd19_antigens" nlabs :test #'equalp)
		(member "cd20_antigens" nlabs :test #'equalp)
		(member "cd3_antigens" nlabs :test #'equalp)
		(member "aggregate" nlabs :test #'equalp)
		(member "there" nlabs :test #'equalp) ;; x
		(member "revealment" nlabs :test #'equalp)
		(member "in_situ_hybridization" nlabs :test #'equalp)
		(member "hospitals" nlabs :test #'equalp)
		(member "scattered" nlabs :test #'equalp)
		;; (member "menopause" nlabs :test #'equalp)
		(member "tendence" nlabs :test #'equalp)
		;; (member "adenoma" nlabs :test #'equalp)

		(some #'(lambda (a) (match-re "^[A-Z]+$" a)) nlabs)
		
		;; this depends on prep_of -> nn, also note expression of often pp attached wrongly
		(and (member "expression_procedure" nlabs :test #'equalp)
			 (not (member "nn" elabs :test #'equalp)))

		;; (>= (count t (list (member "positive" nlabs :test #'equalp)
		;; 				   (member "negative" nlabs :test #'equalp)
		;; 				   (member "positive_negative" nlabs :test #'equalp)
		;; 				   (member "negative_positive" nlabs :test #'equalp)))
		;; 	2)

		(and (= 2 (length nlabs))
			 (member "lymphocyte" nlabs :test #'equalp)
			 (member "small" nlabs :test #'equalp))

		(and (= 2 (length nlabs))
			 (member "no" nlabs :test #'equalp)
			 (member "neg_staining_method" nlabs :test #'equalp))

		(and (= 2 (length nlabs)) 
			 (member "diffuse" nlabs :test #'equalp)
			 (member "patterns" nlabs :test #'equalp))

		(and (= 2 (length nlabs)) 
			 (member "lymphoid" nlabs :test #'equalp)
			 (member "infiltration" nlabs :test #'equalp))

		(and (= 2 (length nlabs))
			 (member "staining_method" nlabs :test #'equalp)
			 (some #'(lambda (a) (match-re "(?i)\\b(cell|antibod)\\b" a)) nlabs)) ;; 07/26 antibod

		(and (< 3 (length nlabs)) ;; 7/26
			 (or (member "positive" nlabs :test #'equalp)
				 (member "negative" nlabs :test #'equalp)
				 (member "positive_negative" nlabs :test #'equalp)
				 (member "negative_positive" nlabs :test #'equalp)))

		(and (= 2 (length nlabs))
			 (or (member "t-lymphocyte" nlabs :test #'equalp)
				 (member "b-lymphocytes" nlabs :test #'equalp)
				 (member "lymphocyte" nlabs :test #'equalp)
				 (member "cells" nlabs :test #'equalp)
				 (member "stains" nlabs :test #'equalp)
				 (member "geographic_population" nlabs :test #'equalp)
				 (member "tumor_cells,_uncertain_whether_benign_or_malignant" nlabs :test #'equalp))
			 (or (member "positive" nlabs :test #'equalp)
				 (member "negative" nlabs :test #'equalp)
				 (member "numerous" nlabs :test #'equalp)
				 (member "present" nlabs :test #'equalp)
				 (member "geographic_population"  nlabs :test #'equalp)
				 (member "dim" nlabs :test #'equalp)
				 (member "few" nlabs :test #'equalp)
				 (member "all" nlabs :test #'equalp)
				 (member "scattered" nlabs :test #'equalp)
				 (member "neoplastic" nlabs :test #'equalp)
				 (member "expression_procedure" nlabs :test #'equalp)))

		(and ;; (= 3 (length nlabs)) 7/26
			 ;; (or (member "t-lymphocyte" nlabs :test #'equalp)
				 ;; (member "b-lymphocytes" nlabs :test #'equalp)
				 ;; (member "cells" nlabs :test #'equalp))
			 (member "positive" nlabs :test #'equalp)
			 (member "negative" nlabs :test #'equalp))

		(and (= 3 (length nlabs))
			 (or (member "t-lymphocyte" nlabs :test #'equalp)
				 (member "b-lymphocytes" nlabs :test #'equalp)
				 (member "cells" nlabs :test #'equalp))
			 (or (member "positive" nlabs :test #'equalp)
				 (member "negative" nlabs :test #'equalp))
			 (member "expression_procedure" nlabs :test #'equalp))
		
		(and (= 3 (length nlabs))
			 (member "cells" nlabs :test #'equalp)
			 (or (member "atypical" nlabs :test #'equalp)
				 (member "large" nlabs :test #'equalp))
			 (member "lymphoid" nlabs :test #'equalp))

		;; (and (= 3 (length nlabs))
		;; 	 (member "cells" nlabs :test #'equalp)
		;; 	 (member "positive" nlabs :test #'equalp)
		;; 	 (member "large" nlabs :test #'equalp))

		(and (= 3 (length nlabs))
			 (member "cells" nlabs :test #'equalp)
			 (member "staining_method" nlabs :test #'equalp)
			 (or (member "numerous" nlabs :test #'equalp)
				 (member "small" nlabs :test #'equalp))))

		;; (member "evaluation" nlabs :test #'equalp)
		;; (member "adequate" nlabs :test #'equalp)
		;; (member "complete" nlabs :test #'equalp)
		;; (and (= 1 (length nlabs))
		;; (and (= 1 (length nlabs)) (> freq 250))
		;; (and (= 1 (length nlabs))
		;; 	 ;; "complete" "numerous" "l" "very" "may" "maybe" "some" "old" "due" "now" "overall" "SYM" "most" "might" "addendum" "additional" "past" "sometimes" "times" "see" "month" "day" "year" "others" "unknown" "S100" "post" "majority" "much" "few" "late" "j" "possibility" "old_year" "s" "millimeter" "sharp_sign" "however" "new" "recent" "different" "last" "suspicious" "B-Lymphocytes" "show" "NR4A2_gene"
		;; 	 (intersection '("l" "SYM" "S100" "j" "s" "sharp_sign" "show" "NR4A2_gene") ;; "revealment" "specimen" "submittal" "CONSIDERATION" "cells" "approximate" "evidence_of" "size" "present" "consistent_with" "note" "out_rule" "cytologic_atypia" "normal" "positive" "negative" "no" "smear_test" "marrow" "ratio"
		;; 				   nlabs :test #'equalp))
		;; (member "that" nlabs :test #'equalp)
		)))

(defmemo tensor-exclude-sg (sub-id)
  (let* ((nlabs (latesql "select lab from sig_subgraph 
                         where sub_id=~a and type='node'" 
						 (sq sub-id)))
		 (nlabs (mapcar #'car nlabs)))
	(or (sigsub-be-trivial? sub-id)
		(member "that" nlabs :test #'equalp)
		(= 1 (length nlabs)))))

(defun get-nontrivial-sigsubs-of-lg (lgid)
  (let* ((h-sigsubs (get-sigsubs-of-lg lgid)))
	(maphash #'(lambda (sub-map-id v)
				 (declare (ignore v))
				 (let* ((sub-id (car (split-re "-" sub-map-id))))
				   (when (sigsub-trivialp sub-id)
					 (remhash sub-map-id h-sigsubs))))
			 h-sigsubs)
	h-sigsubs))






(defun gaston-lg-mapping (fnlg)
  (let* (alg flg ln m lgname nnodes
		 (lgid 0)
		 (hnlab (make-hash-table :test #'equalp)))
	
	(setf flg (open fnlg :direction :input))
	(loop (unless (setf ln (read-ne-line flg)) (return))
	  (setf m (match-re "^#(?<lgname>.*?)\\s*$" ln :return :match))
	  (when m
		(setf lgname (re-submatch m nil nil "lgname"))
		(push lgname alg)
		
		(setf ln (read-ne-line flg))
		(setf nnodes (parse-integer ln))
		(dotimes (i nnodes)
		  (setf ln (read-ne-line flg))
		  (setf (gethash (cons lgid i) hnlab) ln))
		(incf lgid)))
	(close flg)
	(setf alg (coerce (nreverse alg) 'array))
	(list alg hnlab)))




(defmemo gname->gid (gname)
  #||mapping from graph name to graph id||#
  (caar (latesql "select gid from graph where gname=~a" (sq gname))))
