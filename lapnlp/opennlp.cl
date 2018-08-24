;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/10/2015 rewrite using asdf framework
yluo - 07/16/2012 commented out hack for CD.
yluo - 07/19/2009 add opennlp-reinit to unset parameters such as *chunker*. 
Without unsetting those params, reinitializing jlinker will cause problems.
yluo - 11/19/2008 add definitions for chunker 
|#

(defpackage :opennlp
  (:use :javatools.jlinker :jc :common-lisp :umlsize #+allegro :excl)
  (:import-from :late 
		"*ptb-adj-list*"
		"*ptb-adv-list*"
		"*ptb-noun-list*"
		"*ptb-verb-list*"
		"ptb-adjp"
		"accession-number-pattern"
		"add-annotation"
		"analyses"
		"annotations"
		"annotations-spec"
		"chunk-tag"
		"content"
		"data"
		"date-pattern"
		"date-range-pattern"
		"document"
		"end"
		"gsetting"
		"gsw-ver"
		"gtype"
		"in-no-token-area?"
		"phone-number-pattern"
		"phrase-annotation"
		"pos-tag-opennlp"
		"range-of-ratios-pattern"
		"ratio-of-ranges-pattern"
		"sentence-annotation"
		"start"
		"time-pattern"
		"annotations-tree"
		"has-karyotype"
		)
  (:import-from :util
		"tree-delete"))

(in-package :opennlp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :opennlp-wrapper-jarlist "late:;opennlp-wrapper-jarlist"))

(defparameter *opennlp* nil)
(defparameter *sentencizer* nil)
(defparameter *tokenizer* nil)
(defparameter *pos-tagger* nil)
(defparameter *chunker* nil)


(defun opennlp-get-version ()
  "This is a stub function, should be really using opennlp's own get version 
function if it has one with later version"
  *opennlp-version*)

(defun opennlp-tokenizedp (doc)
  (member (format nil "opennlp-tokenized-~a" (opennlp-get-version)) 
	  (analyses doc) 
	  :test #'equalp))

(defun opennlp-parsedp (doc)
  (member (format nil "opennlp-parsed-~a" (opennlp-get-version)) 
	  (analyses doc) :test #'equalp))

(defun find-file-candidate (where name-pattern)
  (let ((poss (directory (merge-pathnames name-pattern where))))
    (when (> (length poss) 1)
      (warn "~%Multiple matches found for OPENNLP component ~s" name-pattern))
    (and poss (car poss))))

(defun opennlp-init ()
  (unless (jlinker-query)
    (jl-init))
  (jlinker-slot :jar-file *classpath-jars*)
  (jlinker-slot :classpath *classpath-dirs*)
  (unless *opennlp*
    (setf *opennlp* (opwrapper.OpennlpMEDG:OpennlpMEDG 
		     (namestring *opennlp-sentencizer-model*)
		     (namestring *opennlp-tokenizer-model*)
		     (namestring *opennlp-postagger-model*)
		     (namestring *opennlp-chunker-model*)))))

(defun sentdetect-init ()
  (unless *sentencizer*
    (opennlp-init)
    (setf *sentencizer* (opwrapper.OpennlpMEDG:initSentencizerME *opennlp*))))

(defun sentdetect (str)
  (sentdetect-init)
  (let* ((jspans (opwrapper.OpennlpMEDG:sentDetect *sentencizer* str))
	 spans span)
    (dotimes (i (opwrapper.OpennlpMEDG:sentsLength jspans))
      (setf span (opwrapper.OpennlpMEDG:getSent jspans i))
      (push (cons (opwrapper.OpennlpMEDG:getSentBegin span)
		  (opwrapper.OpennlpMEDG:getSentEnd span))
	    spans))
    (nreverse spans)))


(defun tokenize-init ()
  (unless *tokenizer*
    (opennlp-init)
    (setq *tokenizer* (opwrapper.OpennlpMEDG:initTokenizerME *opennlp*))))

(defmethod token-detect ((sen sentence-annotation))
  (tokenize-init)
  (let* ((doc (document sen))
	 (offset (start sen))
	 (spans (opwrapper.OpennlpMEDG:tokenDetect *tokenizer* (content sen)))
	 segs)
    (dotimes (i (opwrapper.OpennlpMEDG:tokensLength spans))
      (let ((span (opwrapper.OpennlpMEDG:getToken spans i)))
	(push (cons (+ offset (opwrapper.OpennlpMEDG:getTokenBegin span)) 
		    (+ offset (opwrapper.OpennlpMEDG:getTokenEnd span))) 
	      segs)))
    (setf segs (nreverse segs))
    (dolist (seg segs)
      (let ((ta (make-instance 'opennlp-token
			       :document doc
			       :start (car seg)
			       :end (cdr seg)
			       :sw-ver (gsw-ver 'gtokenize)
			       :setting (gsetting 'gtokenize))))
	(unless (in-no-token-area? ta)
	  (add-annotation doc ta)
	  (add-annotation sen ta))))))

(defun tagger-init ()
  (unless *pos-tagger*
    (opennlp-init)
    (setf *pos-tagger* (opwrapper.OpennlpMEDG:initPOSTaggerME *opennlp*))))

(defun tag-word-vector (str-array)
  (assert (and (arrayp str-array)
	       (= (array-rank str-array) 1)
	       (every #'stringp str-array))
	  ()
	  "Argument to tag-word-vector must be a vector of words: ~s"
	  str-array)
  (tagger-init)
  (opwrapper.OpennlpMEDG:tagTokens *pos-tagger* str-array))

(defun tag-word-probs ()
  "Must be called only after tag-word-vector, to return a vector of tag
probabilities."
  (tagger-init)
  (opwrapper.OpennlpMEDG:tagProbs *pos-tagger*))

(defmethod posdetect ((sent sentence-annotation)
		      &key (allcap->normal? nil))
  (let* ((tas (annotations-spec sent :type (gtype 'gtoken-type)))
	 (tokvect (make-array (list (length tas))))
	 (doc (document sent))
	 (i 0))
    (dolist (ta tas)
      (setf (aref tokvect i) 
	    (cond 
	     (allcap->normal? 
	      (string-downcase (content ta)))
	     (t 
	      (content ta))))
      (incf i))
    (let* ((tags (tag-word-vector tokvect))
	   (probs (tag-word-probs))
	   (j 0)
	   tag prob)
      (dolist (ta tas)
	;; corpora sensitive 
	(cond 
	 ((search "'s" (content ta) :test #'equalp)
	  (setf tag "POS" prob 1.0))
	 ((has-karyotype ta)
	  (setf tag "NN" prob 1.0))
	 ((match-re "^[A-Za-z]+-?\\d+$" (content ta) :case-fold t)
	  (setf tag "NN" prob 1.0))
	 ;; count for CD19+ etc.
	 ((match-re "^.+[\\+\\-]$" (content ta))
	  (setf tag "JJ" prob 1.0))
	 ((match-re "^(\\+|\\+/-|-/\\+)$" (content ta))
	  (setf tag "JJ" prob 1.0))
	 ((match-re "\\w+:+\\w+" (content ta))
	  (setf tag "JJ" prob 1.0))
	 ((match-re "(positve|negative|dim|bright)$" (content ta))
	  (setf tag "JJ" prob 1.0))
	 ((match-re "\\Svariable$" (content ta))
	  (setf tag "JJ" prob 1.0))
	 ((match-re "dim\\b" (content ta))
	  (setf tag "JJ" prob 1.0))
	 ((match-re "\\(s\\)$" (content ta))
	  (setf tag "NNS" prob 1.0))
	 ((equalp "status-post" (content ta))
	  (setf tag "IN" prob 1.0))
	 ((equalp "rule-out" (content ta))
	  (setf tag "VB" prob 1.0))
	 ((and (equalp "left" (content ta))
	       (search "VB" (aref tags j) :test #'equalp))
	  (setf tag "JJ" prob 1.0))

	 ((equalp "shows" (content ta))
	  (setf tag "VBZ" prob 1.0))
	 ((equalp "medium" (content ta))
	  (setf tag "JJ" prob 1.0))
	 ((equalp "sized" (content ta))
	  (setf tag "VBN" prob 1.0))
	 ((equalp "cleaved" (content ta))
	  (setf tag "VBN" prob 1.0))
	 ((and (equalp "transformed" (content ta))
	       (< (1+ j) (length tokvect))
	       (not (equalp (aref tokvect (1+ j)) "into")))
	  (setf tag "VBN" prob 1.0))
	 ((and (equalp "stains" (content ta))
	       (< (1+ j) (length tags))
	       (member (aref tags (1+ j)) '("DT" "JJ" "JJR" "JJS" "VBN") :test #'equalp))
	  (setf tag "VBZ" prob 1.0))
	 ((and (equalp "stain" (content ta))
	       (< (1+ j) (length tags))
	       (member (aref tags (1+ j)) '("DT" "JJ" "JJR" "JJS" "VBN") :test #'equalp))
	  (setf tag "VBP" prob 1.0))
	 ((equalp "immunostaining" (content ta))
	  (setf tag "VBG" prob 1.0))
	 ((equalp "dendritic" (content ta))
	  (setf tag "JJ" prob 1.0))
	 ((equalp "show" (content ta))
	  (setf tag "VBP" prob 1.0))
	 ((equalp "highlight" (content ta))
	  (setf tag "VBP" prob 1.0))
	 ((equalp "highlights" (content ta))
	  (setf tag "VBZ" prob 1.0))
	 ((match-re "[a-z]*-?related" (content ta) :case-fold t)
	  (setf tag "JJ" prob 1.0))
	 ((equalp "myc-igh" (content ta))
	  (setf tag "NN" prob 1.0))
	 ((equalp "#" (content ta))
	  (setf tag "#" prob 1.0))
	 ((equalp "*" (content ta))
	  (setf tag "SYM" prob 1.0))
	 ((match-re "^[><=0-9+\\-,]*[0-9][><=0-9+\\-,]*$" (content ta))
	  (setf tag "CD" prob 1.0))

	 ((match-re "(?i)CD\\d+[A-Z]*" (content ta))
	  (setf tag "NN" prob 1.0))

	 ((equalp "CD4-to-CD8" (content ta))
	  (setf tag "NN" prob 1.0))
	 
	 
	 
	 ((and (equalp "about" (content ta))
	       (equalp "CD" (aref tags (+ 1 j))))
	  (setf tag "RB" prob 1.0))

	 ((and (equalp "lack" (content ta))
	       (< (1+ j) (length tokvect))
	       (not (equalp (aref tokvect (1+ j)) "of")))
	  (setf tag "VBP" prob 1.0))
	 ((equalp "express" (content ta))
	  (setf tag "VBP" prob 1.0))
	 
	 ((annotations ta :type 'date-pattern)
	  (setf tag "CD" prob 1.0))
	 ((annotations ta :type 'accession-number-pattern)
	  (setf tag "CD" prob 1.0))
	 ((annotations ta :type 'time-pattern)
	  (setf tag "CD" prob 1.0))
	 ((annotations ta :type 'date-range-pattern)
	  (setf tag "CD" prob 1.0))
	 ((annotations ta :type 'phone-number-pattern)
	  (setf tag "CD" prob 1.0))
	 ((annotations ta :type 'range-of-ratios-pattern)
	  (setf tag "CD" prob 1.0))
	 ((annotations ta :type 'ratio-of-ranges-pattern)
	  (setf tag "CD" prob 1.0))
	 ;; as of opennlp 1.4.3, there is convertToken hack in 
	 ;; TreebankParser.java 
	 ((string-equal "{" (content ta))
	  (setf tag "-LCB-" 
		prob 1.0))
	 ((string-equal "[" (content ta))
	  (setf tag "-LRB-" 
		prob 1.0))
	 ((string-equal "(" (content ta))
	  (setf tag "-LRB-"	
		prob 1.0))
	 ((string-equal "}" (content ta))
	  (setf tag "-RCB-"	
		prob 1.0))
	 ((string-equal "]" (content ta))
	  (setf tag "-RRB-"	
		prob 1.0))
	 ((string-equal ")" (content ta))
	  (setf tag "-RRB-"	
		prob 1.0))
	 ;; leverage LVG, if this is a sure noun
	 ((equal (list "noun") (w->sp-pos (content ta)))
	  (if (member (aref tags j) *ptb-noun-list* :test #'string=)
	      (setf tag (aref tags j)
		    prob (aref probs j))
	    ;; try to make the best guess.
	    (let* ((agrs (w-sca->sp-agr (content ta) "noun")))
	      (cond 
	       ((or (member "count(thr_plur)" agrs  :test #'string=)
		    (member "count(fst_plur)" agrs  :test #'string=))
		(setf tag "NNS" 
		      prob 1.0))
	       (t
		(setf tag "NN" 
		      prob 1.0))))))
	 ((equal (list "adj") (w->sp-pos (content ta)))
	  (if (member (aref tags j) *ptb-adj-list* :test #'string=)
	      (setf tag (aref tags j)
		    prob (aref probs j))
	    (let* ((agrs (w-sca->sp-agr (content ta) "adj")))
	      (cond 
	       ((member "comparative" agrs  :test #'string=)
		(setf tag "JJR"
		      prob 1.0))
	       ((member "superlative" agrs  :test #'string=)
		(setf tag "JJS"
		      prob 1.0))
	       (t
		(setf tag "JJ"
		      prob 1.0)
		)))))
	 ((equal (list "verb") (w->sp-pos (content ta)))
	  (if (member (aref tags j) *ptb-verb-list* :test #'string=)
	      (setf tag (aref tags j)
		    prob (aref probs j))
	    (let* ((agrs (w-sca->sp-agr (content ta) "verb")))
	      (cond 
	       ((member "past_part" agrs :test #'string=)
		(setf tag "VBN"
		      prob 1.0)
		)
	       ((member "past" agrs :test #'string=)
		(setf tag "VBD"
		      prob 1.0))
	       ((member "pres(thr_sing)" agrs :test #'string=)
		(setf tag "VBZ"
		      prob 1.0))
	       ((member "pres_part" agrs :test #'string=)
		(setf tag "VBG"
		      prob 1.0)
		)
	       ((member "pres(fst_sing,fst_plur,thr_plur,second)" agrs 
			:test #'string=)
		(setf tag "VBP"
		      prob 1.0))
	       ((member "infinitive" agrs  :test #'string=)
		(setf tag "VB"
		      prob 1.0))))))
	 ((equal (list "adv") (w->sp-pos (content ta)))
	  (if (member (aref tags j) *ptb-adv-list* :test #'string=)
	      (setf tag (aref tags j)
		    prob (aref probs j))
	    (let* ((agrs (w-sca->sp-agr (content ta) "adv")))
	      (cond 
	       ((member "comparative" agrs  :test #'string=)
		(setf tag "RBR"
		      prob 1.0))
	       ((member "superlative" agrs  :test #'string=)
		(setf tag "RBS"
		      prob 1.0))
	       (t
		(setf tag "RB"
		      prob 1.0))))))
	 ((equal (list "conj") (w->sp-pos (content ta)))
	  (setf tag "CC" prob 1.0))
	 
	 ((equal (list "prep") (w->sp-pos (content ta)))
	  (setf tag "IN" prob 1.0))

	 (t
	  (setf tag (aref tags j)
		prob (aref probs j))))
	
	(let* ((umls-preps (mapcar #'(lambda (a) (replace-re a " +" "-")) 
				   (umlsize::umls-prep-list)))
	       (umls-conjs (mapcar #'(lambda (a) (replace-re a " +" "-"))
				   (umlsize::umls-conj-list))))
	  ;; I think it's better to do it here, as opennlp regards those as 
	  ;; IN, even if I modify its dictionary, I cannot modify the model 
	  ;; unless I retrain it.
	  (when (and (equalp tag "IN")
		     (member (content ta) '("while" "if" "though") 
			     :test #'equalp))
	    (setf tag "CC" prob 1.0))
	  (when (and (member tag '("CC" "IN") :test #'equalp)
		     (not (equalp (content ta) "that"))
		     (not (member (content ta) umls-preps :test #'equalp))
		     (not (member (content ta) umls-conjs :test #'equalp)))
	    (setf tag "NN" prob 1.0))
	  (when (and (member tag '("," "''") :test #'equalp)
		     (match-re "\\w" (content ta)))				
	    (setf tag "NN" prob 1.0)))
	
	(let ((tag-ann (make-instance 'pos-tag-opennlp
				      :document doc
				      :start (start ta)
				      :end (end ta)
				      :data tag
				      :sw-ver (gsw-ver 'gtagize)
				      :setting (gsetting 'gtagize)
				      :prob prob)))
	  (add-annotation doc tag-ann)
	  (add-annotation sent tag-ann))
	(incf j)))))

(defun chunker-init ()
  (unless *chunker*
    (opennlp-init)
    (setf *chunker* (opwrapper.OpennlpMEDG:initChunkerME *opennlp*))))

(defun phrase-chunk-arr (tok-arr tag-arr)
  "Java String[] and Lisp sequence should be convertable, should test."
  (chunker-init)  
  (opwrapper.OpennlpMEDG:chunkTokens *chunker* tok-arr tag-arr))

(defun opennlp-reinit (&optional (confirm nil))
  (assert confirm
	  (confirm)
	  "Please confirm that you want to null all util the parameters in opennlp.")
  (setq *sentencizer* nil)
  (setq *tokenizer* nil)
  (setq *pos-tagger* nil)
  (setq *chunker* nil)
  )

;; only add downlink from phrase to token, no uplink from token to phrase
(defun add-phrases (p-start p-end ph-type sent)
  (let* ((p-toks (remove-if 
		  #'(lambda (a) (or (< (end a) p-start)
				    (> (start a) p-end)))
		  (annotations-spec sent :type (gtype 'gtoken-type))))
	 (phrase (make-instance (gtype 'gphrase-type)
				:document (document sent)
				:start p-start
				:end p-end
				:sw-ver (gsw-ver 'gchunkize)
				:setting (gsetting 'gchunkize)
				:data (or ph-type "none")
				:h-down p-toks))
	 (doc (document sent)))
    (dolist (ann (annotations-spec phrase :type (gtype 'gphrase-type)))
      (format t "~&deleting ~a~%" ann)
      (tree-delete (annotations-tree doc) ann))
    (add-annotation (document sent) phrase)
    (add-annotation sent phrase)
    phrase))

(defun proper-adjp? (ph)
  (let* ((lpos (annotations ph :type (gtype 'gtag-type))))
    (or (every #'(lambda (pos) (or (search "JJ" (data pos))
				   (equalp "CC" (data pos))
				   (match-re "^\\W+$" (data pos)))) 
	       lpos)
	(ptb-adjp (car (last lpos))))))

(defun add-chunk-tag (ta sent tag-name)
  (let* ((doc (document sent))
	 (tag (make-instance (gtype 'gchunk-tag-type)
			     :document doc
			     :start (start ta)
			     :end (end ta)
			     :data tag-name
			     :sw-ver (gsw-ver 'gchunkize)
			     :setting (gsetting 'gchunkize))))
    (add-annotation doc tag)
    (add-annotation sent tag)))

(defun seq-chunking (tas poss sent)
  (let* (tokvect posvect chunks in-ph p-start p-end ph-type ph) 
    (setf tokvect (mapcar #'content tas))
    (setf posvect (mapcar #'data poss))
    (setf tokvect (mapcar #'string-downcase tokvect))
    (setf tokvect (coerce tokvect 'array))
    (setf posvect (coerce posvect 'array))
    (setf chunks (phrase-chunk-arr tokvect posvect))
    (do* ((j 0 (1+ j))
	  (ta-iter tas (cdr ta-iter))
	  (pos-iter poss (cdr pos-iter))
	  (ta (car ta-iter) (car ta-iter))
	  (pos (car pos-iter) (car pos-iter)))
	((not ta-iter))
      ;; do not include punctuations as last token of a phrase.
      (when (and (match-re "^\\W*$" (content ta))
		 (or (= j (1- (length tas)))
		     (match-re "^B-" (aref chunks (1+ j)))))
	(setf (aref chunks j) "OUT"))
      
      (when (and (< j (- (length tas) 1))
		 (equal "B-NP" (aref chunks j))
		 (equal "I-NP" (aref chunks (1+ j)))
		 (member (data pos) '("VB" "VBZ" "VBP" "VBD") :test #'equal))
	(setf (aref chunks j) "B-VP"
	      (aref chunks (1+ j)) "B-NP"))
      
      (let* ((tag-name (aref chunks j)))
	(add-chunk-tag ta sent tag-name)
	;; handles phrase annotation
	(multiple-value-bind (b-flag b-whole p-type) 
	    (match-re "B-(\\S*)" tag-name)
	  (declare (ignore b-whole))
	  (cond 
	   (b-flag
	    (when in-ph		       ; if already in a phrase, 
					; then this one is new
	      (setf ph (add-phrases p-start p-end ph-type sent))
	      (if (proper-adjp? ph)
		  (setf (data ph) "ADJP")))
	    (setq in-ph t)
	    (setq ph-type p-type)
	    (setq p-start (start ta))
	    (setq p-end (end ta)))
	   
	   ((match-re "I-\\S*" tag-name)
	    (setq p-end (end ta)))
	   
	   ((match-re "O\\S*" tag-name)
	    (when in-ph		        ;; if already in a phrase, 
					;;  then this one is new
	      (setf ph (add-phrases p-start p-end ph-type sent))
	      (if (proper-adjp? ph)
		  (setf (data ph) "ADJP")))
	    (setq in-ph nil)
	    ;; add dummy phrase type OUT for every token not in an actual one.
	    (add-phrases (start ta) (end ta) "OUT" sent))))))

    (when in-ph
      (setf ph (add-phrases p-start p-end ph-type sent))
      (if (proper-adjp? ph)
	  (setf (data ph) "ADJP")))))

(defun parens-chunking (tas poss sent)
  (let* ((in-brackets? 0)
	 pa-tas pa-poss nbtas nbposs pa-start pa-end)
    (do* ((i 0 (1+ i))
	  (ta-iter tas (cdr ta-iter))
	  (pos-iter poss (cdr pos-iter))
	  (ta (car ta-iter) (car ta-iter))
	  (pos (car pos-iter) (car pos-iter)))
	((not ta-iter))
      (cond 
       ((member (data pos) (list "-LCB-" "-LRB-") :test #'string=)
	(incf in-brackets?)
	(add-chunk-tag ta sent "OUT")
	(setf pa-start (1+ i)))

       ((<= in-brackets? 0)
	(push (elt tas i) nbtas)
	(push (elt poss i) nbposs))

       ((member (data pos) (list "-RCB-" "-RRB-") :test #'string=)
	(decf in-brackets?)
	(add-chunk-tag ta sent "OUT")
	(setf pa-end i)
	(setf pa-tas (subseq tas pa-start pa-end))
	(setf pa-poss (subseq poss pa-start pa-end))
	(seq-chunking pa-tas pa-poss sent))))
    (list (nreverse nbtas) (nreverse nbposs))))

;; handles brackets 
(defmethod chunkdetect ((sent sentence-annotation))
  ;; tas and poss should be sorted and aligned
  (let* ((tas (annotations-spec sent :type (gtype 'gtoken-type)))
	 (poss (annotations-spec sent :type (gtype 'gtag-type)))
	 (res (parens-chunking tas poss sent))
	 (nbtas (first res)) 
	 (nbposs (second res)))
    (when nbtas 
      (seq-chunking nbtas nbposs sent))))

(defmethod chunkize2 ((sent sentence-annotation))
  "This is a vanilla version of chunkize. 
tas and poss should be sorted and aligned."
  (let* ((tas (annotations-spec sent :type (gtype 'gtoken-type)))
	 (poss (annotations-spec sent :type (gtype 'gtag-type)))
	 (tokvect (make-array (list (length tas))))
	 (posvect (make-array (list (length tas)))) 
	 (doc (document sent)))

    (dotimes (i (length tas))
      (let* ((pos-str (format nil "~a" (data (elt poss i)))))
	(setf (aref posvect i) pos-str)
	(setf (aref tokvect i) (content (elt tas i)))
	))

    (let* ((chunks (phrase-chunk-arr tokvect posvect))
	   (j 0)
	   (in-ph nil)
	   (p-start nil)
	   (p-end nil)
	   (ph-type nil))

      (dolist (ta tas)
	(let* ((tag (make-instance (gtype 'gchunk-tag-type)
				   :document doc
				   :start (start ta)
				   :end (end ta)
				   :data (aref chunks j)
				   :sw-ver (gsw-ver 'gchunkize)
				   :setting (gsetting 'gchunkize)))
	       (tag-name (data tag)))
	  (add-annotation doc tag)
	  (add-annotation sent tag)
	  
	  ;; handles phrase annotation
	  (multiple-value-bind (b-flag b-whole p-type) 
	      (match-re "B-(\\S*)" tag-name)
	    (declare (ignore b-whole))
	    (cond 
	     (b-flag
	      (when in-ph		; if already in a phrase, 
					;  then this one is new
		(add-phrases p-start p-end ph-type sent))
	      (setq in-ph t)
	      (setq ph-type p-type)
	      (setq p-start (start ta))
	      (setq p-end (end ta)))
	     
	     ((match-re "I-\\S*" tag-name)
	      (setq p-end (end ta))
	      )
	     
	     ((match-re "O\\S*" tag-name)
	      (when in-ph		; if already in a phrase, 
					;  then this one is new
		(add-phrases p-start p-end ph-type sent))
	      (setq in-ph nil)
	      ;; add dummy phrase type OUT for every token not in an actual one.
	      (add-phrases (start ta) (end ta) "OUT" sent)))))	
	(incf j))
      (when in-ph
	(add-phrases p-start p-end ph-type sent)))))
