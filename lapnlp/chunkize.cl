;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
TODO: phrase may actually span over a PRN!!!
yluo - 09/13/2010 move ph-head and ph-pname here.
yluo - 07/21/2010 fix a missing phrase bug, add dummy phrase type OUT
yluo - 11/19/2008 creation 
|#
(defpackage :late
  (:use :common-lisp :util :norm :umlsize)
  (:export 
   "chunkizedp"
   "chunkize" 
   "add-phrases"
   "abstract-ph"
   "ph-head"
   "token-modifiers"
   "ph-pname"
   "noun-ph?"
   "np-safep"
   "in-noun-ph?"))

(in-package :late)

(defmethod chunkize ((doc document))
  "Add chunk annotations to document. Sentence, token and pos-tag-opennlp annotations~
   must be present"
  (unless (and (analyzedp doc :ganalysis 'gsentencize)
	       (analyzedp doc :ganalysis 'gtokenize)
	       (analyzedp doc :ganalysis 'gtagize))
    (format t "~&Cannot compute chunks for sentences in ~a because ~
                sentence or token or pos annotations are not done." (name doc))
    (return-from chunkize nil))
  
  (let* ((sas (annotations doc :type 'sentence-annotation)))
    (dolist (s sas)
      (chunkize s))
    (add-analysis doc :ganalysis 'gchunkize)))

(defmethod chunkize ((sa sentence-annotation))
  "Uses the opennlp chunkizer to find phrase in sentences."
  (let* ((tas (annotations-spec sa :type (gtype 'gtoken-type)))
	 (pos (annotations-spec sa :type (gtype 'gtag-type))))
    (unless (and tas pos)
      (format t "~&Warning: Empty token sentence: ~a~%" sa)
      (return-from chunkize nil)))
  (cond 
   ((equalp (ganalysis 'gchunkize) 'chunkize-opennlp)
    (opennlp:chunkdetect sa))))



(defun np-safep (np)
  (let* ((postags (annotations-spec np :type (gtype 'gtag-type)))
	 (postags (mapcar #'data postags))
	 (postags (mapcar #'(lambda (p)
			      (cond 
			       ((member p *ptb-noun-list* :test #'equalp)
				"ptb_noun")
			       ((member p *ptb-adj-list* :test #'equalp)
				"ptb_adj")
			       (t
				p)))
			  postags)))
    (cond
     ;; Ki67 which is [approximately 90% positive and CD10 and Bcl2].
     ((search '("ptb_adj" "CC" "ptb_noun") postags :test #'equalp)
      nil)
     (t
      t))))

(defmethod abstract-ph ((phrase phrase-annotation))
  "Refer to ftp://ftp.cis.upenn.edu/pub/treebank/doc/faq.cd2
   use the last noun/pronoun as the head word and keep the determiners and 
   pronoun modifiers"
  (let* ((tas (annotations-spec phrase 
				:type (gtype 'gtoken-type)))
	 (poss (annotations-spec phrase 
				 :type (gtype 'gtag-type)))
	 head
	 head-pos
	 has-noun?
	 in-brackets?
	 nbtas
	 nbposs
	 ta-str
	 pos-str
	 ans)				
    (assert (= (length tas) (length poss))
	    ()
	    "In ph-head, tokens and poss in phrase ~a don't agree~%" phrase)
    (dotimes (i (length tas))
      (cond
       ((member (content (elt tas i)) (list "(" "[" "{") :test #'string=)
	(setf in-brackets? t))
       ((member (content (elt tas i)) (list ")" "]" "}") :test #'string=)
	(setf in-brackets? nil))
       ((not in-brackets?)
	(push (elt tas i) nbtas)
	(push (elt poss i) nbposs)
	(when (member (format nil "~a" (data (elt poss i)))
		      (list "NN" "NNS" "NNP" "NNPS")
		      :test #'string=)
	  (setf has-noun? t)))))
    (setf nbtas (nreverse nbtas))
    (setf nbposs (nreverse nbposs))
    (assert (= (length nbtas) (length nbposs))
	    ()
	    "Non bracketed tokens ~{~a~^ ~}~% != non bracketed poss ~{~a~^ ~}." 
	    nbtas nbposs)
    (when has-noun?
      (dotimes (i (length nbtas))
	(setf ta-str (content (elt nbtas i))
	      pos-str (format nil "~a" (data (elt nbposs i))))
	(cond 
	 ((member pos-str
		  (list "NN" "NNS" "NNP" "NNPS") 
		  :test #'string=) 
	  (setf head ta-str)
	  (setf head-pos (elt nbposs i))
	  (when (and (not (w->sp-pos (car (norm head))))
		     (match-re "\\w" head))
	    (setf head (concatenate 'string head "_" 
				    (format nil "~a" (data head-pos))))))
	 ((member pos-str
		  (list "POS" "PRP" "PRP$" "WP" "WP$" "DT" "PDT" "WDT" "CD" "." "%") 
		  :test #'string=)
	  ;; I want to keep the start and end.
	  (push (cons ta-str (elt nbposs i)) ans)))
	))
    
    (cond 
     (head
      (push (cons head head-pos) ans))
     (t					; if no noun encountered
      (dotimes (i (length nbtas))
	(setf head (content (elt nbtas i)))
	(setf head-pos (elt nbposs i))
	(push (cons head head-pos) ans))
      ))
    (nreverse ans)))

(defmethod ph-head ((phrase phrase-annotation))
  "Refer to ftp://ftp.cis.upenn.edu/pub/treebank/doc/faq.cd2
   use the last noun/pronoun as the head word and do not keep the noun pronoun 
   modifiers"
  (let* ((tas (annotations-spec phrase 
				:type (gtype 'gtoken-type)))
	 (poss (annotations-spec phrase 
				 :type (gtype 'gtag-type)))
	 head
	 head-pos
	 in-brackets?
	 nbtas			       ; non-bracketed tas
	 nbposs)				
    (assert (= (length tas) (length poss))
	    ()
	    "tokens and poss in phrase ~a don't agree" phrase)
    (unless (member (data phrase) (list "NP" "ADJP" "ADVP" "VP")
		    :test #'equalp)
      (return-from ph-head (last tas)))
    (dotimes (i (length tas))
      (cond
       ((member (content (elt tas i)) (list "(" "[" "{") :test #'string=)
	(setf in-brackets? t))
       ((member (content (elt tas i)) (list ")" "]" "}") :test #'string=)
	(setf in-brackets? nil))
       ((not in-brackets?)
	(push (elt tas i) nbtas)
	(push (elt poss i) nbposs))))
    (setf nbtas (nreverse nbtas))
    (setf nbposs (nreverse nbposs))
    (assert nbtas
	    ()
	    "Non bracketed tokens are null for ~a." phrase)
    (dotimes (i (length nbtas))
      (when (member (format nil "~a" (data (elt nbposs i)))
		    (list "NN" "NNS" "NNP" "NNPS" "POS" "PRP" "PRP$" "WP" 
			  "WP$") 
		    :test #'string=) 
	(setf head (elt nbtas i))
	(setf head-pos (format nil "~a" (data (elt nbposs i))))
	))
    (unless head
      (setf head (car (last nbtas)))
      (setf head-pos (format nil "~a" (data (car (last nbposs))))))
    (cons head head-pos)))

(defun token-modifiers (pn umls-or-head position)
  (assert pn () "pn must not be nil.")
  (assert position () "position must either be prec or succ.")
  (let* ((ph (car (annotations-spanning-spec 
		   pn 
		   :type (gtype 'gphrase-type)))))
    (cond 
     ((null umls-or-head)
      nil)
     ((string= position "prec")
      (annotations-spec umls-or-head 
			:type (gtype 'gtoken-type) 
			:relation ':<m
			:filter #'(lambda (a) 
				    (and (<= (start ph) (start a))
					 (>= (end ph) (end a))))))
     ((string= position "succ")
      (annotations-spec umls-or-head 
			:type (gtype 'gtoken-type) 
			:relation ':>mi
			:filter #'(lambda (a) 
				    (and (<= (start ph) (start a))
					 (>= (end ph) (end a)))))))))

;;; returns the cui detected in the phrase annotation
;;; if no cui detected, then use _ joined content.
;;; should use the preferred name for the CUI.
;;; then extract the head noun for the preferred name.
(defmethod ph-pname ((phrase phrase-annotation))
  ;; find the longest spanning cui
  (let* ((cuis (annotations phrase :type 'cui-annotation))
	 (longest-cui nil)
	 (max-length 0))
    (unless (listp cuis)
      (setf cuis (cons cuis nil)))
    (dotimes (i (length cuis))
      (if (> (length
	      (annotations-spec (elt cuis i) 
				:type (gtype 'gtoken-type)))
	     max-length)
	  (setf longest-cui (elt cuis i))))
    (if longest-cui 
	(format nil "~a" (data longest-cui))
      (replace-re (replace-re (content phrase) "\\s+" "_") 
		  "(\\(|\\{|\\[|\\)|\\}|\\])" ""))
    ))


(defun noun-ph? (ph)
  (equalp (data ph) "NP"))

(defun in-noun-ph? (a)
  (annotations-spec a 
		    :type (gtype 'gphrase-type) 
		    :relation ':ar
		    :filter #'noun-ph?))
