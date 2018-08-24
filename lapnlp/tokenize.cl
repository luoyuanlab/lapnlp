;;; -*- Mode: LISP; Package: tokenize-*-

#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/12/2015 rewrite using asdf framework
yluo - 4/12/2012 rewriting as a method dispatcher. 
psz  -           creation for opennlp toolkit.
Simple tokenizer dispatcher using either opennlp toolkit, the Link Parser,
or the Stanford Parser
|#

(defpackage :late
  (:use :common-lisp :excl)
  (:export 
   "in-no-token-area?"
   "length-wo-punc"
   "tokenize"
   "tokenize-in-sentences"
   "tokens"
   "hyphen-token-stat"
   "*punc*"
   "non-stop?"
   ))
(in-package :late)

(defparameter *punc* "-.,:;'\"?!\\/()[]{}$%@#&*^")

(defun tokenize? (sen)
  (and (null (annotations sen :type 'quantize-annotation))
       (not (match-re "\\bDr\\." (content sen)))
       (not (match-re "\\bwill\\b" (content sen)))
       (not (match-re "\\bplease\\b" (content sen)))))

(defmethod tokenize ((doc document))
  "Add token annotations to document. Sentence annotations must be present.
Input
======
doc: document to be tokenized
tokenizer: abbr. name for tokenizer to be used, default :sp - stanford parser."
  (assert (gtype 'gtoken-type)
	  ()
	  "Need to call tokenize-init first!")
  (unless (analyzedp doc :ganalysis 'gsentencize)
    (format t "~&Warning: Tokenization depends on having sentence-split ~
                 document ~a, which has not occurred.  Use (sentencize doc).~%" 
	    doc)
    (return-from tokenize nil))
  (let ((sas (annotations doc :type 'sentence-annotation)))
    (dolist (sa sas)
      (if (tokenize? sa) 
	  (tokenize sa))))
  (add-analysis doc :ganalysis 'gtokenize))

(defmethod tokenize ((corp corpus))
  (dolist (d (documents corp))
    (tokenize (document d))))

(defmethod tokenize ((sa sentence-annotation))
  "Uses the opennlp tokenizer to find tokens in sentences."
  (cond 
   ((equalp (ganalysis 'gtokenize) 'tokenize-link)
    (link-parse sa :flush? t :tokenize-only? t))
   
   ((equalp (ganalysis 'gtokenize) 'tokenize-stanford)
    ;; this way sp::tokenize seems to be working while sp:tokenize not
    (sp::tokenize sa))
   
   ((equalp (ganalysis 'gtokenize) 'tokenize-opennlp)
    (opennlp::token-detect sa))))


(defun in-no-token-area? (ta)
  (annotations-spanning ta :type 'cross-ref-annotation))

(defmethod length-wo-punc ((tas list))
  "Count token length excluding punctuations.
Input:
tas - a list of token annotations.
"
  (let* ((cnt 0))
    (mapcar #'(lambda (pnode) 
		(unless (search (content pnode) *punc*) (incf cnt))) 
	    tas)
    cnt))

(defmethod length-wo-punc ((ann annotation))
  (length-wo-punc (annotations-spec ann :type (gtype 'gtoken-type))))

(defun tokens (a)
  (annotations-spec a :type (gtype 'gtoken-type)))

(defun hyphen-token-stat (corpn)
  (let* ((corp (corpus corpn))
	 (h-toks (make-hash-table :test #'equalp))
	 doc)
    (dolist (docid (documents corp))
      (setf doc (document docid))
      (dolist (tok (annotations-spec doc :type (gtype 'gtoken-type)))
	(when (and (match-re "\\w-\\w" (content tok))
		   (not (annotations-on tok :type 'tui-annotation
					:filter #'(lambda (a)
						    (equalp (data a)
							    "T129")))))
	  (incf (gethash (content tok) h-toks 0)))))
    (dolist (kv (hash-table-val-desc-alist h-toks))
      (format t "~&~a: ~a~%" (car kv) (cdr kv)))))

(defun non-stop? (ta)
  (not (h-in-stoplist? (content ta))))
