;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/10/2015 rewrite using asdf framework
yluo - 4/12/2012 rewriting as a method dispatcher
yluo - 07/22/2010 todo: add utility to automatically enriching link grammar's 
word dictionary after tagizing.
yluo - 07/11/2010 add option to convert all capitalized sentence to normal
capitalization, as it does make a difference (see phrase_chunking.sh and 
text in opennlp 1.4.3).
yluo - creation
|#

(defpackage :late
  (:use :common-lisp :excl)
  (:export
   "noun-tokenp"
   "ann-w-tok-len-greaterp"
   "w-tok-len"
   "ptb-prepp"
   "tagize"
   "verb-tokenp"
   "ptb-functionp"
   "ptb-nounp"
   "ptb-adjp"
   "ptb-verbp"
   "ptb-advp"
   "ptb-pronp"
   "ptb-fsp"
   "ptb-nump"
   "ptb-pl-nounp"
   "*ptb-adj-list*"
   "*ptb-adv-list*"
   "*ptb-noun-list*"
   "*ptb-verb-list*"
   "*ptb-pron-list*"
   "*ptb-fs-list*"
   "*ptb-num-list*"
   "ann-has-nounp"
   "ann-end-nounp"
   ))
(in-package :late)

(defparameter *ptb-verb-list* (list "VB" "VBD" "VBG" "VBN" "VBP" "VBZ")
  "Penn Tree Bank verb tags list.")

(defparameter *ptb-noun-list* (list "NN" "NNS" "NNP" "NNPS"))

(defparameter *ptb-adj-list* (list "JJ" "JJR" "JJS"))

(defparameter *ptb-adv-list* (list "RB" "RBR" "RBS" "WRB"))

(defparameter *ptb-pron-list* (list "POS" "PRP" "PRP$" "WP" "WP$"))

(defparameter *ptb-function-list* (list "IN" "CC" "DT" "PDT" "MD" "EX" "RP" "TO" "WDT"))

(defparameter *ptb-num-list* (list "CD"))

;;; foreign words and symbols
(defparameter *ptb-fs-list* (list "FW"   ;; Foreign word
				  "LS"   ;; List item marker
				  "SYM"  ;; symbol
				  "UH"   ;; Interjection
				  ))


(defmethod tagize ((doc document)
		   &key (allcap->normal? nil))
  "Add tag annotations to document. Sentence and token annotations must be
    present"
  (unless (and (analyzedp doc :ganalysis 'gsentencize)
	       (analyzedp doc :ganalysis 'gtokenize))
    (format t "~&Warning: Cannot compute tags for tokens in ~a because ~
                 sentence or token annotations are not done." (name doc))
    (return-from tagize nil))
  
  (let* ((sas (annotations doc :type 'sentence-annotation)))
    (dolist (s sas)
      (tagize s :allcap->normal? allcap->normal?))
    (add-analysis doc :ganalysis 'gtagize)))

(defmethod tagize ((sa sentence-annotation)
		   &key (allcap->normal? nil))
  "Uses the opennlp pos tagger to find tokens in sentences."
  (let* ((tas (annotations-spec sa :type (gtype 'gtoken-type))))
    (unless tas
      (format t "~&Warning: Empty token sentence: ~a~%" sa)
      (return-from tagize nil)))
  (cond 
   ((equalp (ganalysis 'gtagize) 'tagize-stanford)
    ;; this way sp::tokenize seems to be working while sp:tokenize not
    ;; stanford parser does tagging while parsing
    (sp::parse-tokenized sa 
			 :tag-only? t 
			 :allcap->normal? allcap->normal?))
   
   ((equalp (ganalysis 'gtagize) 'tagize-stanford-constrained)
    ;; this way sp::tokenize seems to be working while sp:tokenize not
    ;; stanford parser does tagging while parsing
    (sp::parse-tokenized-constrained sa 
				     :tag-only? t 
				     :allcap->normal? allcap->normal?))
   
   ((equalp (ganalysis 'gtagize) 'tagize-opennlp)
    (op::posdetect sa :allcap->normal? allcap->normal?))))

(defun verb-tokenp (ta &aux tags pos-tag)
  (setf tags (annotations-on-spec ta :type (gtype 'gtag-type)))
  (setf pos-tag (format nil "~a" (data (car tags))))
  (member pos-tag *ptb-verb-list* :test #'string=))

(defun noun-tokenp (ta &aux pos-tag tags)
  (setf tags (annotations-on-spec ta :type (gtype 'gtag-type)))
  (setf pos-tag (format nil "~a" (data (car tags))))

  (member pos-tag *ptb-noun-list* :test #'string=))



(defun pos-str (pos-tag)
  "Convert pos-tag to string, do nothing if input is a string."
  (cond
   ((typep pos-tag 'string)
    pos-tag)
   ((typep pos-tag 'pos-tag)
    (data pos-tag))
   (t
    (error "unrecognized input ~a" pos-tag))))

(defun ptb-nounp (pos-tag)
  (member (pos-str pos-tag) *ptb-noun-list* :test #'equalp))

(defun ptb-pl-nounp (pos-tag)
  (member (pos-str pos-tag) (list "NNS" "NNPS") :test #'equalp))

(defun ptb-vbnp (pos-tag)
  (member (pos-str pos-tag) (list "VBN") :test #'equalp))



(defun ptb-adjp (pos-tag)
  (member (pos-str pos-tag) *ptb-adj-list* :test #'equalp))

(defun ptb-ext-adjp (pos-tag)
  (or (ptb-adjp pos-tag)
      (ptb-vbnp pos-tag)
      (ptb-functionp pos-tag)))

(defun ptb-verbp (pos-tag)
  (member (pos-str pos-tag) *ptb-verb-list* :test #'equalp))

(defun ptb-prepp (pos-tag)
  (member (pos-str pos-tag) '("IN" "TO") :test #'equalp))

(defun ptb-functionp (pos-tag)
  (member (pos-str pos-tag) *ptb-function-list* :test 'equalp))

(defun ptb-pronp (pos-tag)
  (member (pos-str pos-tag) *ptb-pron-list* :test #'equalp))

(defun ptb-advp (pos-tag)
  (member (pos-str pos-tag) *ptb-adv-list* :test #'equalp))

(defun ptb-fsp (pos-tag)
  (member (pos-str pos-tag) *ptb-fs-list* :test #'equalp))

(defun ptb-nump (pos-tag)
  (member (pos-str pos-tag) *ptb-num-list* :test #'equalp))


(defun ann-has-nounp (ann)
  (let* ((l-pos (annotations-spec ann :type (gtype 'gtag-type)))
	 (l-pos (mapcar #'data l-pos)))
    (some #'(lambda (p) (member p *ptb-noun-list* :test #'equalp)) l-pos)))


(defun ann-end-nounp (ann)
  (let* ((l-pos (annotations-spec ann :type (gtype 'gtag-type)))
	 (pos (data (car (last l-pos)))))
    (member pos *ptb-noun-list* :test #'equalp)))

(defun w-tok-len (ann)
  "Count token length excluding punctuations.
Needs test.
"
  (let* ((tas (annotations-spec ann :type (gtype 'gtoken-type)))
	 (tas (remove-if #'(lambda (ta) (search (content ta) *punc*)) tas)))
    (length tas)))

(defun ann-w-tok-len-greaterp (a b)
  "Needs test."
  (>= (w-tok-len a) (w-tok-len b)))

(defun adj-ptags (a)
  (annotations-spec a :type (gtype 'gtag-type) :filter #'ptb-adjp))

(defun adv-ptags (a)
  (annotations-spec a :type (gtype 'gtag-type) :filter #'ptb-advp))

(defun vbn-ptags (a)
  (annotations-spec a :type (gtype 'gtag-type) :filter #'ptb-vbnp))
