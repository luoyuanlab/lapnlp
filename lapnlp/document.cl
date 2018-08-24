;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 06/09/2012 added rewriting for status-post incomplete sentence.
yluo - 05/06/2012 creation
Utility for document-level processings.
|#

(defpackage :late
  (:use :common-lisp :excl :excl.osi :mnegex :cg :wordnet :mm
	:opennlp :javatools.jlinker :jc)
  (:export
   "*debug-document*"
   "*excl-tuis*"
   "analyze-doc-lp"
   "analyze-doc-pre-lp"
   "annotate-doc"
   "load-persist-ann"
   "re-concept-graph-doc"
   "re-import-doc"
   "insert-text"
   "sentence-rewrite"
   "id->doc"
   "update-doc-content"
   ))

(in-package :late)

(defparameter *debug-sen-rewrite* nil 
  "debug flag for sentence-rewriting and insert-text")
(defparameter *excl-tuis* nil)


(defun analyze-doc-lp (doc excl-tuis)
  "Run analyses steps, with and after link parsing, on documents"
  (declare (ignorable excl-tuis))
  (unless (analyzedp doc :ganalysis 'gtagize)
    (format t "~&~a document ~a~%" (ganalysis 'gtagize) (name doc))
    (time-profiling (tagize doc :allcap->normal? t)))
  
  (unless (analyzedp doc :ganalysis 'gchunkize)
    (format t "~&~a document ~a~%" (ganalysis 'gchunkize) (name doc))
    (time-profiling (chunkize doc)))
  
  (unless (analyzedp doc :ganalysis 'ghierarchize)
    (format t "~&~a document ~a~%" (ganalysis 'ghierarchize) (name doc))
    (time-profiling (hierarchize doc)))
  
  (unless (analyzedp doc :ganalysis 'gumlsize)
    (format t "~&umlsizing document ~a~%" (name doc))
    (time-profiling (umlsize doc :flush nil)))
  
  (unless (analyzedp doc :ganalysis 'gparse)
    (format t "~&~a document ~a~%" (ganalysis 'gparse) (name doc))
    (time-profiling (parse doc)))
  
  (save doc))

(defun analyze-doc-pre-lp (doc corp 
			       &key 
			       (line-splits? nil) 
			       (in-sections? t)
			       &aux)
  "Run analyses steps , before link parsing, on documents"
  
  (unless (analyzedp doc :analysis "sectionize") 
    (format t "~&sectionizing document ~a~%" (name doc))
    (time-profiling (findstruct doc :spec (description corp) :flush nil)))

  ;; (format t "~{~a~%~}" (annotations doc :type 'section-annotation))
  (when (and (ganalysis 'gquantize) 
	     (not (analyzedp doc :ganalysis 'gquantize)))
    (format t "~&~a document ~a~%" (ganalysis 'gquantize) (name doc))
    (time-profiling (quantize doc)))
  
  (when (and (ganalysis 'gformat-mark) 
	     (not (analyzedp doc :ganalysis 'gformat-mark)))
    (format t "~&~a document ~a~%" (ganalysis 'gformat-mark) (name doc))
    (time-profiling (format-mark doc)))
  
  (when (and (ganalysis 'gsentencize) 
	     (not (analyzedp doc :ganalysis 'gsentencize))) 
    (format t "~&sentencizing document ~a~%" (name doc))
    (time-profiling (sentencize doc :line-splits? line-splits? 
				:in-sections? in-sections?)))
  
  (when (and (ganalysis 'gcross-ref) 
	     (not (analyzedp doc :ganalysis 'gcross-ref)))
    (format t "~&~a document ~a~%" (ganalysis 'gcross-ref) (name doc))
    (time-profiling (cross-ref doc)))

;;;  (format t "~&Checking UMLS augmented Opennlp tag inconsistency~% on ~a" doc)
;;;  (time-profiling (tag-inconsistency doc))



  (when (and (ganalysis 'gtokenize) 
	     (not (analyzedp doc :ganalysis 'gtokenize)))
    (format t "~&~a document ~a and populating dict~%" 
	    (ganalysis 'gtokenize) (name doc))
    (time-profiling (tokenize doc)))
  
  (when (and (ganalysis 'gnum-recognize) 
	     (not (analyzedp doc :ganalysis 'gnum-recognize)))
    (format t "~&~a document ~a~%" (ganalysis 'gnum-recognize) (name doc))
    (time-profiling (token-parse doc :save nil)))
  
  (when (and (ganalysis 'gtagize) 
	     (not (analyzedp doc :ganalysis 'gtagize)))
    (format t "~&~a document ~a~%" (ganalysis 'gtagize) (name doc))
    (time-profiling (tagize doc :allcap->normal? t)))
  
  (when (and (ganalysis 'gchunkize) 
	     (not (analyzedp doc :ganalysis 'gchunkize)))
    (format t "~&~a document ~a~%" (ganalysis 'gchunkize) (name doc))
    (time-profiling (chunkize doc)))





  (when (and (ganalysis 'gparse) 
	     (not (analyzedp doc :ganalysis 'gparse)))
    (format t "~&~a document ~a~%" (ganalysis 'gparse) (name doc))
    (time-profiling (parse doc)))
  
  (when (and (ganalysis 'gumlsize) 
	     (not (analyzedp doc :ganalysis 'gumlsize)))
    (format t "~&umlsizing document ~a~%" (name doc))
    ;; :flush nil is necessary, o.w. it will increase the processing time 10x
    (time-profiling (umlsize doc :flush nil)))

  (when (and (ganalysis 'gcoded-text) 
	     (not (analyzedp doc :ganalysis 'gcoded-text)))
    (format t "~&~a document ~a~%" (ganalysis 'gcoded-text) (name doc))
    (time-profiling (coded-text doc)))

  (when (and (ganalysis 'gif-list) 
	     (not (analyzedp doc :ganalysis 'gif-list)))
    (format t "~&~a document ~a~%" (ganalysis 'gif-list) (name doc))
    (time-profiling (if-list doc)))

  (when (and (ganalysis 'gif-adj) 
	     (not (analyzedp doc :ganalysis 'gif-adj)))
    (format t "~&~a document ~a~%" (ganalysis 'gif-adj) (name doc))
    (time-profiling (if-adj doc)))

  (when (and (ganalysis 'grange-adj) 
	     (not (analyzedp doc :ganalysis 'grange-adj)))
    (format t "~&~a document ~a~%" (ganalysis 'grange-adj) (name doc))
    (time-profiling (range-adj doc)))
  
  (when (and (ganalysis 'ghierarchize) 
	     (not (analyzedp doc :ganalysis 'ghierarchize)))
    (format t "~&~a document ~a~%" (ganalysis 'ghierarchize) (name doc))
    (time-profiling (hierarchize doc)))


  
  (when (and (ganalysis 'ghier-parse) 
	     (not (analyzedp doc :ganalysis 'ghier-parse)))
    (format t "~&~a document ~a~%" (ganalysis 'ghier-parse) (name doc))
    (time-profiling (hier-parse doc)))

  (when (and (ganalysis 'gmetamap)
	     (not (analyzedp doc :ganalysis 'gmetamap)))
    (format t "~&~a document ~a~%" (ganalysis 'gmetamap) (name doc))
    (time-profiling (mm::metamap doc)))
  
  (when (and (ganalysis 'gic-pair)
	     (not (analyzedp doc :ganalysis 'gic-pair)))
    (format t "~&extracting structured feature from document ~a~%"
	    (name doc))
    (time-profiling (item-content-pair doc)))
  
  (when (and (ganalysis 'gimmuchem)
	     (not (analyzedp doc :ganalysis 'gimmuchem)))
    (format t "~&immuchem analyzing document ~a~%" (name doc))
    (time-profiling (extract-immun-histchem doc)))

  
  (when (and (ganalysis 'ghier-concept-graph) 
	     (not (analyzedp doc :ganalysis 'ghier-concept-graph)))
    (format t "~&~a document ~a~%" (ganalysis 'ghier-concept-graph) (name doc))
    (time-profiling (parse->graph doc :hier? t)))
  
  (when (and (ganalysis 'gload-persist-ann) 
	     (not (analyzedp doc :ganalysis 'gload-persist-ann)))
    (format t "~&loading persist annotations for document ~a~%"  (name doc))
    (time-profiling (load-persist-ann doc)))
  
  (format t "~&saving doc:~%~a~%" doc) 
  (time-profiling (save doc))  
  
  )

#||Interface for loading persist annotations for a document.||#
(defgeneric load-persist-ann ((doc document)))


(defun annotate-doc (dname cname 
			   &key (line-splits? nil)
			   (mode :cont)
			   &aux doc)
  (setf doc (document dname))
  (when (member mode '(:redo :re-import))
    (format t "~&clearing doc ~a~%" dname)
    (setf (analyses doc) nil)
    (setf (dirty doc) t)
    (format t "flushing annotations~%")
    (flush-annotations doc)  
    (format t "flushing concept graphs~%")
    (flush-concept-graph (id doc))
    )
  (when (eq mode :re-import)
    (setf (content doc) (read-file (source doc)))
    (setf (size doc) (length (content doc)))
    (setf (dirty doc) t)
    (save doc)
    )  
  (analyze-doc-pre-lp doc (corpus cname) :line-splits? line-splits?)
  
  )

(defun re-concept-graph-doc (dname &aux doc)
  (setf doc (document dname))
  (flush-concept-graph (id doc))
  (parse->graph doc))

(defun re-import-doc (dname cname 
			    &key (line-splits? "\\n *\\n"))
  (annotate-doc dname cname 
		:mode :re-import
		:line-splits? line-splits?))



(defmemo in-unitlist? (token)
  (open-gazette-database)
  (sql (format nil "select id from gazette where type='unit' and entry=~a"
	       (sq token))
       :db *gazette-db*))

(defmemo id->doc (id)
  (document id))

(defun update-doc-content (docid)
  (let* ((doc (document docid))
	 (fn (source doc))
	 (text (read-file fn)))
    (setf (content doc) text)
    (setf (size doc) (length text))
    (setf (analyses doc) nil)
    (setf (dirty doc) t)
    (save doc)))
