;;; -*- Mode: Lisp; Package: stanford-parser; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/10/2015 rewrite using asdf framework
yluo - 03/30/2012 creation 

Note: the Opennlp wrapper jar should be in the same directory as the 
Opennlp jar.
|#

(defpackage :opennlp
  (:nicknames :op)
  (:use #+allegro :excl :common-lisp :javatools.jlinker :java-wrap-gen 
	:list-gen :jc)
  (:import-from :late "pathdir" "get-env" "get-directory-pathname")
  (:export 
   "*opennlp-version*"
   "*pos-tagger-setting*"
   "chunker-init"
   "chunkdetect"
   "find-file-candidate"
   "opennlp-env-init"
   "opennlp-get-version"
   "opennlp-init"
   "opennlp-parsedp"
   "opennlp-reinit"
   "opennlp-tokenizedp"
   "phrase-chunk-arr"
   "phrase-chunk-list"
   "posdetect"
   "sentdetect"
   "sentdetect-init"
   "tag-word-probs"
   "tag-word-vector"
   "tagger-init"
   "token-detect"
   "tokenize-init"
   ))

;;; in-package has effect at both compile time and load time
(in-package :opennlp)

(defparameter *opennlp-wrapper-home* nil 
  "location of the Stanford Parser tools")

(defparameter *opennlp-wrapper-jar* nil 
  "location of the Opennlp wrapper jar.")

(defparameter *opennlp-jar* nil 
  "location of the OpenNLP tool jar.")

(defparameter *opennlp-maxent-jar* nil 
  "location of the OpenNLP maxent jar.")

(defparameter *opennlp-home* nil
  "location of the Stanford Parser wrapper jar.")

(defparameter *opennlp-version* nil
  "The version of the currently used OpenNLP Toolkit.")

(defparameter *opennlp-sentencizer-model* nil)
(defparameter *opennlp-tokenizer-model* nil)
(defparameter *opennlp-postagger-model* nil)
(defparameter *opennlp-chunker-model* nil)


(defun opennlp-env-init (&key (ver "1.5.2"))
  "Opennlp doesn't seem to follow a convention of naming jars and where to put 
those jars, so might settle with this manual specification as necessary evil.
But wait, I think I only need the wrapper."
  (cond 
   ((equalp ver "1.5.2")
    (setf *opennlp-wrapper-home*
	  (let ((onh (get-env "OPENNLP_1p5p2_WRAPPER_HOME")))
	    (and onh 
		 (get-directory-pathname (translate-logical-pathname onh)))))
    (setf *opennlp-home*
	  (let ((onh (get-env "OPENNLP_1p5p2_HOME")))
	    (and onh 
		 (get-directory-pathname (translate-logical-pathname onh)))))))
  
  (when *opennlp-wrapper-home*
    (setf *opennlp-version* ver)
    
    (setf *opennlp-jar* 
	  (probe-file
	   (merge-pathnames
	    (make-pathname :directory '(:relative "opennlp-tools" "target")
			   :name "opennlp-tools-1.5.2-incubating"
			   :type "jar")
	    *opennlp-home*)))
    (pushnew *opennlp-jar* *classpath-jars* :test #'equalp)	
    (pushnew (pathdir *opennlp-jar*) *classpath-dirs* :test #'equalp)
    
    (setf *opennlp-maxent-jar* 
	  (probe-file
	   (merge-pathnames
	    (make-pathname :directory '(:relative "opennlp-maxent" "target")
			   :name "opennlp-maxent-3.0.2-incubating"
			   :type "jar")
	    *opennlp-home*)))
    (pushnew *opennlp-maxent-jar* *classpath-jars* :test #'equalp)	
    (pushnew (pathdir *opennlp-maxent-jar*) *classpath-dirs* :test #'equalp)
    
    (setf *opennlp-wrapper-jar* 
	  (probe-file
	   (merge-pathnames
	    (make-pathname :type "jar"
			   :name "opennlp-wrapper")
	    *opennlp-wrapper-home*)))
    (pushnew *opennlp-wrapper-jar* *classpath-jars* :test #'equalp)
    (pushnew (pathdir *opennlp-wrapper-jar*) *classpath-dirs* :test #'equalp)
    
    ;; update classpath-dirs and classpath-jars
    (jlinker-slot :jar-file *classpath-jars*)
    (jlinker-slot :classpath *classpath-dirs*))

  (setf *opennlp-sentencizer-model*
	(probe-file
	 (merge-pathnames
	  (make-pathname :directory '(:relative "models")
			 :name "en-sent"
			 :type "bin")
	  *opennlp-home*)))

  (setq *opennlp-tokenizer-model*
	(probe-file
	 (merge-pathnames
	  (make-pathname :directory '(:relative "models")
			 :name "en-token"
			 :type "bin")
	  *opennlp-home*)))

  (setq *opennlp-postagger-model*
	(probe-file
	 (merge-pathnames
	  (make-pathname :directory '(:relative "models")
			 :name "en-pos-maxent"
			 :type "bin")
	  *opennlp-home*)))

  (setq *opennlp-chunker-model*
	(probe-file
	 (merge-pathnames
	  (make-pathname :directory '(:relative "models")
			 :name "en-chunker"
			 :type "bin")
	  *opennlp-home*)))

  
  ;; compile and load stanford parser wrappers.
  (cond 
   ((null *opennlp-wrapper-home*)
    (format t "~&;;; Warning: no OpenNLP ~a wrapper.~%" ver))
   
   ((null *opennlp-wrapper-jar*)
    (format t "~&;;; Warning: no OpenNLP ~a wrapper jar.~%" ver))
   
   (t
    (unless (probe-file "late:;opennlp-wrapper-jarlist.cl")
      (format t "~&;;; before generating OpenNLP wrapper jar list~%")
      
      (list-gen:list-jar-classes (namestring *opennlp-wrapper-jar*) 
				 "late:;opennlp-wrapper-jarlist.cl"
				 "opwrapper"
				 :supersede)
      (format t "~&;;; after generating OpenNLP wrapper jar list~%"))
    (unless (jlinker-query) (jl-init))
    (require :opennlp "late:;opennlp"))))
