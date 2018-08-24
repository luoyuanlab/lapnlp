;;; -*- Mode: Lisp; Package: stanford-parser; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/18/2015 rewrite using asdf framework
yluo - 03/30/2012 creation 
Note: the Stanford Parser wrapper jar should be in the same directory as the 
Stanford Parser jar.
|#

(defpackage :stanford-parser
  (:nicknames :sp)
  (:use #+allegro :excl :common-lisp :javatools.jlinker :java-wrap-gen 
	:list-gen :jc)
  (:export 
   "*parser*"
   "*stanford-parser-home*"
   "*stanford-parser-jar*"
   "*stanford-parser-wrapper-jar*"
   "augment-pos-tag"
   "d-depid"
   "d-govid"
   "d-rel"
   "deprep"
   "parse"
   "parse-hier-tagged"
   "parse-hier-tokenized"
   "parse-tagged"
   "parse-tokenized"
   "pos-parse-hier-tagged"
   "parse-tokenized-constrained"
   "stanford-parser-env-init"
   "tokenize"
   "adjs-n?"
   ))

;;; in-package has effect at both compile time and load time
(in-package :stanford-parser)

(defparameter *stanford-parser-home* nil 
  "location of the Stanford Parser tools")

(defparameter *stanford-parser-jar* nil 
  "location of the Stanford Parser jar.")

(defparameter *stanford-parser-wrapper-jar* nil
  "location of the Stanford Parser wrapper jar.")

(defparameter *parser* nil
  "The java reference to the Stanford Parser object.")

(defparameter *stanford-parser-grammar* nil)

(defparameter *stanford-parser-version* nil
  "The version of the currently used Stanford Parser.")

(defun stanford-parser-env-init ()
  (setf *stanford-parser-home*
	(let ((onh (late:get-env "STANFORD_PARSER_HOME")))
	  (and onh (late:get-directory-pathname (translate-logical-pathname onh)))))
  
  (when *stanford-parser-home*
    (pushnew *stanford-parser-home* *classpath-dirs* :test #'equalp)
    (setq *stanford-parser-jar*
	  (multiple-value-bind (matched? all ver)
	      (match-re "stanford-parser-([\\d\\-]+)"
			(car (last (pathname-directory *stanford-parser-home*)))
			:case-fold t)
	    (declare (ignore all))
	    (and matched?
		 (setf *stanford-parser-version* ver)
		 (probe-file
		  (merge-pathnames
		   (make-pathname ;;:directory '(:relative "output")
		    :name "stanford-parser"
		    :type "jar")
		   *stanford-parser-home*)))))
    (pushnew *stanford-parser-jar* *classpath-jars* :test #'equalp)
    
    (setq *stanford-parser-wrapper-jar*
	  (multiple-value-bind (matched? all ver)
	      (match-re "stanford-parser-([\\d\\-]+)"
			(car (last (pathname-directory *stanford-parser-home*)))
			:case-fold t)
	    (declare (ignore all ver))
	    (and matched?
		 (probe-file
		  (merge-pathnames
		   (make-pathname ;;:directory '(:relative "output")
		    :name "stanford-parser-wrapper"
		    :type "jar")
		   *stanford-parser-home*)))))
    (pushnew *stanford-parser-wrapper-jar* *classpath-jars* :test #'equalp)
    ;; update classpath-dirs and classpath-jars
    (jlinker-slot :jar-file *classpath-jars*)
    (jlinker-slot :classpath *classpath-dirs*)
    
;;;    (setf *stanford-parser-grammar*
;;;	  (probe-file
;;;	   (merge-pathnames
;;;		(make-pathname :directory '(:relative "grammar")
;;;					   :name "englishPCFG"
;;;					   :type "ser.gz")
;;;		*stanford-parser-home*)))
    (setf *stanford-parser-grammar*
	  "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz")
    )

  
  ;; compile and load stanford parser wrappers.
  (cond 
   ((null *stanford-parser-home*)
    (format t "~&;;; Warning: could not find location of Stanford Parser.~%"))
   
   ((null *stanford-parser-jar*)
    (format t "~&;;; Warning: could not find location of Stanford Parser jar.~%"))
   ((null *stanford-parser-wrapper-jar*)
    (format t "~&;;; Warning: could not find location of Stanford Parser wrapper jar.~%"))
   
   (t
    (unless (probe-file "late:;stanford-parser-wrapper-jarlist.cl")
      (format t "~&;;; before generating Stanford Parser wrapper jar list~%")
      
      (list-gen:list-jar-classes (namestring *stanford-parser-wrapper-jar*) 
				 "late:;stanford-parser-wrapper-jarlist.cl"
				 "spwrapper"
				 :supersede)
      (format t "~&;;; after generating Stanford Parser jar list~%"))
    (unless (jlinker-query) (jl-init))
    (require :stanford-parser "late:;stanford-parser"))))
