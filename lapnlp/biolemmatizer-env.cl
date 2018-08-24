;;; -*- Mode: Lisp; Package: stanford-parser; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 10/20/2012 creation 
Note: the Biolemmatizer wrapper jar should be in the same directory as the 
Biolemmatizer jar.
|#

(defpackage :biolemmatizer
  (:nicknames :bl)
  (:use #+allegro :excl :common-lisp :javatools.jlinker :java-wrap-gen 
	:list-gen :jc)
  (:export 
   "*biolemmatizer*"
   "*biolemmatizer-home*"
   "*biolemmatizer-jar*"
   "*biolemmatizer-wrapper-jar*"
   "biolemmatizer-env-init"
   "get-lemma"
   ))

;;; in-package has effect at both compile time and load time
(in-package :biolemmatizer)

(defparameter *biolemmatizer-home* nil 
  "location of the BioLemmatizer tools")

(defparameter *biolemmatizer-jar* nil 
  "location of the BioLemmatizer jar.")

(defparameter *biolemmatizer-wrapper-jar* nil
  "location of the BioLemmatizer wrapper jar.")

(defparameter *biolemmatizer* nil
  "The java reference to the BioLemmatizer object.")

(defparameter *biolemmatizer-grammar* nil)

(defparameter *biolemmatizer-version* nil
  "The version of the currently used BioLemmatizer.")

(defun biolemmatizer-env-init ()
  (setf *biolemmatizer-home*
	(let ((onh (late:get-env "BIOLEMMATIZER_HOME")))
	  (and onh (late:get-directory-pathname (translate-logical-pathname onh)))))
  
  (when *biolemmatizer-home*
    (pushnew *biolemmatizer-home* *classpath-dirs* :test #'equalp)
    (setq *biolemmatizer-jar*
	  (multiple-value-bind (matched? all ver)
	      (match-re "biolemmatizer-([\\d\\-.]+)"
			(car (last (pathname-directory *biolemmatizer-home*)))
			:case-fold t)
	    (declare (ignore all))
	    (and matched?
		 (setf *biolemmatizer-version* ver)
		 (probe-file
		  (merge-pathnames
		   (make-pathname ;;:directory '(:relative "output")
		    :name "biolemmatizer"
		    :type "jar")
		   *biolemmatizer-home*)))))
    (pushnew *biolemmatizer-jar* *classpath-jars* :test #'equalp)
    
    (setq *biolemmatizer-wrapper-jar*
	  (multiple-value-bind (matched? all ver)
	      (match-re "biolemmatizer-([\\d\\-]+)"
			(car (last (pathname-directory *biolemmatizer-home*)))
			:case-fold t)
	    (declare (ignore all ver))
	    (and matched?
		 (probe-file
		  (merge-pathnames
		   (make-pathname ;;:directory '(:relative "output")
		    :name "biolemmatizer-wrapper"
		    :type "jar")
		   *biolemmatizer-home*)))))
    (pushnew *biolemmatizer-wrapper-jar* *classpath-jars* :test #'equalp)
    ;; update classpath-dirs and classpath-jars
    (jlinker-slot :jar-file *classpath-jars*)
    (jlinker-slot :classpath *classpath-dirs*))

  
  ;; compile and load biolemmatizer wrappers.
  (cond 
   ((null *biolemmatizer-home*)
    (format t "~&;;; Warning: could not find location of BioLemmatizer.~%"))
   
   ((null *biolemmatizer-jar*)
    (format t "~&;;; Warning: could not find location of BioLemmatizer jar.~%"))
   ((null *biolemmatizer-wrapper-jar*)
    (format t "~&;;; Warning: could not find location of BioLemmatizer wrapper jar.~%"))
   
   (t
    (unless (probe-file "late:;biolemmatizer-wrapper-jarlist.cl")
      (format t "~&;;; before generating BioLemmatizer wrapper jar list~%")
      
      (list-gen:list-jar-classes (namestring *biolemmatizer-wrapper-jar*) 
				 "late:;biolemmatizer-wrapper-jarlist.cl"
				 "blwrapper"
				 :supersede)
      (format t "~&;;; after generating BioLemmatizer jar list~%"))
    (unless (jlinker-query) (jl-init))
    (require :biolemmatizer "late:;biolemmatizer"))))
