;;; -*- Mode: Lisp; Package: metamap; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 03/30/2012 creation 
Note: the MetaMap wrapper jar should be in the same directory as the 
MetaMap jar.
|#

(defpackage :metamap
  (:nicknames :mm)
  (:use #+allegro :excl :common-lisp :javatools.jlinker :java-wrap-gen 
	:list-gen :jc)
  (:export 
   "*metamap*"
   "*metamap-wrapper-home*"
   "metamap-env-init"
   "metamap"
   "*debug-metamap*" 
   "write-unmasked-corpus"
   ))

;;; in-package has effect at both compile time and load time
(in-package :metamap)


(defparameter *metamap-wrapper-home* nil
  "location of the MetaMap wrapper")

(defparameter *metamap-jar* nil 
  "location of the MetaMap jar.")

(defparameter *pb-jar* nil
  "locatoin of the PrologBeans jar.")

(defparameter *metamap-wrapper-jar* nil
  "location of the MetaMap wrapper jar.")

(defparameter *metamap* nil
  "The java reference to the MetaMap object.")

(defparameter *metamap-version* 2012
  "The version of the currently used MetaMap.")

(defun metamap-env-init ()
  (setf *metamap-wrapper-home*
	(let ((onh (late:get-env "METAMAP_WRAPPER_HOME")))
	  (and onh (late:get-directory-pathname (translate-logical-pathname onh)))))

  (when *metamap-wrapper-home*
    (pushnew *metamap-wrapper-home* *classpath-dirs* :test #'equalp)
    (setf *metamap-jar* 
	  (probe-file
	   (merge-pathnames
	    (make-pathname ;;:directory '(:relative "output")
	     :name "MetaMapApi"
	     :type "jar")
	    *metamap-wrapper-home*)))
    (pushnew *metamap-jar* *classpath-jars* :test #'equalp)

    (setf *pb-jar* 
	  (probe-file
	   (merge-pathnames
	    (make-pathname ;;:directory '(:relative "output")
	     :name "prologbeans"
	     :type "jar")
	    *metamap-wrapper-home*)))
    (pushnew *pb-jar* *classpath-jars* :test #'equalp)
    
    (setq *metamap-wrapper-jar*
	  (probe-file
	   (merge-pathnames
	    (make-pathname ;;:directory '(:relative "output")
	     :name "metamap-wrapper"
	     :type "jar")
	    *metamap-wrapper-home*)))
    (pushnew *metamap-wrapper-jar* *classpath-jars* :test #'equalp)
    ;; update classpath-dirs and classpath-jars
    (jlinker-slot :jar-file *classpath-jars*)
    (jlinker-slot :classpath *classpath-dirs*))

  
  ;; compile and load stanford parser wrappers.
  (cond 
   ((null *metamap-jar*)
    (format t "~&;;; Warning: could not find MetaMap jar.~%"))
   ((null *metamap-wrapper-jar*)
    (format t "~&;;; Warning: could not find MetaMap wrapper jar.~%"))
   ((null *pb-jar*)
    (format t "~&;;; Warning: could not find prologbeans jar.~%"))
   
   (t
    (unless (probe-file "late:;metamap-wrapper-jarlist.cl")
      (format t "~&;;; before generating MetaMap wrapper jar list~%")
      
      (list-gen:list-jar-classes (namestring *metamap-wrapper-jar*) 
				 "late:;metamap-wrapper-jarlist.cl"
				 "mmwrapper"
				 :supersede)
      (format t "~&;;; after generating MetaMap jar list~%"))
    (unless (jlinker-query) (jl-init))
    (require :metamap-wrapper-jarlist "late:;metamap-wrapper-jarlist")
    (require :metamap "late:;metamap"))))
