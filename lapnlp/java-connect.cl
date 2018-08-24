;;; -*- Mode: LISP; Package: USER -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo -            creation
To use various Java programs, we have to set up the Java connections
and then call jl-init to open them.

This code determines appropriate bindings for various system-defined
location, such as the pathname of the jlinker.jar file. Basic
definitions are given in config.cl of where things such as the Java
libraries, jlinker, etc., are to be found. Therefore, that file must
be loaded first.

When jl-init is called, we expect that applications will have pushed
pathnames or their corresponding strings onto the parameters
*classpath-dirs* and *classpath-jars*, to be passed on to the jlinker
interface.

|#




(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :jlinker))

(defpackage :javatools.jlinker (:nicknames :jl))

(defpackage :java-connect 
  (:nicknames :jc)
  (:use :javatools.jlinker :late :common-lisp)
  (:export "*jlinker-home*"
	   "*classpath-dirs*"
	   "*classpath-jars*"
	   "jl-free"
	   "jl-init"
	   "*jl-end-callbacks*"
	   "jl-end"
	   ;; need to clean up this bit by automatically generating java 
	   ;; wrappers
	   "jvector"
	   "vsize"
	   "elt-at"
	   "jobject"
	   "jstring"
	   "str-clone"
	   "obj-tostring"
	   "new-string"
	   "str-tostring"
	   "jlist"
	   "new-jlist"
	   "list-add"
	   "list-get"
	   "list-size"
	   ))
;;; used for debugging jlinker time out, doesn't seem to work for acl90
;;;(in-package :net.jlinker)
;;;(def-fwrapper tfmt (&rest args)
;;;  (declare (ignorable args))
;;;  (print (get-universal-time))
;;;  (call-next-fwrapper))
;;;(def-fwrapper tlret (&rest args)
;;;  (declare (ignorable args))
;;;  (print (get-universal-time))
;;;  (call-next-fwrapper))
;;;(fwrap '(flet jlinker-init-socket fmt) :jm 'tfmt)
;;;(fwrap '(flet jlinker-init-socket lret) :jm 'tlret)
(in-package :jc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java connection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; parameters from javatools.jlinker:, no need to export again
(defparameter *jlinker-java-home*
    (namestring (get-directory-pathname (get-env-pathname
    "JAVA_HOME")))
  "The namestring (not the pathname!) of the directory where Java's
    needed components are.")

;;; parameters from javatools.jlinker:, no need to export again
(defparameter *jni-library*
    (namestring
     (or (get-directory-pathname (get-env-pathname "JNI_LIBRARY"))
	 ;; javatools.jlinker:
	 (and *jlinker-java-home*
	      (let ((jni-offset (get-env "JNI_OFFSET")))
		(and jni-offset
		     (probe-file (make-path
				  jni-offset
				  ;; javatools.jlinker:
				  *jlinker-java-home*)))))))
  "The namestring of the java runtime dynamic library.")

(defparameter *jlinker-home*
    (get-directory-pathname (get-env-pathname "JLINKER_HOME")))

;;; javatools.jlinker:
(defparameter *jni-ld-path-p* t)

(defparameter *jlinker-jar*
    (or (get-env-pathname "JLINKER_JAR")
	(and *jlinker-home*
	     (probe-file (make-path "jlinker.jar" *jlinker-home*)))))

(defparameter *classpath-dirs*
    nil
  "List of directories that should be added to the CLASSPATH when Java
    is invoked by jl-init.")

(defparameter *classpath-jars*
    nil
  "List of JARs that should be added to the CLASSPATH when Java is
    invoked by jl-init.")

;;; wrapper for discard-in-java function
(defun jl-free (to-free)
  (discard-in-java to-free)
  )
;; the :options keywords should really be there, it lets the -Xms and -Xmx
;; options to be passed before other jlinker options.
(defun jl-init (&key (dirs *classpath-dirs*)
					 (jars *classpath-jars*))
  (format t "~%Initializing jlinker connection, with additional~
~% directories=~s and ~
~% jars=~s"
		  dirs jars)
  ;; value 10 allow 30 seconds delay (per franz)
  (setf net.jlinker::*jlinker-retry-delay* 20)
  ;; start jlinker in native mode hope to boost performance
  (jlinker-init :mode :jni
				:debug nil
				:classpath
				(mapcar #'namestring (cons *jlinker-home* dirs))
				:jar
				(mapcar #'namestring (cons *jlinker-jar* jars))
				:java-args
				(list (list :options "-Xms1536M" "-Xmx1536M"))))

(defparameter *jl-end-callbacks*
    nil
  "List of forms to be evaluated when (jl-end) is called.")

(defun jl-end ()
  (dolist (form *jl-end-callbacks*)
    (eval form))
  (jlinker-end))

;;; General purpose utility definitions for Lisp access to Java data

(def-java-class (jvector "java.util.Vector") ()
 ()
 ()
 ())

(def-java-method (vsize "size")
   (jvector))

(def-java-method (elt-at "elementAt")
   (jvector "int"))

(def-java-class (jobject "java.lang.Object") ()
  () () ())

(def-java-class (jstring "java.lang.String") ()
  () () ())

(def-java-method (str-clone "clone")
    (jstring))

(def-java-method (obj-tostring "toString")
    (jobject))

(def-java-constructor new-string
    (jstring "java.lang.String"))

(def-java-method (str-tostring "toString")
    (jstring))

(def-java-class (jlist "java.util.ArrayList") ()
  () () ())

(def-java-constructor new-jlist
    (jlist))

(def-java-method (list-add "add")
    (jlist "E"))

(def-java-method (list-get "get")
    (jlist "int"))

(def-java-method (list-size "size")
    (jlist))

#|
(defun show-java-connect ()
  (format t "~%*site*=~s; *software*=~s~
~%jl:*jlinker-java-home*=~s~
~%jl:*jni-library*=~s~
~%*jlinker-dll-home*=~s~
~%*classpath-dirs*=~s~
~%*classpath-jars*=~s~%"
	  *site*
	  *software*
	  javatools.jlinker:*jlinker-java-home*
	  javatools.jlinker:*jni-library*
	  *jlinker-dll-home*
	  *classpath-dirs*
	  *classpath-jars*))
|#

;;; (provide :java-connect)
