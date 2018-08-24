;;; -*- Mode: Lisp; Package: weka; -*-

#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework

yluo - 05/14/2010 changed the way wrapper are loaded, now on demand basis
yluo - 10/20/2009 creation 

See LATE-vs-cascading_file.vsd for reason to operate WEKA API directly on
in-memory or DB stored features.
TODO: 
1. WEKA API operate directly on in-memory features;
2. Store the features in DB in serialized part of the corresponding
annotations, lisp will automatically unserialize them.
The reason that I have to write these WEKA wrappers is that WEKA in general
expects .arff files as input rather than chunks from memory
|#

(defpackage :weka
  (:use #+allegro :excl :common-lisp :javatools.jlinker :java-wrap-gen 
	:list-gen :jc :late :dbi.mysql)
  (:export "*weka-home*"
	   "*weka-jar*"))

;;; in-package has effect at both compile time and load time
(in-package :weka)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :jlinker)

  (defparameter *weka-home* nil "location of the WEKA tools")
  (setq *weka-home*
	(let ((onh (late:get-env "WEKAHOME")))
	  (and onh (late:get-directory-pathname (translate-logical-pathname onh)))))
  
  (defparameter *weka-jar* nil "location of the WEKA jar")
  (when *weka-home*
    (setq *weka-jar*
	  (multiple-value-bind (matched? all ver)
	      (match-re "weka-([\\d\\-]+)"
			(car (last (pathname-directory *weka-home*)))
			:case-fold t)
	    (declare (ignore all ver))
	    (and matched?
		 (probe-file
		  (merge-pathnames
		   (make-pathname ;;:directory '(:relative "output")
		    :name "weka"
		    :type "jar")
		   *weka-home*))))))

  
  ;; compile and load weka-jarlist wrappers.
  (cond 
   ((null *weka-home*)
    (format t "~&;;; Warning: could not find location of WEKA toolkit.~%")
    )
   ((null *weka-jar*)
    (format t "~&;;; Warning: could not find location of WEKA jar.~%")
    )
   (t
    (unless (probe-file "late:;weka-jarlist.cl")
      (format t "~&;;; before generating weka jar list~%")
      
      (list-gen:list-jar-classes (namestring *weka-jar*) 
				 "late:;weka-jarlist.cl"
				 "weka"
				 :supersede)
      (format t "~&;;; after generating weka jar list~%"))
    (require :weka "late:;weka"))))
