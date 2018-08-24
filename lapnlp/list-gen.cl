;;; -*- Mode: Lisp; Package: list-gen; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 06/01/2015 rewrite using asdf framework
yluo -            creation
|#
;;; for all forms that invoke define-java-methods-class directly or indirectly
;;; we should put it into a eval-when form of situation :compile-toplevel only,
;;; not together with :load-toplevel. O.W. the define-java-methods-class will  
(defpackage :list-gen
  (:use :java-wrap-gen #+allegro :excl :common-lisp :jc :javatools.jlinker)
  (:export "list-jar-classes"))

(in-package :list-gen)

(eval-when (:compile-toplevel :load-toplevel :execute)  
  (require :jlinker)

  ;;java-wrap-gen:  
  (write-java-wrapper "java.util.ArrayList"
		      "late:;java;util;ArrayList.cl"
		      nil)
  (require :java.util.ArrayList
	   "late:;java;util;ArrayList")
  
  (write-java-wrapper "java.io.FileInputStream"
		      "late:;java;io;FileInputStream.cl"
		      nil)
  (require :java.io.FileInputStream
	   "late:;java;io;FileInputStream")

  (write-java-wrapper "java.util.jar.JarInputStream"
		      "late:;java;util;jar;JarInputStream.cl"
		      nil)
  (require :java.util.jar.JarInputStream
	   "late:;java;util;jar;JarInputStream")
  
  (write-java-wrapper "java.util.jar.JarEntry"
		      "late:;java;util;jar;JarEntry.cl"
		      nil)
  (require :java.util.jar.JarEntry
	   "late:;java;util;jar;JarEntry")
  
  (write-java-wrapper "java.lang.String"
		      "late:;java;lang;String.cl"
		      nil)
  (require :java.lang.String
	   "late:;java;lang;String")

  

;;;   (jc:jl-end)

  )

;; redirects wrappres into a single file named by jf-list
;; remember for package :java.util, its package name is java.util
;;; output wrappers to separate files
(defun list-jar-classes (jar-name jf-list package-name supersede)
  (unless (javatools.jlinker:jlinker-query)
    (jc:jl-init)
    )
  ;; jlinker-slot is not fully documented
  ;;javatools.jlinker:   jc:
  (jlinker-slot :jar-file *classpath-jars*)
  ;;javatools.jlinker:   jc:
  (jlinker-slot :classpath *classpath-dirs*)
  
  (with-open-file 
   (jfls jf-list :direction :output :if-exists supersede)
   (let* (;; (classes (java.util.ArrayList:ArrayList))
	  jar-file jar-entry jar-string)
     (setq package-name (replace-re package-name "\\." "/"))
     (setq jar-file (java.util.jar.JarInputStream:JarInputStream 
		     (java.io.FileInputStream:FileInputStream 
		      jar-name)))
     (format jfls "(eval-when (:compile-toplevel :load-toplevel :execute)~%")
     (while (setq jar-entry (java.util.jar.JarInputStream:getNextJarEntry 
			     jar-file))

       (when (and (setq jar-string (jcall "getName" jar-entry))
		  (>= (length jar-string) (length package-name))
		  (>= (length jar-string) (length ".class"))
		  (string= jar-string package-name
			   :end1 (length package-name))
		  (string= jar-string ".class"
			   :start1 (- (length jar-string) (length ".class")))
		  ;; filter out anonymous classes
		  (not (find #\$ jar-string)))
	 (format t "~&list-jar-classes: ~a~%" (replace-re jar-string "/" "\."))
	 (let (cname outfile)
	   (setq cname (replace-re jar-string "/" "\."))
	   (setq cname (replace-re cname "\\.class$" ""))
	   ;; changed replacement string from ".cl" to ""
	   (setq outfile (concatenate 'string
				      (namestring (translate-logical-pathname "late:;")) 
				      (replace-re jar-string 
						  "\\.class$" "")))
	   
	    ;; (setq dirname 
	    ;;   (concatenate 'string "./"
	    ;; 		   (replace-re (format nil "~a" jar-string) 
	    ;; 			       "^(.*)/.*" "\\1")))
	    ;; (unless (probe-file dirname)
	    ;;   (create-dir dirname))
	    ;; (define-java-methods-class cname outfile nil supersede)
	   (write-java-wrapper cname (concatenate 'string outfile ".cl"))

	   ;; should compile and load file one at a time to avoid symbol
	   ;; name conflict
	   ;; (compile-file outfile)
	   ;; (load (replace-re outfile "\\.cl$" ".fasl"))
	   (format jfls "  (require ~a \"~a\")~%~%" 
		   (concatenate 'string ":" cname)
		   outfile)
	   ;; (format jfls "(unless (probe-file \"~a\")~%"
	   ;; 	    (replace-re outfile "\\.cl$" ".fasl"))
	   ;; (format jfls "~&(compile-file \"~a\")~%)~%" outfile)
	   ;; (format jfls "~&(load \"~a\")~%~%" 
	   ;; 	    (replace-re outfile "\\.cl$" ".fasl"))
	   ))
       )
     (format jfls "~&)~%")
     )
   )
  )
