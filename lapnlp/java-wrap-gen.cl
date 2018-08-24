;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 02/14/2010 changed fboundp in new-sym-string to find-all-symbols

The following code generates Lisp calls to all the public methods 
declared in a Java class.  The generated code will create duplicate
Lisp definitions if several classes have methods of the same name
and there may be need fo many other refinements.  One could also
use a more complex generator to emit Lisp method definitions 
created with calls to def-java-class, def-java-method and 
def-java-static.  


Jlinker uses the Java 2 reflection API to locate classes and methods 
and to perform calls. The Jlinker implementation is not aware of 
parametrized classes.

If you need to instantiate parametrized classes, you must write Java
methods that return the desired instances.  The jlinker code will
treat the instances as instances of the un-parametrized type.

TODO: add support for generics. Generics in java are implemented 
by type erasure, which means that generic type information won't 
be seen by runtime. Thus there is no way to declare parameterized 
ttypes/generics through marshalling or reflection in runtime. One
such example is Jlinker, which connects to Java through a runtime 
server with marshalling. On the other hand, providing that we 
pass the correctly typed generics to the run time, and we 
correctly cast the returning typed generics, we may just omit the 
"type of" declaration in our method-signature wrapper. However, we
still keep the typed generics information in the documentation of
the wrapper as mnemonics. This has been tested by the atest class 
defined in weka.cl. 
Question: since Java reflection cannot determine the "type of" 
declaration for the generics, it seems not possible to add 
mnemonics, but how does Javadoc do it?
But what if you want to define classes 
for Collection<E>? Maybe I can hit it with a class Collection
and treat all Collection<E> classes as class Collection modulo 
casting. Let me give a try.
|#
;; should use lisp namespace
;; you may not want to write wrappers for annonymous class.
(defpackage :java-wrap-gen
  (:use :excl :common-lisp :javatools.jlinker :jc :util)
  (:export "new-sym-string"
	   "jsuperclasses"
	   "jparam-arg-compat"
	   "jparam-arg-list-compat"
	   "define-java-methods-class"
	   ;; "create-dir"
	   "write-java-wrapper"))

(in-package :java-wrap-gen)

;;; fboundp seems not able to catch the symbol named "satisfies"
;;; that is external in some package having package-definition-lock set.
;;; find-all-symbols seem to do the job
(defmacro new-sym-string (x)
  ;; not sure
  `(if ;;(find-all-symbols ,x)
       (or (find-symbol ,x :excl) (find-symbol ,x :common-lisp)
	   (find-symbol ,x :javatools.jlinker) (find-symbol ,x :java-wrap-gen))
       (concatenate 'string "dr-" ,x) ;; dr for duplicate removal
					;(symbol-name (gensym ,x))
     ,x))

;; return x and x's superclasses
;; x must be an name of a class
;; note that Java doesn't allow mutliple inheritance
(defun jsuperclasses (x)
  (let (jsupers c sc)
    (setf c (jclass x))
    (when (setf sc (jcall "getSuperclass" c))
      (setf jsupers (jsuperclasses sc)))
    (pushnew (jcall "getName" c) jsupers :test #'equal)
    jsupers))

;; returns whether parameter and argument is compatible in Java
;; true if param and arg are of the same class
;; or if param is superclass of arg
(defun jparam-arg-compat-strict (param arg)
  (member param (jsuperclasses arg) :test #'equal)
  )

;; Include as compatible "argument conversions from Lisp to Java"
;; see jLinker
;; When a java method is invoked that causes null value to be passed as an 
;; argument to a java parameter whose datatype is a java class, it is passed
;; as a java null reference value, when a null value is passed as an argument
;; to a java parameter of a java primitive datatype, however, an exception is
;; raised because the java primitive datatype has no representation for a null
;; value.
;; note that isInstance, which is a dynamic equivalent to instanceOf operator
;; always returns false if the class object represents a primitive type
;; this explains why jparam-arg-compat is still necessary.
(defun jparam-arg-compat (param parg)
  (or 

   ;; how to handle boolean?
   (and (equal param "boolean") (or (eq t parg) (eq nil parg)))
   (and (equal param "char") (characterp parg))
   (and (equal param "byte") (integerp parg))
   (and (equal param "short") (integerp parg))
   (and (equal param "int") (integerp parg))
   (and (equal param "long") (integerp parg))
   (and (equal param "float") (numberp parg))
   (and (equal param "double") (numberp parg))
   (and (equal param "java.lang.String") (numberp parg))
   (and (equal param "[Lbyte;") (or (vectorp parg) (listp parg)))
   (and (equal param "[Lshort;") (or (vectorp parg) (listp parg)))
   (and (equal param "[Lint;") (or (vectorp parg) (listp parg)))
   (and (equal param "[Lfloat;") (or (vectorp parg) (listp parg)))
   (and (equal param "[Ldouble;") (or (vectorp parg) (listp parg)))
   (and (equal param "[Ljava.lang.String;") (or (vectorp parg) (listp parg)))

;;;   (and (not (null parg))
;;;	(not (null (jcall "getClass" parg)))
;;;	(not (null (jcall "getName" 
;;;			  (jcall "getClass" parg))))
;;;	(member param (jsuperclasses (jcall "getName" 
;;;					    (jcall "getClass" parg))) 
;;;		:test #'equal))
   ;; this should already handles the class casting problems
   (and (not (null parg))
	(jcall "isInstance" (jclass param) parg))
   
   (and (null parg)
	(not (member param '("boolean" "byte" "short" "int" "long" 
			     "char" "float" "double") :test #'equal))
	)
   )
  )

(defun jparam-arg-list-compat-strict (param-l arg-l)
  (let ((res t))
    (if (/= (length param-l) (length arg-l))
	(return-from jparam-arg-list-compat-strict nil))
    (dotimes (i (length arg-l))
      (setq res (and res 
		     (jparam-arg-compat-strict (elt param-l i)
					       (elt arg-l i ))))
      (unless res
	(return-from jparam-arg-list-compat-strict nil)))
    res
    ))

(defun jparam-arg-list-compat (param-l parg-l)
  (let ((res t))
    (if (/= (length param-l) (length parg-l))
	(return-from jparam-arg-list-compat nil))
    (dotimes (i (length parg-l))
      (setq res (and res 
		     (jparam-arg-compat (elt param-l i)
					(elt parg-l i))))
      (unless res
	(return-from jparam-arg-list-compat nil)))
    res
    ))

;; for the class model
(defun define-java-methods-class (class outfile def-super supersede)
  ;; the result of find-all-symbols on package after delete-package is 
  ;; undefined, but for ACL, it seems to return nil
  (unless (javatools.jlinker:jlinker-query)
    (jc:jl-init)
    )
  ;;javatools.jlinker:   jc:
  (jlinker-slot :jar-file *classpath-jars*)
  ;;javatools.jlinker:   jc:
  (jlinker-slot :classpath *classpath-dirs*)

  (if (find-package class)
      (delete-package (find-package class)))
  (with-open-file ;; do nothing if already have wrapper
   (out outfile :direction :output :if-exists supersede 
;;;       :if-does-not-exist :create
	)
;;;    (format t "~&if-exists ~s~%" supersede)
   (let* ((cname class) ;; needs refinement 
	  (lcname (new-sym-string (replace-re class ".*\\." "")))
	  (cref (jclass class))
	  ;; (package (jcall "getPackage" cref))
	  ;; (pname (jcall "getName" package))
	  ;; include all fields accessible
	  (fields (jcall "getFields" cref))
	  ;; there is no multiple inheritance in Java
	  (jsuper (jcall "getSuperclass" cref)) 
	  (constructors (jcall "getDeclaredConstructors" cref))
	  ;; include all methods accessible
	  (methods (jcall "getMethods" cref))
	  method constructor mod jname lname static statics ifields jargs args
	  export-syms
	  )

     ;; def-java-class
     ;; class name

     (format out "~&~%~%;;; This is automatically generated.~%")
     (format out ";;; Please do not modify this file.~%")
     (format out ";;; Instead, please modify java-wrap-gen.cl.~%")
     ;; use class names to name packages, because java methods donot belong to
     ;; classes
     (format out "~&(defpackage :~a~%  (:use :excl :common-lisp :javatools.jlinker :java-wrap-gen))~%" cname)
     (format out "~&(in-package :~a)~%" cname)

     (format out "~&(def-java-class (~a \"~a\")~%" lcname cname)
     ;; push the class name to export list
     (push lcname export-syms)
     ;; super class
     (if def-super
	 (format out "~&    (\"~a\")~%" (jcall "getName" jsuper))
       (format out "~&    ()~%"))
     ;; statics
     (let (lsname jsname) ;; l(j)sname lisp/java static name
       (dotimes (j (or (jarrayp fields 0) 0))
	 (setq jsname (jcall "getName" (jarray-ref fields j)))
	 ;; prepend "f" in field name, to avoid field and method have the 
	 ;; same name.
	 (setq lsname (concatenate 'string "f" 
				   (new-sym-string 
				    (replace-re jsname "(\\.|_)" "-"))))
	 (setq mod (jcall "getModifiers" (jarray-ref fields j)))
	 ;; include all non-private fields
	 (unless (jstatic "isPrivate" "java.lang.reflect.Modifier" mod)
	   (push lsname export-syms)
	   (if (jstatic "isStatic" "java.lang.reflect.Modifier" mod)
	       (push `(,lsname ,(concatenate 'string "\"" jsname "\"")) 
		     statics)
	     (push `(,lsname ,(concatenate 'string "\"" jsname "\"")) 
		   ifields)))))
     ;; this is ugly, should use pretty print to resolve indentation issues.
     (format out "~&    (~{(~a)~^~%     ~})~%" (nreverse statics))
     ;; fields
     (format out "~&    (~{(~a)~^~%     ~})~%" (nreverse ifields))
     
     ;; slots
     (format out "~&    ())~%")
     
     ;; def-java-constructor, able to overload
     (format out "~%~%;; def-java-constructor, able to overload.~%~%")
     (format out "~&(defun ~a (&rest pargs)~%" lcname)
     (format out "~&  (cond~%")
     ;; the line below goes with strict mode compatibility check
     ;;      (format out "~&  (let ((args (mapcar #'(lambda (x) (if (stringp x) \"java.lang.String\" (jcall \"getName\" (jcall \"getClass\" x)))) pargs) ))~%")
     (dotimes (j (jarrayp constructors 0))
       (setq constructor (jarray-ref constructors j))
       (setq jargs (jcall "getParameterTypes" constructor))
       (setq args nil)
       (dotimes (i (jarrayp jargs 0))
	 (push (jcall "getName" (jarray-ref jargs i)) args))
       (setq args (nreverse args))
       ;; a better way is to check one-one correspondence, not set equality
       (format out "~&    ((jparam-arg-list-compat '(~{\"~a\"~^ ~}) pargs)~%" args)
       ;; strict compatibility check
       ;; (format out "~&    (if (jparam-arg-list-compat '(~{\"~a\"~^ ~}) args)~%" args)

       (format out "~&        (apply #'jnew (jconstructor \"~a\" ~{\"~a\"~^ ~}) pargs))~%" cname args)
       )
     ;; for strict compatibility
     ;;      (format out "    ))~%")
     (format out "    )~%  )~%")
     
     ;; def-java-method, able to overload
     ;; another limitation of jlinker.  When calling a Java
     ;; method from Lisp, we test the types of the arguments agains the
     ;; signature but do not do any casting or inferencing.  The match must be
     ;; exact. 
     (format out "~%~%;; def-java-method, able to overload.~%~%")
     (let ((h-method (make-hash-table :test #'equal))
	   (h-static (make-hash-table :test #'equal)))
       (dotimes (j (jarrayp methods 0))
	 (setq method (jarray-ref methods j))
	 (setq mod (jcall "getModifiers" method))
	 (when (jstatic "isPublic" "java.lang.reflect.Modifier" mod)
	   (setf static (jstatic "isStatic" "java.lang.reflect.Modifier"
				 mod))
	   (setq jargs (jcall "getParameterTypes" method))
	   (setq args nil) 
	   (dotimes (i (jarrayp jargs 0))
	     (push (jcall "getName" (jarray-ref jargs i)) args))
	   (setq args (nreverse args))
	   (setq jname (jcall "getName" method))
	   (setf (gethash (concatenate 'string 
				       jname " "
				       (format nil "~{\"~a\"~^ ~}" args)
				       ) 
			  h-static 
			  nil) 
		 static)
	   (setf (gethash jname h-method nil)
		 (cons (format nil "~{\"~a\"~^ ~}" args)
		       (gethash jname h-method nil)))
	   ))

       (with-hash-table-iterator
	(get-method h-method)
	(labels 
	    ((hfun-iter (got-one &optional jname val)
			(when got-one
			  (setq lname (new-sym-string jname))
			  (format out "~&~%(defmethod ~a " lname)
			  ;; (format out "((instance ~a) &rest pargs)~%" lcname)
			  (format out "(&rest pargs)~%")
			  
			  ;; strict compat check
			  ;; (format out "~&  (let ((args (mapcar #'(lambda (x) (if (stringp x) \"java.lang.String\" (jcall \"getName\" (jcall \"getClass\" x)))) pargs) ))~%")
			  (format out "~&  (cond~%")
			  (dotimes (i (length val))
			    (setq args (elt val i))
			    ;; strict compat check
			    ;; (format out "~&    (if (jparam-arg-list-compat '(~a) args)~%" args)
			    (cond 
			     ((gethash (concatenate 'string jname " " args) 
				       h-static nil) ;; added
			      (format out "~&    ((jparam-arg-list-compat '(~a) pargs)~%" args)
			      (format out "~&        (apply #'jstatic (jmethod \"~a\" \"~a\" ~a) \"~a\" pargs))~%" cname jname args cname)
			      )
			     
			     (t 
			      (format out "~&    ((jparam-arg-list-compat '(~a) pargs)~%" (concatenate 'string "\"" cname "\" " args))
			      (format out "~&        (apply #'jcall (jmethod \"~a\" \"~a\" ~a) (first pargs) (rest pargs)))~%" cname jname args) 
			      ) ;; added
			     )


			    )
			  (format out "    )~%  )~%")
			  ;; strict compat check
			  ;; (format out "    ))~%")
			  (push lname export-syms)
			  (multiple-value-call #'hfun-iter (get-method)))))
	  (multiple-value-call #'hfun-iter (get-method))))
       )
     (format out "~&~%;; export fields and methods names~%")
     ;; simplified by psz, I think; also added eval-when:
;;;      (print `(eval-when (:compile-toplevel :load-toplevel :execute)
;;;		(export ',export-syms))
;;;	     out)
     ;; (format out "~&(export '(~{~a~^~%~}))~%" export-syms)
     (format out "~&(eval-when (:compile-toplevel :load-toplevel :execute)~%")
     (format out "~&  (export '(~{~a~^~%~}) :~a)~%" export-syms cname)
     (format out ")~%")
     )))




;; create dir with dirname, use recursion if necessary
;;;(defun create-dir (dir)
;;;  (let ((udir (replace-re dir "^(.*)(/|\\\\).+" "\\1")))
;;;    (unless (probe-file udir)
;;;      (create-dir udir))
;;;    (excl.osi:mkdir dir)))

;; for the funcall model
(defun define-java-methods-funcall (class outfile)
  (jlinker-init :classpath nil) ; make sure jlinker is running
  (with-open-file
   (out outfile :direction :output :if-exists :supersede)
   (let* ((cref (jclass class))
	  (methods (jcall "getDeclaredMethods" cref))
	  method mod jname lname static jargs args done
	  )
     (dotimes (i (jarrayp methods 0))
       (setf method (jarray-ref methods i))
       (setf mod (jcall "getModifiers" method))
       (when (jstatic "isPublic" "java.lang.reflect.Modifier" mod)
	 (setf static (jstatic "isStatic" "java.lang.reflect.Modifier"
			       mod))
	 (setf args 
	       (subseq '(a b c d e f g h i j k)  ; this will work up to 11 args
		       0
		       (jarrayp (setf jargs (jcall
					     "getParameterTypes" method)) 0)))
	 (setf jname (jcall "getName" method))
	 ;; TODO: suffix with class names and parameter types
	 ;; suffix the number of arguments to the Lisp name to avoid
	 ;;  problems with overloaded methods
	 (setf lname (intern (format nil "~A-~A" jname (length args))))
	 (format out "~&~%; ~A(~{~A ~})~%"
		 jname
		 (let (types)
		   (dotimes (j (length args))
		     (push (jcall "getName"
				  (jarray-ref
				   jargs j))
			   types))
		   (nreverse types)))
	 (when (not (member lname done))
	   ;; if the name is already in the list, we generated a
	   ;; Lisp call with this arity already.  The same Lisp call
	   ;; will work with any pattern of types.
	   (push lname done)
	   (pprint
	    `(defun 
		 ,lname
		 ,(if static 
		      args
		    (cons
		     'instance args))
	       (,@(if static
		      `(jstatic ,jname ,class)
		    `(jcall ,jname instance))
		,@args))
	    out)))))))

;;; add if-exists option, delete unless enclosing
;;; if-exists is defaulted to :supersede, because I want to ensure correctness
;;; nevertheless, one can use nil to avoid duplicate wrappre generation
;;; note compatibility for updated wrappers.
(defmethod write-java-wrapper ((java-class string)
			       (outfile string)
			       &optional
			       (if-exists :supersede))
;;;  (format t "~&write-java-wrapper ~a, ~s" outfile java-class)
;;;  (unless (probe-file outfile)
  (multiple-value-bind (match? dir) 
      (match-re ".*(/|\\\\)" (namestring (translate-logical-pathname outfile)))
;;;      (format t "~&file to probe ~a" dir)
    (declare (ignore match?))
    (unless (probe-file dir)
      (create-dir dir))
    (if (or (not (probe-file outfile))
	    (eq if-exists :supersede))
	(define-java-methods-class java-class
	  outfile nil if-exists)))
;;;    )
  )
