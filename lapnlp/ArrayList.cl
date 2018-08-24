;;; This is automatically generated.
;;; Please do not modify this file.
;;; Instead, please modify java-wrap-gen.cl.
(defpackage :java.util.ArrayList
  (:use :excl :common-lisp :javatools.jlinker :java-wrap-gen))
(in-package :java.util.ArrayList)
(def-java-class (ArrayList "java.util.ArrayList")
    ()
    ()
    ()
    ())


;; def-java-constructor, able to overload.

(defun ArrayList (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("int") pargs)
        (apply #'jnew (jconstructor "java.util.ArrayList" "int") pargs))
    ((jparam-arg-list-compat '() pargs)
        (apply #'jnew (jconstructor "java.util.ArrayList" ) pargs))
    ((jparam-arg-list-compat '("java.util.Collection") pargs)
        (apply #'jnew (jconstructor "java.util.ArrayList" "java.util.Collection") pargs))
    )
  )


;; def-java-method, able to overload.


(defmethod indexOf (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" "java.lang.Object") pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "indexOf" "java.lang.Object") (first pargs) (rest pargs)))
    )
  )

(defmethod dr-set (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" "int" "java.lang.Object") pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "set" "int" "java.lang.Object") (first pargs) (rest pargs)))
    )
  )

(defmethod ensureCapacity (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" "int") pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "ensureCapacity" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod clone (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" ) pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "clone" ) (first pargs) (rest pargs)))
    )
  )

(defmethod dr-add (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" "int" "java.lang.Object") pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "add" "int" "java.lang.Object") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.util.ArrayList" "java.lang.Object") pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "add" "java.lang.Object") (first pargs) (rest pargs)))
    )
  )

(defmethod lastIndexOf (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" "java.lang.Object") pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "lastIndexOf" "java.lang.Object") (first pargs) (rest pargs)))
    )
  )

(defmethod dr-remove (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" "java.lang.Object") pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "remove" "java.lang.Object") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.util.ArrayList" "int") pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "remove" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod clear (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" ) pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "clear" ) (first pargs) (rest pargs)))
    )
  )

(defmethod toArray (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" "[Ljava.lang.Object;") pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "toArray" "[Ljava.lang.Object;") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.util.ArrayList" ) pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "toArray" ) (first pargs) (rest pargs)))
    )
  )

(defmethod addAll (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" "int" "java.util.Collection") pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "addAll" "int" "java.util.Collection") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.util.ArrayList" "java.util.Collection") pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "addAll" "java.util.Collection") (first pargs) (rest pargs)))
    )
  )

(defmethod trimToSize (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" ) pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "trimToSize" ) (first pargs) (rest pargs)))
    )
  )

(defmethod isEmpty (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" ) pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "isEmpty" ) (first pargs) (rest pargs)))
    )
  )

(defmethod dr-size (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" ) pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "size" ) (first pargs) (rest pargs)))
    )
  )

(defmethod contains (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" "java.lang.Object") pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "contains" "java.lang.Object") (first pargs) (rest pargs)))
    )
  )

(defmethod dr-get (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.ArrayList" "int") pargs)
        (apply #'jcall (jmethod "java.util.ArrayList" "get" "int") (first pargs) (rest pargs)))
    )
  )

;; export fields and methods names
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(dr-get
contains
dr-size
isEmpty
trimToSize
addAll
toArray
clear
dr-remove
lastIndexOf
dr-add
clone
ensureCapacity
dr-set
indexOf
ArrayList) :java.util.ArrayList)
)
