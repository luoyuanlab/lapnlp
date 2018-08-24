;;; This is automatically generated.
;;; Please do not modify this file.
;;; Instead, please modify java-wrap-gen.cl.
(defpackage :java.io.FileInputStream
  (:use :excl :common-lisp :javatools.jlinker :java-wrap-gen))
(in-package :java.io.FileInputStream)
(def-java-class (FileInputStream "java.io.FileInputStream")
    ()
    ()
    ()
    ())


;; def-java-constructor, able to overload.

(defun FileInputStream (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String") pargs)
        (apply #'jnew (jconstructor "java.io.FileInputStream" "java.lang.String") pargs))
    ((jparam-arg-list-compat '("java.io.File") pargs)
        (apply #'jnew (jconstructor "java.io.FileInputStream" "java.io.File") pargs))
    ((jparam-arg-list-compat '("java.io.FileDescriptor") pargs)
        (apply #'jnew (jconstructor "java.io.FileInputStream" "java.io.FileDescriptor") pargs))
    )
  )


;; def-java-method, able to overload.


(defmethod available (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.io.FileInputStream" ) pargs)
        (apply #'jcall (jmethod "java.io.FileInputStream" "available" ) (first pargs) (rest pargs)))
    )
  )

(defmethod getChannel (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.io.FileInputStream" ) pargs)
        (apply #'jcall (jmethod "java.io.FileInputStream" "getChannel" ) (first pargs) (rest pargs)))
    )
  )

(defmethod dr-skip (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.io.FileInputStream" "long") pargs)
        (apply #'jcall (jmethod "java.io.FileInputStream" "skip" "long") (first pargs) (rest pargs)))
    )
  )

(defmethod getFD (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.io.FileInputStream" ) pargs)
        (apply #'jcall (jmethod "java.io.FileInputStream" "getFD" ) (first pargs) (rest pargs)))
    )
  )

(defmethod dr-read (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.io.FileInputStream" "[B" "int" "int") pargs)
        (apply #'jcall (jmethod "java.io.FileInputStream" "read" "[B" "int" "int") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.io.FileInputStream" "[B") pargs)
        (apply #'jcall (jmethod "java.io.FileInputStream" "read" "[B") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.io.FileInputStream" ) pargs)
        (apply #'jcall (jmethod "java.io.FileInputStream" "read" ) (first pargs) (rest pargs)))
    )
  )

(defmethod dr-close (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.io.FileInputStream" ) pargs)
        (apply #'jcall (jmethod "java.io.FileInputStream" "close" ) (first pargs) (rest pargs)))
    )
  )

;; export fields and methods names
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(dr-close
dr-read
getFD
dr-skip
getChannel
available
FileInputStream) :java.io.FileInputStream)
)
