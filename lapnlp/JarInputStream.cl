;;; This is automatically generated.
;;; Please do not modify this file.
;;; Instead, please modify java-wrap-gen.cl.
(defpackage :java.util.jar.JarInputStream
  (:use :excl :common-lisp :javatools.jlinker :java-wrap-gen))
(in-package :java.util.jar.JarInputStream)
(def-java-class (JarInputStream "java.util.jar.JarInputStream")
    ()
    ()
    ()
    ())


;; def-java-constructor, able to overload.

(defun JarInputStream (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.io.InputStream") pargs)
        (apply #'jnew (jconstructor "java.util.jar.JarInputStream" "java.io.InputStream") pargs))
    ((jparam-arg-list-compat '("java.io.InputStream" "boolean") pargs)
        (apply #'jnew (jconstructor "java.util.jar.JarInputStream" "java.io.InputStream" "boolean") pargs))
    )
  )


;; def-java-method, able to overload.


(defmethod getNextJarEntry (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.jar.JarInputStream" ) pargs)
        (apply #'jcall (jmethod "java.util.jar.JarInputStream" "getNextJarEntry" ) (first pargs) (rest pargs)))
    )
  )

(defmethod getManifest (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.jar.JarInputStream" ) pargs)
        (apply #'jcall (jmethod "java.util.jar.JarInputStream" "getManifest" ) (first pargs) (rest pargs)))
    )
  )

(defmethod dr-read (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.jar.JarInputStream" "[B" "int" "int") pargs)
        (apply #'jcall (jmethod "java.util.jar.JarInputStream" "read" "[B" "int" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod getNextEntry (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.jar.JarInputStream" ) pargs)
        (apply #'jcall (jmethod "java.util.jar.JarInputStream" "getNextEntry" ) (first pargs) (rest pargs)))
    )
  )

;; export fields and methods names
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(getNextEntry
dr-read
getManifest
getNextJarEntry
JarInputStream) :java.util.jar.JarInputStream)
)
