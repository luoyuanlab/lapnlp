;;; This is automatically generated.
;;; Please do not modify this file.
;;; Instead, please modify java-wrap-gen.cl.
(defpackage :java.util.jar.JarEntry
  (:use :excl :common-lisp :javatools.jlinker :java-wrap-gen))
(in-package :java.util.jar.JarEntry)
(def-java-class (JarEntry "java.util.jar.JarEntry")
    ()
    ()
    (((fattr "attr"))
     ((fcerts "certs"))
     ((fsigners "signers")))
    ())


;; def-java-constructor, able to overload.

(defun JarEntry (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String") pargs)
        (apply #'jnew (jconstructor "java.util.jar.JarEntry" "java.lang.String") pargs))
    ((jparam-arg-list-compat '("java.util.zip.ZipEntry") pargs)
        (apply #'jnew (jconstructor "java.util.jar.JarEntry" "java.util.zip.ZipEntry") pargs))
    ((jparam-arg-list-compat '("java.util.jar.JarEntry") pargs)
        (apply #'jnew (jconstructor "java.util.jar.JarEntry" "java.util.jar.JarEntry") pargs))
    )
  )


;; def-java-method, able to overload.


(defmethod getCodeSigners (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.jar.JarEntry" ) pargs)
        (apply #'jcall (jmethod "java.util.jar.JarEntry" "getCodeSigners" ) (first pargs) (rest pargs)))
    )
  )

(defmethod getCertificates (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.jar.JarEntry" ) pargs)
        (apply #'jcall (jmethod "java.util.jar.JarEntry" "getCertificates" ) (first pargs) (rest pargs)))
    )
  )

(defmethod getAttributes (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.jar.JarEntry" ) pargs)
        (apply #'jcall (jmethod "java.util.jar.JarEntry" "getAttributes" ) (first pargs) (rest pargs)))
    )
  )

;; export fields and methods names
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(getAttributes
getCertificates
getCodeSigners
fsigners
fcerts
fattr
JarEntry) :java.util.jar.JarEntry)
)
