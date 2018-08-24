;;; This is automatically generated.
;;; Please do not modify this file.
;;; Instead, please modify java-wrap-gen.cl.
(defpackage :java.lang.String
  (:use :excl :common-lisp :javatools.jlinker :java-wrap-gen))
(in-package :java.lang.String)
(def-java-class (String "java.lang.String")
    ()
    (((fCASE-INSENSITIVE-ORDER "CASE_INSENSITIVE_ORDER")))
    ()
    ())


;; def-java-constructor, able to overload.

(defun String (&rest pargs)
  (cond
    ((jparam-arg-list-compat '() pargs)
        (apply #'jnew (jconstructor "java.lang.String" ) pargs))
    ((jparam-arg-list-compat '("java.lang.String") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "java.lang.String") pargs))
    ((jparam-arg-list-compat '("[C") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "[C") pargs))
    ((jparam-arg-list-compat '("[C" "int" "int") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "[C" "int" "int") pargs))
    ((jparam-arg-list-compat '("[I" "int" "int") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "[I" "int" "int") pargs))
    ((jparam-arg-list-compat '("[B" "int" "int" "int") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "[B" "int" "int" "int") pargs))
    ((jparam-arg-list-compat '("[B" "int") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "[B" "int") pargs))
    ((jparam-arg-list-compat '("[B" "int" "int" "java.lang.String") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "[B" "int" "int" "java.lang.String") pargs))
    ((jparam-arg-list-compat '("[B" "int" "int" "java.nio.charset.Charset") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "[B" "int" "int" "java.nio.charset.Charset") pargs))
    ((jparam-arg-list-compat '("[B" "java.lang.String") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "[B" "java.lang.String") pargs))
    ((jparam-arg-list-compat '("[B" "java.nio.charset.Charset") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "[B" "java.nio.charset.Charset") pargs))
    ((jparam-arg-list-compat '("[B" "int" "int") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "[B" "int" "int") pargs))
    ((jparam-arg-list-compat '("[B") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "[B") pargs))
    ((jparam-arg-list-compat '("java.lang.StringBuffer") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "java.lang.StringBuffer") pargs))
    ((jparam-arg-list-compat '("java.lang.StringBuilder") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "java.lang.StringBuilder") pargs))
    ((jparam-arg-list-compat '("int" "int" "[C") pargs)
        (apply #'jnew (jconstructor "java.lang.String" "int" "int" "[C") pargs))
    )
  )


;; def-java-method, able to overload.


(defmethod dr-concat (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String") pargs)
        (apply #'jcall (jmethod "java.lang.String" "concat" "java.lang.String") (first pargs) (rest pargs)))
    )
  )

(defmethod indexOf (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "indexOf" "java.lang.String" "int") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String") pargs)
        (apply #'jcall (jmethod "java.lang.String" "indexOf" "java.lang.String") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "int" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "indexOf" "int" "int") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "indexOf" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod getBytes (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" ) pargs)
        (apply #'jcall (jmethod "java.lang.String" "getBytes" ) (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "java.nio.charset.Charset") pargs)
        (apply #'jcall (jmethod "java.lang.String" "getBytes" "java.nio.charset.Charset") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String") pargs)
        (apply #'jcall (jmethod "java.lang.String" "getBytes" "java.lang.String") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "int" "int" "[B" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "getBytes" "int" "int" "[B" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod subSequence (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "int" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "subSequence" "int" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod codePointAt (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "codePointAt" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod dr-replace (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.CharSequence" "java.lang.CharSequence") pargs)
        (apply #'jcall (jmethod "java.lang.String" "replace" "java.lang.CharSequence" "java.lang.CharSequence") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "char" "char") pargs)
        (apply #'jcall (jmethod "java.lang.String" "replace" "char" "char") (first pargs) (rest pargs)))
    )
  )

(defmethod trim (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" ) pargs)
        (apply #'jcall (jmethod "java.lang.String" "trim" ) (first pargs) (rest pargs)))
    )
  )

(defmethod dr-length (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" ) pargs)
        (apply #'jcall (jmethod "java.lang.String" "length" ) (first pargs) (rest pargs)))
    )
  )

(defmethod getChars (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "int" "int" "[C" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "getChars" "int" "int" "[C" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod compareToIgnoreCase (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String") pargs)
        (apply #'jcall (jmethod "java.lang.String" "compareToIgnoreCase" "java.lang.String") (first pargs) (rest pargs)))
    )
  )

(defmethod charAt (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "charAt" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod lastIndexOf (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "lastIndexOf" "java.lang.String" "int") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String") pargs)
        (apply #'jcall (jmethod "java.lang.String" "lastIndexOf" "java.lang.String") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "int" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "lastIndexOf" "int" "int") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "lastIndexOf" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod toCharArray (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" ) pargs)
        (apply #'jcall (jmethod "java.lang.String" "toCharArray" ) (first pargs) (rest pargs)))
    )
  )

(defmethod split (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String") pargs)
        (apply #'jcall (jmethod "java.lang.String" "split" "java.lang.String") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "split" "java.lang.String" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod copyValueOf (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("[C") pargs)
        (apply #'jstatic (jmethod "java.lang.String" "copyValueOf" "[C") "java.lang.String" pargs))
    ((jparam-arg-list-compat '("[C" "int" "int") pargs)
        (apply #'jstatic (jmethod "java.lang.String" "copyValueOf" "[C" "int" "int") "java.lang.String" pargs))
    )
  )

(defmethod dr-format (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.util.Locale" "java.lang.String" "[Ljava.lang.Object;") pargs)
        (apply #'jstatic (jmethod "java.lang.String" "format" "java.util.Locale" "java.lang.String" "[Ljava.lang.Object;") "java.lang.String" pargs))
    ((jparam-arg-list-compat '("java.lang.String" "[Ljava.lang.Object;") pargs)
        (apply #'jstatic (jmethod "java.lang.String" "format" "java.lang.String" "[Ljava.lang.Object;") "java.lang.String" pargs))
    )
  )

(defmethod valueOf (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("double") pargs)
        (apply #'jstatic (jmethod "java.lang.String" "valueOf" "double") "java.lang.String" pargs))
    ((jparam-arg-list-compat '("float") pargs)
        (apply #'jstatic (jmethod "java.lang.String" "valueOf" "float") "java.lang.String" pargs))
    ((jparam-arg-list-compat '("long") pargs)
        (apply #'jstatic (jmethod "java.lang.String" "valueOf" "long") "java.lang.String" pargs))
    ((jparam-arg-list-compat '("int") pargs)
        (apply #'jstatic (jmethod "java.lang.String" "valueOf" "int") "java.lang.String" pargs))
    ((jparam-arg-list-compat '("char") pargs)
        (apply #'jstatic (jmethod "java.lang.String" "valueOf" "char") "java.lang.String" pargs))
    ((jparam-arg-list-compat '("boolean") pargs)
        (apply #'jstatic (jmethod "java.lang.String" "valueOf" "boolean") "java.lang.String" pargs))
    ((jparam-arg-list-compat '("[C" "int" "int") pargs)
        (apply #'jstatic (jmethod "java.lang.String" "valueOf" "[C" "int" "int") "java.lang.String" pargs))
    ((jparam-arg-list-compat '("[C") pargs)
        (apply #'jstatic (jmethod "java.lang.String" "valueOf" "[C") "java.lang.String" pargs))
    ((jparam-arg-list-compat '("java.lang.Object") pargs)
        (apply #'jstatic (jmethod "java.lang.String" "valueOf" "java.lang.Object") "java.lang.String" pargs))
    )
  )

(defmethod toLowerCase (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" ) pargs)
        (apply #'jcall (jmethod "java.lang.String" "toLowerCase" ) (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "java.util.Locale") pargs)
        (apply #'jcall (jmethod "java.lang.String" "toLowerCase" "java.util.Locale") (first pargs) (rest pargs)))
    )
  )

(defmethod offsetByCodePoints (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "int" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "offsetByCodePoints" "int" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod toString (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" ) pargs)
        (apply #'jcall (jmethod "java.lang.String" "toString" ) (first pargs) (rest pargs)))
    )
  )

(defmethod replaceFirst (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String" "java.lang.String") pargs)
        (apply #'jcall (jmethod "java.lang.String" "replaceFirst" "java.lang.String" "java.lang.String") (first pargs) (rest pargs)))
    )
  )

(defmethod startsWith (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String") pargs)
        (apply #'jcall (jmethod "java.lang.String" "startsWith" "java.lang.String") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "startsWith" "java.lang.String" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod toUpperCase (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" ) pargs)
        (apply #'jcall (jmethod "java.lang.String" "toUpperCase" ) (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "java.util.Locale") pargs)
        (apply #'jcall (jmethod "java.lang.String" "toUpperCase" "java.util.Locale") (first pargs) (rest pargs)))
    )
  )

(defmethod equals (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.Object") pargs)
        (apply #'jcall (jmethod "java.lang.String" "equals" "java.lang.Object") (first pargs) (rest pargs)))
    )
  )

(defmethod codePointCount (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "int" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "codePointCount" "int" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod isEmpty (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" ) pargs)
        (apply #'jcall (jmethod "java.lang.String" "isEmpty" ) (first pargs) (rest pargs)))
    )
  )

(defmethod replaceAll (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String" "java.lang.String") pargs)
        (apply #'jcall (jmethod "java.lang.String" "replaceAll" "java.lang.String" "java.lang.String") (first pargs) (rest pargs)))
    )
  )

(defmethod hashCode (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" ) pargs)
        (apply #'jcall (jmethod "java.lang.String" "hashCode" ) (first pargs) (rest pargs)))
    )
  )

(defmethod equalsIgnoreCase (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String") pargs)
        (apply #'jcall (jmethod "java.lang.String" "equalsIgnoreCase" "java.lang.String") (first pargs) (rest pargs)))
    )
  )

(defmethod matches (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String") pargs)
        (apply #'jcall (jmethod "java.lang.String" "matches" "java.lang.String") (first pargs) (rest pargs)))
    )
  )

(defmethod dr-intern (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" ) pargs)
        (apply #'jcall (jmethod "java.lang.String" "intern" ) (first pargs) (rest pargs)))
    )
  )

(defmethod contentEquals (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.CharSequence") pargs)
        (apply #'jcall (jmethod "java.lang.String" "contentEquals" "java.lang.CharSequence") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.StringBuffer") pargs)
        (apply #'jcall (jmethod "java.lang.String" "contentEquals" "java.lang.StringBuffer") (first pargs) (rest pargs)))
    )
  )

(defmethod endsWith (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String") pargs)
        (apply #'jcall (jmethod "java.lang.String" "endsWith" "java.lang.String") (first pargs) (rest pargs)))
    )
  )

(defmethod codePointBefore (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "codePointBefore" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod contains (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.CharSequence") pargs)
        (apply #'jcall (jmethod "java.lang.String" "contains" "java.lang.CharSequence") (first pargs) (rest pargs)))
    )
  )

(defmethod regionMatches (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "boolean" "int" "java.lang.String" "int" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "regionMatches" "boolean" "int" "java.lang.String" "int" "int") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "int" "java.lang.String" "int" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "regionMatches" "int" "java.lang.String" "int" "int") (first pargs) (rest pargs)))
    )
  )

(defmethod compareTo (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.Object") pargs)
        (apply #'jcall (jmethod "java.lang.String" "compareTo" "java.lang.Object") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "java.lang.String") pargs)
        (apply #'jcall (jmethod "java.lang.String" "compareTo" "java.lang.String") (first pargs) (rest pargs)))
    )
  )

(defmethod substring (&rest pargs)
  (cond
    ((jparam-arg-list-compat '("java.lang.String" "int" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "substring" "int" "int") (first pargs) (rest pargs)))
    ((jparam-arg-list-compat '("java.lang.String" "int") pargs)
        (apply #'jcall (jmethod "java.lang.String" "substring" "int") (first pargs) (rest pargs)))
    )
  )

;; export fields and methods names
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(substring
compareTo
regionMatches
contains
codePointBefore
endsWith
contentEquals
dr-intern
matches
equalsIgnoreCase
hashCode
replaceAll
isEmpty
codePointCount
equals
toUpperCase
startsWith
replaceFirst
toString
offsetByCodePoints
toLowerCase
valueOf
dr-format
copyValueOf
split
toCharArray
lastIndexOf
charAt
compareToIgnoreCase
getChars
dr-length
trim
dr-replace
codePointAt
subSequence
getBytes
indexOf
dr-concat
fCASE-INSENSITIVE-ORDER
String) :java.lang.String)
)
