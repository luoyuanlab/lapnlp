;;; -*- Mode: LISP; Package: norm -*-
#| 
yluo - 08/16/2018 clean and reorganization
yluo - 06/10/2015 rewrite using asdf framework
yluo - 09/01/2010 creation 

yluo - changed lvg jar name, but still hard coded 
|#


(defpackage :norm
  (:use :jc :common-lisp :opennlp :late :util 
	#+allegro :excl :javatools.jlinker)
  (:export 
   "*lvg-home*"
   "*lvg-jar*"
   "*lvg-params*"
   "*norm-cache*"
   "*the-normalizer*"
   "*copular*"
   "cleanup"
   "new-normalizer"
   "norm"
   "norm-end"
   "norm-init"
   "normalizer"
   "norml"
   "safe-norm"
   "medg-norm"
   ))
(in-package :norm)


(defparameter *lvg-home*
  (get-directory-pathname (get-env "LVG_DIR"))
  "The local system's home directory for the UMLS LVG distribution.")

(defparameter *lvg-params*
  (namestring (make-path 
	       (make-pathname :directory '(:relative "data" "config")
			      :name "lvg"
			      :type "properties")
	       *lvg-home*)
	      )
  "The LVG properties file is at a known offset from LVG_HOME.")

;;; merge-pathnames will have the following effect
;;;cl-user(18): (merge-pathnames "a/b/c" "/d/e/f")
;;;#P"/d/e/a/b/c"
;;;cl-user(19): (merge-pathnames "a/b/c" "/d/e/f/")
;;;#P"/d/e/f/a/b/c"
(defparameter *lvg-jar* nil)
(setq *lvg-jar*
      (multiple-value-bind (matched? all ver)
	  (match-re "lvg(\\d+)"
		    (car (last (pathname-directory *lvg-home*)))
		    :case-fold t)
	(declare (ignore all))
	(and matched?
	     (probe-file
	      (merge-pathnames
	       (make-pathname :directory '(:relative "lib")
			      :name (concatenate 'string "lvg" ver "dist")
			      :type "jar")
	       *lvg-home*)))))

(format t "~%*lvg-home* = ~s~%*lvg-params* = ~s~%*lvg-jar* = ~s"
	*lvg-home* *lvg-params* *lvg-jar*)

(push *lvg-home* *classpath-dirs*)
(push *lvg-jar* *classpath-jars*)
(push '(norm-end) *jl-end-callbacks*)

(def-java-class (normalizer "gov.nih.nlm.nls.lvg.Api.NormApi") ()
  ()
  ()
  ())

(def-java-constructor new-normalizer 
  (normalizer "java.lang.String"))

(def-java-method (norml "Mutate")
  (normalizer "java.lang.String"))

(def-java-method (cleanup "CleanUp")
  (normalizer))

(defparameter *the-normalizer* nil)

(defun norm-init ()
  (unless (jlinker-query) 
    (jl-init)
    (when *the-normalizer*
      (cleanup *the-normalizer*)
      (setq *the-normalizer* nil)))

  (unless *the-normalizer*
    (setq *the-normalizer* (new-normalizer *lvg-params*))))

(defparameter *norm-cache*
  (make-hash-table :test #'equal :weak-keys t)
  "Keep results of normalization in a weak hash-table.")

;; this is what you will get from norm command of lvg, not lvg command
;; speed up looking up by building local cache
(defmemo norm (string)
  "Returns a list of normalized results for the phrase in string,
using the UMLS lexical tools' norm interface.  This (roughly) throws
out stop words, finds lexical roots, lowercases and alphabetizes all
the words in string. The resulting list may have multiple values
because there may be multiple lexical roots of words in string. For
example, norm of 'well being' yields both 'be well' and 'being well'."
  (norm-init)
  (setf string (replace-re string "\\s+" " "))
  ;; (setf string (replace-re string "-(\\d+)[+\\-/]*$" "\\1"))
  (let* ((jv (norml *the-normalizer* string))
	 (n (vsize jv))
	 (ans nil))
    (when string
      (dotimes (i n)
	(push (elt-at jv i) ans)))
    (nreverse ans)))

(defparameter *copular* '("am" "is" "are" "was" "were"))

(defun medg-norm (str &aux suff)
  "added customized handling."
  (when (< 1 (length (split-re "-" str)))
    (setf suff (first (nreverse (split-re "-" str))))
    (setf suff (replace-re suff "[+\\-/]+$" "")))
  (cond
   ;; pure digits after -
   ((match-re "-(\\d+)[+\\-/]*$" str)
    (replace-re str "-(\\d+)[+\\-/]*$" "\\1"))
   ;; alphanumeric or short after -
   ((and suff (or (match-re "[0-9]" suff) 
		  (and (match-re "\\w" suff) (< (length suff) 3))))
    (replace-re str "[+\\-/]+$" ""))
   ((member str *copular* :test #'equalp)
    "be")
   ((equalp "%" str)
    "percent")
   ((equalp "+" str)
    "positive")
   ((equalp "+/-" str)
    "positivity_negativity")
   ((equalp "-/+" str)
    "negativity_positivity")
   (t
    (car (norm str)))))

(defun safe-norm (str)
  "No matter which case, I should be able to give you one normalized string of
the input str."
  (or (medg-norm str) (string-downcase str)))



(defun norm-end ()
  (format t "~%Ending Normalizer")
  (when *norm-cache*
    (format t "~%... deleting cache")
    (clrhash *norm-cache*))
  (when (jlinker-query)
    (when *the-normalizer*
      (format t "~%... cleaning up the Java implementation")
      (cleanup *the-normalizer*)
      (setq *the-normalizer* nil))))

