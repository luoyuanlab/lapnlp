;;; -*- Mode: Lisp; Package: User; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 07/11/2009 modify open-lp to handle cygwin dlls.
psz  -            creation
|#



(defpackage :link
  (:use :common-lisp :late :foreign #+allegro :excl)
  (:export 
   "*link-home*"
   "*link-work*"
   "*link-library*"
   "*dict-n*"
   "*know-n*"
   "*post-n*"
   "*affix-n*"
   "*maxword*"
   "*dict*"
   "*h-dict*"
   "*cygwin-dll*"
   "*link-dll*"
   "*link-dict-list*"
   "open-lp"
   "populate-link-dict-hash"
   ))

(in-package :link)

(defparameter *link-home* nil
  "pointing to the original link grammar dictionaries")
(defparameter *link-work* nil
  "point to the directory containing different configurations of link grammar
dictionaries, if *link-work* is present, *link-home* is overwritten.")

(defparameter *link-library* nil)

(defparameter *dict-n* nil)
(defparameter *know-n* nil)
(defparameter *post-n* nil)
(defparameter *affix-n* nil)
(defparameter *maxword* 200)
(defparameter *dict* nil)

(defparameter *cygwin-dll* nil)
(defparameter *link-dll* nil)

(defparameter *h-dict* (make-hash-table :test #'equalp)
  "key is word, value is a list of dictionary files that the word occurs in.")
(defparameter *link-dict-list* 
  (list "currency" "units.1.dot" "words.v.1.1"
	"currency.p" "units.3" "words.v.1.2"
	"entities.given-bisex.sing" "units.4" "words.v.1.3"
	"entities.given-female.sing" "units.4.dot" "words.v.1.4"
	"entities.given-male.sing" "words.adj.1" "words.v.1.p"
	"entities.locations.sing" "words.adj.2" "words.v.2.1"
	"entities.national.sing" "words.adj.3" "words.v.2.2"
	"entities.organizations.sing" "words.adv.1" "words.v.2.3"
	"entities.us-states.sing" "words.adv.2" "words.v.2.4"
	"words.adv.3" "words.v.2.5"
	"words.adv.4" "words.v.4.1"
	"words-medical.adv.1" "words.v.4.2"
	"words-medical.prep.1" "words.v.4.3"
	"words-medical.v.4.1" "words.v.4.4"
	"words-medical.v.4.2" "words.v.4.5"
	"words-medical.v.4.3" "words.v.5.1"
	"words-medical.v.4.4" "words.v.5.2"
	"words-medical.v.4.5" "words.v.5.3"
	"words.n.1" "words.v.5.4"
	"words.n.1.wiki" "words.v.6.1"
	"words.n.2.s" "words.v.6.2"
	"words.n.2.s.biolg" "words.v.6.3"
	"words.n.2.s.wiki" "words.v.6.4"
	"words.n.2.x" "words.v.6.5"
	"words.n.2.x.wiki" "words.v.8.1"
	"words.n.3" "words.v.8.2"
	"words.n.4" "words.v.8.3"
	"words.n.t" "words.v.8.4"
	"words.v.10.1" "words.v.8.5"
	"words.v.10.2" "words.y"
	"words.v.10.3"
	"units.1" "words.v.10.4"))

(defmethod populate-link-dict-hash ((fns-words list))
  "Read in dictionareis including 4.0.dict (may want to customize later) into 
the hash *h-dict*. Spurious entries like them_all or <marker-entity> might be 
generated, downstream program should take this into account.
Because the hash table of *h-dict* is only meant to grow incrementally and we 
use pushnew to maintain the list of dictionary files, no clrhash is necessary
upon reloading link parser.
Input
======
fns-words: a list of dictionary names under words/ dir to be read in"
  (let* (line
	 (in-enum t))
    (with-open-file
     f-words (merge-pathnames 
	      (make-pathname :directory '(:relative "en") 
			     :name "4.0.dict")
	      (or *link-work* *link-home*))
     :direction :input)
    (loop
     (unless (setq line (read-line f-words nil nil)) (return))
     (setf line (replace-re line "(^|\\b)%.*$" "")) ; skip comment
     (when in-enum
       (dolist (word (remove "" (split-re " +" (replace-re line ":.*$" "")) 
			     :test #'equalp))
	 (multiple-value-bind (has-pos word-pos lp-pos) 
	     ;; a.m., p.m.
	     (match-re "^.*\\.([^.]+)$" word)
	   (declare (ignore has-pos word-pos))
	   (setf word (replace-re word "\\.[^.]+$" ""))
	   (pushnew (if lp-pos (format nil "4.0.dict.~a" lp-pos) "4.0.dict")
		    (gethash word *h-dict*) :test #'string=))))
     ;; keep the order of matching : then ;, they could be in one line
     (when (match-re "[^\"]:" line)	; exclude ":":
       (setf in-enum nil))
     (when (match-re "[^\"];" line) ; exclude ";":
       (setf in-enum t)))))
(mapcar #'populate-link-dict-hash fns-words))

(defmethod populate-link-dict-hash ((fn-words string))
  (let* (line)
    (with-open-file
     (f-words (merge-pathnames
	       (make-pathname :directory '(:relative "en" "words")
			      :name fn-words)
	       (or *link-work* *link-home*))
	      :direction :input :if-does-not-exist :create)
     (loop
      (unless (setf line (read-line f-words nil nil)) (return))
      (dolist (word (remove "" (split-re " +" line) :test #'equalp))
	(setf word (replace-re word "\\.[^.]*$" ""))
	(pushnew fn-words (gethash word *h-dict*) :test #'string=))))))

;; switched to files in /usr/local/share/link-grammar/en

(defun open-lp (&aux link-dir)
  (setf *link-home* (late:get-env-pathname "LINK"))
  (assert *link-home* (*link-home*) "Link home is undefined!")
  (setf *link-library* (late:get-env-pathname "LINK_GRAMMAR_LIBRARY"))
  (assert *link-library* (*link-library*) "Link library not found!")
  (setf link-dir (or *link-work* *link-home*))
  (format t "~&*link-home* is ~a~%" *link-home*) 
  
  (setf *dict-n* (probe-file (late:make-path "en/4.0.dict" link-dir))
	*know-n* (probe-file (late:make-path "en/4.0.knowledge" link-dir))
	*post-n* (probe-file (late:make-path "en/4.0.constituent-knowledge" 
					     link-dir))
	*affix-n* (probe-file (late:make-path "en/4.0.affix" link-dir)))
  
  
  (assert (and *dict-n* *know-n* *post-n* *affix-n*)
	  (*dict-n* *know-n* *post-n* *affix-n*)
	  "All of the following must be defined:~%~%*dict-n*=~s~%*know-n*=~s~%*post-n*=~s~%*affix-n*=~s"
	  *dict-n* *know-n* *post-n* *affix-n*)
  (unless *cygwin-dll*
    (let ((cygwin-path (late:get-env-pathname "cygwin-dll-dir")))
      (when cygwin-path
	(setq *cygwin-dll*
	      (load "cygwin-dll-dir:cygwin1.dll")))))
  
  (unless *link-dll*
    (format t "~%;;; Probing ~a~%" 
	    (probe-file (late:get-env-pathname "LINK_GRAMMAR_LIBRARY")))
    (load (late:get-env-pathname "LINK_GRAMMAR_LIBRARY"))
    (setq *link-dll* t))
  (format t "~%;;; creating dictionary for link parser")
  (format t "~%;;; *dict-n* is ~a, ~%;;; *know-n* is ~a, ~
             ~%;;; *post-n* is ~a, ~%;;; *affix-n* is ~a~%" 
	  (namestring *dict-n*) (namestring *know-n*) 
	  (namestring *post-n*) (namestring *affix-n*))
  (populate-link-dict-hash *link-dict-list*)
  (when *link-work* 
    (dictionary_set_data_dir (namestring (probe-file *link-work*))))
  ;; (setf *dict* (dictionary_create_lang "en"))
  (setf *dict* (dict-create (namestring *dict-n*)
			    (namestring *know-n*)
			    (namestring *post-n*)
			    (namestring *affix-n*)))
  

  (format t ";;; open-lp completed."))


(defun load-link-lib ()

  (load "dll-dir:cyglink-grammar-4.dll")

  ;;(load "/usr/lib/liblink-grammar.so")
  )



