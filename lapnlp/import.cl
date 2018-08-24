;;; -*- Mode: Lisp; Package: late -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation

If the user meant that the last element of filespec should
be a directory name but left the trailing "/" off, we
recognize that situation and add it. This is like
get-directory-pathname (defined in config.cl), except that it
handles wildcards.

On psz's laptop, for example, we can import the CHED 100-document
sample data and the full CHED corpus as:

(import-corpus "~/Documents/Projects/NLP/data/ched-100/*.txt"
	       :spec "late:;ched-note.xml")
(import-corpus "~/Documents/Projects/NLP/data/ched/*.txt"
	       :spec "late:;ched-note.xml")
|#

(defpackage :late
  (:use :common-lisp #+allegro :excl :util)
  (:export "import-corpus"
	   "import-groundtruth"))

(in-package :late)

(defun import-corpus (filespec &key (name nil) (spec nil) (mode nil))
  "Imports a set of documents into a new corpus. The documents to
  import are specified by the filespec, which will generally include
  wildcards. Name, if given, becomes the name of the corpus; otherwise,
  the name is computed from the directory name.  Spec, if given, is an
  XML file describing the section and subsection formats of the
  documents, to permit sectionizing via the code in findstruct.cl."  
  (let* ((pn (pathname filespec))
	 (h (pathname-host pn))
	 (d (pathname-directory pn))
	 (n (pathname-name pn))
	 (e (pathname-type pn)))
    (when (and n (not (eq n :wild)) (null e))
      (setq d (append d (list n))
	    n nil
	    pn (make-pathname :directory d :name n :type e)))
    (let* ((cn (or name
		   (and d (find-if-not #'(lambda (x) (eq x :wild-inferiors))
				       d
				       :from-end t))
		   (symbol-name (gensym "corpus-"))))
	   (c (make-instance 'corpus :name cn)))
      (when spec
	(setf (description c)
	      (car (read-xml spec :package :late))))
      (dolist (f (directory pn))
	(let* ((text  (read-file f))
	       (docn (format nil "~a~@[.~a~]" (pathname-name f) mode))
	       (src (format nil "~a:;~{~a;~}~a" h (cdr d) (pathname-name f)))
	       (doc (make-instance 'document
				   :name docn
				   :source src
				   :dirty t
				   :size (length text)
				   :content text)))
	  (save doc)
	  (push (id doc) (documents c))))
      (save c))))

(defun import-groundtruth (fn type label &aux line label2)
  "deprecated"
  (with-open-file
   (f fn :direction :input)
   (loop
    (unless (setf line (read-line f nil nil)) (return))
    (multiple-value-bind (match? whole mrn institution) 
	(match-re "^\\s*(\\d+),(\\w*)\\s*$" line)
      (declare (ignore whole))
      (when match?
	(if (setf label2 
		  (caar (latesql "SELECT label FROM groundtruth 
WHERE mrn = ~a AND institution = ~a AND type = ~d" (sq mrn) (sq institution) (sq type))))
	    (assert (equalp label label2)
		    ()
		    "conflict in mrn: ~a, institution: ~a, label1: ~a, label2: ~a"
		    mrn institution label label2)
	  (latesql "INSERT INTO groundtruth(mrn, label, institution, type)
                      VALUES (~a, ~a, ~a, ~d)"
		   (sq mrn) (sq label) (sq institution) (sq type))))))))

