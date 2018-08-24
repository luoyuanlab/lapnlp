;;; -*- Mode: Lisp; Package: User -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 02/03/2013 creation
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :regexp2))

(defpackage :boostexter
  (:use :common-lisp :late :util :excl)
  (:export 
   "assign-class"
   "ngram-gen"
   "write-names"))

(in-package :boostexter)

(defparameter *text-replacements*
  '(("\\s+" " ")
	("," " COMMA ")
	(":" " COLON ")
	("\\." " PERIOD ")
	("%" " PERCENT ")
	(";" " SEMICOLON ")
	("#" " NUMBERSIGN ")
	("/" " SLASH ")
	("\\+" " PLUS ")
	("-" " DASH ")
	("\"" " DQ ")
	("'" " QT ")
	("\\s+" " ")))

(defun reformat-text (text &optional (shorten nil) &aux (part 0))
  "Changes characters that might cause problems for Boostexter into words
that correspond to the character."
  (dolist (r *text-replacements*)
    (setq text (replace-re text (car r) (cadr r))))
  (when shorten
    ;; Use only the first SHORTEN words of the text
    (dotimes (i shorten)
      (setq part (and part (position #\Space text :start (1+ part)))))
    (when part
      (setq text (subseq text 0 part))))
  text)


(defun assign-class (l c h)
  (dolist (mrn l)
	(setf (gethash mrn h) c)))

(defun ngram-gen (mrns hclass fn)
  (let* ((f (open-new fn)))
	(dolist (mrn mrns)
	  (dolist (docid (mrn->docids mrn))
		(let* ((doc (document docid))
			   ngrams sen-str)
		  (dolist (sen (annotations doc :type 'sentence-annotation))
			(setf ngrams (extract-ngram 1 sen))
			(when ngrams
			  (setf sen-str (format nil " ~{~a~^ ~}" ngrams))
			  (setf sen-str (reformat-text sen-str))
			  (format f "~a PERIOD NEWLINE " sen-str)))))
	  (format f ", ~a.~%" (gethash mrn hclass)))
	(close f)))

(defun cg-gen (fnin fnout)
  #||reads in arff, output bt||#
  (let* ((fout (open-new fnout))
		 ln m c ft)
	(with-open-file (fin fnin :direction :input)
	  (loop (unless (setf ln (read-line fin nil nil)) (return))
		 (setf m (match-re "^(?<c>\\d+),\\d+,'(?<ft>.*)'" ln :return :match))
		 (when m
		   (setf c (re-submatch m nil nil "c"))
		   (cond
			 ((equalp "1" c)
			  (setf c "p"))
			 ((equalp "0" c)
			  (setf c "n")))
		   (setf ft (re-submatch m nil nil "ft"))
		   (format fout "~a, ~a.~%" ft c))))
	(close fout)))


(defun write-names (fn)
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (format f "p, n.~%note: text.~%")))

