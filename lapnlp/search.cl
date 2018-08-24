;;; -*- Mode: Lisp; Package: late; -*-
#||
yluo - 08/16/2018 clean and reorganization
yluo - 06/06/2015 rewrite using asdf framework
yluo - 05/19/2010 creation 
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :regexp2))

(defpackage :late
  (:use :common-lisp #+allegro :excl :util)
  (:export "late-search"))

(defmethod late-search ((c corpus) pattern)
  (let ((re (if (typep pattern 'regexp::regular-expression)
		pattern
	      (compile-re pattern))))
    (dolist (did (documents c))
      (late-search (document did) re))))

(defmethod late-search ((d document) pattern)
  (let ((text (content d))
	(re (if (typep pattern 'regexp::regular-expression)
		pattern
	      (compile-re pattern)))
	(pos 0))
    (loop
      (multiple-value-bind (m? match)
	  (match-re re text :return :index :multiple-lines t :start pos)
	(unless m? (return))
	(format t "~%~d: ~5d ~5d ~s"
		(id d) (car match) (cdr match)
		(subseq text (car match) (cdr match)))
	(setq pos (cdr match))))))

