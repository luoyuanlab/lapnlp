;;; -*- Mode: Lisp; Package: util -*-

#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 02/20/2009 creation
|#

(defpackage :util
  (:use :common-lisp #+allegro :excl)
  (:export "read-lines" 
	   "read-file"
	   "read-ne-line"))

(in-package :util)

(defun read-ne-line (f &aux ln)
  (setf ln (read-line f nil nil))
  (loop (unless (and ln (match-re "^\\s*$" ln)) (return ln))
	(setf ln (read-line f nil nil))))

(defun read-lines (filename &key (external-format :default))
  (let ((line nil)
	(eof (list 'eof))
	(ans nil))
    (with-open-file (f filename
		       :direction :input
		       :external-format external-format)
		    (multiple-value-bind (eol encoding)
			(eol-convention f)
		      (loop
		       (when (eq eof (setq line (read-line f nil eof)))
			 (return))
		       (push line ans))
		      (values (nreverse ans)
			      eol
			      encoding)))))

(defun read-file (filename &key (external-format :default))
  (with-open-file (f filename
		     :direction :input
		     :external-format external-format)
		  (let* ((len (file-length f))
			 (buffer (make-array (list len)
					     :element-type 'character
					     :initial-element #\null))
			 (c)
			 (eof (list 'eof))
			 (i -1))
		    (loop
		     (when (eq eof (setq c (read-char f nil eof)))
		       (return))
		     (setf (schar buffer (incf i)) c))
		    buffer)))
