;;; -*- Mode:Lisp; Package: util; -*-

#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 02/20/2009 creation
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :regexp2))

(defpackage :util
  (:use :common-lisp :regexp)
  (:export "read-csv" "write-csv"))

(in-package :util)

(defun list-strip (l)
  "Creates a new list of its argument list, with each element, a string,
trimmed of leading/trailing white space."
  (mapcar #'(lambda (item) (string-trim " 	
" item))
	  l))

(defun read-csv (file &key (fun #'list-strip) (comma ","))
  "Reads a comma-separated file.  Returns a list, one element per line
   in the input file, where each element is a list of the values in the
   input line.  Keyword arguments :comma specify the delimiter (which
   must be a single character), and :fun the function applied to the list
   of separated string values.  If :fun is #'identity or nil, each
   element is the list of strings in its input line.  By default, :fun is
   #'list-strip, which trims white-space off these strings.  Thus, \"3,
   4\" yields (\"3\" \"4\"), not (\"3\" \" 4\")."
  (let ((eof (list 'eof))
	(ans nil)
	(l nil)
	(splitter (compile-re comma)))
    (with-open-file (f file :direction :input)
      (loop
	(when (eq eof (setq l (read-line f nil eof)))
	  (return (nreverse ans)))
	(push (funcall fun (split-re splitter l)) ans)))))

(defun write-csv (content file &key (comma ",") (quote nil))
  "Writes content to file, where content is a list of lists. Each
  element is written on a new line, with its items separated by comma.
  Quote, if true, uses ~s instead of ~a, thus assuring that the Lisp
  reader can read back the elements."
  (let ((format-control (concatenate 'string
			  "~{~" (if quote "s" "a") "~^" comma "~}~%")))
    (with-open-file (f file :direction :output :if-exists :supersede)
      (dolist (element content)
	(format f format-control element)))))
