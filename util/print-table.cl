;;; -*- Mode: Lisp; Package: util; -*-

#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  - 08/01/2009 creation


A facility to print a table of values, somewhat like an html <table>
element or a LaTeX tabular specification. Each row is given as a list
of elements, where each should be of the same length. A list of format
specifiers should be given, one for each column. These can be one of
the LaTeX specifiers, :l, :c, or :r, representing left, centered, or
right alignment, or (:t k), which truncates to k characters, or (:m k),
which elides the middle of the string to k characters. Each element is
converted to a string by (format nil spec element), where spec
defaults to "~a". The LaTeX-style directives may be combined with
these specs by giving a format specifier as, say, (:c "~d").

|#

(defpackage :util
  (:export "print-table"))
(in-package :util)

(defun print-table (list-of-lists &key (specs nil) (stream t))
  "Prints a table specified by a list of lists. Actually, a sequence
  of sequences is acceptable."
  (and list-of-lists
       (let* ((the-specs-s
	       (map 'list
		 #'(lambda (s)
		     (cond ((stringp s) (cons nil s))
			   ((member s '(:l :c :r)) (cons s "~a"))
			   ((and (consp s) (member (car s) '(:t :m)))
			    (cons s "~a"))
			   ((consp s) (cons (car s) (cadr s)))
			   (t (error "Unknown table formatting directive: ~s"
				     s))))
		 specs))
	      (short-by (- (length (elt list-of-lists 0))
			   (length the-specs-s)))
	      (the-specs
	       (cond ((plusp short-by)
		      (setq the-specs-s (nreverse the-specs-s))
		      (dotimes (i short-by)
			(push '(nil . "~a") the-specs-s))
		      (nreverse the-specs-s))
		     (t the-specs-s)))
	      (lines nil))
	 (map nil
	   #'(lambda (row)
	       (push
		(map 'list
		  #'(lambda (cell spec)
		      (let ((str
			     (format
			      nil
			      (or (and spec (cdr spec)) "~a")
			      cell)))
			(cond ((null spec) str)
			      ((and (consp (car spec))
				    (eq (caar spec) ':t)
				    (format
				     nil "~a..."
				     (subseq str 0
					     (- (cadar spec) 3)))))
			      ((and (consp (car spec))
				    (eq (caar spec) ':m)
				    (text-abbrev str (cadar spec))))
			      (t str))))
		  row
		  the-specs)
		lines))
	   list-of-lists)
	 (let ((widths (apply #'mapcar
			      #'(lambda (&rest cells-in-col)
				  (apply #'max
					 (mapcar #'length cells-in-col)))
			      lines)))
	   (dolist (row (nreverse lines))
	     (terpri stream)
	     (do ((cells row (cdr cells))
		  (specs the-specs (and specs (cdr specs)))
		  (ws widths (cdr ws)))
		 ((null cells))
	       (let ((cell (car cells))
		     (spec (and specs (car specs)))
		     (wid (car ws)))
		 ;;(format t "~%cell: ~s, spec=~s, w=~d" cell spec wid)
		 (cond ((or (null spec) (eq (car spec) ':l))
			(format stream "~v<~a~;~> " wid cell))
		       ((eq (car spec) ':c)
			(format stream "~v:@<~a~> " wid cell))
		       ((eq (car spec) ':r)
			(format stream "~v:<~a~> " wid cell))
		       (t (format stream "~v<~a~;~> " wid cell))))))))))
