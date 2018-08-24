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
  (:export "join"
	   "split"
	   "text-abbrev"
	   "find-all-re"
	   "sys-time-str" 
	   "outstr-init"
	   "edit-dist"))

(in-package :util)

(defun join (list-of-strings &optional (separator-string " "))
  (apply #'concatenate
	 'string
	 (cdr (mapcan #'(lambda (x)
			  (list separator-string (string x)))
		      list-of-strings))))

(defun split (string &optional (sep-char #\Space))
  "Splits a string into a list of strings at every occurrence of sep-char,
which must be a single character.  There is no special handling of resulting
null elements if multiple sep-chars follow each other. See split-re for
greater flexibility using regular expressions."
  (do ((ll (length string))
       (p (position sep-char string)
	  (position sep-char string :start (+ p 1)))
       (last-p -1 p)
       (ans nil))
      ((or (null p) (>= p ll))
       (nreverse (cons (subseq string (1+ last-p)) ans)))
    (push (subseq string (1+ last-p) p) ans)))

(defun text-abbrev (text maxlength)
  (let ((mid (floor maxlength 3)))
    ;; replace-re doesn't seem to do the job!
    (and text
	 (replace-re
	  (if (<= (length text) maxlength)
	      text
	    (concatenate 'string
			 (subseq text 0 (- maxlength mid))
			 "..."
			 (subseq text (- (length text) mid))))
	  "\\s"
	  " "))))

(defun sys-time-str ()
  (multiple-value-bind (sec min hr day mon yr)
      (get-decoded-time)
    (format nil "~a-~a-~a ~a:~a:~a" mon day yr hr min sec)))

(defun find-all-re (regexp input
			   &key (return :string)
			   (case-fold nil)
			   (single-line nil)
			   (multiple-lines nil)
			   (ignore-whitespace nil)
			   (start nil)
			   (end nil)
			   (back-end 'regexp:vm))
  "Like match-re, except that we find all non-overlapping instances of
   the regexp pattern in input. We return a list of lists, which
   include the string or index matched by the overall pattern,
   followed by the matched data for all subpatterns."
  (let ((posn (or start 0))
	(limit (or end (length input)))
	(result nil))
    (unless (typep regexp 'regexp::regular-expression)
      (setq regexp (compile-re
		    regexp
		    :return :index
		    :case-fold case-fold
		    :single-line single-line
		    :multiple-lines multiple-lines
		    :ignore-whitespace ignore-whitespace
		    :back-end back-end)))
    (loop
     (let ((m (multiple-value-list
	       (match-re regexp input
			 :return :index
			 :case-fold case-fold
			 :single-line single-line
			 :multiple-lines multiple-lines
			 :ignore-whitespace ignore-whitespace
			 :start posn
			 :end limit
			 :back-end back-end))))
       (cond ((car m)			;matched
	      (push (cdr m) result)
	      (setq posn (1+ (cdadr m)))
	      (when (> posn limit) (return)))
	     (t (return)))))
    (setq result (nreverse result))
    (if (eq return :string)
	(mapcar
	 #'(lambda (match)
	     (mapcar
	      #'(lambda (s-e)
		  (and s-e (subseq input (car s-e) (cdr s-e))))
	      match))
	 result)
      result)))

(defun outstr-init ()
  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))


(defun edit-dist (str1 str2)
  "Calculates the Levenshtein distance between str1 and str2, returns an editing distance (int).
From http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Common_Lisp"
  (let ((n (length str1))
        (m (length str2)))
    ;; Check trivial cases
    (cond ((= 0 n) (return-from edit-dist m))
          ((= 0 m) (return-from edit-dist n)))
    (let ((col (make-array (1+ m) :element-type 'integer))
          (prev-col (make-array (1+ m) :element-type 'integer)))
      ;; We need to store only two columns---the current one that
      ;; is being built and the previous one
      (dotimes (i (1+ m))
        (setf (svref prev-col i) i))
      ;; Loop across all chars of each string
      (dotimes (i n)
        (setf (svref col 0) (1+ i))
        (dotimes (j m)
          (setf (svref col (1+ j))
                (min (1+ (svref col j))
                     (1+ (svref prev-col (1+ j)))
                     (+ (svref prev-col j)
                        (if (char-equal (schar str1 i) (schar str2 j)) 0 1)))))
        (rotatef col prev-col))
      (svref prev-col m))))
