;;; -*- Mode: Lisp; Package: late -*-
#| 
yluo - 08/16/2018 clean and reorganization
yluo - 06/05/2015 rewrite using asdf framework
yluo - 06/06/2013 creation 
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Programs to manipulate de-identified discharge summaries from
;;;MIMIC-II.
;;;
;;; These de-identified records have markers embedded in them where
;;;PHI used to be in the originals.  Each marker starts with [** and
;;;ends with **]. The core of the markers seem to come in three forms,
;;;empirically determined: 
;;; 1. Sequences of digits and hyphens.  These can be be "fixed"
;;;simply by leaving the core.
;;; 2. A generic marker, such as [**Last Name**]. These are all
;;;independent of each other, and each can be replaced by a pseudonym
;;;generated at random from a gazette.
;;; 3. A marker such as [**Last Name 324**]. These are meant, I think,
;;;to represent the same item in multiple places in the text. We
;;;generate a random pseudonym once, and then use it for each instance.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :mimic
  (:use :common-lisp #+allegro :excl :late :util)
  (:export "*pseudonyms*"
	   "import-mimic-corpus"
	   "*case-splitter*"
	   "split-cases"
	   "*marker-finder*"
	   "*marker-finder-i*"
	   "*n-m*"
	   "*date*"
	   "*4-digit*"
	   "*ending-digits*"
	   "*num-s*"
	   "find-markers"
	   "find-marker-bounds"
	   "find-marker-types"
	   "collect-pseudonyms"
	   "generate-pseudonym"
	   "gen-pseudo"
	   "*days-per-month*"
	   "julian->mo-day"
	   "*month-names*"
	   "*month-short-names*"
	   "generate-month"
	   "ordinal-suffix"
	   "generate-area-code"
	   "generate-phone-prefix"
	   "*gazette-counts*"
	   "init-gazette-counts"
	   "generate-random-gazette"
	   "generate-random-name"
	   "generate-random-first-name"
	   "generate-initials"
	   "generate-random-digits"
	   "generate-date"
	   "generate-address"
	   "generate-signature"
	   "generate-wardname"
	   "all-markers"
	   "fragment-disch-summ"
	   "clean-case"
	   "clean-returns"
	   "*header-pat*"
	   "*hh*"
	   "find-headers"
	   "get-hh"
	   "flush-mimic-annotations"
	   "import-mimic-deid"
	   ))

(in-package :mimic)

(defparameter *pseudonyms*
    (make-hash-table :test #'equal)
  "A hash table that maps entries such as [**Last Name 922**] to a
    specific value, in this case drawn at random from the Census last
    name table.")

#|
   In principle, we should be able to import MIMIC II data from the
   MIMIC Oracle database. However, there is a bug in the ACL
   implementation of CLOB access so that this cannot be done.  In
   addition, we do not have a license to use the Oracle interface on
   every platform of ACL, so even the set-up below can cause
   errors. For now, we only import MIMIC data from text files prepared
   by an export script that accesses Oracle by other means.


|#

(defun import-mimic-corpus (filespec
			    &key
			    (spec nil) (name nil) (unix t))
  "Imports a set of MIMIC discharge summary documents into a new
  corpus. The documents to import are specified by the filespec, which
  will generally include wildcards. Name, if given, becomes the name
  of the corpus; otherwise, the name is computed from the directory
  name.  Spec, if given, is an XML file describing the section and
  subsection formats of the documents, to permit sectionizing via the
  code in findstruct.cl. Unix, if true, gets rid of \r characters at
  the ends of lines, typically found in DOS files.

  Importing these data is more complex than other documents for two
  reasons:
  1. Each file contains multiple discharge summaries, which must be
  separated from each other. The name of each document is taken from
  the START_OF_RECORD identifier.
  2. The de-identification process has inserted data marked by
  brackets such as '[**Last Name 13**] or [**2012-06-13**]', for which
  we substitute actual names, dates and other suitable items." 
  (let* ((pn (pathname filespec))
	 (d (pathname-directory pn))
	 (n (pathname-name pn))
	 (e (pathname-type pn))
	 (dos-return (compile-re "\\r$" :multiple-lines t)))
    (when (and n (not (eq n :wild)) (null e))
      (setq d (append d (list n))
	    n nil
	    pn (make-pathname :directory d :name n :type e)))
    (let* ((cn (or name
		   n
		   (and d (find-if-not #'(lambda (x) (eq x :wild-inferiors))
				       d
				       :from-end t))
		   (symbol-name (gensym "corpus-"))))
	   (c (make-instance 'corpus :name cn)))
      (when spec
	(setf (description c)
	  (car (read-xml spec :package :late))))
      (format t "~%Importing ~s to ~s" pn cn)
      (dolist (f (directory pn))
	;; Preparing the cases is a two-step process, so we can
	;; substitute pseudonyms for the various PHI markers
	;; introduced by de-identification.
	(let* ((text-raw  (read-file f))
	       (text (if unix
			 (replace-re text-raw dos-return "")
		       text-raw))
	       (cases (split-cases text)))
	  (format t "~%Read ~s, with ~d cases; find pseudonyms"
		  f (length cases))
	  (collect-pseudonyms text)
	  (format t "~%Collected ~d pseudonyms"
		  (hash-table-count *pseudonyms*))
	  (dolist (case cases)
	    (format t "~%Storing case ~a" (car case))
	    (multiple-value-bind (pseudo phi-marks)
		(clean-case (cdr case))
	      (let* ((doc (make-instance 'document
			    :name (caar case)
			    :data (list 'subj (cadar case)
					'hadm (caddar case)
					'date (cadddr (car case)))
			    :source (namestring f)
			    :dirty t
			    :size (length pseudo)
			    :content pseudo)))
		(dolist (a phi-marks)
		  (let ((ann (make-instance 'phi-pseudonym
			       :document doc
			       :start (car a)
			       :end (cadr a)
			       :data (caddr a))))
		    (add-annotation doc ann)))
		(save doc)
		(push (id doc) (documents c)))))))
      (save c))))

(defparameter *case-splitter*
    (compile-re ;;"^START_OF_RECORD=(\\S+)\s*"
     ;; I assume format is report_id,subject_id,hadm_id,report_dt
     "^START_OF_RECORD=(\\d+),(\\d+),(\\d+),(\\d+)-(.+)-(\\d+)\\|"
		:multiple-lines t
		:return :index))

(defun split-cases (text)
  "Returns a list of cases split out from text by
  *case-splitter*. Each result is a pair containing the record id of
  the case and the text of the case."
  (let ((start 0)
	;;(last-sep-end 0)
	(len (length text))
	(last-id) (last-subj) (last-adm) (last-dt)
	(parts nil))
    (loop
      (multiple-value-bind (matched? span report-id subject-id hadm-id
			    report-day report-month report-year)
	  (match-re *case-splitter* text :start start)
	(cond ((not matched?)
	       ;; No more occurrences of "START_OF_RECORD"; if there was
	       ;; any text before it, with a name, add that to parts.
	       (when (and last-id (< start len))
		 (push (cons (list last-id last-subj last-adm last-dt)
			     (subseq text start))
		       parts))
	       (return parts))
	      ((= start 0)
	       ;; beginning of the file, nothing before this separator
	       )
	      (t ;; matched separator in middle of file
	       (format t "~%Start of record ~a" last-id)
	       (push (cons (list last-id last-subj last-adm last-dt)
			   (subseq text start (car span)))
		     parts)))
	(let* ((ya (read-from-string text nil nil
				     :start (car report-year)
				     :end (cdr report-year)))
	       (y (cond ((> ya 1900) ya)
			((< ya 30) (+ 2000 ya))
			(t (+ 1900 (mod ya 100)))))
	       (m (1+ (position (subseq text
					(car report-month)
					(cdr report-month))
				'("JAN" "FEB" "MAR" "APR" "MAY" "JUN"
				  "JUL" "AUG" "SEP" "OCT" "NOV" "DEC")
				:test #'equalp)))
	       (d (read-from-string text nil nil
				    :start (car report-day)
				    :end (cdr report-day))))
	  ;;(format t "~%ya=~s, y=~s, m=~s, d=~s" ya y m d)
	  (setq last-id (subseq text (car report-id) (cdr report-id))
		last-subj (subseq text (car subject-id) (cdr subject-id))
		last-adm (subseq text (car hadm-id) (cdr hadm-id))
		last-dt (format nil "~4d-~2,'0d-~2,'0d"
				y m d)
		start (cdr span)))))))

(defparameter *marker-finder*
    (compile-re "\\[\\*\\*(.+?)\\*\\*\\]" :multiple-lines t :return :string))

(defparameter *marker-finder-i*
    (compile-re "\\[\\*\\*(.+?)\\*\\*\\]" :multiple-lines t :return :index))

(defparameter *n-m*
    (compile-re "^(\\d\\d?)-(\\d\\d?)$"))

(defparameter *date*
    (compile-re "^(\\d\\d(\\d\\d)?)-(\\d\\d?)-(\\d\\d?)$"))

(defparameter *4-digit*
    (compile-re "^\\d\\d\\d\\d$"))

(defparameter *ending-digits*
    (compile-re "^(.+)\\s(\\d+)?$"))

(defparameter *num-s*
    (compile-re "^[0-9-]+$")
  "A pattern containing only digits and hyphens, used to mark PHI
    without any indication of its type.")

(defun find-markers (text)
  (let ((start 0)
	(ans nil))
    (loop (multiple-value-bind (matched? span item)
	      (match-re *marker-finder-i*
			text
			:start start)
	    (when (not matched?)
	      (return (sort ans #'string<)))
	    (push (subseq text (car item) (cdr item))
		  ans)
	    (setq start (cdr span))))))

(defun find-marker-bounds (text)
  "Returns a list of elements, each describing a marker. Each
  element's car is a pair of the start/end positon of the whole
  marker, and its cdr is a pair describing the content."
  (let ((start 0)
	(ans nil))
    (loop (multiple-value-bind (matched? span item)
	      (match-re *marker-finder-i* text :start start)
	    (when (not matched?) (return (nreverse ans)))
	    (push (cons span item) ans)
	    (setq start (cdr span))))))
	    
	

(defun find-marker-types (list-of-markers)
  (let ((h (make-hash-table :test #'equal)))
    (dolist (m list-of-markers)
      (multiple-value-bind (m? dummy name id)
	  (match-re *ending-digits* m)
	(declare (ignore dummy))
	(when m?
	  (push id (gethash name h nil)))))
    (let ((ans nil))
      (maphash #'(lambda (k v) (push (cons k v) ans)) h)
      (sort ans #'string< :key #'car))))

(defun collect-pseudonyms (text)
  "Collects all markers substituted for PHI by the de-id process. For
  all but the n-m, date and 4-digit styles (where we can simply delete
  the boundary markers), we compute an appropriate substitution from
  the gazette, if available. These substitutions are memoized in
  *pseudonyms* so that we assign the same pseudonym each time the same
  PHI marker appears in text."
  (dolist (marker (find-markers text))
    (unless (match-re *num-s* marker)
      ;; Leave markers that are just digits and hyphens alone for now.
      ;; For the others, be sure to create a pseudonym.
      (multiple-value-bind (matched? whole marker id)
	  (match-re *ending-digits* marker)
	;; Markers without explicit id's need not be memoized, because
	;; they can just be generated at the time we do the
	;; replacement.
	(when (and id matched?)
	  (or (gethash whole *pseudonyms*)
	      (setf (gethash whole *pseudonyms*)
		(generate-pseudonym marker))))))))

(defun generate-pseudonym (marker)
  (gen-pseudo (intern marker :keyword)))

(defmethod gen-pseudo ((m (eql :|Age over 90|)))
  "Computes an age over 90, using random number and death rate
  estimate of 0.15 per year, increasing by 0.01 yearly.
  From actuarial tables, 0.15 is the rate for 88yo males, 91yo
  females, and the rate increases non-linearly, but this is close
  enough."
  (do ((age 90 (1+ age))
       (fate (random 1.0))
       (death-rate 0.15 (+ death-rate 0.02))
       (surv .85 (* surv (- 1.0 death-rate))))
      ((<= surv fate) (format nil "~d" age))))

(defmethod gen-pseudo ((m (eql :|Attending Info|)))
  (generate-signature))

(defmethod gen-pseudo ((m (eql :|CC Contact Info|)))
  (generate-signature))

(defmethod gen-pseudo ((m (eql :|Clip Number (Radiology)|)))
  (generate-random-digits 3 4))

(defmethod gen-pseudo ((m (eql :|Company|)))
  (generate-random-gazette "company"))

(defmethod gen-pseudo ((m (eql :|Country|)))
  (generate-random-gazette "country"))

(defparameter *days-per-month*
    #(31 28 31 30 31 30 31 31 30 31 30 31))

(defun julian->mo-day (j)
  "Converts a Julian day number (1-365) into month/day. Leap years are
  ignored."
  (do ((m 0 (1+ m))
       (month-end (aref *days-per-month* 0)
		  (+ month-end (aref *days-per-month* (1+ m))))
       (cum 0 month-end))
       ((<= j month-end)
	;;will fail on 1st iteration, so min m = 1.
	;; j falls into the prev month
	(values (1+ m) (- j cum)))))

(defmethod gen-pseudo ((m (eql :|Date range (1)|)))
  "Date range such as mm/dd-mm/dd. We choose duration as uniformly
  distributed over 30 days. Range cannot end on 1/1, and duration is
  limited if end is in January."
  (let* ((j-end (+ 2 (random 364)))
	 (duration (1+ (random (min 30 j-end))))
	 (j-start (- j-end duration)))
    (multiple-value-bind (m1 d1)
	(julian->mo-day j-start)
      (multiple-value-bind (m2 d2)
	  (julian->mo-day j-end)
	(format nil "~d/~d-~d/~d" m1 d1 m2 d2)))))

(defmethod gen-pseudo ((m (eql :|Date range (2)|)))
  "Date range of the form mm/dd/yy-mm/dd/yy or mm/dd/yyyy-mm/dd/yyyy."
  (let* ((year-end (+ (random 100) 1920))
	 (j-end (1+ (random 365)))
	 (duration (1+ (random 30)))
	 (year-start
	  (if (< duration j-end) year-end (1- year-end)))
	 (j-start
	  (if (< duration j-end)
	      (- j-end duration)
	    (- j-end duration -365)))
	 (2dig (zerop (random 2))))
    (multiple-value-bind (m1 d1)
	(julian->mo-day j-start)
      (multiple-value-bind (m2 d2)
	  (julian->mo-day j-end)
	(format nil "~d/~d/~2,'0d-~d/~d/~2,'0d"
		m1 d1 (if 2dig (mod year-start 100) year-start)
		m2 d2 (if 2dig (mod year-end 100) year-end))))))

(defmethod gen-pseudo ((m (eql :|Date range (3)|)))
  "Date range of the form mm/dd-mm/dd/yy or mm/dd-mm/dd/yyyy."
  (let* ((j-end (+ 2 (random 364)))
	 (duration (1+ (random (min 30 j-end))))
	 (j-start (- j-end duration))
	 (year (+ 1920 (random 100)))
	 (2dig (zerop (random 2)))
	 (y (if 2dig (mod year 100) year)))
    (multiple-value-bind (m1 d1)
	(julian->mo-day j-start)
      (multiple-value-bind (m2 d2)
	  (julian->mo-day j-end)
	(format nil "~d/~d-~d/~d/~2,'0d" m1 d1 m2 d2 y)))))

(defparameter *month-names*
    #("January" "February" "March" "April" "May" "June" "July"
		 "August" "September" "October" "November" "December"))

(defparameter *month-short-names*
    #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct"
      "Nov" "Dec"))

(defun generate-month (&optional (month nil))
  (let ((m (or month (random 12))))
    (if (zerop (random 2))		;random short of long month name
	(elt *month-names* m)
      (let ((mn (elt *month-short-names* m)))
	(if (zerop (random 3))		;random dot for abbreviation
	    mn
	  (concatenate 'string mn "."))))))

(defmethod gen-pseudo ((m (eql :|Date range (4)|)))
  "Date range of the form 2 to 11-May-2009. The 'to' could also be '-'
  or 'through', the hyphen separating the day and month could be a
  space, and the hyphen separating the month and year could also be a
  space or comma."
  (let* ((year (+ 1920 (random 100)))
	 (y (if (zerop (random 2)) (mod year 100) year))
	 (month (random 12))
	 (end-day			;at least 2nd of month
	  (1+ (random (1- (elt *days-per-month* month)))))
	 (start-day (random end-day)))
    (format nil "~d~a~d~a~a~a~2,'0d"
	    (1+ start-day)
	    (elt #("-" " to " " through ") (random 3))
	    (1+ end-day)
	    (elt #(" " "-") (random 2))
	    (generate-month month)
	    (elt #(" " "-" ", ") (random 3))
	    y)))

(defmethod gen-pseudo ((m (eql :|Date range (6)|)))
  "Date range of the form May 11 to 21, 2009. The 'to' can also be
  'through' or '-', and the comma may be omitted."
  (let* ((year (+ 1920 (random 100)))
	 (y (if (zerop (random 2)) (mod year 100) year))
	 (month (random 12))
	 (end-day			;at least 2nd of month
	  (1+ (random (1- (elt *days-per-month* month)))))
	 (start-day (random end-day)))
    (format nil "~a ~d~a~d~a~2,'0d"
	    (generate-month month)
	    (1+ start-day)
	    (elt #("-" " to " " through ") (random 3))
	    (1+ end-day)
	    (elt #(" " ", ") (random 2))
	    y)))

(defun ordinal-suffix (n)
  "Returns the suffix to use for the number n to make it ordinal."
  (let ((1d (mod n 10))
	(2d (mod n 100)))
    (cond ((or (= 2d 11) (= 2d 12) (= 2d 13)) "th")
	  ((= 1d 1) "st")
	  ((= 1d 2) "nd")
	  ((= 1d 3) "rd")
	  (t "th"))))

(defmethod gen-pseudo ((m (eql :|Date range (10)|)))
  "Date range (only within a single year), using ordinal days. E.g.,
  'May 11th to 21st' or 'Apr 11th - 21st'" 
  (let* ((month (random 12))
	 (end-day			;at least 2nd of month
	  (1+ (random (1- (elt *days-per-month* month)))))
	 (start-day (random end-day)))
    (format nil "~a ~d~a~a~d~a"
	    (generate-month month)
	    (1+ start-day) (ordinal-suffix (1+ start-day))
	    (elt #("-" " to " " through ") (random 3))
	    (1+ end-day) (ordinal-suffix (1+ end-day)))))

(defmethod gen-pseudo ((m (eql :|Dictator Info|)))
  (generate-signature))

(defmethod gen-pseudo ((m (eql :|Doctor First Name|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|Doctor Last Name|)))
  (generate-random-name "last"))

(defmethod gen-pseudo ((m (eql :|E-mail address|)))
  "There are only 19 of these in the entire corpus, just use the same
  address."
  "random@mimic.mit.edu")

(defmethod gen-pseudo ((m (eql :|Ethnicity|)))
  "Generates a random ethnicity. In MIMIC, this is interpreted as
  either a recognized ethnicity, such as Caucasian, African, Hispanic,
  etc., or as a national origin, such as 'Nigerian'. We generate thes
  probabilistically: 2/3 of the time, an ethnicity, 1/3 a
  nationality. This comes from the probability distribution in the
  ethnicity gazette."
  (generate-random-name "ethnicity"))

(defmethod gen-pseudo ((m (eql :|Female First Name (un)|)))
  (generate-random-name "female"))
(defmethod gen-pseudo ((m (eql :|First Name (STitle)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|First Name (Titles)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|First Name3 (LF)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|First Name4 (NamePattern1)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|First Name5 (NamePattern1)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|First Name7 (NamePattern1)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|First Name8 (NamePattern2)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|First Name9 (NamePattern2)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|First Name11 (Name Pattern1)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|Holiday|)))
  (elt #("christmas" "thanksgiving" "easter" "hannukah"
	 "rosh hashanah" "ramadan")
       (random 6)))

(defmethod gen-pseudo ((m (eql :|Hospital|)))
  (generate-random-gazette "hospital"))

(defmethod gen-pseudo ((m (eql :|Hospital Unit Name|)))
  (let ((unit-names #("CMED" "CMED CCU" "CMED CSRU")))
    (elt unit-names (random (length unit-names)))))

(defmethod gen-pseudo ((m (eql :|Hospital Unit Number|)))
  ;;Li-Wei claims these should be names, not numbers.
  (gen-pseudo :|Hospital Unit Name|))

(defmethod gen-pseudo ((m (eql :|Hospital1|)))
  (generate-random-gazette "hospital"))

(defmethod gen-pseudo ((m (eql :|Hospital2|)))
  (generate-random-gazette "hospital"))

(defmethod gen-pseudo ((m (eql :|Hospital3|)))
  (generate-random-gazette "hospital"))

(defmethod gen-pseudo ((m (eql :|Hospital4|)))
  (generate-random-gazette "hospital"))

(defmethod gen-pseudo ((m (eql :|Hospital5|)))
  (generate-random-gazette "hospital"))

(defmethod gen-pseudo ((m (eql :|Hospital6|)))
  (generate-random-gazette "hospital"))

(defmethod gen-pseudo ((m (eql :|Initial (NamePattern1)|)))
  (generate-initials 1))

(defmethod gen-pseudo ((m (eql :|Initials (NamePattern4)|)))
  (generate-initials))

(defmethod gen-pseudo ((m (eql :|Initials (NamePattern5)|)))
  (generate-initials))

(defmethod gen-pseudo ((m (eql :|Job Number|)))
  (generate-random-digits 5))

(defmethod gen-pseudo ((m (eql :|Last Name|)))
  (generate-random-name "last"))

(defmethod gen-pseudo ((m (eql :|Last Name (LF)|)))
  (generate-random-name "last"))

(defmethod gen-pseudo ((m (eql :|Last Name (NamePattern1)|)))
  (generate-random-name "last"))

(defmethod gen-pseudo ((m (eql :|Last Name (NamePattern4)|)))
  (generate-random-name "last"))

(defmethod gen-pseudo ((m (eql :|Last Name (NamePattern5)|)))
  (generate-random-name "last"))

(defmethod gen-pseudo ((m (eql :|Last Name (Prefixes)|)))
  (generate-random-name "last"))

(defmethod gen-pseudo ((m (eql :|Last Name (STitle)|)))
  (generate-random-name "last"))

(defmethod gen-pseudo ((m (eql :|Last Name (Titles)|)))
  (generate-random-name "last"))

(defmethod gen-pseudo ((m (eql :|Last Name (ambig)|)))
  (generate-random-name "last"))

(defmethod gen-pseudo ((m (eql :|Last Name (un)|)))
  (generate-random-name "last"))

(defmethod gen-pseudo ((m (eql :|Location|)))
  (generate-random-gazette "city"))

(defmethod gen-pseudo ((m (eql :|Location (Universities)|)))
  "There are only 3 instances of this in the corpus, and they all seem
  to be in error."
  "")

(defmethod gen-pseudo ((m (eql :|Location (un)|)))
  (generate-random-gazette "city"))

(defmethod gen-pseudo ((m (eql :|MD Number|)))
  (generate-random-digits 2 3))

(defmethod gen-pseudo ((m (eql :|Male First Name (un)|)))
  (generate-random-name "male"))

(defmethod gen-pseudo ((m (eql :|Medical Record Number|)))
  (generate-random-digits 3 7))

(defmethod gen-pseudo ((m (eql :|Month Day|)))
  "Generates dates like Apr 3 or May 4th."
  (let* ((month (random 12))
	 (day (random (elt *days-per-month* month))))
    (format nil "~a ~d~a"
	    (generate-month month)
	    (1+ day)
	    (if (zerop (random 3))
		(ordinal-suffix (1+ day))
	      ""))))

(defmethod gen-pseudo ((m (eql :|Month Day Year (2)|)))
  (let* ((month (random 12))
	 (day (random (elt *days-per-month* month)))
	 (year (+ 1920 (random 100))))
    (format nil "~a ~d~a ~2,'0d"
	    (generate-month month)
	    (1+ day)
	    (if (zerop (random 2)) "," "")
	    (if (zerop (random 2)) year (mod year 100)))))

(defmethod gen-pseudo ((m (eql :|Month/Day (1)|)))
  (let* ((month (random 12))
	 (day (random (elt *days-per-month* month))))
    (format nil "~d~a~d"
	    (1+ month)
	    (if (zerop (random 4)) "-" "/") ;/ 3/4 of the time
	    (1+ day))))

(defmethod gen-pseudo ((m (eql :|Month/Day (2)|)))
  (gen-pseudo :|Month/Day (1)|))

(defmethod gen-pseudo ((m (eql :|Month/Day (3)|)))
  (gen-pseudo :|Month/Day (1)|))

(defmethod gen-pseudo ((m (eql :|Month/Day (4)|)))
  (gen-pseudo :|Month/Day (1)|))

(defmethod gen-pseudo ((m (eql :|Month/Day/Year|)))
  (let* ((month (random 12))
	 (day (random (elt *days-per-month* month)))
	 (year (+ 1920 (random 100)))
	 (sepr (random 12))
	 (sep (if (zerop sepr) "."
		(if (<= sepr 3) "-" "/"))))
    (format nil "~d~a~d~a~2,'0d"
	    (1+ month)
	    sep
	    (1+ day)
	    sep
	    (if (zerop (random 2)) year (mod year 100)))))

(defmethod gen-pseudo ((m (eql :|Month/Year (2)|)))
  (format nil "~d~a~2,'0d"
	  (1+ (random 12))
	  (if (zerop (random 5)) "-" "/")
	  (random 100)))

(defmethod gen-pseudo ((m (eql :|Month/Year 1|)))
  (format nil "~d~a~d"
	  (1+ (random 12))
	  (if (zerop (random 5)) "-" "/")
	  (+ 1020 (random 100))))

(defmethod gen-pseudo ((m (eql :|Name (NI)|)))
  "These seem to be single-word names, thus probably first names."
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|Name (STitle)|)))
  (generate-random-name "last"))

(defmethod gen-pseudo ((m (eql :|Name Initial (MD)|)))
  (generate-initials 1))

(defmethod gen-pseudo ((m (eql :|Name Initial (NameIs)|)))
  (generate-initials))

(defmethod gen-pseudo ((m (eql :|Name Initial (PRE)|)))
  (generate-initials 1))

(defmethod gen-pseudo ((m (eql :|Name Prefix (Prefixes)|)))
  (generate-random-gazette "last-prefix"))

(defmethod gen-pseudo ((m (eql :|Name10 (NameIs)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|Name11 (NameIs)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|Name12 (NameIs)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|Name13 (STitle)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|Name14 (STitle)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|Name2 (NI)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|Name5 (PTitle)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|Name6 (MD)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|Name7 (MD)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|Name8 (MD)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|Name9 (PRE)|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|PO Box|)))
  (format nil "~d" (1+ (random 999))))

(defmethod gen-pseudo ((m (eql :|PT ID Number (Radiology)|)))
  (generate-random-digits 3 2 2))

(defmethod gen-pseudo ((m (eql :|Pager number|)))
  (generate-random-digits 3 3 4))

(defmethod gen-pseudo ((m (eql :|Provider Number|)))
  (generate-random-digits 6))

(defmethod gen-pseudo ((m (eql :|Social Security Number|)))
  (generate-random-digits 3 2 4))

(defmethod gen-pseudo ((m (eql :|State|)))
  (generate-random-gazette "state"))

(defmethod gen-pseudo ((m (eql :|State/Zipcode|)))
  (format nil "~a ~5,'0d"
	  (generate-random-gazette "state")
	  (random 100000)))

(defmethod gen-pseudo ((m (eql :|Street Address|)))
  (generate-address))

(defun generate-area-code ()
  "Generates an area code. The first digit can be 2-9, the second 0-8
  and the third 0-9."
  (let ((d1 (+ 2 (random 8)))
	(d2 (random 9))
	(d3 (random 10)))
    (format nil "~1,'0d~1,'0d~1,'0d" d1 d2 d3)))

(defun generate-phone-prefix ()
  "Generates the 3-digit telephone exchnange/prefix. The first digit
  may be 2-9, the others anything."
  (let ((d1 (+ 2 (random 8)))
	(d23 (random 100)))
    (format nil "~1d~2,'0d" d1 d23)))

(defmethod gen-pseudo ((m (eql :|Telephone/Fax (1)|)))
  (let* ((ac? (plusp (random 4)))	;3/4 have area codes
	 (ac (and ac? (generate-area-code)))
	 (ex? (zerop (random 20)))	;1/20 have extensions
	 (ex (and ex? (random 10000)))
	 (pa? (and ac? (plusp (random 5)))) ;4/5 use (...) for a/c
	 (sep (if (zerop (random 3)) " " "-")))
    (format nil "~:[~3*~;~:[~a~a~;(~a) ~*~]~]~a~a~4,'0d~[~*~; x~4,'0d~; x-~4,'0d~; x ~4,'0d~; x. ~4,'0d~; ext~4,'0d~; ext-~4,'0d~; ext ~4,'0d~; ext. ~4,'0d~]"
	    ac?
	    pa?
	    ac
	    sep
	    (generate-phone-prefix)
	    sep
	    (random 10000)
	    (if ex? (1+ (random 8)) 0)
	    ex)))

(defmethod gen-pseudo ((m (eql :|Telephone/Fax (2)|)))
  (gen-pseudo :|Telephone/Fax (1)|))

(defmethod gen-pseudo ((m (eql :|Telephone/Fax (3)|)))
  (gen-pseudo :|Telephone/Fax (1)|))

(defmethod gen-pseudo ((m (eql :|Telephone/Fax (4)|)))
  (gen-pseudo :|Telephone/Fax (1)|))

(defmethod gen-pseudo ((m (eql :|Telephone/Fax (5)|)))
  (gen-pseudo :|Telephone/Fax (1)|))

(defmethod gen-pseudo ((m (eql :|URL|)))
  "http://mimic.mit.edu")

(defmethod gen-pseudo ((m (eql :|Unit Number|)))
  (generate-random-digits 7))

(defmethod gen-pseudo ((m (eql :|University/College|)))
  (generate-random-gazette "college"))

(defmethod gen-pseudo ((m (eql :|Wardname|)))
  (let ((wards #("5 West" "3 North" "2 South" "4 East" "Smith" "Pendleton")))
    (elt wards (random (length wards)))))

(defmethod gen-pseudo ((m (eql :|Year (4 digits)|)))
  (format nil "~d" (+ 1920 (random 100))))

(defmethod gen-pseudo ((m (eql :|Year/Month/Day|)))
  (let* ((month (random 12))
	 (day (random (elt *days-per-month* month)))
	 (year (+ 1920 (random 100)))
	 (sep (if (zerop (random 3)) "-" "/")))
    (format nil "~2,'0d~a~2,'0d~a~2,'0d"
	    (if (zerop (random 2)) year (mod year 100))
	    sep
	    (1+ month)
	    sep
	    (1+ day))))

(defmethod gen-pseudo ((m (eql :|firstname|)))
  (generate-random-first-name))

(defmethod gen-pseudo ((m (eql :|lastname|)))
  (generate-random-name "last"))

#| This was the original version, based on the 2006 PHI markers.

(defun generate-pseudonym (marker)
  (cond ((string= marker "Age over 90")
	 (format nil "~d" (+ (random 10) 91)))
	((string= marker "Company")
	 (generate-random-gazette "company"))
	((string= marker "Country")
	 (generate-random-gazette "country"))
	((member marker
		 '("Doctor First Name" "Female First Name (un)"
		   "First Name" "First Name11" "First Name3"
		   "First Name4" "First Name5" "First Name7"
		   "First Name8" "First Name9"
		   "Known patient firstname"
		   "Male First Name (un)")
		 :test #'string=)
	 (generate-random-first-name))
	((member marker
		 '("Doctor Last Name" "Known patient lastname"
		   "Last Name" "Last Name (un)")
		 :test #'string=)
	 (generate-random-name "last"))
	((member marker
		 '("Name1" "Name10" "Name11" "Name12" "Name13"
		   "Name14" "Name2" "Name3" "Name4" "Name5" "Name6"
		   "Name9")
		 :test #'string=)
	 ;; I'm not sure what this is supposed to be, so I just
	 ;; generate a last name.
	 (generate-random-name "last"))
	((member marker
		 '("Initial" "Initials" "Name Initial")
		 :test #'string=)
	 (generate-initials))
	((string= marker "Date range")
	 (generate-date))
	((string= marker "Ethnicity")
	 (generate-random-name "ethnicity"))
	((member marker
		 '("Hospital" "Hospital1" "Hospital2" "Hospital3"
		   "Hospital4" "Hospital5" "Hospital6")
		 :test #'string=)
	 (generate-random-gazette "hospital"))
	((member marker
		 '("Location" "Location (un)")
		 :test #'string=)
	 (generate-random-gazette "city"))
	((member marker
		 '("Pager number" "Telephone/Fax")
		 :test #'string=)
	 (generate-random-digits 3 3 4))
	((member marker
		 '("Year (2 digits)" "Unit Number")
		 :test #'string=)
	 (generate-random-digits 2))
	((string= marker "Street Address")
	 (generate-address))
	((string= marker "Signature")
	 (generate-signature))
	((string= marker "Wardname")
	 (generate-wardname))
	((string= marker "State")
	 (generate-random-gazette "state"))
	((string= marker "State/Zipcode")
	 (concatenate 'string
	   (generate-random-gazette "state")
	   ", "
	   (generate-random-digits 5)))))
|#

(defparameter *gazette-counts*
    nil
  "Alist of maximum number of entries of each gazette type in the LATE
    repository.")

(defun init-gazette-counts ()
  (open-late-database)
  (unless *gazette-counts*
    (setq *gazette-counts*
      (latesql
       "select gazette, count(*) c, max(cum) cm from gazette group by gazette")))
  *gazette-counts*)

(defun generate-random-gazette (type)
  (let* ((e (init-gazette-counts))
	 (cp (assoc type e :test #'string=))
	 (c (and cp (cdr cp) (cadr cp)))
	 (res (and c
		   (latesql
		    "select entry from gazette where gazette=~a limit ~d,1"
				(sq type) (random c)))))
    (if res (caar res)
      (format nil "random ~a" type))))

(defun generate-random-name (type)
  "Generates a random name of type (last, male, or female), with
  probability given by the census bureau. It will not generate names
  missing from the Census published list of most common names, which
  cover about 90% of names."
  (let* ((e (init-gazette-counts))
	 (cum (float (caddr (assoc type e :test #'string=))))
	 (sample (random cum))
	 (candidates
	  (latesql
	   "select entry from gazette where gazette=~a  ~
and cum=(select min(cum) from gazette where gazette=~a and cum>=~8,4f)"
		       (sq type)
		       (sq type)
		       sample)))
    (string-capitalize (car (elt candidates (random (length candidates)))))))

(defun generate-random-first-name ()
  (generate-random-name (if (zerop (random 2)) "male" "female")))

(defun generate-initials (&optional (n nil))
  "Generate a random initial or initials, with or without periods. The
  optional argument can determine 1 or 2 initials. Without it, we
  generate one except two in 1/7 of the cases."
  (let* ((base (char-code #\A))
	 (range (- (char-code #\Z) base -1))
	 (dots (zerop (random 2)))
	 (2nd (or (eql n 2) (zerop (random 7))))
	 (space (zerop (random 2))))
    (concatenate 'string
      (format nil "~a" (code-char (+ base (random range))))
      (if dots "." "")
      (if (and dots space 2nd) " " "")
      (if 2nd (format nil "~a" (code-char (+ base (random range)))) "")
      (if (and 2nd dots) "." ""))))
	 
(defun generate-random-digits (&rest counts)
  "Generates a hyphen-separated set of numbers, where each field
  contains one of the counts number of digits. E.g., a Social Security
  Number would be made by 3 2 4, a phone number by 3 3 4."
  (let ((parts (mapcar
		#'(lambda (c)
		    (do ((i 0 (1+ i))
			 (res ""))
			((>= i c) res)
		      (setq res (concatenate
				    'string res
				    (format nil "~d" (random 10))))))
		counts)))
    (format nil "~{~a~^-~}" parts)))

(defun generate-date ()
  (let* (;;(year (+ 1920 (random 100)))
	 (month (1+ (random 12)))
	 (day (1+ (random (getf *days-per-month* month)))))
    (format nil "~d/~d" month day)))

(defun generate-address ()
  (format
   nil
   "~d~[~; N.~; E.~; S.~; W.~] ~a ~a"
   (generate-random-digits 4)
   (random 5)
   (generate-random-gazette "street")
   (elt '("St." "Ave." "Rd." "Cir." "Cr." "Blvd.")
	(random 6))))

(defun generate-signature ()
  (format nil
	  "~a ~a ~a, M.D."
	  (generate-random-name
	   (if (zerop (random 2)) "male" "female"))
	  (generate-initials)
	  (generate-random-name "last")))

(defun generate-wardname ()
  "5 West")
	  

(defun all-markers
    (&optional
     (filespec
      "~/Documents/Projects/NLP/data/mimic-2009-disch-sums/*.txt")
     &aux (ans nil))
  "Utility to find all the [** ... **] PHI markers in the text."
  (dolist (fn (directory filespec))
    (let* ((txt (read-file fn))
	   (mark (find-markers txt)))
      (setq ans (append mark ans))))
  ans)

;;; START_OF_RECORD=9913,2,25967,27-JUN-16|
;;; report_id||','||subject_id||','||hadm_id||','||report_dt
(defun fragment-disch-summ
    (&optional (filespec "~/Documents/Projects/NLP/data/mimic-discharge-sum.txt"))
  (labels ((fragment-file-name (index)
	     (concatenate 'string (pathname-name filespec)
			  (format nil "-~d" index))))
    (let* ((n 0)
	   (line)
	   (pat (compile-re "^START_OF_RECORD=(\\d+),(\\d+),(\\d+),(\\d+-[a-zA-Z]+-\\d+)\\|"
			    :return :index))
	   (ofi 0)
	   (o nil))
      (with-open-file (f filespec :direction :input)
	(loop
	  (unless (setq line (read-line f nil nil)) (return))
	  (multiple-value-bind (m?)
	      (match-re pat line)
	    (when m?
	      ;; Found the beginning of the next record.
	      (when (> (incf n) 1000)	;after 1000 records, close the file
		(when o
		  (close o)
		  (setq o nil))
		(setq n 0)))
	    (unless o
	      (setq o (open (merge-pathnames (fragment-file-name (incf ofi))
					     filespec)
			    :direction :output
			    :if-exists :supersede)))
	    (write-line line o)))
	(when o (close o)))
      ofi)))


#|
The following are the marker types and number of occurrences of each,
from the first file of 500 cases. These were drawn from the 2006 de-id
runs, and have been superseded by the 2009 version, which uses
slightly different markers.

Age over 90: 28
Company: 4
Country: 38
Date range: 44
Doctor First Name: 426
Doctor Last Name: 875
Ethnicity: 54
Female First Name (un): 34
First Name: 156
First Name11: 476
First Name3: 253
First Name4: 157
First Name5: 79
First Name7: 8
First Name8: 587
First Name9: 74
Hospital: 455
Hospital1: 276
Hospital2: 22
Hospital3: 82
Hospital4: 17
Hospital5: 16
Hospital6: 43
Initial: 31
Initials: 538
Known patient firstname: 525
Known patient lastname: 698
Last Name: 2113
Last Name (un): 346
Location: 156
Location (un): 117
Male First Name (un): 21
Name Initial: 156
Name1: 8
Name10: 134
Name11: 104
Name12: 81
Name13: 10
Name14: 14
Name2: 37
Name3: 107
Name4: 34
Name5: 11
Name6: 1
Name9: 99
Pager number: 3
Signature: 25
State: 11
State/Zipcode: 32
Street Address: 6
Telephone/Fax: 529
Unit Number: 499
Wardname: 59
Year (2 digits): 19
|#

(defun clean-case (text)
  "Finds and replaces all MIMIC II PHI markers with randomly generated
  text of the appropriate type. For markers of PHI that has occurred
  multiple times in the text, we have already found and memoized
  appropriate replacements in *pseudonyms*. Others get generated and
  replaced here by new random instances.  As a second value, we also
  return a list of data suitable to create annotations on the
  document, marking each instance of replaced PHI."
  (let ((mbounds (find-marker-bounds text))
	(result "")
	(marks nil)
	(lastmb nil)
	(start 0))
    (labels ((add (s phi-type)
	       (let ((start (length result)))
		 (setq result (concatenate 'string result s))
		 (when phi-type
		   (push (list start (length result) phi-type) marks)))))
      (dolist (mb mbounds)
	(setq lastmb mb)
	(when (< start (caar mb))
	  (add (subseq text start (caar mb)) nil))
	(multiple-value-bind (m? item)
	    (match-re *num-s* text
		      :start (cadr mb)
		      :end (cddr mb)
		      :return :index)
	  (if m?			;the marker was just numbers
	      (add (subseq text (car item) (cdr item)) 'nums)
	    (let ((marker (subseq text (cadr mb) (cddr mb))))
	      (multiple-value-bind (matched? whole guts)
		  (match-re *ending-digits* marker)
		(declare (ignore whole))
		(when matched?
		  (add (or (gethash marker *pseudonyms*)
			   (generate-pseudonym guts))
		       guts))))))
	(setq start (cdar mb)))
      (add (subseq text (cdar lastmb)) nil))
    (values result (nreverse marks))))

(defmethod clean-returns ((corpus corpus))
  "Gets rid of extra <carriage return> characters from DOS text."
  (dolist (doc (documents corpus))
    (clean-returns (document doc))))

(defmethod clean-returns ((d document))
  "Gets rid of extra <carriage return> characters from DOS text."
  (setf (content d)
    (replace-re (content d) "\\r$" "" :multiple-lines t))
  (setf (dirty d) t)
  (save d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code to help find headers in the MIMIC II discharge summaries.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *header-pat*
    (compile-re ;;"^(\\d{1,2}[:)\\.]\\s+)?(.+?):"
     "^([^\\d\\n\\r\\s].+?):"
		:multiple-lines t
		:return :index))

(defparameter *hh*
    (make-hash-table :test #'equalp))

(defmethod find-headers ((corp corpus))
  (dolist (doc (documents corp))
    (find-headers (document doc)))
  *hh*)

(defmethod find-headers ((d document))
  (let* ((s 0)
	 (text (content d))
	 (e (length text))
	 (ans nil))
    (loop
      (multiple-value-bind (m? whole title)
	  (match-re *header-pat* text :start s :end e)
	(declare (ignore whole))
	(unless m? (return (nreverse ans)))
	(let ((item (subseq text (car title) (cdr title))))
	  (incf (gethash item *hh* 0)))
	(setq s (cdr title)))))
  *hh*)

(defun get-hh ()
  (let ((ans nil))
    (maphash #'(lambda (k v) (push (cons v k) ans)) *hh*)
    (sort ans #'> :key #'car)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility to clean up mimic annotations without deleting
;;; phi-pseudonym markers.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flush-mimic-annotations (&optional (name "mimic"))
  "Utility to delete all annotations from mimic documents except for
  phi-pseudonym, which are assigned on import."
  (open-late-database)
  (let ((m (corpus name)))
    (if (null m)
	"There is no mimic corpus defined."
      (latesql
       "delete a from annotations a join corpora_documents c on a.document_id=c.document_id where c.corpus_id=~d and a.type<>'phi-pseudonym'"
       (id m)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities to import terms from the MIMIC deid gazettes into our
;;;data, if they are not already present.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun import-mimic-deid
    (&optional (dir "~/Documents/Projects/NLP/mimic-deid/lists/"))
  (dolist (fn (directory (merge-pathnames "*.txt" dir)))
    (format t "~%Reading ~a" fn)
    (set (intern (pathname-name fn))
	 (mapcar #'car
		 (read-csv fn :comma #\|)))))

#|
The following are the types and number of instances of PHI markers in
the MIMIC II discharge summaries, sampled May 2009.

   1280 "Age over 90"
  11510 "Attending Info"
    661 "CC Contact Info"
     81 "Clip Number (Radiology)"
    682 "Company"
   1217 "Country"
   1108 "Date range (1)"
      2 "Date range (10)"
    216 "Date range (2)"
     69 "Date range (3)"
      1 "Date range (4)"
     20 "Date range (6)"
  13515 "Dictator Info"
  20657 "Doctor First Name"
   7470 "Doctor Last Name"
     19 "E-mail address"
   2958 "Ethnicity"
   2649 "Female First Name (un)"
  11543 "First Name (STitle)"
   2246 "First Name (Titles)"
  24911 "First Name11 (Name Pattern1)"
   2822 "First Name3 (LF)"
  12432 "First Name4 (NamePattern1)"
    873 "First Name5 (NamePattern1)"
    256 "First Name7 (NamePattern1)"
  29512 "First Name8 (NamePattern2)"
   1360 "First Name9 (NamePattern2)"
     89 "Holiday"
  28241 "Hospital"
  25227 "Hospital Unit Name"
    434 "Hospital Unit Number"
  16572 "Hospital1"
      7 "Hospital2"
     47 "Hospital3"
      1 "Hospital4"
      2 "Hospital5"
      1 "Hospital6"
   2011 "Initial (NamePattern1)"
  15456 "Initials (NamePattern4)"
    230 "Initials (NamePattern5)"
  13574 "Job Number"
    112 "Last Name"
   1916 "Last Name (LF)"
  33331 "Last Name (NamePattern1)"
  13359 "Last Name (NamePattern4)"
    224 "Last Name (NamePattern5)"
   4599 "Last Name (Prefixes)"
  34401 "Last Name (STitle)"
   3407 "Last Name (Titles)"
     27 "Last Name (ambig)"
  12747 "Last Name (un)"
   1950 "Location"
      3 "Location (Universities)"
  11579 "Location (un)"
  29357 "MD Number"
   3223 "Male First Name (un)"
     97 "Medical Record Number"
     40 "Month Day"
     58 "Month Day Year (2)"
      6 "Month/Day (1)"
     63 "Month/Day (2)"
     16 "Month/Day (3)"
     31 "Month/Day (4)"
    151 "Month/Day/Year"
     66 "Month/Year (2)"
      4 "Month/Year 1"
   3274 "Name (NI)"
   6051 "Name (STitle)"
     12 "Name Initial (MD)"
    676 "Name Initial (NameIs)"
    459 "Name Initial (PRE)"
    4607 "Name Prefix (Prefixes)"
   4790 "Name10 (NameIs)"
   1117 "Name11 (NameIs)"
    260 "Name12 (NameIs)"
    719 "Name13 (STitle)"
    309 "Name14 (STitle)"
   2439 "Name2 (NI)"
    352 "Name5 (PTitle)"
   4391 "Name6 (MD)"
  17003 "Name7 (MD)"
   3313 "Name8 (MD)"
   1320 "Name9 (PRE)"
      8 "PO Box"
     58 "PT ID Number (Radiology)"
    161 "Pager number"
      7 "Provider Number"
      4 "Social Security Number"
   1817 "State"
   2692 "State/Zipcode"
   2189 "Street Address"
    661 "Telephone/Fax (1)"
  24077 "Telephone/Fax (2)"
    136 "Telephone/Fax (3)"
     14 "Telephone/Fax (4)"
     93 "Telephone/Fax (5)"
      3 "URL"
    757 "Unit Number"
    218 "University/College"
    745 "Wardname"
     23 "Year (4 digits)"
      3 "Year/Month/Day"
   1757 "firstname"
  14576 "lastname"
 503820 TOTAL

 |#
