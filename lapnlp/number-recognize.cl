;;; -*- Mode: Lisp; Pacakge: late -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 03/10/2015 rewrite using asdf framework
yluo - 05/01/2012 added add-analysis to integrate it into pipeline
yluo - 06/03/2010 added *date-pat5* *date-pat6* for mgh path reports.
psz  - mm/dd/yyyy creation

This set of procedures began as an attempt to empirically analyze
the types of tokens found in the MIMIC II database.  Having done
much of this analysis, we now adapt the patterns discovered here
to help create token-interpretation annotations.  The earlier
parts of the file are exploratory and define the patterns.  The
end includes the procedures that process documents to create the
token-interpretations.

Unresolved is whether numbers should continue to be represented as
strings or whether they should be converted to Lisp numbers.  As
of Aug 2010, some are converted, others not.

Code to recognize numeric patterns, as in the MIMIC de-id code.

Empirically, from get-all-numeric-tokens, we see 137778 distinct
tokens that contain at least one digit.  There are 2391621 total
such tokens, so on average such a token appears 20 times in the
corpus. These data are strongly influenced by the specific
tokenization rules of the Link Grammar Parser, whose tokenizer we
use here.  If we were to tokenize by different rules, many of the
specifics below would change.  By inspections, here are some of
the types of meanings of such tokens:

1. all-digits, or digits with one decimal point; these are almost
surely a quantity
2. digit(s) followed by ) or .) and then addtitional text;
probably intended to be sequential enumeration, but missing a
space; Unfortunately, we don't notice this until after find-struct
has already broken the text into lists.
3. (decimal) digits followed by a unit of measure, such as L,
L/minutes, mm, cm2, mg, cal, gm/kg, etc.
4. (decimal) numbers - (decimal) number; a range
5. like 100-120/60-70; probably systolic and diastolic blood
pressure range. Also, 70s, 50s-70s, etc. ~500 instances.
6. like 1/2-3/14; probably date ranges, though might also be
something like 110/50-140/80, as in blood pressure. The example
can also be ambiguous, meaning Jan 2 to Mar 14, or Jan 2 to 3,
2014. There are almost 900 instances of this.

The basic pattern for a quantity is 
"(\d+\.?|\d*\.\d+)"
which permits digits optionally followed by a decimal point or
optionally digits, a decimal point, and additional digits.
This recurs frequently in various contexts, and in the comments I will
refer to it as the N motif. For example, a pattern like
^[+-]?N$
will parse just a number, optionally preceded by a sign.

Certain patterns, such as dates and times are important to pull out
early, because they have more specific rules that disambiguate them
from other possible interpretations of data.  For example, 1/3 could
be a date or a grade or ..., but 16/22 cannot be a date.

The data also contain several other common patterns:
a. A range of values, separated by -, but sometimes by other
separators. ^[+-]?N-N$
b. Values separated by /, often blood pressure readings, but sometimes
series of up to 5 such values. ^N(/N)*$
c. A value or range followed (without token-breaking spaces) by
units. ^[+-]N.+$
d. A specification of a measurement followed by a - or : (or sometimes
nothing) and then a number, possibly followed by * and/or #. On some
occasions, the value is preceded by *, and on some it is a range.
^.+[:-]\*?N(-N)?\*?\#?$

There are also combinations of a and b, but alas in both ways:
142/80-163/95 or 142-163/80-95.
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :regexp2)
  (require :datetime))

(defpackage :late
  (:use :common-lisp #+allegro :excl :util.date-time)
  (:export "*ntok*"
	   "empty?"
	   "*basic-pat*"
	   "*basic-pat1*"
	   "sift"
	   "map-match-re"
	   "parse-basic-pat"
	   "anal"
	   "number-patterns"
	   "get-all-numeric-tokens"
	   "*time-pat*"
	   "time?"
	   "*date-pat1*"
	   "*date-pat2*"
	   "*date-pat3*"
	   "*date-pat4*"
	   "*date-pat5*"
	   "*date-pat6*"
	   "*date-range1*"
	   "*break-year*"
	   "fix-year"
	   "check-date"
	   "date?"
	   "date-ut?"
	   "date-range?"
	   "*phone-pat*"
	   "phone?"
	   "*num-comma-pat*"
	   "*ratio-of-ranges-pat*"
	   "*range-of-ratios-pat*"
	   "range-of-ratios?"
	   "ratio-of-ranges?"
	   "*num-unit*"
	   "num-unit?"
	   "*enum-pat*"
	   "enum?"
	   "*numlist-pat*"
	   "numlist?"
	   "try-ntok"
	   "out-ntok"
	   "token-parse"
	   "discretize-pct"
	   "discretize-age"
	   ))

(in-package :late)

(defvar *ntok* nil
  "List of tokens that contain a numeric value, as retrieved by get-all-numeric-tokens.")

(declaim (inline empty?))
(defun empty? (x)
  "True if x is null or the empty string."
  (or (null x) (equal x "")))

(defparameter *basic-pat*
  (compile-re
   "^([A-Za-z%].*?)?([:<>=~-])?([+-]?\\d+\\.?|[+-]?\\d*\\.\\d+)(([/-])(\\d+\\.?|\\d*\\.\\d+))?([-/])?([a-zA-Z%][^*#]*)?(\\*?)(\\#?)$")
  "The basic pattern contains, in order:
    (optionally) a prefix, starting with an alpha, usually an indication of what is measured
    (optionally) a separator ending the prefix
    a number, with possible leading sign and possible decimal point
    (optionally) a / or - followed by another number
    (optionally) an alphanumeric representing units
    (optionally) a * and/or #, indicating normality"
  )

(defun sift (pat &optional (l *ntok*))
  (let ((yes nil) (no nil))
    (dolist (x l)
      (if (if (typep pat 'function)
	      (funcall pat x)
	    (match-re pat x))
	  (push x yes)
	(push x no)))
    (format t "~%Matched ~d, not ~d" (length yes) (length no))
    (list (nreverse yes) (nreverse no))))

(defun map-match-re (proc pattern text &rest match-keywords)
  "Iterates over text, looking for the pattern, and invoking proc each
  time it is found. Rest arguments are keyword arguments passed on to
  match-re, but should not include :start or :return, which is always
  :index. Proc is applied to the matching range of the pattern plus as
  many other arguments as are matched by (...) in the pattern."
  (let ((pos 0))
    (loop
     (let ((match
	    (multiple-value-list
	     (apply #'match-re pattern text :return :index :start pos match-keywords))))
       (when (or (null match) (null (car match))) (return t))
       (apply proc (cdr match))
       (setq pos (cdadr match))))))

(defun parse-basic-pat (token)
  (multiple-value-bind
      (m? whole label rel num1 dummy conn num2 sep units star sharp)
      (match-re *basic-pat* token)
    (declare (ignore whole dummy))
    (if m?
	(nconc (and label (list 'label label))
	       (and rel (list 'sep rel))
	       (list 'num1 (read-from-string num1))
	       (and conn (list 'conn conn))
	       (and num2 (list 'num2 (read-from-string num2)))
	       (and sep (list 'sep sep))
	       (and units (list 'units units))
	       (and (not (empty? star)) (list 'star t))
	       (and  sharp (not (equal sharp "")) (list 'sharp t)))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities to investigate the set of tokens in MIMIC as they were
;;;tokenized by the Link-Grammar parser.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-all-numeric-tokens ()
  (setq *ntok*
	(mapcar #'car
		(latesql
		 "select distinct substring(d.content,a.start+1,a.end-a.start) as c ~
from documents d join annotations a on d.id=a.document_id ~
where a.type='lp-token' ~
and substring(d.content,a.start+1,a.end-a.start) regexp '[0-9]' ~
order by c"))))

(defparameter *time-pat*
  (compile-re "^(\\d\\d?):(\\d\\d)(:(\\d\\d))?(AM|PM)?$" :case-fold t))

(defun time? (s)
  (multiple-value-bind (match? time hr mn secc sec ampm)
      (match-re *time-pat* s)
    (declare (ignore time secc))
    (and match?
	 (let ((hrn (read-from-string hr))
	       (min (read-from-string mn))
	       (sen (or (and sec (read-from-string sec)) 0)))
	   ;;(print (list hr mn sec ampm hrn min sen))
	   (and (<= 0 sen 59)
		(<= 0 min 59)
		(if ampm (<= 1 hrn 12) (<= 0 hrn 23))
		(nconc (list 'hr hrn)
		       (list 'mn min)
		       (and sec (list 'sec sen))
		       (and ampm (list 'ampm ampm))))))))

(defparameter *date-pat1*
  (compile-re "^(\\d\\d?)/(\\d\\d?)(/(\\d\\d(\\d\\d)?))?$"))

(defparameter *date-pat2*
  (compile-re "^-?(\\d\\d(\\d\\d)?)-(\\d\\d?)-(\\d\\d?)(\\*\\*)?$")
  "Date pattern: optional hyphen, year-month-day, optional **")

(defparameter *date-pat3*
  (compile-re "^(\\d\\d?)-(\\d\\d?)-(\\d\\d(\\d\\d)?)$")
  "Date pattern: month-day-year")

(defparameter *date-pat4*		;only for multiple tokens!
  (compile-re "^((jan|feb|mar|apr|jun|jul|aug|sep|oct|nov|dec)\\w*)\\s+(\\d+),\\s+(\\d{2,4})$"
	      :case-fold t))

(defparameter *date-pat5*
  (compile-re "^(\\d+)-(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)-(\\d{2,4})$"
	      :case-fold t))

(defparameter *date-pat6*		;only for multiple tokens!
  (compile-re "^(\\d*)\\s+(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)\\s+(\\d{2,4})$"
	      :case-fold t))



(defparameter *date-range1*
  (compile-re
   "^(\\d\\d?)/(\\d\\d?)(/(\\d\\d(\\d\\d)?))?-(\\d\\d?)/(\\d\\d?)/(\\d\\d(\\d\\d)?)$"))

(defparameter *break-year* 30
  "2-digit years >= this are in 1900's, otherwise in 2000's")

(defun fix-year (yr)
  "Transforms a 2-digit year number into an appropriate 4-digit year, based on *break-year*"
  (if (>= yr 100) yr
    (if (>= yr *break-year*)
	(+ 1900 yr)
      (+ 2000 yr))))

(defun check-date (mo da)
  "Checks that the given year, month and day represent a valid date.
   We accept Feb 29 in any year."
  (and (<= 1 mo 12)
       (<= 1 da (elt #(31 29 31 30 31 30 31 31 30 31 30 31) (1- mo)))))


(defun date? (s &aux m1? m2? m3? m5? m6? date mo da yr ignore)
  "Checks if the string s can be interpreted as a token representing a
  date. This uses USA conventions, so 12/9/2000 is Dec 9, not Sep 12."
  (declare (ignore ignore date))
  (multiple-value-setq (m1? date mo da ignore yr) (match-re *date-pat1* s))
  (unless m1?
    (multiple-value-setq (m2? date yr ignore mo da)
      (match-re *date-pat2* s))
    ;; The problem here is that this pattern matches both "1-12-96"
    ;; and "96-1-14", but assumes that the first number is a year. We
    ;; must test here to make sure we properly recognize instances of
    ;; the first date type as well, or else the tests at the end of
    ;; this function will reject the interpretation of the first date
    ;; as December 96, 2001, an invalid date.
    (when m2?
      (let ((mon (read-from-string mo))
	    (dan (read-from-string da))
	    (yrn (read-from-string yr)))
	(unless (check-date mon dan)
	  (psetq mo yr da mo yr da
		 mon yrn dan mon yrn dan)
	  (unless (check-date mon dan)
	    ;; if neither pattern works, allow others to try matching
	    (setq m2? nil))))))
  (unless (or m1? m2?)
    (multiple-value-setq (m3? date mo da yr)
      (match-re *date-pat3* s)))
  (unless (or m1? m2? m3?)
    (multiple-value-setq (m5? date da mo yr)
      (match-re *date-pat5* s)))
  (unless (or m1? m2? m3? m5?)
    (multiple-value-setq (m6? date da mo yr)
      (match-re *date-pat6* s)))
  (and (or m1? m2? m3? m5? m6?)
       (let ((mon (read-from-string mo))
	     (dan (read-from-string da))
	     (yrn (and yr (read-from-string yr))))
	 (unless (integerp mon)
	   (let ((month-assoc
		  (assoc mon
			 '(("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4) 
			   ("may" . 5) ("jun" . 6) ("jul" . 7) ("aug" . 8) 
			   ("sep" . 9) ("oct" . 10) ("nov" . 11) ("dec" . 12)) 
			 :test #'string-equal)))
	     (setf mon (and month-assoc (cdr month-assoc)))))
	 (if (or (null mon)
		 (null dan)
		 (not (check-date mon dan)))
	     nil			;bad date
	   (nconc (list 'mo mon 'da dan)
		  (and yrn (list 'yr (fix-year yrn))))))))

;;; I think this is no longer needed, because parse-token applied to
;;; something that matches date? will compute the universal time of
;;; the date, also defaulting to GMT.
(defun date-ut? (s)
  "Uses date? above to parse a date, and then convert it to a universal time assuming a time of 0:00:00Z"
  (let* ((date (date? s)))
    (and date
	 (date-time-to-ut
	  (format nil "~d-~2,'0d-~2,'0dT00:00:00Z"
		  (getf date 'yr) (getf date 'mo) (getf date 'da))))))

(defun date-range? (token)
  (multiple-value-bind (m? whole m1 d1 dummy1 y1 dummy2 m2 d2 y2 dummy3)
      (match-re *date-range1* token)
    (declare (ignore whole dummy1 dummy2 dummy3))
    (if m?
	(let ((mon1 (read-from-string m1))
	      (day1 (read-from-string d1))
	      (yr1 (and y1 (read-from-string y1)))
	      (mon2 (read-from-string m2))
	      (day2 (read-from-string d2))
	      (yr2 (read-from-string y2)))
	  (if (and (check-date mon1 day1)
		   (check-date mon2 day2))
	      `(mo1 ,mon1 da1 ,day1 yr1 ,(fix-year (or yr1 yr2))
		    mo2 ,mon2 da2 ,day2 yr2 ,(fix-year yr2)))))))

(defparameter *phone-pat*
  ;; 323-2345
  ;; 1-323-2345
  ;; 1-617-323-2345
  ;; Fax:1-617-323-2345
  (compile-re
   "^((Phone|Cell|Mobile|Work|Home|Fax):)?(1[ -]?)?(((\\d\\d\\d)[ -]|\\((\\d\\d\\d)\\)\\s*))?(\\d\\d\\d)[ -](\\d\\d\\d\\d)$"))


(defun phone? (token)
  (multiple-value-bind (m? whole type type-base prefix acdum1 acdum2 ac1 ac2 exch num)
      (match-re *phone-pat* token)
    (declare (ignore type whole prefix acdum1 acdum2))
    (if m?
	(nconc (and type-base (list 'type type-base))
	       (and (or ac1 ac2) (list 'ac (or ac1 ac2)))
	       (list 'exch exch)
	       (list 'num num)))))

(defparameter *num-comma-pat*
  (compile-re "^[+-]?\\d{1,3}(,\\d\\d\\d)+$")
  "Pattern to recognize numbers written with commas between sets of 3 digits.")

(defparameter *ratio-of-ranges-pat*
  (compile-re
   "^([+-]?\\d+\\.?|[+-]?\\d*\\.\\d+)-(\\d+\\.?|\\d*\\.\\d+)/([+-]?\\d+\\.?|[+-]?\\d*\\.\\d+)-(\\d+\\.?|\\d*\\.\\d+)([-/])?([a-zA-Z%][^*#]*)?(\\*?)(\\#?)$")
  "Pattern matches things like 1-2/3-4")

(defparameter *range-of-ratios-pat*
  (compile-re
   "^([+-]?\\d+\\.?|[+-]?\\d*\\.\\d+)/(\\d+\\.?|\\d*\\.\\d+)-([+-]?\\d+\\.?|[+-]?\\d*\\.\\d+)/(\\d+\\.?|\\d*\\.\\d+)([a-zA-Z%].*?)?(\\*?)(\\#?)$")
  "Pattern matches things like 1/2-3/4")

(defun range-of-ratios? (token)
  (multiple-value-bind (m? whole numer1 denom1 numer2 denom2 suffix star sharp)
      (match-re *range-of-ratios-pat* token)
    (declare (ignore whole))
    (if m?
	(let ((m1 (read-from-string numer1))
	      (d1 (read-from-string denom1))
	      (m2 (read-from-string numer2))
	      (d2 (read-from-string denom2)))
	  (cond ((and (check-date m1 d1)
		      (check-date m2 d2)
		      (null suffix) (null sharp) (null star))
		 ;; a range of dates
		 `(date-range mo1 ,m1 da1 ,d1
			      mo2 ,m2 da2 ,d2))
		((and (check-date m1 d1) (check-date m1 m2)
		      (or (<= 0 d2 99) (>= d2 1900))
		      (null suffix) (null sharp) (null star))
		 ;; a range of dates such as 1/4-7/1989
		 `(date-range mo1 ,m1 da1 ,d1
			      mo2 ,m1 da2 ,m2
			      yr1 ,d2 yr2 ,d2))
		(t
		 `(range-ratios n1 ,m1 d1 ,d1
				n2 ,m2 d2 ,d2
				,@(and suffix `(suffix ,suffix))
				,@(and star (not (equal "" star))
				       '(star t))
				,@(and sharp (not (equal "" sharp))
				       '(sharp t)))))))))

(defun ratio-of-ranges? (token)
  (multiple-value-bind (m? whole start1 end1 start2 end2 suffix star sharp)
      (match-re *ratio-of-ranges-pat* token)
    (declare (ignore whole))
    (if m?
	(let ((s1 (read-from-string start1))
	      (e1 (read-from-string end1))
	      (s2 (read-from-string start2))
	      (e2 (read-from-string end2)))
	  `(ratio-ranges s1 ,s1 e1 ,e1 s2 ,s2 e2 ,e2			
			 ,@(and suffix `(suffix ,suffix))
			 ,@(and star (not (equal "" star))
				'(star t))
			 ,@(and sharp (not (equal "" sharp))
				'(sharp t)))))))

(defparameter *num-unit*
  (compile-re
   "^([+-]?\\d+\\.?|[+-]?\\d*\\.\\d+)(([/-])(\\d+\\.?|\\d*\\.\\d+))?([/-])([a-zA-Z%].*)$"))

(defun num-unit? (token)
  "Matches a token starting with a possibly signed number, possibly
  followed by / or - and another number, then a / or - and something
  starting with an alphabetic char." 
  (multiple-value-bind (m? whole num1 dummy1 sep num2 conn suffix)
      (match-re *num-unit* token)
    (declare (ignore whole dummy1))
    (if m?
	(let ((n1 (read-from-string num1))
	      (n2 (and num2 (read-from-string num2))))
	  `(num1 ,n1
		 ,@(and sep `(sep ,sep))
		 ,@(and num2 `(num2 ,n2))
		 ,@(and conn `(conn ,conn))
		 suffix ,suffix)))))

(defparameter *enum-pat*
  (compile-re "^(\\d+\\.?)\\)([a-zA-Z].*)$"))

(defun enum? (token)
  "Matches a token starting with digits, then a ), followed by an
  alphabetic and possibly other characters. These are often bullet
  points that were missing a space."
  (multiple-value-bind (m? whole num text)
      (match-re *enum-pat* token)
    (declare (ignore whole))
    (if m?
	`(enum num ,(read-from-string num) text ,text))))

(defparameter *numlist-pat*
  (compile-re
   "^([+-]?\\d+\\.?|[+-]?\\d*\\.\\d+)((/|-|-+>)([+-]?\\d+\\.?|[+-]?\\d*\\.\\d+))+$")
  "Recognizes lists of numbers separated by /'s or -'s or ->'s. Note that
    *basic-pat* will include one or two number connected by - or /, so
    that should be used first.")

(defun numlist? (token)
  "Matches a series of numbers separated by / or - or ->, -->, ... ."
  (let ((nums nil) (seps nil) (end (length token)))
    (loop
     (multiple-value-bind (m? whole first-num last-match sep num)
	 (match-re *numlist-pat* token :end end)
       (declare (ignore whole first-num last-match))
       (cond (m?
	      (push (read-from-string num) nums)
	      (push sep seps)
	      (decf end (+ (length sep) (length num))))
	     (t
	      ;; if failed to match on whole string, return nil
	      ;; otherwise, we have exhausted all subsequent
	      ;; sep-number pairs, so just add the first number.
	      (if nums
		  (push (read-from-string (subseq token 0 end)) nums))
	      (return nil)))))
    (and nums seps
	 (list 'nums nums 'seps seps))))

;;; the following two patterns were suggested by Ken Burford (SUNY/Albany), from www.regexlib.com
(defparameter *email-pattern*
  "([a-zA-Z0-9_\-])+(\\.([a-zA-Z0-9_\-])+)*@((\\[(((([0-1])?([0-9])?[0-9])|(2[0-4][0-9])|(2[0-5][0-5])))\\.(((([0-1])?([0-9])?[0-9])|(2[0-4][0-9])|(2[0-5][0-5])))\\.(((([0-1])?([0-9])?[0-9])|(2[0-4][0-9])|(2[0-5][0-5])))\\.(((([0-1])?([0-9])?[0-9])|(2[0-4][0-9])|(2[0-5][0-5]))\\]))|((([a-zA-Z0-9])+(([\-])+([a-zA-Z0-9])+)*\\.)+([a-zA-Z])+(([\-])+([a-zA-Z0-9])+)*))")

(defparameter *ip-address-pattern*
  "(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9])\\.(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0)\\.(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0)\\.(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[0-9])")

(defun try-ntok (&optional (l *ntok*))
  "Utility to test a variety of patterns agains a list of tokens in a
  corpus. It reports how many matched each pattern, and returns the
  non-matching ones as its answer."
  (let* ((bp (sift *basic-pat* l))
	 (tp (sift *time-pat* (cadr bp)))
	 (dp (sift #'date? (cadr tp)))
	 (dr (sift #'date-range? (cadr dp)))
	 (pp (sift *phone-pat* (cadr dr)))
	 (cp (sift *num-comma-pat* (cadr pp)))
	 (rro (sift #'range-of-ratios? (cadr cp)))
	 (rra (sift #'ratio-of-ranges? (cadr rro)))
	 (nu (sift *num-unit* (cadr rra)))
	 (en (sift *enum-pat* (cadr nu)))
	 (nl (sift #'numlist? (cadr en))))
    (format t "~%TOTAL: ~d~%basic: ~d~%time: ~d~%date: ~d~
~%date-range: ~d~%phone: ~d~
~%commanum: ~d~%range-of-ratios: ~d~%ratio-of-ranges: ~d~
~%number(s)-units: ~d~%enum: ~d~%numlist: ~d~
~%rest: ~d"
	    (length l)
	    (length (car bp))
	    (length (car tp))
	    (length (car dp))
	    (length (car dr))
	    (length (car pp))
	    (length (car cp))
	    (length (car rro))
	    (length (car rra))
	    (length (car nu))
	    (length (car en))
	    (length (car nl))
	    (length (cadr nl)))
    (cadr nl)))

(defun out-ntok (l filename)
  "Utility to print list of tokens to a file on the desktop."
  (with-open-file (f (merge-pathnames filename "~/Desktop/")
		     :direction :output
		     :if-exists :supersede)
		  (print l f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Notes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Other patterns that we have not implemented:

3. Ranges expressed with an arrow; e.g., "2-->15"

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Annotators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun to-universal-time (alist &optional (tz 0))
  "Utility to convert a parsed date or time to a universal time. Note
  that all components are already parsed/converted to numbers, unless
  nil. Universal times are only representable since 1900, hence a time
  before then will yield nil."
  (let* ((sec (getf alist 'sec 0))
	 (mn (getf alist 'mn 0))
	 (pm? (getf alist 'ampm nil))
	 (hr-base (getf alist 'hr 0))
	 (hr (cond ((null pm?) ;; assume 24-hour clock
		    hr-base)
		   ((equal pm? "am")
		    (if (= hr-base 12)
			0
		      hr-base))
		   ((equal pm? "pm")
		    (if (< hr-base 12)
			(+ hr-base 12)
		      hr-base))
		   (t 0)))
	 (da (getf alist 'da 1))
	 (mo (getf alist 'mo 1))
	 (yr (getf alist 'yr 1900)))
    (ignore-errors (encode-universal-time sec mn hr da mo yr tz))))

(defmethod token-parse ((c corpus) &key (type 'token-annotation))
  (do-corpus (doc c)
	     (token-parse doc :type type)))

(defmethod token-parse ((d document) &key (save t))
  "Adds annotations for the various token patterns from above. Note
  that the sequence in which we try these patterns matters. For
  example, 918-3476 can be a phone number or a number range, and 4-5
  can be a date or number range.  We prefer the phone number or date
  or time interpretation, if they make sense."
  ;;  Sometimes we detect a pattern but then cannot form the
  ;;  appropriate annotation. We wrap each make-instance in an
  ;;  ignore-errors so that failure to create the annotation (signaled
  ;;  by a value of nil) will simply pass on it and go on to the next
  ;;  possibility. Within the make-instance code, it's possible to
  ;;  check for non-serious error conditions and take special action
  ;;  on them, e.g., in range-of-ratios-pattern if the denominator is
  ;;  0.
  (dolist (tok (annotations-spec d :type (gtype 'gtoken-type)))
    (let* ((ttext (content tok))
	   (new-ann
	    (or       
	     ;; time pattern
	     (let ((time-alist (time? ttext)))
	       (if time-alist
		   (ignore-errors
		     (make-instance 'time-pattern
				    :hr (getf time-alist 'hr)
				    :mn (getf time-alist 'mn)
				    :sec (getf time-alist 'sec)
				    :ampm (getf time-alist 'ampm)
				    ;; create a universal time, assuming the base date,
				    ;; 1/1/1900 GMT, which is the first representable
				    ;; universal time (whose numerical value is 0).
				    :data (to-universal-time time-alist 0)
				    :start (start tok)
				    :end (end tok)))))
	     
	     ;; date
	     (let ((date-alist (date? ttext)))
	       (if date-alist
		   (ignore-errors
		     (make-instance 'date-pattern
				    :yr (getf date-alist 'yr)
				    :mo (getf date-alist 'mo)
				    :da (getf date-alist 'da)
				    :data (to-universal-time date-alist 0)
				    :start (start tok)
				    :end (end tok)))))

	     ;; date range
	     (let ((dr-alist (date-range? ttext)))
	       (if dr-alist
		   (ignore-errors
		     (make-instance 'date-range-pattern
				    :yr1 (getf dr-alist 'yr1)
				    :mo1 (getf dr-alist 'mo1)
				    :da1 (getf dr-alist 'da1)
				    :yr2 (getf dr-alist 'yr2)
				    :mo2 (getf dr-alist 'mo2)
				    :da2 (getf dr-alist 'da2)
				    :data (to-universal-time
					   `(yr ,(getf dr-alist 'yr1)
						mo ,(getf dr-alist 'mo1)
						da ,(getf dr-alist 'da1))
					   0)
				    :start (start tok)
				    :end (end tok)))))
	     
	     ;; phone number
	     (multiple-value-bind (m? whole type type-base prefix acdum1 acdum2 ac1 ac2 exch num)
		 ;;(m? whole prefix prefix-root acdum1 acdum2 ac1 ac2 exch num)
		 (match-re *phone-pat* ttext)
	       (declare (ignore whole type prefix acdum1 acdum2))
	       (if m?
		   (ignore-errors
		     (make-instance 'phone-number-pattern
				    :phone-type type-base
				    :country 1	;assume USA/Canada
				    :area (or ac1 ac2)
				    :exch exch
				    :num num
				    :data (format nil "~@[1 (~a) ~]~a-~a"
						  (or ac1 ac2) exch num)
				    :start (start tok)
				    :end (end tok)))))

	     ;; basic patterns, including rel-pat
	     (let ((bp-alist (parse-basic-pat ttext)))
	       (if bp-alist
		   (ignore-errors
		     (make-instance 'numeric-data-pattern
				    :label (getf bp-alist 'label)
				    :reln (getf bp-alist 'rel)
				    :data (getf bp-alist 'num1) ;1st (main) number becomes data value
				    :conn (getf bp-alist 'conn)
				    :num2 (getf bp-alist 'num2)
				    :sep (getf bp-alist 'sep)
				    :units (getf bp-alist 'units)
				    :star (getf bp-alist 'star)
				    :sharp (getf bp-alist 'sharp)
				    :start (start tok)
				    :end (end tok)))))

	     ;; number with commas comma-delimited groups of three digits
	     (multiple-value-bind (m? whole)
		 (match-re *num-comma-pat* ttext)
	       (if m?
		   (ignore-errors
		     (make-instance 'numeric-data-pattern
				    :data (read-from-string (replace-re whole "," ""))
				    :start (start tok)
				    :end (end tok)))))
	     
	     ;; range-of-ratios, such as 140/95-120/72. Note that some
	     ;; patterns that look like this are actually date ranges, such
	     ;; as 1/13-15/07, which is 2-3 days in Jan 2007, or 11/8-11/10,
	     ;; which is the same span in Nov of some unspecified year. If
	     ;; it can be interpreted as a date range, we do that. Note that
	     ;; neither of such dates are not picked up by the
	     ;; date-range? function, which insists on a pattern such as
	     ;; 11/8/10-11/10/10 or abbreviated as 11/8-11/10/10, but will
	     ;; not allow the more condensed forms here.
	     (let* ((rng-rat (range-of-ratios? ttext))
		    (type (and rng-rat (car rng-rat)))
		    (alist (and rng-rat (cdr rng-rat))))
	       (cond ((null rng-rat) nil)
		     ((eq type 'date-range)
		      ;; range of dates, with no year; we encode the start
		      ;; date to a universal time of 1904, which was the
		      ;; first leap-year that is representable. This permits
		      ;; encoding of 2/29.
		      (ignore-errors
			(make-instance 'date-range-pattern
				       :yr1 (getf alist 'yr1)
				       :mo1 (getf alist 'mo1)
				       :da1 (getf alist 'da1)
				       :yr2 (getf alist 'yr2)
				       :mo2 (getf alist 'mo2)
				       :da2 (getf alist 'da2)
				       :data (to-universal-time
					      `(yr ,(getf alist 'yr1 1904)
						   mo ,(getf alist 'mo1)
						   da ,(getf alist 'da1))
					      0)
				       :start (start tok)
				       :end (end tok))))
		     ((eq type 'range-ratios)
		      (ignore-errors
			(make-instance 'range-of-ratios-pattern
				       :n1 (getf alist 'n1)
				       :d1 (getf alist 'd1)
				       :n2 (getf alist 'n2)
				       :d2 (getf alist 'd2)
				       :sharp (getf alist 'sharp)
				       :star (getf alist 'star)
				       :suffix (getf alist 'suffix)
				       :data (and (not (zerop (getf alist 'd1)))
						  (/ (getf alist 'n1) (getf alist 'd1)))
				       :start (start tok)
				       :end (end tok))))
		     (t (error "Unexpected type from range-of-ratios: ~s"
			       rng-rat))))
	     
	     ;; ratio-of-ranges -- matches items such as 1-2/5-7
	     ;; data becomes the ratio of the first numbers in numerator and denominator
	     (multiple-value-bind (m? whole n1 n2 d1 d2 sep units star sharp)
		 (match-re *ratio-of-ranges-pat* ttext)
	       (declare (ignore whole))
	       (if m?
		   (ignore-errors
		     (let ((nn1 (read-from-string n1))
			   (dn1 (read-from-string d1)))
		       (make-instance 'ratio-of-ranges-pattern
				      :n1 nn1
				      :n2 (read-from-string n2)
				      :d1 dn1
				      :d2 (read-from-string d2)
				      :sep sep
				      :units units
				      :star (not (empty? star))
				      :sharp (not (empty? sharp))
				      :data (and (numberp nn1) (numberp dn1)
						 (not (zerop dn1))
						 (/ nn1 dn1))
				      :start (start tok)
				      :end (end tok))))))

	     ;; number list -- matches a list of items separated by -, /, or
	     ;; ->, -->, --->, ...
	     ;; This pattern will match some tokens that also match the
	     ;; basic pattern, so that should be matched earlier.
	     (let ((nl-alist (numlist? ttext)))
	       (if nl-alist
		   (ignore-errors
		     (make-instance 'number-list-pattern
				    :nums (getf nl-alist 'nums)
				    :seps (getf nl-alist 'seps)
				    :data (car (getf nl-alist 'nums))
				    :start (start tok)
				    :end (end tok)))))
	     
	     ;; missed enumeration; there are many tokens that look
	     ;; like "3)Verapamil" because the space that belonged
	     ;; after the ) is missing.  We identify these as a kind
	     ;; of special case.
	     (multiple-value-bind (m? whole num text)
		 (match-re *enum-pat* ttext)
	       (declare (ignore whole))
	       (if m?
		   (ignore-errors
		     (make-instance 'missed-list-item-pattern
				    :num (read-from-string num)
				    :data text
				    :start (start tok)
				    :end (end tok)))))
	     )				;end of (or ...
	    ))
      (when new-ann
	(add-annotation (document tok) new-ann))
      )					;end of (let ...
    )					;end of (dolist ...
  (add-analysis d :ganalysis 'gnum-recognize)
  (when save (save d))
  )					;end of token-parse

(defun fix-phone-number-patterns ()
  (open-late-database)
  (let* ((problems (latesql "select id,document_id from annotations where type='phone-number-pattern'"))
	 (by-doc (partition problems :key #'cadr :test #'equal)))
    (dolist (dl by-doc)
      (let ((d (document (car dl))))
	(dolist (al (cdr dl))
	  (let ((a (find-annotation (car al) d)))
	    (multiple-value-bind (m? whole type type-base prefix acdum1 acdum2 ac1 ac2 exch num)
		;;(m? whole prefix prefix-root acdum1 acdum2 ac1 ac2 exch num)
		(match-re *phone-pat* (content a))
	      (declare (ignore whole type prefix acdum1 acdum2))
	      (assert m? ()
		      "phone-number-pattern failed to parse as a phone number: ~s"
		      (content a))
	      (setf (phone-type a) type-base)
	      (setf (country a) 1)
	      (setf (area a) (or ac1 ac2))
	      (setf (exch a) exch)
	      (setf (num a) num)
	      (setf (data a) (format nil "~@[1 (~a) ~]~a-~a"
				     (or ac1 ac2) exch num)))
	    (save a)
	    (print (data a))))))))


(defun discretize-pct (n)
  ;; (setf str (replace-re str "\\D" ""))
  (let* (str);; (n (parse-integer str)))
    (cond 
     ((>= n 90)
      (setf str "90"))
     ((>= n 80)
      (setf str "80"))
     ((>= n 70)
      (setf str "70"))
     ((>= n 60)
      (setf str "60"))
     ((>= n 50)
      (setf str "50"))
     ((>= n 40)
      (setf str "40"))
     ((>= n 30)
      (setf str "30"))
     ((>= n 20)
      (setf str "20"))
     ((>= n 10)
      (setf str "10"))
     (t
      (setf str "0")))
    str))

(defun discretize-pct-th (n th)
  (let* (str)
    (cond 
     ((and (>= th 50) (> n th))
      (setf str (format nil "~d_up" th)))
     ((and (<= th 50) (<= n th))
      (setf str (format nil "~d_down" th))))
    str))

(defun discretize-age (n)
  ;; (setf str (replace-re str "\\D" ""))
  (let* (str);; (n (parse-integer str)))
    (cond 
     ((>= n 90)
      (setf str "90"))
     ((>= n 80)
      (setf str "80"))
     ((>= n 70)
      (setf str "70"))
     ((>= n 60)
      (setf str "60"))
     ((>= n 50)
      (setf str "50"))
     ((>= n 40)
      (setf str "40"))
     ((>= n 30)
      (setf str "30"))
     ((>= n 20)
      (setf str "20"))
     ((>= n 10)
      (setf str "10"))
     (t
      (setf str "0")))
    str))
