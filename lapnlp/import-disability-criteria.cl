;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Program to read a section of the Social Security Administration's
;;;"Blue Book", which defines disability criteria.  I assume these are
;;;extracted from 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :aserve)
  (require :uri)
  (require :phtml))

(defpackage :late
  (:use :common-lisp
	#+allegro :excl
	#+allegro :net.aserve.client
	#+allegro :net.html.parser
	:util)
  (:export "import-disability-criteria"
	   "get-html-page"))

(in-package :late)

(defparameter *ssa-cv*
    "http://www.ssa.gov/disability/professionals/bluebook/4.00-Cardiovascular-Adult.htm"
  "URL of Adult Cardiovascular disability definitions.")

#|
Once we strip out the surrounding HTML for headers, navigation,
footers, etc., we are left with a relatively flat list of html <p>
sections. These are actually meant to represent a hierarchy of
condition descriptions, which are determined from the markers at the
start of each line (paragraph).  The hierarchy seems to be as follows:

4.01
A.
1.
a.
(i)

There are also a VERY few paragraphs that do not begin with one of
these markers.  They seem to be only "AND" and "OR", indicating the
logic of criteria used to assess various disability conditions.

Our parse of this html also shows huge numbers of <b> tags, many with
absolutely no content.  We simply ignore these. By contrast, <em> tags
seem more worth retaining, but for now we also flush those.
|#

(defun get-html-page (&optional (uri *ssa-cv*))
  (multiple-value-bind (body response-code headers accessed-uri)
      (do-http-request uri)
    (declare (ignore headers accessed-uri))
    (unless (equal response-code 200)
      (error "Return code (~a); Unable to access URI: ~a"
	     response-code uri))
    (let* ((x (mapcar #'lxml-strip-blanks
		      (parse-html body
				  :eliminate-blank-strings t
				  :parse-entities t)))
	   (y (lpath (cadr x)
		     '(:html :body (:table 2) :tr (:td 1) (:table 1) :tr (:td 1))))
	   (c (xml-parts y)))
      c)))

(defun un-b (lxml)
  "Returns a copy of lxml with extraneous (all) :b ... tags removed."
  (cond ((atom lxml) (list lxml))
	((member (car lxml) '(:b :em :strong :i :u))
	 (mapcan #'un-b (cdr lxml)))
	((member (xml-head lxml) '(:a))
	 (mapcan #'un-b (xml-parts lxml)))
	((eq (xml-head lxml) :p)
	 (let ((body
		(delete :br (mapcan #'un-b (xml-parts lxml)))))
	   (if (every #'stringp body)
	       (list (list :p (join body)))
	     (list (cons :p body)))))
	((eq (xml-head lxml) :span)
	 nil)
	(t (list (mapcan #'un-b lxml)))))

(defparameter *part-pat*
    (compile-re "^\\s*(\\d+\\.\\d+)\\s+(.*)\\s*$"))
(defparameter *sec-pat*
    (compile-re "^\\s*([A-Z]\\.)\\s+(.*)\\s*$"))
(defparameter *subsec-pat*
    (compile-re "^\\s*(\\d+\\.)\\s+(.*)\\s*$"))
(defparameter *subsubsec-pat*
    (compile-re "^\\s*([a-z]\\.)\\s+(.*)\\s*$"))
(defparameter *subsubsubsec-pat*
    (compile-re "^\\s*(\\([ivxl]+\\))\\s+(.*)\\s*$"))

(defparameter *part-hier*
    '(:part :sec :subsec :subsubsec :subsubsubsec))

(defun import-ssa (&optional (uri *ssa-cv*))
  (multiple-value-bind (m? whole name)
      (match-re "/[^/]*\\.htm$"
		(if (net.uri:uri-p uri) (net.uri:uri-path uri) uri))
    (declare (ignore whole))
    (let* ((docname (if m? name (symbol-name (gensym "ssa-"))))
	   (c (corpus "ssa"))
	   (lines (mapcar #'cadr (car (un-b (get-html-page uri)))))
	   (lns (mapcar #'(lambda (l) (replace-re l "\\s+" " "))
			lines))
	   (lni (mapcar
		 #'(lambda (l)
		     (or
		      (multiple-value-bind (m? whole ind txt)
			  (match-re *part-pat* l)
			(declare (ignore whole))
			(and m? (list :part ind txt)))
		      (multiple-value-bind (m? whole ind txt)
			  (match-re *sec-pat* l)
			(declare (ignore whole))
			(and m? (list :sec ind txt)))
		      (multiple-value-bind (m? whole ind txt)
			  (match-re *subsec-pat* l)
			(declare (ignore whole))
			(and m? (list :subsec ind txt)))
		      (multiple-value-bind (m? whole ind txt)
			  (match-re *subsubsec-pat* l)
			(declare (ignore whole))
			(and m? (list :subsubsec ind txt)))
		      (multiple-value-bind (m? whole ind txt)
			  (match-re *subsubsubsec-pat* l)
			(declare (ignore whole))
			(and m? (list :subsubsubsec ind txt)))
		      (list :plain nil l)))
		 lns))
	   (levels (list :part nil docname)))
      ;; We create a hierachic structure from the paragraphs in lni
      ;; The structure we build is a tree, where at the top level are
      ;; the successive parts, then each of those has a list of
      ;; sections, each of which has a list of subsections, etc.
      ;; The levels structure holds a list of the parents of the
      ;; current element we process, in reverse order. We also push
      ;; the elements at each level in reverse order.
      (dolist (l lni)
	(cond ((eq (car l) :plain)
	       ;; Don't change levels, just push it.
	       (push (caddr l) (cdddr (car levels))))
	      ((eq (car l) (caar levels))
	       ;; Same level as previous one
	       (push l (cdddr (car levels))))
	      (t 
	       (let* ((hierx (member (caar levels) *part-hier*))
		      (hier (and hierx (cdr hierx)))
		      (target-down (member (car l) hier))
		      (target-up (member (car l) *part-hier*)))
		 (cond (target-down
			;; The next line is below the previous. Move
			;; or create new levels
			)))))))))
