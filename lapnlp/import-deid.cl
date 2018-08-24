;;; -*- Mode: Lisp; Package: late; -*-

#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation
Contains important utility to convert from XML based annotation to standoff annotations
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Programs to manipulate data from the i2b2 de-identification
;;;challenge of 2006. These are actual clinical data that have been
;;;de-identified already, but then "salted" with various forms of
;;;PHI.
;;;
;;; These data sets are in the form of XML files that start with a
;;;ROOT tag and are then followed by any number of RECORD elements,
;;;each of which contains a TEXT element. Within each TEXT element are
;;;the text of the note, interspersed with PHI tags that show the type
;;;of PHI.
;;;
;;; To import these data in a form consistent with standoff
;;;annotations, we read in the XML file with the option to preserve
;;;blanks, break up the data to create documents out of each
;;;individual record, and then concatenate all the actual text in the
;;;TEXT elements, stripping out the PHI markers.  However, we note the
;;;location of the PHI tags and use these as the start and end
;;;positions for our standoff annotation.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :late
  (:use :common-lisp #+allegro :excl :util)
  (:export "import-deid-corpus"))

(in-package :late)

(defun import-deid-corpus (filespec
			   &key
			   (spec nil)
			   (name nil)
			   (unix t)
			   (compress t)
			   (save t))
  "Imports an XML representation of a set of RECORDS (documents) that
 contain PHI tags, into the normal LATE standoff annotation
 style. Name names the new corpus, spec provides an XML file
 containing a grammar to parse sections of each document/record, unix,
 if true, says to convert DOS style line endings to Unix convention,
 and compress says to get rid of extra spaces inserted in tokenization
 of the deid corpus, to make it more like real text."

  (let* ((pn (pathname filespec))
	 (n (pathname-name pn))
	 (cn (or name n (symbol-name (gensym "corpus-"))))
	 (c (make-instance 'corpus :name cn))
	 (docs nil))
    (when spec (setf (description c) (car (read-xml spec))))
    (format t "~5Importing ~s to ~s" pn cn)
    (let ((data (car (read-xml pn :strip-blanks nil))))
      (assert (eq (car data) 'user::ROOT)
	  ()
	"Top-level XML entity must be ROOT, but is ~s"
	(car data))
      (dolist (rec (cdr data))		;iterate over all records
	(unless (stringp rec)
	  ;; Ignore top-level strings in the file.
	  (assert (and (eq (caar rec) 'user::RECORD)
		       (cdar rec) (cddar rec)
		       (eq (cadar rec) 'user::ID))
	      ()
	    "Record must start with a RECORD tag, but is ~s"
	    (car rec))
	  (let ((recnum (caddar rec))
		;; each record contains one TEXT element
		(text-elt
		 (find-if #'(lambda (e)
			      (and (listp e)
				   (eq (car e) 'user::TEXT)))
			  (cdr rec))))
	    (assert text-elt ()
	      "Record ~a does not contain a required TEXT element." recnum)
	    (do ((el (cdr text-elt) (cdr el))
		 (posn 0)
		 (text-fragments nil)
		 (annotations nil))
		((null el)
		 (multiple-value-bind (str ann)
		     (compress-artificial-spacing
		      (apply #'concatenate 'string
			     (nreverse text-fragments))
		      annotations
		      unix
		      compress)
		   (let ((doc (make-instance 'document
				:name recnum
				:source (namestring pn)
				:dirty t
				:size (length str)
				:content str)))
		     (dolist (a ann)
		       (let ((ann (make-instance 'phi-pseudonym
				    :document doc
				    :start (car a)
				    :end (cadr a)
				    :data (caddr a))))
			 (add-annotation doc ann)))
		     (cond (save
			    (save doc)
			    (push (id doc) (documents c)))
			   (t (push doc docs))))))
	      (cond ((stringp (car el))
		     (push (car el) text-fragments)
		     (incf posn (length (car el))))
		    ((and (listp (car el))
			  (eq (caaar el) 'user::PHI)
			  (eq (cadaar el) 'user::TYPE))
		     (let ((phi-type (caddar (car el)))
			   (phi (cadar el)))
		       (push phi text-fragments)
		       (push (list posn
				   (setq posn (+ posn (length phi)))
				   phi-type)
			     annotations))))))))
      (if save (save c) (nreverse docs)))))

;;; Notes on eliminating the extra spaces inserted by Tawanda's pre-processing:
;;; Any pattern of <space>[,.;:], flush space.
;;; Pattern " \w / \w " should have the spaces around / stripped.

(defun compress-artificial-spacing (text annotations unix compress)
  "Eliminates spaces before commas, periods, colons and semi-colons,
  and around / in abbreviations such as S/P or w/o.  These were
  introduced in the pre-processingn of the i2b2 challenge data but are
  not realistic in actual text. The triple representation of
  annotations (start end type) are adjusted to account for these
  deletions."
  (if (and (not unix) (not compress))
      (values text annotations)
    (let ((posn 0)
	  (flush nil)
	  (fragments)
	  (flushed 0)
	  (punct (compile-re " [.,:;](\\s|$)"))
	  #|(slashed (compile-re "\\s\\w\\s/\\s\\w\\s"))
	  (slashed1 (compile-re "\\s\\w\\s/\\s\\S\\S"))|#
	  (slash (compile-re "\\w / \\w"))
	  (apos (compile-re "\\s'"))
	  #|(parens (compile-re "\\s\\(\\s.+?\\s\\)"))|#
	  (space-par (compile-re "\\s\\)"))
	  (par-space (compile-re "\\(\\s"))
	  (plural (compile-re "\\s\\(\\s*[sS]\\s*\\)"))
	  (dos-return (compile-re "\\r$" :multiple-lines t)))
      (labels
	  ((rip (pattern offsets)
	     (setq posn 0)
	     (loop (multiple-value-bind (m? where)
		       (match-re pattern text
				 :start posn :return :index)
		     (unless m? (return))
		     (dolist (o offsets)
		       (pushnew (+ o (car where)) flush
				:test #'=))
		     (setq posn (cdr where))))))
      
	(when compress
	  ;; every period, comma, colon or semicolon preceded by space
	  ;; if also followed by space or end of text.
	  (rip punct '(0))
	  ;; find every / expression of the form w / o or s / p .
	  ;;(rip slashed '(2 4))
	  ;; find every / expression of the form w/ where it is not
	  ;; followed by a single character, and just get rid of the space
	  ;; between w and /.
	  ;;(rip slashed1 '(2))
	  ;; delete spaces around /
	  (rip slash '(1 3))
	  ;; delete space before apostrophe
	  (rip apos '(0))
	  ;; delete spaces just inside ( ... )
	  (rip space-par '(0))
	  (rip par-space '(1))
	  ;; 
	  (rip plural '(0))
	  #|
	  (setq posn 0)
	  (loop
	    ;; find all expressions of the form " ( x...z )" and get rid of
	    ;; the spaces inside the parentheses. If x...z is just s or
	    ;; S, then also flush the space before the (.
	    (multiple-value-bind (m? where)
		(match-re parens text :start posn :return :index)
	      (unless m? (return))
	      ;; if content was just an s or S, eliminate previous space.
	      (when (and (= 6 (- (cdr where) (car where)))
			 (char-equal (char text (+ 3 (car where))) #\S))
		(push (car where) flush))
	      (push (+ 2 (car where)) flush)
	      (push (- (cdr where) 2) flush)
	      (setq posn (cdr where))))|#)
	(when unix
	  (rip dos-return '(0))))
      (setq posn 0)
      (setq flush (sort flush #'<))	;put them in ascending order
      ;; create the fragments remaining after we flush all the extra
      ;; spaces
      (dolist (f flush (push (subseq text posn) fragments))
	(push (subseq text posn f) fragments)
	(setq posn (1+ f)))
      (setq fragments (nreverse fragments))
      ;; adjust the annotation spans to account for the shortened text
      (setq annotations (sort annotations
			      #'(lambda (x y)
				  (or (< (car x) (car y))
				      (and (= (car x) (car y))
					   (< (cadr x) (cadr y)))))))
      (dolist (fl flush)
	(let ((f (- fl flushed)))
	  (dolist (a annotations)
	    (when (< f (car a)) (decf (car a)))
	    (when (< f (cadr a)) (decf (cadr a))))
	  (incf flushed)))
      (let ((str (apply #'concatenate 'string fragments)))
	(values str annotations)))))
