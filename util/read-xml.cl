;;; -*- Mode: Lisp; Package: util; -*-

#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 02/20/2009 creation
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :regexp2)
  (require :pxml))

(defpackage :util
  (:use :common-lisp #+allegro :excl)
  (:export "read-xml"
	   "*trimchars*"
	   "lxml-strip-blanks"
	   "allwhite"
	   "for-lxml"
	   "for-lxml-h"
	   "lxml-match-head"
	   "lxml-part"
	   "lxml-get1"
	   "xml-head"
	   "xml-attrs"
	   "xml-parts"
	   "*xml-namestartchars*"
	   "*xml-nameotherchars*"
	   "*xml-namechars*"
	   "xml-name?"
	   "print-xml"
	   "print-xml-f"
	   "attr-compat"
	   "lpath"))

(in-package :util)

(defun read-xml
    #+common-graphics
    (&optional 
     (fn (ask-user-for-existing-pathname 
	  "Choose XML file to read"
	  :allowed-types '(("XML file" . "*.xml") ("All files" . "*.*"))))
     &key (content-only t) (strip-blanks t) (package *package*))
    #-common-graphics
    (fn &key (content-only t) (strip-blanks t) (package *package*))
    "Reads a file as a list of XML expressions. Symbols read are in
     the current package."
  (let* ((*package* (find-package package))
	 (x (with-open-file (f fn :direction :input :element-type 'base-char)
	      (net.xml.parser:parse-xml f :content-only content-only))))
    (if strip-blanks
	(mapcar #'lxml-strip-blanks x)
      x)))

(defmethod net.xml.parser:parse-xml ((f pathname) &rest args)
  (with-open-file (file f :direction :input)
    (apply #'net.xml.parser:parse-xml file args)))

(defun read-html (fn &key (content-only t) (strip-blanks t) (package *package*))
  (let* ((*package* (find-package package))
	 (x (net.html.parser:parse-html
	     fn
	     :eliminate-blank-strings strip-blanks
	     :parse-entities t)))
    (when strip-blanks
      (setq x (mapcar #'lxml-strip-blanks x)))
    (if content-only
	(cadr x)
      x)))

(defvar *trimchars*
    '(#\Space #\Tab #\Return #\Newline #\Page #\Rubout #\Backspace))

(defun lxml-strip-blanks (lxml)
  "Strips purely blank elements out of an lxml structure."
  ;; Remember, an xlml structure consists of a list of expressions,
  ;; where the first is interpreted as the XML header element and the
  ;; rest as its body.  The elements are either atoms, in which case
  ;; they are simply printed to the output stream, or sublists, in
  ;; which case each is interpreted as another lxml expression. If the
  ;; head of an lxml expression is itself a list, that is treated as a
  ;; specification of a head with properties.
  (labels ((strip (lxml-or-atom)
	     ;; Recursively strip out blanks except in the heads,
	     ;; where property values may remain blank. This returns a
	     ;; list of the remaining results, which are combined by
	     ;; mapcan.
	     (cond ((consp lxml-or-atom)
		    ;; it's an lxml expression
		    (list
		     (cons (car lxml-or-atom)
			   (mapcan #'strip (cdr lxml-or-atom)))))
		   ((allwhite lxml-or-atom) nil)
		   (t (list lxml-or-atom)))))
    ;; Get rid of the extra level of list created by strip at top
    ;; level.
    (car (strip lxml))))

(defun allwhite (item)
  (and (stringp item)
       (every #'(lambda (char) (member char *trimchars* :test #'char=))
	      item)))

(defun print-xml (lxml &optional (stream t) (indent 0) &aux (head nil))
  (cond ((null lxml))
        ((not (consp lxml))
	 ;; This is just data
         (format stream " ~a" (escape-xml (princ-to-string lxml))))
	(t ;; This is an XML expression, so sort out the cases.
	 (setq head (if (consp (car lxml)) (caar lxml) (car lxml)))
	 (assert (xml-name? head)
	     (head)
	   "Invalid XML name as header of lxml expression: ~s"
	   lxml)
	 ;; If there is a body, then we need a </foo> at the end; otherwise, just use <foo/>
	 (format stream "~%~v,0T<~a" indent head)
	 (when (consp (car lxml))	;there are properties
	   (do ((pl (cdar lxml) (cddr pl)))
	       ((null pl))
	     (format stream " ~a=\"~a\""
		     (car pl)
		     (escape-xml (cadr pl)))))
	 (cond ((cdr lxml)
		;; There is a body
		(format stream ">")
		(dolist (item (cdr lxml))
		  (print-xml item stream (+ indent 2)))
		(format stream "~%~v,0T</~a>" indent head))
	       (t ;; No body
		(format stream "/>"))))))

(defun escape-xml (string)
  (replace-re
   (replace-re 
    (replace-re string "&" "&amp;")
    ">" "&gt;")
   "<" "&lt;"))

#| W3C spec for XML names:
NameStartChar	   ::=   	":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
NameChar	   ::=   	NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
Name	   ::=   	NameStartChar (NameChar)*
|#

(defparameter *xml-namestartchars*
    "A-Za-z_:\\x{C0}-\\x{D6}\\x{D8}-\\x{F6}\\x{F8}-\\x{2FF}\\x{370}-\\x{37D}\\x{37F}-\\x{1FFF}\\x{200C}-\\x{200D}\\x{2070}-\\x{218F}\\x{2C00}-\\x{2FEF}\\x{3001}-\\x{D7FF}\\x{F900}-\\x{FDCF}\\x{FDF0}-\\x{FFFD}"
  "W3C's definition of characters that can start an XML name. Omits
   \\x{10000}-\\x{EFFFF} because ACL only supports 16-bit characters.")

(defparameter *xml-nameotherchars*
    "-.0-9\\x{B7}\\x{0300}-\\x{036F}\\x{203F}-\\x{2040}"
  "W3C's definition of additional characters that may appear in an XML name.")

(defparameter *xml-namechars*
    (concatenate 'string
      *xml-nameotherchars*
      *xml-namestartchars*))

(defparameter *xml-name-re*
    (compile-re (concatenate 'string
		  "^[" *xml-namestartchars* "][" *xml-namechars* "]*$")))

(defun xml-name? (string-or-symbol)
  (match-re *xml-name-re* (string string-or-symbol)))

#+broken
(defun print-xml-f (lxml &optional (stream t))
  (cond ((null lxml))
	((not (consp lxml))
	 (format stream "~a" lxml))
	(t (let ((attrs? (consp (car lxml))))
	     (format stream
		     ;;"~<<~w~{~:_~w~_=~_\"~w\"~}~:[/~;~]>~_~w~:>"
		     "<~w ~{~w=~s~}>"
		     (if attrs? (caar lxml) (car lxml))
		     (if attrs? (cdar lxml) nil)
		     (cdr lxml))))))

(defun for-lxml (fn lxml-expr)
  "Calls fn on each element of the lxml expression.  Fn has two arguments:
   (1) list of xml tags to this element, in reverse order;
   (2) value associated with the tag."
  (labels ((inner (expr path)
	     ;; this must be a sub-list
	     (let ((tag (car expr)))
	       (cond ((consp tag)
		      (push (car tag) path)
		      (do ((al (cdr tag) (cddr al))
			   (attr)
			   (val))
			  ((null al) nil)
			(setq attr (car al)
			      val (cadr al))
			(funcall fn (cons attr path) val)))
		     (t (push tag path)))
	       (dolist (part (cdr expr))
		 (if (consp part)
		     (inner part path)
		   (funcall fn path part))))))
    (inner lxml-expr nil)))

(defun for-lxml-h (fn lxml-expr)
  "Calls fn on each element of the lxml expression.  Fn has three arguments:
   (1) list of xml tags to this element, in reverse order;
   (2) value associated with the tag.
   (3) list of higher-level expressions of which the current one is part."
  (labels ((inner (expr path history)
                  ;; this must be a sub-list
                  (let ((tag (car expr)))
                    (cond ((consp tag)
                           (push (car tag) path)
                           (do ((al (cdr tag) (cddr al))
                                (attr)
                                (val))
                               ((null al) nil)
                             (setq attr (car al)
                                 val (cadr al))
                             (funcall fn (cons attr path) val (cons expr history))))
                          (t (push tag path)))
                    (dolist (part (cdr expr))
                      (if (consp part)
                          (inner part path (cons expr history))
                        (funcall fn path part (cons expr history)))))))
    (inner lxml-expr nil nil)))

(defun lxml-match-head (x s)
  (and (consp x)
       (or (eq (car x) s)
           (and (consp (car x))
                (eq (caar x) s)))))

(defun lxml-part (lxml-expr spec &key (include-substructure nil))
  "Extracts the 'path' specified by SPEC from LXML-expr. If the keyword argument
:include-substructure is false (default), then only strings at this level are returned."
  (labels
      ((inner (el sp)
	 ;; Gets a list of body of current expression in EL, remaining SP spec
	 ;; of where to walk down to.
	 ;;(format t "~% el=~s,~%***sp=~s" el sp)
	 (if (null sp)
	     ;; Return everything at this level unless :include-substructure false
	     (if include-substructure
		 (append el nil)
	       (remove-if-not #'stringp el))
	   (let ((ans nil))
	     (dolist (e el)
	       (when (lxml-match-head e (car sp))
		 (setq ans (nconc ans (inner (cdr e) (cdr sp))))))
	     ans))))
    (unless (lxml-match-head lxml-expr (car spec))
      (format *error-output*
	      "~%LXML-EXPR mismatch: ~s vs. ~s."
	      (car spec) lxml-expr))
    (inner (cdr lxml-expr) (cdr spec))))

(defun lxml-get1 (expr spec)
  "Gets the leftmost instance of SPEC in the EXPR."
  (cond ((null spec) expr)
        ((not (consp expr)) nil)
        ((consp (car expr)) t) ;;I'm lazy
        (t (dolist (sub (cdr expr))
             (when (lxml-match-head sub (car spec))
               (return (lxml-get1 sub (cdr spec))))))))

(defun xml-head (xml-expr)
  (and (consp xml-expr)
       (let ((head (car xml-expr)))
	 (if (consp head) (car head) head))))

(defun xml-attrs (xml-expr)
  (and (consp xml-expr)
       (let ((head (car xml-expr)))
	 (if (consp head) (cdr head) nil))))

(defun xml-parts (xml-expr)
  (and (consp xml-expr)
       (cdr xml-expr)))

#|
We define a facility to permit searching an xml/xhtml expression
represented as a Lisp lxml expression for paths. A path is a sequence
of specifications, which are searched for within the input expression
in a left-to-right depth-first manner.  A step on a path is
represented in one of the following ways:
1. A keyword symbol, which is to match the xml-head of the current
level of the expression being searched.
2. A 2-list consisting of a specification and a number. This means to
descend the number-th instance of that symbol in the expression.
3. A list with an odd number of elements. The first is a symbol, and
the rest are a specification of attributes to match on that symbol, as
pairs of entries. Each contains a symbol that names an attribute and a
value that should equal the given value.  NIL means any value is OK,
if that attribute is present.

|#

(defun attr-compat (xml-expr spec)
  "Determines whether the heads match and the attributes given in spec
  are all present and have the desired values in xml-expr. Note that
  ((:table :border 1)) would match :table, (:table :border 1) or (:table :border nil)"
  (and (eq (xml-head xml-expr) (if (atom spec) spec (car spec)))
       (do ((ss (and (consp spec) (cdr spec)) (cddr ss)))
	   ((null ss) t)
	 (let ((a (car ss))
	       (v (cadr ss)))
	   (format t "~%Test ~a=~a" a v)
	   (unless
	       (do ((as (xml-attrs xml-expr) (cddr as)))
		   ((null as) nil)
		 (if (and (eq (car as) a)
			  (or (null v)
			      (equal (cadr as) v)))
		     (return t)))
	     (return nil))))))

(defun lpath (expr spec)
  "Search expr for the path given in spec, and return the first
  found. Search is left-to-right, top-down, depth-first. Expr must be
  a single lxml expression, not a list of them, though of course it
  will generally have a list of subexpressions. Note that we cannot
  distinguish by the returned value between finding a path with no
  further subexpressions or not finding it at all, both of which yield
  NIL.  Therefore, we return a second value that indicates success or
  failure."
  (labels
      ((find-it (f l)
	 (dolist (x l)
	   (let ((v (funcall f x)))
	     (when v (return v)))))
       (inner (old-e el sp)
	 ;; el is a list of lxml-expr's that must match the next spec
	 ;; in sp; if one does, we search for its subexpressions that
	 ;; match the rest of sp. skip tells us to look for the n+1 th
	 ;; match, not the first. Old-e is the last thing matched, in
	 ;; case sp is exhausted.
	 (if (null sp)
	     old-e
	   (let* ((s (car sp))
		  (counting? (and (consp s) (= 2 (length s))))
		  (targ (if counting? (car s) s))
		  (skip (if counting? (cadr s) 0)))
	     ;;(format t "~%Match ~a in ~s" targ el)
	     (find-it #'(lambda (e)
			  (if (attr-compat e targ)
			      (cond ((zerop skip)
				     (inner e (xml-parts e) (cdr sp)))
				    (t (decf skip) nil))
			    (inner old-e (cdr el) sp)))
		      el)))))
    (cond ((null spec) expr)
	  ((attr-compat expr (car spec))
	   (inner expr (xml-parts expr) (cdr spec)))
	  (t nil))))

	 
