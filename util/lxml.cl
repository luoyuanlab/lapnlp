;;; -*- Mode: Lisp; Package: net.xml.parser; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation
|#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :pxml))

(defpackage :net.xml.parser
  (:export "print-xml" "read-xml" "strip-blanks" "allwhite" "for-lxml" "for-lxml-h" "lxml-part")) 

(in-package :net.xml.parser)

#|
LXML utilities

LXML is a Lisp-ized form of XML, defined by the pxml files in Allegro CL.
|#

(defun print-xml (lxml &optional (stream t) (indent 0))
  (cond ((null lxml))
        ((not (consp lxml))
	 ;; no sub-structure
         (format stream "~a" lxml))
        ((not (consp (car lxml)))
	 ;; an XML header, but without attributes
         (format stream "~%~v,0T<~a>" indent (car lxml))
         (dolist (item (cdr lxml))
           (print-xml item stream (+ indent 2)))
         (format stream "</~a>" (car lxml)))
        (t
	 ;; an XML header, with attributes
         (format stream "~%~v,0T<~a~{ ~a=\"~a\"~}>"
		 indent
		 (caar lxml)
		 (cdar lxml))
         (dolist (item (cdr lxml))
           (print-xml item stream (+ indent 2)))
         (format stream "~%</~a>" (caar lxml)))))

(defun read-xml 
    #+common-graphics
  (&optional 
   (fn (ask-user-for-existing-pathname 
	"Choose XML file to read"
	:allowed-types '(("XML file" . "*.xml") ("All files" . "*.*"))))
   &key content-only)
  #-common-graphics
  (fn &key content-only)
  (with-open-file (f fn :direction :input :element-type 'base-char)
    (net.xml.parser:parse-xml f :content-only content-only)))

(defmethod net.xml.parser:parse-xml ((f pathname) &rest args)
  (with-open-file (file f :direction :input)
    (apply #'net.xml.parser:parse-xml file args)))

(defparameter *trimchars*
    '(#\Space #\Tab #\Newline #\Page #\Return))

(defun strip-blanks (lst)
  "Strips purely blank elements out of an lxml structure."
  (cond ((null lst) nil)
        ((not (consp lst))
         lst)
        (t (do ((ans nil)
                (l lst (cdr l)))
               ((null l) (nreverse ans))
             (if (not (and (stringp (car l))
                           (allwhite (car l))))
                 (push (strip-blanks (car l)) ans))))))

(defun allwhite (string)
  (every #'(lambda (char) (member char *trimchars* :test #'char=))
         string))

#|
Allegro's pxml module represents XML expressions in list structure.
The three valid types of elements in this list structure are:
1. A character string, representing content
2a. A list representing an XML tag (symbol), possibly followed by content.
2b. Like 2a, except the first element is a list of the XML tag and its
cdr is a property list of its attributes.
3. To represent metadata such as XML comments and processing instructions,
etc. These are suppressed when parse-xml is called with :content-only t.

parse-xml yields a list of top-level xml expressions in the input. We are
often interested just in the first, of type 1 or 2.

We define an xpath-like facility to search through XML expressions represented
as above. There are several orthogonal variations on its use:

1. one[nil]: Input is a single pxml expression vs. a list of such
2. all[nil]: Find all matching elements vs. just the first in a left-traversal
3. sub[t]: Return the entire content of the found element(s) vs. just the
string(s).

The path is a list of symbols or sublists that must match. If a sublist,
then it specifies the symbol and attributes/values as in a pxml expression.
* matches anything, and ** matches any sequence.

ALAS, this implementation is not yet complete.
|#

#|
(defun xpath (object path
	      &key (one nil) (all nil) (sub t))
  (xpath0 (if one (list object) object)
	  path
	  nil				;ans
	  all
	  sub))

(defun xpath0 (objlist path ans all sub)
  (if (null path)
      (if all objlist (car objlist))
    (dolist (o objlist)
      (let ((spec (car path)))
	(cond ((eq spec '*)
	       (xpath0 (cdr o) (cdr path) ans all sub))
	      ((eq spec '**)
	       (let ((substructs (xpath o (cdr path) ans all sub)))
		 (if substructs 
|#

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
   (3) list of higher-level expressions of which the current one is part.
   This is like for-lxml, but adds the third argument."
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
  (labels ((inner (el sp)
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

(provide :lxml)
