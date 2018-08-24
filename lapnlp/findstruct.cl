;;; -*- Mode: Lisp; Package: late -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 04/22/2012 added section-content-stats
yluo - 04/18/2012 added section-content
yluo - 11/27/2011 added sec_name to capture section names from bindings
added semi_colon_lists to parse lists separated by ;
yluo - 05/24/2010 introduce other_section and in_sec_sep, other_section receives                  lowest priority.
yluo - 05/19/2010 should allow a pattern to match multiple places. 
yluo - 05/18/2010 added whole-pat to specify whether the pattern xml attr is
for whole section matching
yluo - 05/18/2010 modify sanity check in parse-struct to handle contained
secctions
psz  -            creation 

This code uses a specification of the expected section headers in a
document to annotate the document according to that structure.  The
spec gives a hierarchical decomposition of the document, where at
every level there may be an arbitrarily ordered set of substructures,
each with its own possible further substructure. The structure
annotations will generally, therefore, span text that may contain
further such annotations.

Specifications for structures are written in XML and read into a
Lispified notation called pxml, where each XML expression is
represented by an s-expr whose head is the XML tag and whose other
elements are the successive elements of the XML structure. If the head
has attributes, then it is represented not by a symbol but by a list
whose head is the symbol and the rest of which is a property list.

The top level element is expected to have a number of attributes that
define parameters for matching, including those to be passed to
match-re, standard prefixes and suffixes to look for in section and
subsection headers, whether and how lists are to be recognized, etc.
The options and their defaults are given below.

In the simplest case, such as
<report><history/><physical_exam/>...</report>
we assume that the entire text of a document matches the top-level
structure (in this case, report), and then look for headers such as
history, physical_exam, etc. within that. If there is further
sub-structure under these, we look for that sub-structure within each
element. When "_" occurs in a header, we actually allow any sequence
of spaces to match at that point.

Because of variations in documents, we also allow an
alt=... parameter, which gives a vertical-bar-separated list of
alternative versions of the header.  For example, physical_exam might
also be "PHYSICAL EXAMINATION|ADMISSION PHYSICAL EXAMINATION|". In
these, space, not underscore, stands for a sequence of spaces.  In
addition, we provide two other ways to specity headers:
1. If a start_pattern attribute is given, it is taken to be the regular
expression with which we are to search for the start of this pattern.
2. If a pattern attribute is given, it should match not simply the start
of a section, but the entire section. In this latter case, we also
bind any matching groups to the vertical-bar-separated attributes
given by the attrs attribute of the pattern.

Each annotation contains a list of the hierarchic substructures to
which it belongs (in reverse order). In addition, if list structure is
to be parsed, each list item annotation also includes the numeric list
header.

|#

(defpackage :late
  (:use :common-lisp #+allegro :excl :util)
  (:export 
   "*findstruct-debug*" 
   "find-pattern"
   "findstruct" 
   "note-in-final-diagnosis"
   "section-content"
   "section-content-stats"
   "section-stats"
   "addendum-star-section-stats"
   "sec-check"
   "doc-mrn"
   ))

(in-package :late)

(defmethod findstruct ((corp corpus)
		       &key (spec nil) (flush t))
  (dolist (d (documents corp))
    (findstruct (document d)
		:spec (or spec (description corp))
		:flush flush)))

(defmethod findstruct ((doc document)
		       &key (spec nil) 
		       (flush t))

  "Annotates DOC with structure segment boundaries as specified by spec.
spec is a pxml expression (i.e., an XML expression encoded as an s-expr)
as it is read by (car (read-xml ..)). We pay attention only to the first 
expression; an XML file might in fact contain many top-level expressions.
Also, we assume the file has been read with :content-only t, so that 
meta-data are already elided.
   The top level expression specifies the top-level annotation for the
document, and it carries in its properties global matching options, as
described at
http://medg.csail.mit.edu/projects/text/findstruct/FindStruct_doc.html
   Its sub-expressions then define the structure we expect to find in DOC,
as described in the same document.
   Note that some of the regular expression matching options available in
Java are not supported in the Lisp REGEX package. In particular, of the
first six options, canon_eq and unix_lines and comments are ignored.
"
  ;;  (format t "~&in findstruct~%")
  (let* ((global-options
	  (list 'case_insensitive nil
		'canon_eq t
		'comments nil
		'dotall nil
		'multiline t
		'unix_lines nil
		'prefix ""
		'suffix ""
		'lists nil
		'listpat "\\d{1,2}"		;one or two digits
		'listprefix "^\\s+"		;start of line, was \\s^
		'listsuffix "[):.]\\s+"	;forms: 1) 2: 3.
		'listlastitemend "^^"	;blank line, usually need to customize!
		'semi_colon_lists nil
		'proc nil
		'list_proc "narrative"
		))
	 (text (content doc))
	 (the-spec (or spec
		       (find-if-not
			#'null
			(mapcar #'description (corpora doc)))))
	 (opts (merge-options the-spec global-options))
	 (proc (getf (xml-attrs the-spec) 'proc)))
    (when flush
      (flush-annotations doc :type 'section-annotation))
    (assert (not (null the-spec))
	    ()
	    "Findstruct must be given a pxml structure specification or must obtain it from the corpus of the document ~s."
	    doc)
    (assert (null (document-annotations doc))
	    ()
	    "Document ~s already has a document-annotation. Flush-annotations if you want to re-annotate sections in it."
	    doc)
    (let* ((doc-annotation
	    (make-instance 'document-annotation
			   :document doc
			   :start 0
			   :end (length text)
			   :data (format nil "~a" (xml-head the-spec))
			   :processor proc)))
      (add-annotation doc doc-annotation)
      (parse-struct doc
		    text
		    0
		    (length text)
		    (xml-parts the-spec)
		    opts
		    doc-annotation)
      (add-analysis doc :analysis  "sectionize")
      )))

(defstruct part-match
  (start nil :type integer)
  (match-end nil :type integer)		;where header or pattern ended
  (end nil :type integer)			;where entire element ends
  bindings
  hier
  spec
  opts)

(defparameter *findstruct-debug* nil
  "Flag to turn on copious printing as findstruct works. Useful to
  debug XML specifications.")


(defun other-sectionp (match-part)
  (string-equal "Other_Section"	(part-match-hier match-part)))
;; At a certain level, we impose that there are certain required sections
;; if none of the others are present, it got to be the default - Yuan


(defun parse-struct (doc text start end parts-spec options hierarchy)
  "Parses text[start, end] by the pattern of structures specified by spec.
This inherits options from outer specs and hierarchy contains the h-up
pointer for hierachical decomposition."
;;;  (when *findstruct-debug*
;;;    (format t "~&parse-struct ~a~%~a-~a~%~a~%" (name doc) start end parts-spec))
  ;; (format t "parse-struct: ~a~%" hierarchy)

  (let* (matched-parts opts whole-pat pat)
    ;;(break "start to match")
    (dolist (p parts-spec)
      (setf opts (merge-options p options)
	    whole-pat (and (getf (xml-attrs p) 'whole) ; whether it's whole?
			   (string-equal "yes" (getf (xml-attrs p) 'whole)))
	    pat (getf (xml-attrs p) 'pattern))
      (when *findstruct-debug*
	(format t "~%Now matching ~a~%" p))
      (let* ((head (xml-head p)))	

	(when *findstruct-debug*
	  (format t "~% with options=~s" opts))
	(do* ((mstart start mend) ; do not do (1+ anymore
	      (mend start))
	    ((>= mstart end))
	  (when *findstruct-debug* 
	    (format t "~&mstart: ~a, mend: ~a~%" mstart mend))
	  (cond
	   (pat				  ;; if there is an explicit pattern
	    (let* ((attrs (split-re
			   "\\|"
			   (or (getf (xml-attrs p) 'attrs) "val")))
		   (res (multiple-value-list
			 (match-re pat text
				   :start mstart 
				   :end end
				   :return :index
				   :case-fold (getf opts 'case_insensitive)
				   :multiple-lines (getf opts 'multiline)
				   :single-line (getf opts 'dotall)))))
	      (cond
	       ((and res (car res))
		(let* ((head-attr-pos (position "heading" attrs 
						:test #'string-equal))
		       head-range
		       matched-part
		       hstart
		       hend
		       bindings)
		  (setf mend (cdadr res))
		  (when head-attr-pos
		    (setq head-range (elt (cddr res) head-attr-pos)))
		  (when (consp head-range)
		    (setf hstart (car head-range))
		    (setf hend (cdr head-range)))
		  (setf bindings (mapcar #'(lambda (attr range)
					     (cons attr
						   (subseq text
							   (car range)
							   (cdr range))))
					 attrs
					 (cddr res)))
		  (setf matched-part 
			(make-part-match
			 :start (or hstart (caadr res))
			 :match-end (or hend (cdadr res))
			 :end (and whole-pat (cdadr res))
			 :bindings bindings
			 :hier head
			 :spec p
			 :opts opts))
		  (when *findstruct-debug*
		    (format t "~&Matched: start ~a, match-end ~a, end ~a~%"
			    (part-match-start matched-part)
			    (part-match-match-end matched-part)
			    (part-match-end matched-part)))
		  (push matched-part matched-parts)))
	       (t (when (and (= mstart start) *findstruct-debug*)
		    (format t "~%failed"))
		  (setf mend end)))))
	   (t				;; if there is no explicit pattern
	    (multiple-value-bind (matched range)
		(match-re (gen-re p opts) text
			  :start mstart :end end)
	      (cond (matched
		     (setf mend (cdr range))
		     (when *findstruct-debug*
		       (format t "~%Matched ~s"
			       (subseq text (car range) (cdr range))))
		     (push (make-part-match
			    :start (car range)
			    :match-end (cdr range)
			    :bindings nil
			    :hier head
			    :spec p
			    :opts opts)
			   matched-parts))
		    (t (when (and (= mstart start) *findstruct-debug*)
			 (format t "~%failed"))
		       (setf mend end)))))))))
    
    (setq matched-parts (sort matched-parts
			      #'(lambda (x y)
				  (or (< (part-match-start x)
					 (part-match-start y))
				      (and (= (part-match-start x)
					      (part-match-start y))
					   (> (part-match-match-end x)
					      (part-match-match-end y)))))))
    
    (when *findstruct-debug*
      (format t "~%Matched: ~s" matched-parts))
    ;; yluo - 05/24/2010 delete overlaping other_sections
    ;; yluo - 05/18/2010 delete contained setions.
    ;; sanity check the integrity of the matched headings.
    (setf matched-parts (make-array (length matched-parts) 
				    :initial-contents matched-parts))
    (let* ((cleaned-matched-parts nil))
      (do ((i 0 (incf i))
	   (j 1 (incf j)))
	  ((= i (length matched-parts)))
	(cond 
	 ;; if contain or overlap
	 ((and (< j (length matched-parts))
	       (< (part-match-start (aref matched-parts j))
		  (or (part-match-end (aref matched-parts i))
		      (part-match-match-end (aref matched-parts i)))))
	  ;; delete head-contained sections or headings
	  (cond
	   ((<= (part-match-match-end (aref matched-parts j))
		(or (part-match-end (aref matched-parts i))
		    (part-match-match-end (aref matched-parts i))))
	    ;; if i is not other_section, 
	    ;; let i stay, skip this j, move on to next j
	    ;; otherwise, there are more specific and detailed section j
	    ;; let j stay, skip i, in this case, j must be i+1
	    (unless (other-sectionp (aref matched-parts i))
	      (decf i)))
	   
	   (t							; if overlap
	    (cond 
	     ((other-sectionp (aref matched-parts j))
	      (decf i))
	     ((other-sectionp (aref matched-parts i))
	      ;; let j stay if i is Other_Section
	      )
	     ;; error on overlap and both i and j are not other section
	     (t
	      (error "~%Error in doc ~a findstruct:~%~s~%precedes end of ~s"
		     (id doc) 
		     (aref matched-parts j) 
		     (aref matched-parts i)))))))
	 ;; if normal, or reach end
	 (t
	  (pushnew (aref matched-parts i) cleaned-matched-parts)
	  ;; sync i,j back
	  (setq i (- j 1)))))
      
      (setf matched-parts (nreverse cleaned-matched-parts)))
    ;; only keep other_section subsections that are in the beginning, this 
    ;; implies the limitations of other_section, if the users want other_section
    ;; to stay, just change the name to say cap_section
    (let* (trimmed-matched-parts (keep? t))
      (dolist (match-part matched-parts)
	(cond
	 ((other-sectionp match-part)
	  (when keep?
	    (pushnew match-part trimmed-matched-parts)))
	 (t
	  (pushnew match-part trimmed-matched-parts)
	  (setf keep? t))))			;; I want all other sections
      (setf matched-parts (nreverse trimmed-matched-parts)))
    
    
    
    
    (when *findstruct-debug*
      (format t "~%Cleaned Matched: ~s" matched-parts))
    ;; (format t "Cleaned Matched: ~a~%" matched-parts)
    ;; (format t "lists option is ~a~%" (getf options 'lists))
    (if (null matched-parts)
	;; If this section has no substructure and
	;; lists are to be parsed, try to find items within the text
	;; and add annotations for them.
	
	(when (getf options 'lists)
	  (cond 
	   ((and (getf options 'semi_colon_lists)
		 (member (format nil "~a" (data hierarchy))
			 (split-re "\\|" (getf options 'semi_colon_lists)) 
			 :test #'equalp))
	    (parse-semi-colon-lists doc text start end hierarchy
				    (getf options 'list_proc)))
	   (t
	    (parse-lists doc text start end options hierarchy
			 (getf options 'list_proc)))))
      
      ;; For specs that only give the start of a section, we have
      ;; to calculate the true end of the section, namely the
      ;; beginning of the next one. 
      (do ((pl matched-parts (and pl (cdr pl)))
	   (pos start))
	  ((>= pos end))
	(let* ((pm (car pl))
	       (e (or (and pl (part-match-start pm)) end))
	       proc)
	  (when (< pos e)
	    ;; There is stuff before the first substructure;
	    ;; We assume that it has already received an annotation
	    ;; at a higher level
	    (when *findstruct-debug*
	      (format t "~%Stuff before ~s: ~s"
		      (or (and pl (xml-head (part-match-spec (car pl))))
			  (list 'end end))
		      (subseq text pos e)))
	    (setq pos e))
	  (cond 
	   ((null pl)
	    ;; This is scrap at the end; already noted above
	    )
	   ((null (part-match-end pm))
	    ;; This was a match to a pattern showing the beginning
	    ;; of a section. Set its end to the beginning of the next
	    (let* ((true-end (if (cdr pl) (part-match-start (cadr pl))
			       end)))
	      (setf (part-match-end pm) true-end)
	      (setf proc (getf (xml-attrs (part-match-spec pm)) 'proc))
	      (parse-heading-lists doc text pm opts hierarchy)
	      (let* ((secname2 (format 
				nil "~a"
				(or (cdr 
				     (assoc 'sec_name 
					    (part-match-bindings pm) 
					    :test #'string-equal))
				    (part-match-hier pm))))
		     (secname1 (replace-re secname2 "(^\\s+|\\s+$)" ""))
		     (secname (replace-re secname1  "\\s+" "_"))
		     (sh-ann (make-instance 'section-head-annotation
					    :document doc
					    :start (part-match-start pm)
					    :end (part-match-match-end pm)
					    :data secname
					    :h-up hierarchy
					    :processor proc))
		     (s-ann (if (part-match-bindings pm)
				(make-instance 'pattern-section-annotation
					       :document doc
					       :start (part-match-match-end pm)
					       :end true-end
					       :prefix-start (part-match-start pm)
					       :data secname
					       :bindings (part-match-bindings pm)
					       :h-up hierarchy
					       :processor proc)
			      (make-instance 'section-annotation
					     :document doc
					     :start (part-match-match-end pm)
					     :end true-end
					     :prefix-start (part-match-start pm)
					     :data secname 
					     :h-up hierarchy
					     :processor proc))))
		
		(add-annotation doc sh-ann)
		(add-annotation doc s-ann)
		(add-annotation hierarchy s-ann)
		(add-annotation hierarchy sh-ann)
		;; yluo - 05/19/2010 guarded against unnecesary recursion
		(when t			 ;;(xml-parts (part-match-spec pm))
		  (parse-struct doc
				text
				(part-match-match-end pm)
				true-end
				(xml-parts (part-match-spec pm))
				(part-match-opts pm)
				s-ann))
		(setq pos true-end))))
	   (t ;; This was a pattern that matched a section. This happens when
	    ;; this code generate a regular expression using gen-re
	    (let* ((proc (getf (xml-attrs (part-match-spec pm)) 'proc))
		   (heading (cdr (assoc 'heading 
					(part-match-bindings (car pl)) 
					:test #'string-equal)))
		   (secname2 (format 
			      nil "~a"
			      (or (cdr 
				   (assoc 'sec_name 
					  (part-match-bindings (car pl)) 
					  :test #'string-equal))
				  (part-match-hier (car pl)))))
		   (secname1 (replace-re secname2 "(^\\s+|\\s+$)" ""))
		   (secname (replace-re secname1  "\\s+" "_"))
		   (s-ann (make-instance 'pattern-section-annotation
					 :document doc
					 ;; yluo - 05/20/2010 changed to match-end to be
					 ;; consistent when have heading
					 :start (cond 
						 (heading (part-match-match-end (car pl)))
						 (t (part-match-start (car pl))))
					 :end (part-match-end (car pl))
					 :prefix-start (part-match-start (car pl))
					 :data secname
					 :bindings (part-match-bindings (car pl))
					 :h-up hierarchy
					 :processor proc))
		   (sh-ann (and heading 
				(make-instance 'section-head-annotation
					       :document doc
					       :start (part-match-start (car pl))
					       :end (part-match-match-end (car pl))
					       :data secname
					       :h-up hierarchy
					       :processor proc))))
	      
	      (when sh-ann
		(add-annotation doc sh-ann)
		(add-annotation hierarchy sh-ann))
	      (add-annotation doc s-ann)
	      (add-annotation hierarchy s-ann)
	      (when t		      ;;(xml-parts (part-match-spec (car pl)))
		(parse-struct doc
			      text
			      (part-match-match-end (car pl))
			      (part-match-end (car pl))
			      (xml-parts (part-match-spec (car pl)))
			      (part-match-opts (car pl))
			      s-ann))
	      (setq pos (part-match-end (car pl)))))))))
    t))

(defstruct item-match
  pre
  itm
  suf)


(defun parse-semi-colon-lists (doc text start end hierarchy list-proc
				   &aux possibles)
  (dolist (litm (split-re ";" text :start start :end end :return :index))
    (let* ((pos (max start (car litm))))
      (push (make-item-match :pre (cons pos pos) 
			     :itm (cons pos pos)
			     :suf (cons pos pos))
	    possibles)))
  (setf possibles (nreverse possibles))
  (do ((pl possibles (cdr pl)))
      ((null pl))
    ;; Create an annotation for the text following a list marker.
    (let* ((e (or (and (cdr pl)
		       (car (item-match-pre (cadr pl))))
		  end))
	   (ann (make-instance 'list-item-annotation
			       :document doc
			       :start (cdr (item-match-suf (car pl)))
			       :end e
			       :data (subseq text
					     (car (item-match-itm (car pl)))
					     (cdr (item-match-itm (car pl))))
			       :h-up hierarchy
			       :processor list-proc)))
      (add-annotation doc ann)
      (add-annotation hierarchy ann))))


(defun parse-lists (doc text start end opts hierarchy list-proc)
  ;; (format t "list matching: ~%\"~a\"~%" (subseq text start end))
  (let* ((pat (compile-re
	       (concatenate 'string
			    "(?<pref>"
			    (getf opts 'listprefix)
			    ")(?<item>"
			    (getf opts 'listpat)
			    ")(?<suff>"
			    (getf opts 'listsuffix)
			    ")")
	       :case-fold (getf opts 'case_insensitive)
	       :multiple-lines (getf opts 'multiline)
	       :single-line (getf opts 'dotall)))
	 (endpat (compile-re
		  (getf opts 'listlastitemend)
		  :case-fold (getf opts 'case_insensitive)
		  :multiple-lines (getf opts 'multiline)
		  :single-line (getf opts 'dotall)
		  :return :index))
	 (s start)
	 (matches nil)
	 (possibles
	  (loop
	   (let* ((m (match-re pat text :start s :end end :return :match))
		  pref item suff)
	     (unless m
	       (return (nreverse matches)))
	     (setf pref (re-submatch m nil nil "pref" :type :index))
	     (setf item (re-submatch m nil nil "item" :type :index)) 
	     (setf suff (re-submatch m nil nil "suff" :type :index))
	     (push (make-item-match :pre pref :itm item :suf suff)
		   matches)
	     (setq s (cdr suff))))))

    ;; (format t "pattern: ~a~%" pat)
    ;; We've collected in order all the list header patterns that
    ;; match. Check that they are in ascending order and create
    ;; annotations.
    (do ((pl possibles (cdr pl)))
	((null pl))
      ;; Create an annotation for the text following a list marker.
      (let* ((e (or (and (cdr pl)
			 (car (item-match-pre (cadr pl))))
		    (multiple-value-bind (m? whole)
			(match-re endpat text
				  :start (cdr (item-match-suf (car pl)))
				  :end end)
		      (when *findstruct-debug*
			(format t
				"~%~:[No~;Found~] end of list element ~
                                 start at ~d: ~s"
				m?
				(cdr (item-match-suf (car pl)))
				whole))
		      (and m? (car whole)))
		    end))
	     (lh (make-instance 'list-head-annotation
				:document doc
				:start (cdr (item-match-pre (car pl)))
				:end (cdr (item-match-suf (car pl)))
				:data (subseq text
					      (cdr (item-match-pre (car pl)))
					      (cdr (item-match-suf (car pl))))
				:h-up hierarchy))
	     (la (make-instance 'list-item-annotation
				:document doc
				:start (cdr (item-match-suf (car pl)))
				:end e
				:data (subseq text
					      (car (item-match-itm (car pl)))
					      (cdr (item-match-itm (car pl))))
				:h-up hierarchy
				:processor list-proc)))
	(add-annotation doc la)
	(add-annotation hierarchy la)
	(add-annotation doc lh)
	(add-annotation hierarchy lh)))))


(defun parse-heading-lists (doc text pm opts hierarchy)
  ;; (format t "list matching: ~%\"~a\"~%" (subseq text start end))
  (let* ((start (part-match-start pm))
	 (end (part-match-match-end pm))
	 (pat (compile-re
	       (concatenate 'string
			    "(?<pref>^)(?<item>"
			    (getf opts 'listpat)
			    ")(?<suff>"
			    (getf opts 'listsuffix)
			    ")")
	       :case-fold (getf opts 'case_insensitive)
	       :multiple-lines (getf opts 'multiline)
	       :single-line (getf opts 'dotall)))
	 (possible
	  (let* ((m (match-re pat text :start start :end end :return :match))
		 pref item suff)
	    (when m
	      (setf pref (re-submatch m nil nil "pref" :type :index))
	      (setf item (re-submatch m nil nil "item" :type :index)) 
	      (setf suff (re-submatch m nil nil "suff" :type :index))
	      (make-item-match :pre pref :itm item :suf suff)))))

    ;; (format t "pattern: ~a~%" pat)
    ;; We've collected in order all the list header patterns that
    ;; match. Check that they are in ascending order and create
    ;; annotations.
    
    ;; Create an annotation for the text following a list marker.
    (when possible
      (let* ((lh (make-instance 'list-head-annotation
				:document doc
				:start (cdr (item-match-pre possible))
				:end (cdr (item-match-suf possible))
				:data (subseq text
					      (cdr (item-match-pre possible))
					      (cdr (item-match-suf possible)))
				:h-up hierarchy))
	     (m (match-re "\\S" text :start (end lh) :return :match))
	     pm-start)
	(cond
	 (m
	  (setf pm-start (car (re-submatch m nil nil 0 :type :index)))
	  (add-annotation doc lh)
	  (add-annotation hierarchy lh)
	  (setf (part-match-start pm) pm-start))
	 (t
	  (format t "~&Warning: abnormal head list matching at ~a~%"
		  (subseq text start end))))))))


;;; yluo - 05/19/2010 pattern and whole shouldn't be merged into lower level.
;;; changed behavior to not carry on options by default unless in prespecified
;;; list 
(defun merge-options (expr opts)
  "Given an XML expression, we parse the attributes from the expression
into the form used internally. Mainly, we turn 'yes' and 'no' specifications 
into t and nil, and prepend these to opts."
  (append
   (do ((l (xml-attrs expr) (cddr l))
	(ans nil))
       ((or (null l) (null (cdr l))) (nreverse ans))
     (unless (member (car l) '(pattern whole proc))
       (push (car l) ans)
       (push (if (member (car l)
			 '(case_insensitive canon_eq comments
					    dotall multiline unix_lines lists))
		 (not (equalp (cadr l) "no"))
	       (cadr l))
	     ans)))
   opts))

(defun gen-re (expr opts)
  "Generate a regexp matcher that searches for either the string of the 
head of the expression or any of the alt's."
  (let* ((pref (getf opts 'prefix ""))
	 ;;(ppref (parse-re pref))
	 (suff (getf opts 'suffix ""))
	 ;;(psuff (parse-re suff))
	 (alts (getf (xml-attrs expr) 'alt))
	 (start-pat (getf (xml-attrs expr) 'start_pattern))
	 (main (replace-re (format nil "~a" (xml-head expr)) "_" "\\s+"))
	 (pmain (concatenate 'string pref main suff)))
    (compile-re
     (cond (start-pat start-pat)
	   ((null alts) pmain)
	   (t (concatenate 'string
			   pref
			   "("
			   main
			   "|"
			   alts 
			   ")"
			   suff)))
     :case-fold (getf opts 'case_insensitive)
     :multiple-lines (getf opts 'multiline)
     :single-line (getf opts 'dotall)
     :return :index)))

(defun find-pattern (pattern text &key (start 0) (end (length text)))
  "A mostly debugging utility to find every occurrence of a pattern in text."
  (do ((s start (cdar ans))
       (ans nil))
      ((null s))
    (multiple-value-bind (m? where) 
	(match-re pattern text
		  :multiple-lines t :return :index :start s :end end)
      (unless m? (return (nreverse ans)))
      (push where ans))))

(defun section-content (doc secname)
  (content
   (car
    (annotations 
     doc 
     :type 'section-annotation 
     :filter #'(lambda (a) 
		 (and (equalp secname (format nil "~a" (data a)))
		      (not (typep a 'section-head-annotation))))))))

(defun doc-mrn (doc)
  (replace-re (section-content doc "MRN") "\\s+" ""))

(defun section-content-stats (corpn out-fn secn &aux h-cont keys)
  ;; Sample usage:
  ;; (section-content-stats "process_list" "late:;part_section_content_stats" "part")
  (setf h-cont (make-hash-table :test #'equalp))
  (with-open-file (out-f out-fn
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
		  (dolist (docid (documents (corpus corpn)))
		    (let* ((doc (document docid))
			   sec-cont)
		      (dolist (sa (annotations doc :type 'section-annotation))
			(when (and (equalp secn (format nil "~a" (data sa)))
				   (not (typep sa 'section-head-annotation)))
			  (setf sec-cont (content sa))
			  (incf (gethash sec-cont h-cont 0))))))
		  (maphash #'(lambda (k v) (declare (ignore v)) (push k keys)) h-cont)
		  (dolist (k (sort keys #'(lambda (x y) (> (gethash x h-cont)
							   (gethash y h-cont)))))
		    (format out-f "~a || ~a~%" k (gethash k h-cont)))))


(defun note-in-final-diagnosis (corpn out-fn 
				      &aux (fd-cnt 0) 
				      (fd-note-cnt 0))
  (with-open-file (out-f out-fn
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
		  (dolist (docid (documents (corpus corpn)))
		    (let* ((doc (document docid))
			   notes)
		      (format out-f "~&~%======~%~a~%" (name doc))
		      (dolist (sa (annotations doc 
					       :type 'section-annotation
					       :filter #'(lambda (a) 
							   (equalp "FINAL_DIAGNOSIS" (data a)))))
			(incf fd-cnt)
			(setf notes (annotations sa :type 'section-annotation
						 :filter #'(lambda (a)
							     (equalp "NOTE" (data a)))))
			(incf fd-note-cnt (length notes))
			(dolist (note notes)
			  (format out-f "~&------~%~a~%" note)))))
		  (format t "final diagnosis notes #: ~a; final diagnosis #: ~a"
			  fd-note-cnt fd-cnt)))

(defun section-stats (corpn &aux h-secs)
  (setf h-secs (make-hash-table :test #'equalp))
  (dolist (docid (documents (corpus corpn)))
    (let* ((doc (document docid)))
      (dolist (sec (annotations doc :type 'section-annotation))
	(when (and (annotations sec :type 'sentence-annotation)
		   (not (typep sec 'list-item-annotation)))
	  (incf (gethash (data sec) h-secs 0))))))
  (let* ((l-secs (hash-table-val-desc-alist h-secs)))
    (dolist (l-sec l-secs)
      (format t "~&~a: ~a~%" (car l-sec) (cdr l-sec)))))


(defun addendum-star-section-stats (corpn &aux h-secs)
  (setf h-secs (make-hash-table :test #'equalp))
  (dolist (docid (documents (corpus corpn)))
    (let* ((doc (document docid))
	   sn)
      (dolist (sec (annotations doc :type 'section-head-annotation))
	(when (equalp "addendum_star" (data sec))
	  (setf sn (content sec))
	  (setf sn (replace-re sn "(^\\s+|\\s+$)" ""))
	  (setf sn (replace-re sn "\\s+" " "))
	  (incf (gethash sn h-secs 0))))))
  (let* ((l-secs (hash-table-val-desc-alist h-secs)))
    (dolist (l-sec l-secs)
      (format t "~&~a: ~a~%" (car l-sec) (cdr l-sec)))))

(defun sec-check (l-secn l-excl-secn sas)
  (and (or (not l-secn)
	   (intersection l-secn (mapcar #'data sas) :test #'equalp))
       (or (not l-excl-secn)
	   (not (intersection l-excl-secn (mapcar #'data sas) :test #'equalp)))))



