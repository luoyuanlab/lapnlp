;;; -*- Mode: Lisp; Package: late -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation
The various types of annotations are defined in late.cl.  Here, we
define various useful procedures to search for an manipulate them.
|#

(defpackage :late
  (:use :common-lisp :util)
  (:export 
   "add-annotation"
   "annotation"
   "annotation-equal"
   "annotation-id-equal"
   "annotation-id<"
   "annotation-lessp"
   "annotations"
   "annotations-spec"
   "annotations-after"
   "annotations-after-spec"
   "annotations-before"
   "annotations-before-spec"
   "annotations-by-span"
   "annotations-by-span-and-type"
   "annotations-on"
   "annotations-on-spec"
   "annotations-spanning"
   "annotations-spanning-spec"
   "annotations-overlap"
   "annotations-overlap-spec"
   "annotations-share"
   "annotations-share-spec"
   "annotations-within"
   "annotations-within-spec"
   "depth"
   "document-annotations"
   "flush-annotations"
   "flush-annotations-spec"
   "next-anns"
   "pnode-from-token"
   "prev-anns"
   "print-ann"
   "show-annotation-counts"
   "show-annotations"
   "print-anns"
   "size"))

(in-package :late)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Linking and deleting annotations;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-annotation ((doc document) (ann annotation))
  "Adds the annotation ann to the document doc.  These are stored in
  an interval-tree within the document, indexed by start and end
  positions of the annotation."
  (setf (document ann) doc)
  (let* ((ann-cnt (doc-ann-cnt doc)))
    (setf (id ann) (n2-biject-n (id doc) (1+ ann-cnt))))
  (tree-insert (annotations-tree doc) ann)
  
  (setf (dirty-annotations doc) t
	(dirty ann) t
	(new ann) t))

;; the mapping from ann-above to ann is one to many
(defmethod add-annotation ((ann-above annotation) (ann annotation))
  "Adds a hierarchical sub-annotation ann to ann-above.  This is only
used for annotations that form a natural up/down hierarcy.  Note that
this does not automatically add ann or ann-above as an annotation on
their document.  Thus, although (annotations ann) retrieves all
annotations within ann, to add such an annotation, one should use
;; (add-annotation (document ann) ann-inside)."
  (assert (and (id ann-above) (id ann))
	  ()
	  "no id for either ~a or ~a" ann-above ann)
  (push ann (h-down ann-above))
  (setf (h-up ann) ann-above)
  (setf (dirty ann) t
	(dirty ann-above) t)
  (setf (dirty-annotations (document ann-above)) t))

(defmethod depth ((ann annotation))
  (if (null (h-up ann))
      0
      (1+ (depth (h-up ann)))))

(defmethod size ((ann annotation))
  (- (end ann) (start ann)))

(defun sw-ver-compatp (sw-ver ann)
  (or (null sw-ver) 
      (null (sw-ver ann)) 
      (equalp sw-ver (sw-ver ann))))

(defun setting-compatp (setting ann)
  (or (null setting) 
      (null (setting ann))
      (equalp setting (setting ann))))

(defmethod flush-annotations ((doc document)
			      &key (type 'annotation)
			      (sw-ver nil)
			      (setting nil))
  "
Input
======
analysis: analysis that generates annotations to be flush, this is needed if one
wants to upate the analyses slot of the document."
  (open-late-database)
  (let ((old-tree (annotations-tree doc))
	(nt (make-instance 'interval-tree)))
    (setf (annotations-tree doc) nt)
    (do-tree (ann old-tree)
      (cond 
	((and (typep ann type) 
	      (sw-ver-compatp sw-ver ann)
	      (setting-compatp setting ann))
	 (push ann (annotations-deleted doc))
	 (setf (dirty-annotations doc) t))
	(t 
	 (tree-insert (annotations-tree doc) ann))))
;;;    (format t "~&size of annotations-deleted: ~a~%" (length (annotations-deleted doc)))
    (save doc)))


(defmethod flush-annotations-spec ((doc document)
				   &key (type 'annotation)
				   &aux sw-ver setting)
  (let* ((type-inst (make-instance type :start 0 :end 0)))
    (setf sw-ver (gsw-ver (sw-ver-key type-inst))
	  setting (gsetting (setting-key type-inst))))
  (assert (and sw-ver setting)
	  ()
	  "Calling annotations-spec but without specifying sw-ver and setting in the 
.spec file, please call annotations instead with expicit sw-ver and setting 
specifications.")
  (flush-annotations 
   doc
   :type type
   :sw-ver sw-ver
   :setting setting))

(defmethod flush-annotations ((corp corpus)
			      &key (type 'annotation)
			      (sw-ver nil)
			      (setting nil))
  (dolist (d (documents corp))
    (flush-annotations (document d) 
		       :type type
		       :sw-ver sw-ver
		       :setting setting)))

(defmethod flush-annotations-spec ((corp corpus)
				   &key (type 'annotation))
  (dolist (d (documents corp))
    (flush-annotations-spec (document d) :type type)))

(defmethod flush-annotations ((corp (eql nil))
			      &key (type 'annotation)
			      (analysis nil)
			      (sw-ver nil)
			      (setting nil))
  (dolist (c (corpora nil))
    (flush-annotations c 
		       :type type
		       :analysis analysis
		       :sw-ver sw-ver
		       :setting setting)))

(defmethod flush-annotations-spec ((corp (eql nil))
				   &key (type 'annotation))
  (dolist (c (corpora nil))
    (flush-annotations-spec c :type type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ordering of annotations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun annotation-equal (ann1 ann2)
  (int= ann1 ann2))

(defun annotation-id-equal (ann1 ann2)
  (= (id ann1) (id ann2)))

(defun annotation-id< (ann1 ann2)
  (< (id ann1) (id ann2)))

(defun annotation-lessp (ann1 ann2)
  ;; Because annotations are intervals, we just adopt their ordering.
  (int< ann1 ann2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Retrieval of annotations. This is based on our interval tree
;;; implementation. Note that we can use any of the interval relations
;;; as arguments to (annotation ...), specified in the keyword
;;; package.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod annotations ((doc document)
			&key (type 'annotation)
			(sw-ver nil)
			(setting nil)
			(relation nil)
			(filter nil))
  "Returns a list of the annotations on the document of type
   type (or its subtypes). This method considers all such annotations,
   without care about their location.  The result is ordered by
   int<."
  ;; The relation argument is ignored for documents.
  (declare (ignore relation))
  (let ((ans nil))
    (do-tree-rev (a (annotations-tree doc))
      (when (and (typep a type)
		 (sw-ver-compatp sw-ver a)
		 (setting-compatp setting a)
		 (or (null filter)
		     (funcall filter a)))
	(push a ans)))
    (setf ans (stable-sort ans #'annotation-id<))
    (setf ans (stable-sort ans #'annotation-lessp))))

(defmethod annotations-spec ((doc document)
			     &key (type 'annotation)
			     (relation nil)
			     (filter nil)
			     &aux sw-ver setting)
  (let* ((type-inst (make-instance type :start 0 :end 0)))
    (setf sw-ver (gsw-ver (sw-ver-key type-inst))
	  setting (gsetting (setting-key type-inst))))
  
  (assert (and sw-ver setting)
	  ()
	  "Calling annotations-spec on ~a but without specifying sw-ver and setting in the 
.spec file, please call annotations instead with expicit sw-ver and setting 
specifications." type)
  (annotations 
   doc 
   :type type 
   :sw-ver sw-ver
   :setting setting
   :relation relation
   :filter filter))

(defmethod annotations ((ann annotation)
			&key (type 'annotation)
			(sw-ver nil)
			(setting nil)
			(filter nil)
			(relation ':in))
  "Returns a list of the annotations on the document on which ann is
    a base annotation, of type type, that satisfy the
    interval relation x relation ann, which defaults to :in. Filter, if
    given, eliminates any retrieved annotations that fail the filter
    predicate." 
  (let* ((doc (document ann))
	 (ans (tree-search (annotations-tree doc)
			   ann
			   relation
			   (if filter
			       #'(lambda (a)
				   (and (typep a type)
					(sw-ver-compatp sw-ver a)
					(setting-compatp setting a)
					(funcall filter a)))
			       #'(lambda (a) (typep a type))))))
    (setf ans (stable-sort ans #'annotation-id<))
    (setf ans (stable-sort ans #'annotation-lessp))))

(defmethod annotations-spec ((ann annotation)
			     &key (type 'annotation)
			     (filter nil)
			     (relation ':in)
			     &aux sw-ver setting)
  (let* ((type-inst (make-instance type :start 0 :end 0)))
    (setf sw-ver (gsw-ver (sw-ver-key type-inst))
	  setting (gsetting (setting-key type-inst))))
  (assert (and sw-ver setting)
	  ()
	  "Calling annotations-spec on ~a but without specifying sw-ver and setting in the 
.spec file, please call annotations instead with expicit sw-ver and setting 
specifications." type)
  (annotations 
   ann
   :type type 
   :sw-ver sw-ver
   :setting setting
   :relation relation
   :filter filter))

;;; We provide the following for convenience in finding annotations of
;;; a document relative to a base annotation.  -before, -after and -on
;;; are strict, but -spanning and -within may include -on. Note that
;;; the base annotation is not returned as part of the result, even if
;;; it matches the criteria.
;;; We may need to think through how to handle annotations with
;;; degenerate intervals.  ***psz***

(defmethod annotations-within ((ann annotation)
			       &key 
			       (type 'annotation)
			       (sw-ver nil)
			       (setting nil)
			       (filter nil))
  "Returns a list of the annotations on the document on which ann is
    a base annotation, of type type, whose span is within
    the base.  This is equivalent to the default arguments of
    annotations, except we don't return the base."
  (annotations ann
	       :type type
	       :sw-ver sw-ver
	       :setting setting
	       :relation ':in
	       :filter #'(lambda (a) (and (not (eq a ann))
					  (or (null filter)
					      (funcall filter a))))))



(defmethod annotations-within-spec ((ann annotation)
				    &key 
				    (type 'annotation)
				    (filter nil))
  (annotations-spec ann
		    :type type
		    :relation ':in
		    :filter #'(lambda (a) (and (not (eq a ann))
					       (or (null filter)
						   (funcall filter a))))))



(defmethod annotations-share ((ann annotation)
			      &key (type 'annotation)
			      (sw-ver nil)
			      (setting nil)
			      (filter nil))
  "Returns a list of the annotations on the document on which ann is
    a base annotation, of type type, whose span is overlapping (ar, in o, oi)
    the base.  We don't return the base."
  (or (annotations ann
		   :type type
		   :sw-ver sw-ver
		   :setting setting
		   :relation ':ar
		   :filter #'(lambda (a) (and (not (eq a ann))
					      (or (null filter)
						  (funcall filter a)))))
      (annotations ann
		   :type type
		   :sw-ver sw-ver
		   :setting setting
		   :relation ':in
		   :filter #'(lambda (a) (and (not (eq a ann))
					      (or (null filter)
						  (funcall filter a)))))
      (annotations ann
		   :type type
		   :sw-ver sw-ver
		   :setting setting
		   :relation ':o
		   :filter #'(lambda (a) (and (not (eq a ann))
					      (or (null filter)
						  (funcall filter a)))))
      (annotations ann
		   :type type
		   :sw-ver sw-ver
		   :setting setting
		   :relation ':oi
		   :filter #'(lambda (a) (and (not (eq a ann))
					      (or (null filter)
						  (funcall filter a)))))))


(defmethod annotations-overlap ((ann annotation)
				&key 
				(type 'annotation)
				(sw-ver nil)
				(setting nil)
				(filter nil))
  "Returns a list of the annotations on the document on which ann is
    a base annotation, of type type, whose span is overlapping (ar, in o, oi)
    the base.  We don't return the base."
  (or (annotations ann
		   :type type
		   :sw-ver sw-ver
		   :setting setting
		   :relation ':o
		   :filter #'(lambda (a) (and (not (eq a ann))
					      (or (null filter)
						  (funcall filter a)))))
      (annotations ann
		   :type type
		   :sw-ver sw-ver
		   :setting setting
		   :relation ':oi
		   :filter #'(lambda (a) (and (not (eq a ann))
					      (or (null filter)
						  (funcall filter a)))))))

(defmethod annotations-share-spec ((ann annotation)
				   &key 
				   (type 'annotation)
				   (filter nil))
  (or (annotations-spec ann
			:type type
			:relation ':ar
			:filter #'(lambda (a) (and (not (eq a ann))
						   (or (null filter)
						       (funcall filter a)))))
      (annotations-spec ann
			:type type
			:relation ':in
			:filter #'(lambda (a) (and (not (eq a ann))
						   (or (null filter)
						       (funcall filter a)))))
      (annotations-spec ann
			:type type
			:relation ':o
			:filter #'(lambda (a) (and (not (eq a ann))
						   (or (null filter)
						       (funcall filter a)))))
      (annotations-spec ann
			:type type
			:relation ':oi
			:filter #'(lambda (a) (and (not (eq a ann))
						   (or (null filter)
						       (funcall filter a)))))))

(defmethod annotations-overlap-spec ((ann annotation)
				     &key 
				     (type 'annotation)
				     (filter nil))
  (or (annotations-spec ann
			:type type
			:relation ':o
			:filter #'(lambda (a) (or (null filter)
						  (funcall filter a))))
      (annotations-spec ann
			:type type
			:relation ':oi
			:filter #'(lambda (a) (or (null filter)
						  (funcall filter a))))))

(defmethod annotations-before ((ann annotation)
			       &key 
			       (type 'annotation)
			       (cnt nil)
			       (sw-ver nil)
			       (setting nil)
			       (filter nil))
  "Returns a list of the annotations on the document on which ann is a
    base annotation, of type annotation-type, that are before
    the base.  I.e., they must start before the base and end at most
    at the start of the base."
  (let* ((anns (annotations ann 
			    :type type
			    :sw-ver sw-ver
			    :setting setting
			    :relation ':<m
			    :filter filter)))
    (cond
      (cnt
       (subseq anns (max 0 (- (length anns) cnt))))
      (t
       anns))))

(defmethod annotations-before-spec ((ann annotation)
				    &key 
				    (cnt nil)
				    (type 'annotation)
				    (filter nil))
  (let* ((anns (annotations-spec ann 
				 :type type
				 :relation ':<m
				 :filter filter)))
    (cond
      (cnt
       (subseq anns (max 0 (- (length anns) cnt))))
      (t
       anns))))

(defmethod annotations-after ((ann annotation)
			      &key 
			      (type 'annotation)
			      (cnt nil)
			      (sw-ver nil)
			      (setting nil)
			      (filter nil))
  "Returns a list of the annotations on the document on which ann is a
    base annotation, of type annotation-type, that are after
    the base."
  (let* ((anns (annotations ann 
			    :type type
			    :sw-ver sw-ver
			    :setting setting
			    :relation ':>mi
			    :filter filter)))
    (cond
      (cnt 
       (subseq anns 0 (min cnt (length anns))))
      (t
       anns))))

(defmethod annotations-after-spec ((ann annotation)
				   &key 
				   (type 'annotation)
				   (cnt nil)
				   (filter nil))
  (let* ((anns (annotations-spec ann 
				 :type type
				 :relation ':>mi
				 :filter filter)))
    (cond
      (cnt
       (subseq anns 0 (min cnt (length anns))))
      (t
       anns))))

(defmethod annotations-spanning ((ann annotation)
				 &key 
				 (type 'annotation)
				 (sw-ver nil)
				 (setting nil)
				 (filter nil))
  "Returns a list of the annotations on the document on which ann is a
    base annotation, of type annotation-type, that span the base
    annotation. This means that they start before or at the start of
    ann and end at or after the end of ann. They are returned in
    lexicographic order by span."
  (annotations ann
	       :type type
	       :sw-ver sw-ver
	       :setting setting
	       :relation ':ar 
	       :filter #'(lambda (a) (and (not (eq a ann))
					  (or (null filter)
					      (funcall filter a))))))

(defmethod annotations-spanning-spec ((ann annotation)
				      &key 
				      (type 'annotation)
				      (filter nil))
  (annotations-spec ann
		    :type type
		    :relation ':ar 
		    :filter #'(lambda (a) (and (not (eq a ann))
					       (or (null filter)
						   (funcall filter a))))))

(defmethod annotations-on ((ann annotation)
			   &key 
			   (type 'annotation)
			   (sw-ver nil)
			   (setting nil)
			   (filter nil))
  "Returns a list of the annotations on the document on which ann is a
    base annotation, of type annotation-type, that are coincident with
    the base annotation. This means that they have exactly the same
    span. They are returned in arbitrary order."
  (annotations ann
	       :type type
	       :sw-ver sw-ver
	       :setting setting
	       :relation ':=
	       :filter #'(lambda (a) (and (not (eq a ann))
					  (or (null filter)
					      (funcall filter a))))))

(defmethod annotations-on-spec ((ann annotation)
				&key 
				(type 'annotation)
				(filter nil))
  (annotations-spec ann
		    :type type
		    :relation ':=
		    :filter #'(lambda (a) (and (not (eq a ann))
					       (or (null filter)
						   (funcall filter a))))))


(defmethod document-annotations ((doc document))
  "Returns all document-annotations, which span the entire document."
  ;;(annotations doc :type 'document-annotation))
  ;; for greater efficiency:
  (annotations-on (make-instance 'annotation
				 :document doc
				 :start 0
				 :end (size doc))
		  :type 'document-annotation))

;;; We also allow a one-step search forward or backward from a
;;; specific annotation to the 'next' or 'previous', using the
;;; red-black tree implemenation of the interval-tree. This may or may
;;; not be faster than using the interval search procedures, and I am
;;; not sure will give the same results!!!
;;; I didn't adjust these to accommodate (sw-ver ann) and (setting ann), yluo
;;; These should also be filtered by beloning to a single sentence, if
;;; that matters!
;;; Overall, this may not be the right way to compute these. Instead,
;;; we could just do something like
;;; (first-n n (tree-search->mi ann-tree ann))

(defmethod prev-anns ((ann annotation)
		      &key (n 2)
		      (type nil))
  "Returns a list of (at most) n annotations of the type of the input
   annotation (or its subtypes) preceding the input. n defaults to
   2. Type, if given, overrides the derived type. Results are returned
   in right-to-left order, i.e., the 1st is closest to the input!"
  (let ((the-type (or type (type-of ann)))
	(anns-tree (annotations-tree (document ann))))
    (labels ((iter (ans node)
	       (if (or (>= (length ans) n) (null node))
		   ans
		   (multiple-value-bind (annot next-node)
		       (prev-<m anns-tree node :type the-type)
		     (if (null annot)
			 ans
			 (iter (cons annot ans) next-node))))))
      (nreverse (iter nil (tree-search-node anns-tree ann))))))

(defmethod next-anns ((ann annotation)
		      &key (n 2)
		      (type nil))
  "Returns a list of (at most) n annotations of the type of the input
   annotation (or its subtypes) following the input. n defaults to
   2. Type, if given, overrides the derived type. Results are returned
   in left-to-right order, i.e., the 1st is closest to the input!"
  (let ((the-type (or type (type-of ann)))
	(anns-tree (annotations-tree (document ann))))
    (labels ((iter (ans node)
	       (if (or (>= (length ans) n) (null node))
		   ans
		   (multiple-value-bind (annot next-node)
		       (next->mi anns-tree node :type the-type)
		     (if (null annot)
			 ans
			 (iter (cons annot ans) next-node))))))
      (nreverse (iter nil (tree-search-node anns-tree ann))))))

;;; I haven't adjusted the following methods to accommodate sw-ver and setting 
;;; slots, yluo
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Displaying annotations: ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun annotations-by-span (anns)
  "Returns an alist indexed by (start . end) position of each annotated span, where the value of each is a list of the annotations there."
  (let ((partition-hash (make-hash-table :test #'equal))
	(all-spans nil))
    (dolist (a anns)
      (push a (gethash (cons (start a) (end a)) partition-hash nil)))
    (maphash #'(lambda (k v) (push (cons k v) all-spans))
	     partition-hash)
    (sort all-spans
	  #'(lambda (x y)
	      (or (< (caar x) (caar y))
		  (and (= (caar x) (caar y))
		       ;; longest first
		       (> (cdar x) (cdar y))))))))

(defun annotations-by-span-and-type (a-by-span)
  "Creates the data structure needed by print-ann to print a compact
  representation of all the annotations on each distinct span. It's a
  list of the start and end positions, the (abbreviated) text of the
  span, and a list of the lists of annotation types and the data
  elements collected for each distinct type of annotation."
  (mapcar #'(lambda (as)
	      (list  (caar as)
		     (cdar as)
		     (text-abbrev
		      (subseq
		       (content
			(document (cadr as)))
		       (caar as)
		       (cdar as))
		      50)
		     (mapcar
		      #'(lambda (part)
			  (list (getf *annotation-abbrevs*
				      (car part) (car part))
				(mapcar #'data (cdr part))))
		      (partition (cdr as) :key #'type-of))))
	  a-by-span))

(defun print-ann (annlist &optional (*standard-output* *terminal-io*))
  (format *standard-output*
	  ;;"~%~d ~d ~s ~<~3:i~_~@{~<~a: ~@_~{~a~^,~:_~}~:>;~^ ~_~}~:>"
	  "~%~<~d ~d ~s ~3i~_~{~<~a: ~@_~{~a~^,~:_~}~:>;~^ ~:_~}~:>"
	  annlist))

(defmethod show-annotations ((doc document)
			     &key (type 'annotation))
  "Prints a table of the annotations (of the selected type) on doc. We
   try to compact the listing by identifying multiple annotations on
   the same substring and printing them together."
  (dolist (span (annotations-by-span-and-type
		 (annotations-by-span
		  (annotations doc :type type))))
    (print-ann span)))

(defmethod show-annotations ((ann annotation)
			     &key (type 'annotation))
  "Prints table of annotations (of selected type) on the annotation."
  (dolist (a (annotations-within ann :type type))
    (format t "~%~5s ~5d ~a" (start a) (end a) a)))

(defmethod show-annotation-counts ((doc document) &key (type 'annotation))
  "Shows a count of all the types of annotations on the document."
  (print-table
   (sort (mapcar #'(lambda (x) (list (car x) (length (cdr x))))
		 (partition (annotations doc :type type)
			    :key #'type-of))
	 #'>
	 :key #'cadr)
   :specs '(:l :r)))

;;; For debugging 
(defmethod annotation ((doc document)
		       (num integer))
  (do-tree (ann (annotations-tree doc))
    (when (eql num (id ann))
      (return-from annotation ann))))

(defmethod show-annotation-counts ((dummy (eql nil)) &key (type 'annotation))
  "Prints how many annotations are in the database of each type."
  (declare (ignore type))
  (open-late-database)
  (let ((anns (latesql "select type,count(*) c from annotations group by type order by type")))
    (print-table anns :specs '(:l :r))))

(defmethod show-annotation-counts ((c corpus) &key (type 'annotation))
  "Prints how many annotations of each type are on documents in a corpus."
  (declare (ignore type))
  (open-late-database)
  (let ((anns (latesql "select a.type,count(*) c from annotations a join corpora_documents cd on a.document_id=cd.document_id where cd.corpus_id=~d group by a.type order by a.type"
		       (id c))))
    (print-table anns :specs '(:l :r))))

(defmethod show-annotations ((dummy (eql nil)) &key (type 'annotation))
  "Show-annotations on nil is the same as show-annotation-counts."
  (show-annotation-counts nil :type type))

(defmethod show-annotations ((c corpus) &key (type 'annotation))
  "Show-annotations on a corpus is the same as show-annotation-counts."
  (show-annotation-counts c :type type))

(defun pnode-from-token (token)
  "From the commandline, if you give the option -tokenized, then the parser will assume white-space separated tokens, and use your tokenization as is. Of course, parsing will suffer unless your tokenization accurately matches the tokenization of the underlying treebank, for instance Penn Treebank tokenization. A common occurrence is that your text is already correctly tokenized but does not escape characters the way the Penn Treebank does (turning parentheses into -LRB- and -RRB-, and putting a backslash in front of forward slashes and asterisks - presumably a holdover from Lisp). In this case, you can use the -tokenized option but also add the flag:

-escaper edu.stanford.nlp.process.PTBEscapingProcessor
If calling the parser within your own program, the main parse methods take a List of words which should already be correctly tokenized and escaped before calling the parser. You don't need to and cannot give the -tokenized option. If you have untokenized text, it needs to tokenized before parsing. You may use the parse method that takes a String argument to have this done for you or you may be able to use of classes in the process package, such as DocumentPreprocessor and PTBTokenizer for tokenization, much as the main method of the parser does. Or you may want to use your own tokenizer."
  (let* ((tok-str (content token)))
    (cond				;; according to Stanford Parser FAQ
      ((search tok-str "([{")
       (setf tok-str "-LRB-"))
      ((search tok-str ")]}")
       (setf tok-str "-RRB-"))
      ((equalp tok-str "/")
       (setf tok-str "\\/"))
      ((equalp tok-str "*")
       (setf tok-str "\\*")))
    
    (make-instance (gtype 'gparse-node-type)
		   :doc (document token)
		   :start (start token)
		   :end (end token)
		   :sw-ver (gsw-ver 'gparse)
		   :data tok-str
		   :setting (gsetting 'gparse))))

(defun print-anns (anns)
  (format t "~{~a~%~}" anns))
