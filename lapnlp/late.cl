;;; -*- Mode: Lisp; Package: late -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 04/12/2012 introduced distinction between annotations of the same type
       produced by different software version (sw-ver) and under different
       settings (setting)
yluo - 04/04/2012 added and altered hierarchy of various token, pos-tag, parse 
       annotations
wjl  - 01/03/2012 added lcui-annotation for the lite parser
yluo - 12/13/2011 added processor attribute to section to control how it should
       be processed, e.g., as narrative text or as coded text or as numeric or 
       no processing (nil)
yluo - 09/15/2010 added structured feature annotation, use data field as storage
       for feature name for indexing convenience.
psz  - 08/14/2010 Rearranged hierarchy of annotation types.  
psz  - 08/26/2009 introduced distinction between persistent and volatile
       annotations; persistent are those derived from external
       annotation, such as imported phi from MIMIC or human annotators,
       whereas volatile are ones derived by LATE programs, hence
       reproducible, though perhaps at great computational cost.
psz  -            changed annotations-tree to use an interval-tree rather than
       a red-black tree for storage, other minor clean-ups
yluo - 07/17/2009 add immuchem-annotation
yluo - 07/14/2009 fix the phrase-annotation bugs, in slot definition, we
       should use phrase-annoation instead of 'phrase-annotation.
yluo - 07/12/2009 fix the :integer type bugs
psz  - 6/25/2009 added top class late-object, changed lp-token, deprecated
       lp-annotation, added umls-annotation.
psz  -            rewrite, abandoning Allegrocache and using instead Mysql!
yluo - 11/26/2008 add next-anns and prev-anns
yluo - 11/25/2008 add phrase annotation
yluo - 11/01/2008 modify and overload get-annotations method 
yluo - 10/16/2008 add links annotation
psz  -            creation 
|#

(defpackage :late
  (:use :common-lisp :util #+allegro :excl)
  (:export 
   "*PHI-tags*"
   "*annotation-abbrevs*"
   "*clinical-tags*"
   "*h-flow-spec*"
   "*medical-tags*"
   "accession-number-pattern"
   "analyses"
   "annotation"
   "annotations"
   "annotations-deleted"
   "annotations-tree"
   "asp-token"
   "bindings"
   "chunk-tag"
   "chunk-tag-opennlp"
   "clinical-tag"
   "concept-tag"
   "content"
   "content"
   "corpus"
   "cross-ref-annotation"
   "format-mark-annotation"
   "cui-annotation"
   "data"
   "date-pattern"
   "date-range-pattern"
   "decoder-annotation"
   "decoder-karyotype"
   "dep-deps"
   "description"
   "dirty"
   "dirty-annotations"
   "discrete-num"
   "do-corpus"
   "document"
   "document"
   "document-annotation"
   "documents"
   "end"
   "focus"
   "gov-deps"
   "gtype"
   "gsw-ver"
   "gkey"
   "gsetting"
   "ganalysis"
   "h-down"
   "h-up"
   "id"
   "immuchem-annotation"
   "interpretation"
   "item-content-pair"
   "late-object"
   "lcui-annotation"
   "left-links"
   "list-head-annotation"
   "list-item-annotation"
   "lp-token"
   "lparse-interpretation"
   "ltype"
   "map-corpus"
   "mask-annotation"
   "medical-tag"
   "mesh-annotation"
   "metamap-con"
   "metamap-neg"
   "missed-list-item-pattern"
   "modifiers"
   "name"
   "number-list-pattern"
   "numeric-data-pattern"
   "parse-annotation"
   "parse-annotation-link"
   "parse-annotation-link-biased"
   "parse-annotation-link-hier"
   "parse-annotation-opennlp"
   "parse-annotation-stanford"
   "parse-annotation-stanford-hier-tagged"
   "parse-annotation-stanford-hier-tokenized"
   "parse-annotation-stanford-tagged"
   "parse-annotation-stanford-tokenized"
   "parse-annotation-stanford-tokenized-constrained"
   "parse-node"
   "parse-node-link"
   "parse-node-link-biased"
   "parse-node-link-hier"
   "parse-node-link-reg"
   "parse-node-stanford"
   "parse-node-stanford-hier-tagged"
   "parse-node-stanford-hier-tokenized"
   "parse-node-stanford-tagged"
   "parse-node-stanford-tokenized"
   "parse-node-stanford-tokenized-constrained"
   "parsing"
   "pattern-section-annotation"
   "percentage"
   "persistent-annotation"
   "ph-end"
   "ph-start"
   "phi-pseudonym"
   "phi-tag"
   "phone-number-pattern"
   "phrase-annotation"
   "phrase-interpretation"
   "phrase-interpretation"
   "phrase-opennlp"
   "pnode-pos-tag"
   "pos-tag"
   "pos-tag-opennlp"
   "pos-tag-stanford"
   "pos-tag-stanford-umls"
   "positivity"
   "prefix-start"
   "print-object"
   "prob"
   "processor"
   "properties"
   "quantize-annotation"
   "range-of-ratios-pattern"
   "ratio-of-ranges-pattern"
   "res-text"
   "right-links"
   "sample-number-pattern"
   "section-annotation"
   "section-head-annotation"
   "sectionp"
   "sem-annotation"
   "sentence-annotation"
   "setting-key"
   "show-classes"
   "show-content"
   "show-data"
   "size"
   "source"
   "sp-lex-annotation"
   "sp-pos-annotation"
   "sp-token"
   "start"
   "stem"
   "style"
   "sw-ver-key"
   "tag"
   "time-pattern"
   "token-annotation"
   "token-interpretation"
   "tui-annotation"
   "umls-annotation"
   "updated_at"
   "valid-tags"
   "volatile-annotation"
   "whole-pat"
   "*ml-features*"
   "*hg-ann-types*"
   "set-gtype"
   "*flow-spec*"
   "*ml-spec*"
   "*l-link-parse*"
   "*l-stanford-parse*"
   "adj-decode"
   ))

(in-package :late)


(defparameter *h-flow-spec* (make-hash-table :test #'equalp))
(defparameter *l-link-parse* (list 'parse-link-biased 'parse-link-hier))
(defparameter *l-stanford-parse* 
  (list 'parse-stanford-tagged 
		'parse-stanford-tokenized
		'parse-stanford-tokenized-constrained))

(defparameter *h-sw-ver* (make-hash-table :test #'equalp))
(defparameter *ml-features* nil
  "The features used in machine learning steps, read from ml-spec.cl")
(defparameter *hg-ann-types* (make-hash-table :test #'equalp))
(defparameter *flow-spec* nil)
(defparameter *ml-spec* nil)

(defclass late-object () ())

(defclass document (late-object)
  (
   ;; the database ID of this document
   (id :accessor id
       :initarg :id
       :initform nil)
   ;; a name by which to refer to or display the document
   (name :accessor name
	 :initarg :name
	 :initform  (gensym "document-"))
   (data :accessor data
	 :initarg :data
	 :initform nil)
   ;; the source, either a file or URL
   (source :accessor source
	   :initarg :source
	   :initform nil)
   ;; textual description of the document, if any
   (description :accessor description
		:initarg :description
		:initform nil
		:type string)
   ;; The length of the contents
   (size :accessor size
	 :initarg :size
	 :initform nil
	 :type integer)
   ;; A flag showing whether a document has changed, and therefore
   ;; needs to be written back to the data store. New documents are
   ;; dirty. There are two flags: one says whether the basic
   ;; information about the document has changed, the other whether
   ;; its annotations have changed.
   (dirty :accessor dirty
	  :initarg :dirty
	  :initform nil)
   (dirty-annotations :accessor dirty-annotations
		      :initarg :dirty-annotations
		      :initform nil)
   ;; The timestamp of last modification of this document (or its
   ;; annotations) in the data store. Used to detect conflicting
   ;; changes.
   (updated_at :accessor updated_at
	      :initarg :updated_at
	      :initform nil
	      :type string)
   ;; All annotations on a document (even if they are interlinked to
   ;; each other) are stored here.
   (annotations-tree :accessor annotations-tree
		     :initform (make-instance 'interval-tree)
		     :type interval-tree)
   (annotations-deleted :accessor annotations-deleted
			:initform nil)
   ;; a string holding the contents of the document
   (content :accessor content
	    :initarg :content
	    :initform nil
	    :type string)
   ;; performed analyses, is a list of strings denoting analyses performed
   ;; so far, the following annotation type string are used:
   ;; secionized, sentencized, link-tokenized, link-parsed, biased-link-parsed,
   ;; hier-link-parsed
   (analyses :accessor analyses
			 :initarg :analyses
			 :initform nil
			 :type list)))


(defmethod print-object ((doc document) stream)
  (format stream "#<document ~d ~a (~d annotations): ~s>"
	  (id doc)
	  (name doc)
	  (tree-size (annotations-tree doc))
	  (text-abbrev (content doc) 50)))


(defgeneric annotations ((ann annotation)
			 &key type relation filter))

(defclass corpus (late-object)
  (
   ;; ID number of the corpus
   (id :accessor id
       :initarg :id
       :initform nil)
   ;; name of the corpus; often directory from which documents were loaded
   (name :accessor name
	 :initarg :name
	 :initform (symbol-name (gensym "corpus-")))
   ;; an XML spec used by sectionize to find structure in documents of
   ;; this corpus
   (description :accessor description
		:initarg :description
		:initform nil)
   ;; a list of the document ID's (not the documents themselves) in
   ;; this corpus
   (documents :accessor  documents
	      :initarg :documents
	      :initform nil)))

(defmethod print-object ((c corpus) stream)
  (format stream "#<corpus ~a, ~d documents~@[, with a section spec~]>"
	  (name c)
	  (length (documents c))
	  (description c)))

(defun map-corpus (proc corpus)
  "Maps over the documents in a corpus, applying proc to each, and
   returning the list of results."
  (mapcar #'(lambda (doc-id)
	      (funcall proc (document doc-id)))
	  (documents (corpus corpus))))

(defmacro do-corpus ((doc-name corpus) &body body)
  "Macro iterates over the documents belonging to corpus, binding the
  given var to each successive document. Corpus may be specified as an
  actual corpus object or the number or name of a corpus."
  (let ((docnum-var (gensym)))
    `(dolist (,docnum-var (documents (corpus ,corpus)))
       (let ((,doc-name (document ,docnum-var)))
	 ,@body))))
#|

All annotations are implemented as stand-off annotations that contain
the start and end positions within their specific document. We subtype
annotation to record different types of annotations. Annotations may
also use a name field to record data specific to their type. For
hierarchical annotations, such as section headings and subheadings, we
also support h-up and h-down to hold the containing and list of
\(immediately) contained annotations, respectively. This hierarchy is
used only for nested sections, as specified by the xml file guiding
findstruct.cl.

Other hierarchic relations among annotations are all computed
dynamically from the types of the annotations and their start and end
positions.  For example, if sent is a sentence annotation, we can find
all the tokens in the sentence as
\(annotations sent :type 'token-annotation)

All annotations are stored in the annotations of the
relevant document, and each annotation also links back to its
document. We store annotations in an interval-tree, hence we declare
annotation to inherit from interval. This is where its start and end
slots come from.

|#

(defclass annotation (late-object interval)
  ((id :accessor id
       :initarg :id
       :initform nil)
   (document :accessor document
			 :initarg :document
			 :initform nil)
   (data :accessor data
		 :initarg :data
		 :initform nil)
   (h-up :accessor h-up
		 :initarg :h-up
		 :initform nil)
   (h-down :accessor h-down
		   :initarg :h-down
		   :initform nil
		   :type list)
   (sw-ver :accessor sw-ver
		   :initarg :sw-ver
		   :type string
		   :initform nil)
   (sw-ver-key :accessor sw-ver-key
			   :initform nil)
   (setting :accessor setting
			:initarg :setting
			:type string
			:initform nil)
   (setting-key :accessor setting-key
				:initform "")
   ;; default to t, modify with caution.
   (dirty :accessor dirty
		  :initarg :dirty
		  :initform t)
   (new :accessor new
		:initarg :new
		:initform t))
  (:documentation
   "The fundamental class for all annotations."))

(defclass persistent-annotation (annotation)
  ()
  (:documentation
   "Annotations derived from sources that cannot be recomputed, e.g.,
  human annotators or external programs. These should not be deleted
  so long as the document they annotate remains in the LATE data
  store."))

(defclass volatile-annotation (annotation)
  ()
  (:documentation
   "Annotations computed by some LATE component, which can therefore
  be re-computed, though perhaps at significant cost. This class of
  annotations may be flushed from the LATE data store if, for
  instance, we alter the way they should be computed."))

#|

Section annotations note both the text that was used to determine the
beginning of a section (section-head-annotation) and the
section-annotation itself, which marks the content of the
section. These use the h-up and h-down links to maintain the record of
the hierarchy.  Note that both the section-head-annotation and
section-annotation belonging to a subsection will be parts of h-down
of the enclosing section-annotation.

|#

(defclass section-annotation (volatile-annotation)
  ((prefix-start
    :accessor prefix-start
    :initarg :prefix-start
    :initform 0
    :type integer)
   (processor				; way to process it, narrative or coded
    :accessor processor
    :initarg :processor
    :initform nil
    :type string)))

(defclass section-head-annotation (section-annotation)
  ())

(defclass document-annotation (section-annotation)
  ())

(defclass pattern-section-annotation (section-annotation)
  ((bindings :accessor bindings
	     :initarg :bindings
	     :initform nil
	     :type list)))

(defclass list-item-annotation (section-annotation)
  ())

(defclass list-head-annotation (section-head-annotation)
  ())

(defclass phi-pseudonym (persistent-annotation) ())

(defmethod show-data ((ann annotation))
  "Formats the data content of various types of annotations. By
  default, the full content is included in the print-object text."
  (data ann))

(defparameter *annotation-abbrevs*
  '(document-annotation DOC
			pattern-section-annotation SApat
			annotation AN
			section-annotation SA
			section-head-annotation SH
			sentence-annotation sent
			token-annotation tok
			lp-token lptok
			lp-annotation lp
			pos-tag pos
			list-head-annotation LH
			list-item-annotation LI
			phi-pseudonym PHI
			immuchem-annotation IC
			umls-annotation UMLS
			cui-annotation CUI
			lcui-annotation LCUI
			tui-annotation TUI
			mesh-annotation MeSH
			sem-annotation SEM
			sp-pos-annotation SP-POS)
  "For convenience, we can abbreviate the names of various annotation
    types, as listed in this property-list (alternating type/abbrev
    pairs).")

(defmethod print-object ((a annotation) stream)
  (format stream "#<~a ~a (~a-~a):~@[~a:~] ~s>"
	  (getf *annotation-abbrevs* (type-of a) (type-of a))
	  (id a) (start a) (end a)
	  (show-data a)
	  (if (and (document a)
		   (content (document a)))
	      (if (<= (start a) (end a))
		  (text-abbrev (subseq (content (document a))
				       (start a)
				       (end a))
			       50)
		(format nil "***Invalid start/end: ~a/~a***"
			(start a) (end a)))
	    nil)))


(defclass coded-text (volatile-annotation)
  ((sw-ver-key :initform 'gcoded-text)
   (setting-key :initform 'gcoded-text))
  (:documentation
   "Coded text class."))

(defclass if-list (volatile-annotation)
  ((sw-ver-key :initform 'gif-list)
   (setting-key :initform 'gif-list))
  (:documentation
   "immunologic factor list class."))

(defclass if-adj (volatile-annotation)
  ((sw-ver-key :initform 'gif-adj)
   (setting-key :initform 'gif-adj))
  (:documentation
   "immunologic factor list class."))

(defclass range-adj (volatile-annotation)
  ((sw-ver-key :initform 'grange-adj)
   (setting-key :initform 'grange-adj))
  (:documentation
   "immunologic factor list class."))

(defclass metamap-con (volatile-annotation)
  ((head :accessor head :initarg :head :initform nil)
   (sw-ver-key :initform 'gmetamap)
   (setting-key :initform 'gmetamap))
  (:documentation
   "MetaMap concept class."))

(defclass metamap-neg (volatile-annotation)
  ((sw-ver-key :initform 'gmetamap)
   (setting-key :initform 'gmetamap))
  (:documentation
   "MetaMap negation class."))


;;; I've changed the way umls-annotations are stored so that rather
;;; than storing a single umls-annotation on a token or phrase, which
;;; holds all its umls interpretations, we will store a large number
;;; of individual annotations on that same token or phrase, one for
;;; each interpretation.  Thus,
;;;  (annotations-on foo :type 'cui-annotation)
;;; could result in a list of several cui-annotations, and similarly
;;; for the other annotation types. This makes it possible to retrieve
;;; occurrences of various interpretations without the need to search
;;; through the data in a single umls-annotation.  Those now become
;;; simple a category label for such annotations.

(defclass umls-annotation (volatile-annotation)
  ((sw-ver-key :initform 'gumlsize)
   (setting-key :initform 'gumlsize))
  (:documentation
   "Parent class for specific annotation types derived from UMLS."))

(defclass cui-annotation (umls-annotation)
  ()
  (:documentation
   "Annotation giving a CUI interpretation of a section of text. A CUI
  is a concept ID for what the Metathesaurus considers to be a unique
  concept.")) 

(defclass sp-lex-annotation (umls-annotation)
  ((match :accessor match :initarg :match :initform nil))
  (:documentation
   "Annotation giving a Specialist Lexicon interpretation of a section of text. 
A Specialist lexical is an entry for what the Lexicon considers to be a unique
concept."))

(defclass lcui-annotation (cui-annotation)
  ((ltype :accessor ltype :initarg :ltype :initform nil))
  (:documentation
   "Annotation giving a CUI interpretation of a section of text as
  determined by the lite parser"))

(defclass tui-annotation (umls-annotation)
  ()
  (:documentation
   "Annotation giving a TUI interpretation of a section of text. A TUI
  is a semantic type ID for one of the 189 semantic categories in the
  UMLS Semantic Network."))

(defclass mesh-annotation (umls-annotation)
  ()
  (:documentation
   "Annotation giving a MeSH interpretation of a section of text. This
  is a Medical Subject Heading, a stable taxonomy used to index the
  biomedical literature in Pubmed."))

(defclass sem-annotation (umls-annotation)
  ()
  (:documentation
   "Annotation according to the broad semantic categories defined as
  unions of UMLS semantic network categories in umlsize. This
  categorization was created by Dr. William Long from a similar
  earlier effort by Tawanda Sibanda. It is helpful when we want to
  characterize a term by a broad category such as 'medication' rather
  than the more refined TUI's that distinguish different sorts of
  medications."))

(defclass sp-pos-annotation (umls-annotation)
  ()
  (:documentation
   "Annotation on individual tokens showing (one of) the Specialist
  lexicon parts of speech possible for that token."))

(defclass sentence-annotation (volatile-annotation)
  ())



;; the phrase start will be start of the first token
;; the phrase end will be end of the last token

(defclass phrase-annotation (volatile-annotation)
  ((sw-ver-key :initform 'gchunkize)
   (setting-key :initform 'gchunkize)))

(defclass phrase-opennlp (phrase-annotation)
  ())

(defclass parse-annotation (volatile-annotation)
  ((style :accessor style
	  :initarg :style
	  :type string
	  :initform nil)
   (res-text :accessor res-text
	     :initarg :res-text
	     :type string
	     :initform nil)
   (sw-ver-key :initform 'gparse)
   (setting-key :initform 'gparse))
  (:documentation
   "Annotation on individual sentence showing the text output of a parse."))

(defclass parse-annotation-stanford (parse-annotation)
  ())

(defclass parse-annotation-stanford-tagged (parse-annotation-stanford)
  ())

(defclass parse-annotation-stanford-tokenized (parse-annotation-stanford)
  ())

(defclass parse-annotation-stanford-hier-tokenized (parse-annotation-stanford)
  ())

(defclass parse-annotation-stanford-hier-tagged (parse-annotation-stanford)
  ())

(defclass parse-annotation-stanford-tokenized-constrained (parse-annotation-stanford)
  ())

(defclass parse-annotation-opennlp (parse-annotation)
  ())

(defclass parse-annotation-link (parse-annotation)
  ())

;;; use the data slot for keys, the other slot stores feature values etc. 
(defclass item-content-pair (volatile-annotation)
  ((content :accessor content
	    :initarg :content
	    :type string
	    :initform nil)))

(defclass mask-annotation (volatile-annotation)
  ((sw-ver-key :initform 'gmask)
   (setting-key :initform 'gmask)))

;;; immuchem-annotation includes CDs, Bcls, FISH etc. Please refer to 
;;; lymphoma immunophenotype.doc for details
(defclass immuchem-annotation (volatile-annotation)
  ((positivity :accessor positivity
	       :initarg :positivity
	       :type string
	       :initform nil)
   (percentage :accessor percentage
	       :initarg :percentage
	       :type integer
	       :initform nil)))

(defclass token-annotation (volatile-annotation)
  ((stem :accessor stem
	 :initarg :stem
	 :type list
	 :initform nil)
   (sw-ver-key :initform 'gtokenize)
   (setting-key :initform 'gtokenize)))

(defclass cross-ref-annotation (volatile-annotation)
  ((sw-ver-key :initform 'gcross-ref)
   (setting-key :initform 'gcross-ref)))

(defclass format-mark-annotation (volatile-annotation)
  ((sw-ver-key :initform 'gformat-mark)
   (setting-key :initform 'gformat-mark)))

(defclass quantize-annotation (volatile-annotation)
  ((sub-counts :accessor sub-counts
			   :initarg :sub-counts
			   :type list
			   :initform nil)
   (sw-ver-key :initform 'gquantize)
   (setting-key :initform 'gquantize)))

;;; let it be a subclass of tok instead of volatile-annotation?
(defclass lp-token (token-annotation)
  ((left-links :accessor left-links
	       :initarg :left-links
	       :initform nil)
   (right-links :accessor right-links
		:initarg :right-links
		:initform nil))
  )

;;; for stanford parsers
(defclass sp-token (token-annotation)
  ;; dependencies where the token is the governor
  ((gov-deps :accessor gov-deps			
	     :initarg :gov-deps
	     :initform nil)
   ;; dependencies where the token is the dependent
   (dep-deps :accessor dep-deps
	     :initarg :dep-links
	     :initform nil)))

;;; for opennlp parser
(defclass opennlp-token (token-annotation)
  ((left-links :accessor left-links
	       :initarg :left-links
	       :initform nil)
   (right-links :accessor right-links
		:initarg :right-links
		:initform nil)))

(defclass parse-node (volatile-annotation)
  ((head :accessor head
	 :initarg :head
	 :initform nil)
   (sw-ver-key :initform 'gparse)
   (setting-key :initform 'gparse)))



(defclass parse-node-link (parse-node)
  ((left-links :accessor left-links
	       :initarg :left-links
	       :initform nil)
   (right-links :accessor right-links
		:initarg :right-links
		:initform nil)))

(defclass parse-node-link-reg (parse-node-link)
  ())

(defclass parse-node-link-biased (parse-node-link)
  ())

(defclass parse-node-link-hier (parse-node-link)
  ((sw-ver-key :initform 'ghier-parse)
   (setting-key :initform 'ghier-parse)))

(defclass parse-node-stanford (parse-node)
  ((gov-deps :accessor gov-deps
	     :initarg :gov-deps
	     :initform nil)
   (dep-deps :accessor dep-deps
	     :initarg :dep-deps
	     :initform nil)))


(defclass parse-node-stanford-tokenized (parse-node-stanford)
  ())

(defclass parse-node-stanford-tagged (parse-node-stanford)
  ())

;;; coded - nil, t, "substitute"
(defclass parse-node-stanford-hier-tagged (parse-node-stanford)
  ((coded :accessor coded :initarg :coded :initform nil)
   (if-list :accessor if-list :initarg :if-list :initform nil)
   (if-adj :accessor if-adj :initarg :if-adj :initform nil)
   (range-adj :accessor range-adj :initarg :range-adj :initform nil)
   (adj-decode :accessor adj-decode :initarg :adj-decode :initform nil)
   (pnode-pos-tag :accessor pnode-pos-tag
		  :initarg :pnode-pos-tag
		  :initform nil)
   (sw-ver-key :initform 'ghier-parse)
   (setting-key :initform 'ghier-parse)))

(defclass parse-node-stanford-hier-tokenized (parse-node-stanford)
  ((pnode-pos-tag :accessor pnode-pos-tag
		  :initarg :pnode-pos-tag
		  :initform nil)
   (sw-ver-key :initform 'ghier-parse)
   (setting-key :initform 'ghier-parse)))

(defclass parse-node-stanford-tokenized-constrained (parse-node-stanford)
  ())

(defmethod print-object ((a parse-node-stanford-hier-tagged) stream)
  (format stream "#<~a ~a (~a-~a):~@[~a [~a]:~] ~s>"
	  (getf *annotation-abbrevs* (type-of a) (type-of a))
	  (id a) (start a) (end a)
	  (show-data a)
	  (pnode-pos-tag a)
	  (if (and (document a)
		   (content (document a)))
	      (if (<= (start a) (end a))
		  (text-abbrev (subseq (content (document a))
				       (start a)
				       (end a))
			       50)
		(format nil "***Invalid start/end: ~a/~a***"
			(start a) (end a)))
	    nil)))

#||
;;; lp-annotation is on its way to oblivion.  Instead of storing the
;;; results of a link parse in this type of annotation on a sentence,
;;; we now store the relevant links on the individual lp-tokens.  This
;;; is retained for now, in case we want to revert.  See notes in
;;; link-parse about *link-annotation-type*.

(defclass lp-annotation (volatile-annotation) ()
	  )

(defmethod show-data ((ann lp-annotation))
  "An lp-annotation has a fairly large and complex data structure as
  its data element. Here we just summarize it."
  (format nil "~d+~d(~d)"
	  (length (lp-common (data ann)))
	  (length (lp-distinct-list (data ann)))
	  (reduce #'+ (mapcar #'length (lp-distinct-list (data ann))))))
||#

#|| These tag types are an early attempt to create a way to enumerate
the possible values of various tags, but this is probably too naive an
approach.  For now, we eliminate them.
(defparameter *PHI-tags*
    '(:non-phi :patient :physician :date :location :institution :id :phone))

(defparameter *clinical-tags*
    '(:none :disease :symptom :treatment :result :substance :practitioner
      :dosage :test))

(defparameter *medical-tags*
    '(:patient :physician :date :location :institution :id :phone
      :disease :symptom :treatment :result :substance :practitioner
      :dosage :test))
||#

#||
(defclass links (volatile-annotation)
  ((l-link :accessor l-link
	   :initarg :l-link
	   :initform nil)
   (r-link :accessor r-link
	   :initarg :r-link
	   :initform nil)))
||#

(defclass tag (volatile-annotation)
  (#||(valid-tags :reader valid-tags
	       :initarg :valid-tags
	       :initform nil)||#))

(defclass phi-tag (tag)
  ()
  #|(:default-initargs :valid-tags *PHI-tags*)|#)

(defclass clinical-tag (tag)
  ()
  #|(:default-initargs :valid-tags *clinical-tags*)|#)

;; should users have this kind of freedom?
(defclass concept-tag (tag)
  ())


(defclass medical-tag (tag)
  ()
  #|(:default-initargs :valid-tags *medical-tags*)|#)

(defclass chunk-tag (tag)
  ((sw-ver-key :initform 'gchunkize)
   (setting-key :initform 'gchunkize)))

(defclass chunk-tag-opennlp (chunk-tag)
  ())

(defclass pos-tag (tag)
  ((prob :accessor prob
	 :initarg :prob
	 :initform 1.0
	 :type float)
   (sw-ver-key :initform 'gtagize)
   (setting-key :initform 'gtagize)))


(defclass discrete-num (volatile-annotation)
  ())

(defclass decoder-annotation (volatile-annotation)
  ((decoded :accessor decoded
	 :initarg :decoded
	 :initform nil
	 :type string)))

(defclass decoder-karyotype (decoder-annotation)
  ())

(defclass pos-tag-opennlp (pos-tag)
  ())

(defclass pos-tag-stanford (pos-tag)
  ())

(defclass pos-tag-stanford-umls (pos-tag)
  ()
  (:documentation 
   "POS-tag produced by Stanford parser while corrected by looking up the UMLS
Specialist Lexicon."))


(defmethod content ((ann annotation))
  "Retrieves the string that is being annotated by this
  annotation. Note that it may be empty for a point-annotation."
  (subseq (content (document ann)) (start ann) (end ann)))

(defclass interpretation (volatile-annotation)
  ((properties :accessor properties
	       :initarg :properties
	       :initform nil))
  (:documentation
   "An interpretation of some phrase or token. This abstract class is
  meant to be subclassed so that the class types show the overall
  meaning of the interpretation. The data and properties give
  details."))

(defclass token-interpretation (interpretation)
  ())

(defclass token-pattern-interpretation (token-interpretation)
  ())

(defclass numeric-data-pattern (token-pattern-interpretation)
  ((label :accessor label :initarg :label :initform nil)
   (reln :accessor reln :initarg :reln :initform nil)
   (conn :accessor conn :initarg :conn :initform nil)
   (num2 :accessor num2 :initarg :num2 :initform nil)
   (sep :accessor sep :initarg :sep :initform nil)
   (units :accessor units :initarg :units :initform nil)
   (star :accessor star :initarg :star :initform nil)
   (sharp :accessor sharp :initarg :sharp :initform nil)))

(defclass time-pattern (token-pattern-interpretation)
  ((hr :accessor hr :initarg :hr :initform nil)
   (mn :accessor mn :initarg :mn :initform nil)
   (sec :accessor sec :initarg :sec :initform nil)
   (ampm :accessor ampm :initarg :ampm :initform nil)))

(defclass date-pattern (token-pattern-interpretation)
  ((yr :accessor yr :initarg :yr :initform nil)
   (mo :accessor mo :initarg :mo :initform nil)
   (da :accessor da :initarg :da :initform nil)))

(defclass date-range-pattern (token-pattern-interpretation)
  ((yr1 :accessor yr1 :initarg :yr1 :initform nil)
   (mo1 :accessor mo1 :initarg :mo1 :initform nil)
   (da1 :accessor da1 :initarg :da1 :initform nil)
   (yr2 :accessor yr2 :initarg :yr2 :initform nil)
   (mo2 :accessor mo2 :initarg :mo2 :initform nil)
   (da2 :accessor da2 :initarg :da2 :initform nil)))

(defclass accession-number-pattern (token-pattern-interpretation)
  ())

(defclass phone-number-pattern (token-pattern-interpretation)
  ((phone-type :accessor phone-type :initarg :phone-type :initform nil)
   (country :accessor country :initarg :country :initform nil)
   (area :accessor area :initarg :area :initform nil)
   (exch :accessor exch :initarg :exch :initform nil)
   (num :accessor num :initarg :num :initform nil)))

(defclass range-of-ratios-pattern (token-pattern-interpretation)
  ((n1 :accessor n1 :initarg :n1 :initform nil)
   (d1 :accessor d1 :initarg :d1 :initform nil)
   (n2 :accessor n2 :initarg :n2 :initform nil)
   (d2 :accessor d2 :initarg :d2 :initform nil)
   (sep :accessor sep :initarg :sep :initform nil)
   (star :accessor star :initarg :star :initform nil)
   (sharp :accessor sharp :initarg :sharp :initform nil)))


(defclass ratio-of-ranges-pattern (token-pattern-interpretation)
  ((n1 :accessor n1 :initarg :n1 :initform nil)
   (d1 :accessor d1 :initarg :d1 :initform nil)
   (n2 :accessor n2 :initarg :n2 :initform nil)
   (d2 :accessor d2 :initarg :d2 :initform nil)
   (sep :accessor sep :initarg :sep :initform nil)
   (star :accessor star :initarg :star :initform nil)
   (sharp :accessor sharp :initarg :sharp :initform nil)))

(defclass number-list-pattern (token-pattern-interpretation)
  ((nums :accessor nums :initarg :nums :initform nil)
   (seps :accessor seps :initarg :seps :initform nil)))

(defclass missed-list-item-pattern (token-pattern-interpretation)
  ((num :accessor num :initarg :num :initform nil)))

(defclass gazette-annotation (phrase-interpretation)
  ;; Initially, I had this as a subtype of token-interpretation, but
  ;; because there are multi-token gazette entries, it is a phrase
  ;; interpretation. psz
  ((rank :accessor rank :initarg :rank :initform nil)
   (frequency :accessor frequency :initarg :frequency :initform nil)
   (cum :accessor cum :initarg :cum :initform nil)))

(defclass phrase-interpretation (interpretation)
  ())

(defclass lparse-interpretation (interpretation)
  ;; interpretation of concept and modifiers from lparse
  ;; focus is an lcui-annotation and modifiers is a list of them
  ;; context will be needed but not yet used
  ((focus :accessor focus :initarg :focus :initform nil)
   (modifiers :accessor modifiers :initarg :modifiers :initform nil)
   (context :accessor context :initarg :context :initform nil)))



(defun sectionp (a)
  (typep a 'section-annotation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility to trace out the LATE classes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-classes (&optional (top (find-class 'late-object))
			       (depth 0))
  (format t "~%~vT~a" depth (class-name top))
  (mapc #'(lambda (s) (show-classes s (+ depth 3)))
	(mop:class-direct-subclasses top))
  t)

(defun gtype (gkey)
  "Please only use gtype for LATE defined annotation types!"
  (let* ((type (gethash (intern gkey :late) *hg-ann-types*)))
    (cond 
	  ((stringp type)
	   (intern type))
	  ((symbolp type)
	   type)
	  (t
	   (error "missing gtype key ~a" gkey)))))

(defun set-gtype (key type)
  (setf (gethash key *hg-ann-types*) (intern type :late)))

(defun ganalysis (gkey)
  (getf (getf *flow-spec* (intern gkey :late)) 'analysis))

(defun gsw-ver (gkey)
  (getf (getf *flow-spec* (intern gkey :late)) 'ver))

(defun gsetting (gkey)
  (getf (getf *flow-spec* (intern gkey :late)) 'setting))

(defun gkey (gmodule gkey)
  (getf (getf *flow-spec* (intern gmodule :late)) (intern gkey :late)))


(defun whole-pat (pattern)
  "enxsure this pattern matches from beginning to end."
  (let* (ans)
	(cond 
	 ((match-re "^\\^" pattern)
	  (setf ans pattern))
	 (t
	  (setf ans (concatenate 'string "^" pattern))))
	(unless (match-re "\\$$" ans)
	  (setf ans (concatenate 'string ans "$")))
	ans))

#+allegro
(defmethod show-content ((doc document) &optional (buffer nil))
  (lep:with-output-to-temp-buffer (s (or buffer
					 (format nil
						 "Document ~s (id=~d)"
						 (name doc)
						 (id doc))))
				  (print (content doc) s)))



