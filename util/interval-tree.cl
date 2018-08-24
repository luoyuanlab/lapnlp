;;; -*- Mode: Lisp; Package: util; -*-

#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 05/07/2012 changed start and end slots of interval to modifiable.
yluo - 03/09/2012 added allenr-uo, allenr-rel, not for now ...
psz  - 08/30/2009 creation

This is an implementation of interval trees as an extension of
red-black trees somewhat after the presentation in Cormen, Leiserson
and Rivest's "Introduction to Algorithms." The extensions are
described in various other sources, and summarized in the Wikipedia
entry for interval trees. From that description, we follow the CLRS
approach. Instead of just adding maxend to each node, we also add
minend, to make search for various additional interval relations more
efficient.

In principle, the implementation should simply augment the CLOS class
based implementation of red-black trees, but it would take too much
effort to generalize that underlying code to allow this.  Therefore, I
have just copied that implementation and then altered it to store and
search interval trees instead. The implementation is somewhat simpler
because we restrict the "keys" to integers and comparisons to = and <.

The class interval is expected to be mixed in to any object to be
stored in the interval tree, and provides start and end positions,
which are integers.  Note that we assume that the start and end
positions of any interval may be expressed by fixnums! This will fail
if any are > most-positive-fixnum, which in Franz Allegro is
536870911. For out applications in text processing, this restricts the
maximum size of document to that many characters. To eliminate this
restriction, the type declarations of start, end, and various local
variables that depend on them would need to be changed from fixnum to
integer.

This implementation was written by Peter Szolovits, psz@mit.edu, in
August 2009.

|#

(defpackage :util
  (:use :common-lisp #+allegro :excl)
  (:export "interval" "int<" "int=" "start" "end"
	   "interval-tree" "root" "nilnode"
	   "it-node" "it-item" "it-left" "it-right" "it-red?" "it-parent" "maxend" "minend"
	   "inorder-tree-walk" "inorder-tree-walk-rev"
	   "do-tree" "do-tree-rev" "tree-search-node" "show"
	   "*allen-relations*" "*allen-augmented*"
	   "*allen-augmented-relations*"
	   "allen" "allenr" "allenr-in" "allenr-ar" "allenr-<m" "allenr->mi"
	   ;; the Allen relations defined by def-tree-search are
	   ;; exported in that macro
	   "tree-search" "next->mi" "prev-<m"
	   "tree-depth" "b-depth" "tree-size"
	   "tree-insert" "tree-delete"
	   ))

(in-package :util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic definition of intervals, which are all that can be stored in
;;; interval-trees. 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass interval ()
  ((start :accessor start :initarg :start :initform nil :type fixnum)
   (end :accessor end :initarg :end :initform nil :type fixnum))
  (:documentation "A mixin that defines an interval, for use in
  interval trees."))

(defmethod initialize-instance :before ((int interval) &rest args)
  (let ((s (getf args :start nil))
	(e (getf args :end nil)))
    (assert (and s
		 (<= most-negative-fixnum s most-positive-fixnum)
		 e
		 (<= most-negative-fixnum e most-positive-fixnum))
	()
      ":start (~a) and :end (~a) arguments to creating an interval must be expressible as fixnums, but these are not."
      s e)))

(defun int< (int1 int2)
  "Comparison tells if int1 comes before int2 in canonical
  order. Intervals are sorted by start, then by end."
  (let ((s1 (start int1))
	(e1 (end int1))
	(s2 (start int2))
	(e2 (end int2)))
    (declare (fixnum s1 s2 e1 e2)
	     (optimize (speed 3) (safety 0) (space 0)))
    (or (< s1 s2)
	(and (= s1 s2) (< e1 e2)))))

(defun int= (int1 int2)
  "Predicate determines if int1 and int2 span the same interval."
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (and (= (start int1) (start int2))
       (= (end int1) (end int2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Definition of interval-tree and it-node (interval tree node)
;;; Note that we use a single nilnode as a unique sentinel in each
;;; interval-tree.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass interval-tree ()
  ((root :accessor root :initarg :root)
   (nilnode :accessor nilnode :initarg :nilnode)))

(defclass it-node ()
  ((item :accessor it-item :initarg :item :type interval)
   (left :accessor it-left :initarg :left)
   (right :accessor it-right :initarg :right)
   (red? :accessor it-red? :initarg :red? :initform nil)
   (parent :accessor it-parent :initarg :parent)
   (maxend :accessor maxend :initarg :maxend :initform nil :type fixnum)
   (minend :accessor minend :initarg :minend :initform nil :type fixnum)))

(declaim (inline make-red make-black nilnode?))

(defmethod make-red ((node it-node))
  "Make the color of it-node RED. Be sure it's not the sentinel!"
  (setf (it-red? node) t))

(defmethod make-black ((node it-node))
  "Make the color of rb-node BLACK. No effect on the sentinel."
  (setf (it-red? node) nil))

(defvar +nilnode-key+ (make-instance 'interval :start 0 :end 0)
  "A unique marker for the item in an it-node for it being a
  NILNODE. We make the item an interval in case some algorithms refer
  to its start and end before checking for nilnode?. Nothing should
  modify this instance.")

(defun make-it-nilnode ()
  (let ((nn (make-instance 'it-node
	      :item +nilnode-key+ :red? nil :maxend 0 :minend 0
	      :parent nil :left nil :right nil)))
    (setf (it-left nn) nn)
    (setf (it-right nn) nn)
    nn))

(defmethod nilnode? ((object t))
  "Tests whether NODE is a sentinel for a null."
  nil)

(defmethod nilnode? ((node it-node))
  "Tests whether NODE is a sentinel for a null."
  (eq +nilnode-key+ (it-item node)))

(defmethod initialize-instance :after
	   ((itree interval-tree) &rest args)
  (let ((nn (make-it-nilnode)))
    (unless (getf args :nilnode) 
      (setf (nilnode itree) nn))
    (unless (getf args :root)
      (setf (root itree) nn))
    itree))

(defmethod print-object ((node it-node) stream)
  "Prints a compact representation of a node of an interval tree, as
  #<NODE*/\ item>
  The * appears if the node is black, / and \ if it has left and right
  branches, and item is the item stored at this node."
  (if (nilnode? node)
      (format stream "#<NILNODE~:[*~;~] p=~s>"
	      (it-red? node)
	      (it-parent node))
    (format stream
	    "#<~a~:[*~;~]~:[/~;~]~:[\\~;~]~:[~;=~] [~d-~d] ~s>"
	    (type-of node)
	    (it-red? node)
	    (nilnode? (it-left node))
	    (nilnode? (it-right node))
	    (nilnode? (it-parent node))
	    (minend node)
	    (maxend node)
	    (it-item node))))

(defmethod print-object ((itree interval-tree) stream)
  "Prints a compact representation of an interval tree."
  (multiple-value-bind (dep bdep) (tree-depth itree)
    (format stream "#<~a ~d items, ~d depth, ~d black depth>"
	    (type-of itree)
	    (tree-size itree)
	    dep
	    bdep)))

(defmethod print-object ((int interval) stream)
  (format stream "#<~a: ~d-~d>" (type-of int) (start int) (end int)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tree traversal operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inorder-tree-walk (f (tree interval-tree))
  (labels ((walk (node)
	     (unless (nilnode? node)
	       (walk (it-left node))
	       (funcall f node)
	       (walk (it-right node)))))
    (walk (root tree))))

(defmethod inorder-tree-walk-rev (f (tree interval-tree))
  (labels ((walk (node)
	     (unless (nilnode? node)
	       (walk (it-right node))
	       (funcall f node)
	       (walk (it-left node)))))
    (walk (root tree))))

(defmacro do-tree-node ((nv tree) &body steps)
  `(inorder-tree-walk
    #'(lambda (,nv) ,@steps)
    ,tree))

(defmacro do-tree ((item tree) &body steps)
  (let ((nv (gensym)))
    `(do-tree-node (,nv ,tree)
       (let ((,item (it-item ,nv)))
	 ,@steps))))

(defmacro do-tree-node-rev ((nv tree) &body steps)
  `(inorder-tree-walk-rev
    #'(lambda (,nv) ,@steps)
    ,tree))

(defmacro do-tree-rev ((item tree) &body steps)
  (let ((nv (gensym)))
    `(do-tree-node-rev (,nv ,tree)
       (let ((,item (it-item ,nv)))
	 ,@steps))))

(defmethod tree-search-node ((tree interval-tree) (item interval)
			     &optional (predicate nil))
  "Returns the it-node that holds the given item, or nil if not
  found. The search is made more complex because we allow multiple
  distinct items that are int= but not eq. For that case, we must
  search to both left and right in a tree when we find a node that is
  int= but not eq to the one we are seeking. If predicate is non-nil,
  then it, instead of eq, is used to compare the current node to the
  argument item."
  (labels ((iter (node)
	     (if (nilnode? node) nil
	       (let ((it (it-item node)))
		 (cond
		  ((if predicate
		       (funcall predicate it item)
		     (eq it item))
		   node)
		  ((not (int< it item))
		   ;; either item < it or item int= it
		   (iter (it-left node)))
		  ((not (int< item it))
		   ;; either item > it or item int= it
		   (iter (it-right node))))))))
    (iter (root tree))))

#|
The successor algorithm depends on the availability of parent links in the tree.
If the node has a right branch, then the successor is just the minimum
along this branch.  If it does not, then we seek to find the first among
its parents that was reached by heading up from a left branch.  (Right branches
taken up lead to lower values, but the first left branch taken up from any
of its ancestors must lead to a node that is the first higher than the
starting node.  The situation is exactly symmetric for predecessor.
|#

(defmethod tree-minimum-node ((tree interval-tree))
  (tree-minimum-node (root tree)))

(defmethod tree-minimum-node ((node it-node))
  (let ((l (it-left node)))
    (if (nilnode? l) node
      (tree-minimum-node l))))

(defmethod tree-maximum-node ((tree interval-tree))
  (tree-maximum-node (root tree)))

(defmethod tree-maximum-node ((node it-node))
  (let ((r (it-right node)))
    (if (nilnode? r) node
      (tree-maximum-node r))))

(defmethod tree-successor ((node it-node))
  (declare (optimize (speed 3) (safety 0)))
  #+:debug
  (assert (not (nilnode? node))
      ()
    "Trying to take successor of a null node ~s" node)
  (let ((r (it-right node)))
    (if (nilnode? r)
      (labels ((climb (higher lower)
		 (if (or (null higher) (nilnode? higher))
                   higher
		   (if (eq (it-right higher) lower)
                     (climb (it-parent higher) higher)
		     higher))))
	(climb (it-parent node) node))
      (tree-minimum-node r))))

(defmethod tree-predecessor ((node it-node))
  (declare (optimize (speed 3) (safety 0)))
  #+:debug
  (assert (not (nilnode? node))
      ()
    "Trying to take predecessor of a null node ~s" node)
  (let ((r (it-left node)))
    (if (nilnode? r)
      (labels ((climb (higher lower)
		 (if (or (null higher) (nilnode? higher))
                   higher
		   (if (eq (it-left higher) lower)
                     (climb (it-parent higher) higher)
		     higher))))
	(climb (it-parent node) node))
      (tree-maximum-node r))))

(defmethod show ((tree interval-tree)
		 &key (stream t) (depth nil))
  (labels ((ns (node level dep)
	     (when (and (not (nilnode? node))
			(or (null dep) (plusp dep)))
	       (ns (it-left node) (+ level 3) (and dep (1- dep)))
	       (format stream "~%~vT~s~a [~d,~d]"
		       level
		       (it-item node)
		       (if (it-red? node) " " "*")
		       (minend node) (maxend node))
	       (ns (it-right node) (+ level 3) (and dep (1- dep))))))
    (ns (root tree) 0 depth)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interval relations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; James Allen defines 13 exhaustive and mutually exclusive relations
;;; (listed below) that can hold between two intervals, if neither is
;;; degenerate (i.e., has the same start and end points). As others
;;; have done, we extend these to define additional useful relation
;;; sets:
;;; s <m t when s < t or s m t (s before-or-meets t)
;;; s >mi t when s > t or s mi t (s after-or-met-by t)
;;; s in t when s s t or s f t or s d t or s = t (s within t,
;;; inclusive)
;;; s ar t when s di t or s fi t or s si t or s = t (s around t,
;;; inclusive)

;;; We do need to support degenerate intervals, however, and this
;;; raises a problem. If s is degenerate, then s m t and s s t are
;;; indistinguishable, whereas if t is degenerate, then s mi t and s fi
;;; t are indistinguishable.  If both are degenerate, then we cannot tell
;;; apart =, m or mi.
;;; For example, (allen [1,1] [1,5]) could be considered either m or
;;; s. We will prefer the = interpretation in the double degenerate
;;; case, and the m or mi interpretations in the former.
;;;
;;; To make tree-search consistent with this policy, we will need to
;;; check, when searching, for these degenerate cases and make sure we
;;; supress the less preferred interpretations. We also run into
;;; problems with the additional relations. E.g., by our rules, the
;;; above example has [1,1] m [1,5] but [1,1] in [1,5], which takes
;;; the s interpretation. Other additional relations have similar
;;; problems.

(defvar *allen-relations* '(< > = m mi o oi s si f fi d di))

(defvar *allen-augmented-relations*
    '((in allenr-in) (ar allenr-ar) (<m allenr-<m) (>mi allenr->mi)))

(defvar *allen-augmented*
    (mapcar #'car *allen-augmented-relations*))

(defmethod allen ((s interval) (ti interval))
  "Computes one of the set of 13 interval relations codified by James
  Allen. The return value is one of (< > m mi o oi s si d di f fi =)."
  (allenr (start s) (end s) (start ti) (end ti)))

(defun allenr (s- s+ t- t+)
  "Determines the Allen interval relation between the interval [s-,s+]
  and [t-,t+]."
  ;; Because the cond is sequential, we will take care of the cases
  ;; where the two intervals have = start or end points before the
  ;; during and overlap comparisons
  (declare (fixnum s- s+ t- t+)
	   (optimize (speed 3) (space 0) (safety 0)))
  (cond ((< s+ t-) :<)
	((> s- t+) :>)
	((and (= s- t-) (= s+ t+)) :=)
	((= s+ t-) :m)
	((= s- t+) :mi)
	((= s- t-)
	 (cond ((< s+ t+) :s)
	       ((= s+ t+) :=)
	       (t :si)))		;(> s+ t+)
	((= s+ t+)
	 (cond ((> s- t-) :f)
	       (t :fi)))		;(< s- t-) = done above
	((< s- t-)
	 (cond ((< s+ t+) :o)
	       (t :di)))		;(> s+ t+)
	((> s- t-)
	 (cond ((< s+ t+) :d)
	       (t :oi)))		;(> s+ t+)
	))

(defun allenr-in (s- s+ t- t+)
  (declare (fixnum s- s+ t- t+)
	   (optimize (speed 3) (space 0) (safety 0)))
  (and (>= s- t-) (<= s+ t+)))

(defun allenr-ar (s- s+ t- t+)
  (declare (fixnum s- s+ t- t+)
	   (optimize (speed 3) (space 0) (safety 0)))
  (and (<= s- t-) (>= s+ t+)))

(defun allenr-<m (s- s+ t- t+)
  (declare (fixnum s- s+ t- t+)
	   (ignore s- t+)
	   (optimize (speed 3) (space 0) (safety 0)))
  (<= s+ t-))

(defun allenr->mi (s- s+ t- t+)
  (declare (fixnum s- s+ t- t+)
	   (ignore s+ t-)
	   (optimize (speed 3) (space 0) (safety 0)))
  (>= s- t+))

;;;(defun allenr-uo (s- s+ t- t+)
;;;  (member (allenr s- s+ t- t+) '(o oi)))

;;;(defun allenr-rel (s- s+ t- t+)
;;;  (member (allenr s- s+ t- t+) '(o oi = d di f fi s si)))
;; The following tree-search algorithms retrieve those intervals in an
;; interval-tree that satisfy the given Allen relationship to the
;; given int.  The value of storing intervals in an interval-tree
;; rather than, say, a list, is that retrieval should be much faster
;; because we can exclude entire branches of the tree to search. When
;; we are at a node of an interval-tree, we check to see whether the
;; item stored at that node has the relationship we seek to the given
;; int, and then determine for each of the left and right subtrees
;; whether they might also contain matching items. These tests will
;; differ for each relationship. We define a template for each search
;; using the following macro, where the test, left-exclude and
;; right-exclude clauses must be s-expressions that can assume the
;; following bindings: 
;; i-, i+: the start and end of the item at the current node
;; b-, b+: the start and end of the base item to which others are
;; compared
;; l-, l+, r-, r+: the minend and maxend of the left and right
;; branches of the tree from this node, respectively. The left-exclude
;; and right-exclude clauses are evaluated iff the corresponding
;; branches are not nilnode.
;; For cognitive convenience, left-exclude and right-exclude give the
;; conditions under which the left and right subtrees are NOT to be
;; searched.
;; We also define an optional parameter to tree-search-x that permits
;; a filter function to be passed in.  Items that match the above
;; conditions in the interval-tree are only returned as part of the
;; result if the filter function also succeeds.  If it is not given,
;; all succeed.


(defvar *show-tree-search* nil)

(defmacro def-tree-search (allen-relation test
			   left-exclude right-exclude) 
  (let ((name (intern (concatenate
			  'string
			"tree-search-"
			(string allen-relation)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,name (tree base &optional (filter nil))
	 (let ((b- (start base))
	       (b+ (end base))
	       (ans nil))
	   (declare (fixnum b- b+)
		    (ignorable b- b+)
		    (optimize (speed 3) (space 0) (safety 0)))
	   (labels ((try (node)
		      (let* ((i (it-item node))
			     (l (it-left node))
			     (r (it-right node))
			     (i- (start i))
			     (i+ (end i)))
			(declare (fixnum i- i+)
				 (ignorable i- i+))
			(when (nilnode? r) (setq r nil))
			(when (nilnode? l) (setq l nil))
			(let ((r- (if r (minend r) 0))
			      (r+ (if r (maxend r) 0))
			      (l- (if l (minend l) 0))
			      (l+ (if l (maxend l) 0)))
			  (declare (fixnum i- i+ l- l+ r- r+)
				   (ignorable i- i+ l- l+ r- r+))
			  (when *show-tree-search*
			    (format t "~%i=[~d,~d], l-end=[~a,~a], r-end=[~a,~a]"
				    i- i+ l- l+ r- r+))
			  (when (and r (not ,right-exclude))
			    (when *show-tree-search* (format t "~%r to ~a" r))
			    (try r))
			  (when ,test
			    (if (or (null filter)
				    (funcall filter i))
				(push i ans)))
			  (when (and l (not ,left-exclude))
			    (when *show-tree-search* (format t "~%l to ~a" l))
			    (try l))))))
	     (try (root tree)))
	   ans))
       (export ',name)
       )))

;; Test uses only i-, i+, b- and b+ (or some subset of
;; them), because the relation depends only on these.
;; Left-test and right-test normally use b- and b+ and some subset of
;; i-, l-, l+, r- and r+, to identify branches of the interval-tree
;; that cannot hold relevant items.  i+ is typically useless for this
;; purpose.
;; Recall that moving left makes subsequent i-'s smaller, and moving
;; right makes them larger. Because the i's ends are not ordered, only
;; the minend, maxend augmentations on subtrees are useful to exclude
;; the need to search any subtree.  The ways these are used will vary
;; depending on which relation we are searching for.

(def-tree-search =
    ;; i and b are co-terminous
    (and (= i- b-) (= i+ b+))
  ;;i's start before b or longest ends before end of b
  (or (< i- b-) (< l+ b+)) 
  ;;i's start after b or shortest ends after b
  (or (> i- b-) (> r- b+)))

(def-tree-search <
    ;; i is completely before b
    (< i+ b-)
  ;;i's all end at or after b-
  (>= l- b-)
  ;;i's start or all end after b-
  (or (>= i- b-) (>= r- b-)))

(def-tree-search >
    ;; i is completely after b
    (> i- b+)
  ;;i's start before b+
  (<= i- b+)
  ;;no exclusions
  nil)

(def-tree-search m
    ;; end of i is start of b; at least one must be non-degenerate
    (and (= i+ b-)
	 ;; exclude case where both are degenerate, in which case =
	 (or (< i- i+) (< b- b+)))
  ;;i's all end before b- or all end after b-
  (or (< l+ b-) (> l- b-))
  ;;i's all end before b- or all end after b-
  (or (< r+ b-) (> r- b-)))

(def-tree-search mi
    ;; start of i is end of b; at least one non-degenerate
    (and (= b+ i-)
	 ;; exclude case where both are degenerate, in which case =
	 (or (< i- i+) (< b- b+)))
  ;;i's all start before b+
  (< i- b+)
  ;;i's all start after b+
  (> i- b+))

(def-tree-search o
    ;; i starts before b and ends within it
    (and (< i- b-) (> i+ b-) (< i+ b+))
  ;;all i's end before b-
  (<= l+ b-)
  ;;all i's start after b-, all i's end before b, or shortest i ends
  ;;after b
  (or (>= i- b-) (<= r+ b-) (>= r- b+)))

(def-tree-search oi
    ;; i starts within b and ends after it
    (and (> i- b-) (> i+ b+) (< i- b+))
  ;; i starts before b or longest i ends before b+
  (or (<= i- b-) (<= l+ b+))
  ;; i starts after end of b or longest i ends before b+
  (or (>= i- b+) (<= r+ b+)))

(def-tree-search s
    ;; i starts at start of b and ends within it
    (and (= i- b-) (< i+ b+)
	 ;; exclude degenerate i
	 (< i- i+))
  ;; i starts before b or shortest i ends after it
  (or (< i- b-) (>= l- b+))
  ;; i starts after b or shortest i ends after it
  (or (> i- b-) (>= r- b+)))

(def-tree-search si
    ;; b starts at start of i and ends within i
    (and (= i- b-) (> i+ b+)
	 ;; exclude degenerate b
	 (< b- b+))
  ;; i starts before b or longest i ends before b
  (or (< i- b-) (<= l+ b+))
  ;; i starts after b or longest i ends before b
  (or (> i- b-) (<= r+ b+)))

(def-tree-search f
    ;; i starts within b and finishes at end of b
    (and (= i+ b+) (> i- b-)
	 ;; exclude degenerate i
	 (< i- i+))
  ;; i starts at or before b, or shortest i beyond b+, or
  ;; longest i ends before end of b
  (or (<= i- b-) (> l- b+) (< l+ b+))
  ;; i starts at or after b+, or shortest i beyond b+
  (or (>= i- b+) (> r- b+)))

(def-tree-search fi
    ;; b starts within i and finishes at end of i
    (and (= i+ b+) (> b- i-)
	 ;; exclude degenerate b
	 (< b- b+))
  ;; longest i ends before b or shortest i after b
  (or (< l+ b+) (> l- b+))
  ;; i starts after b- or ends can't = b+
  (or (>= i- b-) (< r+ b+) (> r- b+)))

(def-tree-search d
    ;; i completely within b
    (and (> i- b-) (< i+ b+))
  ;; i starts before start of b or shortest ends at or beyond b
  (or (<= i- b-) (>= l- b+))
  ;; i starts at or beyond end of b, or shortest ends at or beyond b
  (or (>= i- b+) (>= r- b+)))

(def-tree-search di
    ;; b completely within i
    (and (< i- b-) (> i+ b+))
  ;; longest i to left ends before or at b's end
  (<= l+ b+)
  ;; i at or beyond b- or longest i ends before or at b+
  (or (>= i- b-) (<= r+ b+)))

(def-tree-search in
    ;; like a union of =, d, s, and f, and m, mi for degenerate i
    (and (>= i- b-) (<= i+ b+))
  (or (< i- b-) (> l- b+))
  (or (> i- b+) (> r- b+)))

(def-tree-search ar
    ;; i spans over b
    ;; like a union of =, di, si, fi
    (and (<= i- b-) (>= i+ b+))
  (< l+ b+)
  nil; (> i- b-)
)

(def-tree-search <m
    ;; union of < and m
    (<= i+ b-)
  (> l- b-)
  (or (> i- b-) (> r- b-)))

(def-tree-search >mi
    ;; union of > and mi
    (>= i- b+)
  (< i- b+)
  nil)

(defparameter *debug-tree-search* nil)

(defmethod tree-search ((tree interval-tree) (base interval) relation
			&optional (filter nil))
  "Dispatches to the appropriate tree-search-x procedure based on the
   relation, which must be in the keyword package."
  (ccase relation
    (:= (tree-search-= tree base filter))
    (:< (tree-search-< tree base filter))
    (:> (tree-search-> tree base filter))
    (:m (tree-search-m tree base filter))
    (:mi (tree-search-mi tree base filter))
    (:o (tree-search-o tree base filter))
    (:oi (tree-search-oi tree base filter))
    (:s (tree-search-s tree base filter))
    (:si (tree-search-si tree base filter))
    (:f (tree-search-f tree base filter))
    (:fi (tree-search-fi tree base filter))
    (:d (tree-search-d tree base filter))
    (:di (tree-search-di tree base filter))
    (:in (tree-search-in tree base filter))
    (:ar (tree-search-ar tree base filter))
    (:<m (tree-search-<m tree base filter))
    (:>mi (tree-search->mi tree base filter))))

;;; We also define two functions to move forward or backward to the
;;; next interval that >mi or <m a given interval. This, in contrast
;;; to the above, does not find them all, but finds one and also
;;; returns (as a 2nd value) the corresponding node, from which the
;;; subsequent one may then be found.  This facility does not make
;;; sense for relations other than <, <m, >, or >mi.  Note that
;;; because these functions use tree-successor and tree-predecessor,
;;; they only use the red-black tree part of the interval-tree
;;; implementation. Hence, they must also check for the appropriate
;;; Allen relation, because in general the tree-successor of an
;;; interval will not be one that follows it.

(defmethod next->mi ((tree interval-tree) (base it-node)
		     &key (type 't) (filter nil))
  (let* ((b (it-item base))
	 (b- (start b))
	 (b+ (end b)))
    (do ((here (tree-successor base) (tree-successor here)))
	((or (null here) (nilnode? here)) nil)
      (let ((i (it-item here)))
	(when (and (allenr->mi (start i) (end i) b- b+)
		   (typep i type)
		   (or (null filter)
		       (funcall filter i)))
	  (return (values i here)))))))

(defmethod next->mi ((tree interval-tree) (base interval)
		     &key (type 't) (filter nil))
  (next->mi tree (tree-search-node tree base)
	    :type type :filter filter))

(defmethod prev-<m ((tree interval-tree) (base it-node)
		    &key (type 't) (filter nil))
  (let* ((b (it-item base))
	 (b- (start b))
	 (b+ (end b)))
    (do ((here (tree-predecessor base) (tree-predecessor here)))
	((or (null here) (nilnode? here)) nil)
      (let ((i (it-item here)))
	(when (and (allenr-<m (start i) (end i) b- b+)
		   (typep i type)
		   (or (null filter)
		       (funcall filter i)))
	  (return (values i here)))))))

(defmethod prev-<m ((tree interval-tree) (base interval)
		     &key (type 't) (filter nil))
  (prev-<m tree (tree-search-node tree base)
	   :type type :filter filter))

(defmethod tree-depth ((tree interval-tree))
  (b-depth (root tree)))

(defmethod b-depth ((node it-node))
  (if (nilnode? node)
      (values 0 0)
    (multiple-value-bind (ld lbd) (b-depth (it-left node))
      (multiple-value-bind (rd rbd) (b-depth (it-right node))
	(values (+ (max ld rd) 1)
		(+ (max lbd rbd) (if (it-red? node) 0 1)))))))

(defmethod tree-size ((tree interval-tree))
  (labels ((inner (node)
	     (if (nilnode? node)
		 0
	       (+ 1 (inner (it-left node)) (inner (it-right node))))))
    (inner (root tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insertion operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod fix-min-max-ends ((node it-node))
  "Recomputes the min and max endpoints of an it-node, which needs to
  be done after an insertion or rotation under the node."
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let* ((it (it-item node))
	 (l (it-left node))
	 (r (it-right node))
	 (mn (end it))
	 (mx mn))
    (unless (nilnode? l)
      (setq mn (min mn (minend l))
	    mx (max mx (maxend l))))
    (unless (nilnode? r)
      (setq mn (min mn (minend r))
	    mx (max mx (maxend r))))
    (setf (minend node) mn)
    (setf (maxend node) mx)
    t))

(defun fix-parent-chain (node)
  (do ((p node (it-parent p)))
      ((nilnode? p))
    (fix-min-max-ends p))
  node)

(defun int-tree-insert-base (tree item)
  "Inserts a new item into an interval-tree, after finding the
  appropriate leaf node for it to extend.  Also updates the minend and
  maxend properties of the parent chain, but does not balance the
  tree." 
  (let* ((e (end item))
	 (parent (tree-search-for-insert tree item))
	 (nn (nilnode tree))
	 (new (make-instance 'it-node
		:item item
		:left nn
		:right nn
		:parent (or parent nn)
		:red? nil
		:minend e
		:maxend e)))
    (cond ((null parent) (setf (root tree) new))
	  ((int< item (it-item parent))
	   (setf (it-left parent) new))
	  (t (setf (it-right parent) new)))
    ;; Fix up min/maxends of all parents above newly inserted node.
    (fix-parent-chain (it-parent new))
    new))

(defun tree-search-for-insert (tree item)
  "Returns the parent node in tree where the new item should be
  inserted."
  (labels ((iter (node par)
	     (if (nilnode? node)
		 par
	       (if (int< item (it-item node))
		   (iter (it-left node) node)
		 (iter (it-right node) node)))))
    (iter (root tree) nil)))

#-allegro
(defmacro while (condition &rest steps)
  `(loop (unless ,condition (return)) ,@steps))

(defmethod tree-insert ((tree interval-tree) (int interval))
  "Inserts KEY/VAL into TREE, and fixes up the red-black properties
  and minend, maxend affected."
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((x (int-tree-insert-base tree int)))
    (make-red x)			; color[x]=red
    (while (and (not (eq x (root tree)))
		(it-red? (it-parent x))) ; color[p[x]]=red
      ;; The rest of the code is symmetric based on whether x is its
      ;; parent's right or left child.
      (let* ((px (it-parent x))		; temp for parent of x
	     (ppx (it-parent px))	; temp for grandparent of x
	     (y))			; uncle of x
	;; x's parent is left child of grandparent
	(cond ((eq px (it-left ppx))
	       (setq y (it-right ppx)) ; thus uncle is right child
					; of grandparent
	       (cond ((it-red? y)	; Case 1: parent of x and
					; uncle y are both red.
		      (make-black px)	; Color them both black, and
					; grandparent red.
		      (make-black y)
		      (make-red ppx)
		      (setq x ppx)) ; continue looking for red/red violations
		     (t	  ; Case 2, falls into 3. x's uncle is black. 
		      (when (eq x (it-right px)) ; if x is the
					; right child of its parent,
					; we rotate left
			(setq x px)
			(rb-left-rotate tree x)
			(setq px (it-parent x)) ; fix px, ppx after rotation
			(setq ppx (it-parent px)))
		      (make-black px)	; Case 3
		      (make-red ppx)
		      (rb-right-rotate tree ppx))))
	      ;; This case is symmetric with the above, but when x's
	      ;; parent is the right child of grandparent 
	      (t 
	       (setq y (it-left ppx))
	       (cond ((it-red? y)
		      (make-black px)	; Case 1
		      (make-black y)
		      (make-red ppx)
		      (setq x ppx))
		     (t (when (eq x (it-left px)) ; Case 2, falls into 3
			  (setq x px)
			  (rb-right-rotate tree x)
			  (setq px (it-parent x)) ; fix px, ppx after rotation
			  (setq ppx (it-parent px)))
			(make-black px)	; Case 3
			(make-red ppx)
			(rb-left-rotate tree ppx)))))))
    (make-black (root tree))
    #+:debug
    (assert (check-rb tree) () "Failed red-black tree check.")
    t))

(defmethod rb-left-rotate ((tree interval-tree) (x it-node))
  "Given a node X with a non-null right branch, this operation lifts
  that branch to be the top of the local sub-tree and makes X its left
  branch.  In pictures,
         x                             y
      a     y        becomes        x     c
          b   c                   a   b
   "
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((y (it-right x)))
    (setf (it-right x) (it-left y))
    (unless (nilnode? (it-left y))
      (setf (it-parent (it-left y)) x))
    (setf (it-parent y) (it-parent x))
    (cond ((nilnode? (it-parent x))
	   (setf (root tree) y))
	  ((eq x (it-left (it-parent x)))
	   (setf (it-left (it-parent x)) y))
	  (t (setf (it-right (it-parent x)) y)))
    (setf (it-left y) x)
    (setf (it-parent x) y)
    ;; fix up the ends; order is critical; a, b, c can't change.
    (fix-min-max-ends x)
    (fix-min-max-ends y)
    t))

(defmethod rb-right-rotate ((tree interval-tree) (x it-node))
  "Given a node X with a non-null left branch, this operation lifts
  that branch to be the top of the local sub-tree and makes X its
  right branch.  In pictures,
         x                             y
      y     c        becomes        a     x
    a   b                               b   c
   "
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((y (it-left x)))
    (setf (it-left x) (it-right y))
    (unless (nilnode? (it-right y))
      (setf (it-parent (it-right y)) x))
    (setf (it-parent y) (it-parent x))
    (cond ((nilnode? (it-parent x))
	   (setf (root tree) y))
	  ((eq x (it-right (it-parent x)))
	   (setf (it-right (it-parent x)) y))
	  (t (setf (it-left (it-parent x)) y)))
    (setf (it-right y) x)
    (setf (it-parent x) y)
    ;; fix up the ends; order is critical; a, b, c can't change.
    (fix-min-max-ends x)
    (fix-min-max-ends y)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Deletion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tree-delete ((tree interval-tree) (item interval)
			&optional (predicate nil))
  "Deletes the (first found) node eq to item in tree and fixes up
  the red-black properties.  Returns the deleted item, or nil if it
  was not in the tree. Predicate, if given, causes deletion of the
  first item in the tree that is int= item and passes the predicate."
  ;; we must fix the minend and maxend properties of all the parent
  ;; chain when we delete a node, because it may change the min and
  ;; max ends on the parents.
  (declare (optimize (speed 3) (safety 0)))
  (let ((z (tree-search-node tree item predicate)))
    (if (null z)
	nil
      ;; We splice out a node (y) from the tree. Either
      ;; a. the node holding item, if at least one of its children is
      ;; nil; then we just link that in place of z.
      ;; b. the successor of z, which will have a nil left child or
      ;; else that child would be the successor of z.
      (let* ((y (if (or (nilnode? (it-left z))
                        (nilnode? (it-right z)))
		    z
                  (tree-successor z)))
	     ;; x is the non-nil child of y or nilnode; it will be
	     ;; spliced into the tree in place of y
	     (x (if (nilnode? (it-left y))
		    (it-right y)
		  (it-left y))))
        (setf (it-parent x) (it-parent y)) ; remember, x may be sentinel!
        (if (nilnode? (it-parent y)) ; link x into its parent (or root)
	    (setf (root tree) x)
          (if (eq y (it-left (it-parent y)))
	      (setf (it-left (it-parent y)) x)
            (setf (it-right (it-parent y)) x)))
        (unless (eq y z)
	  ; copy info to replaced node if y was the successor of z
	  (setf (it-item z) (it-item y))
	  (fix-min-max-ends z))
	;; Now fix minend, maxend of changed nodes and their parents.
	(fix-parent-chain (it-parent x))
	;; If y is black, then we must fix up the tree after the
	;; deletion done above.
        (when (not (it-red? y))
	  (tree-delete-fixup tree x))
	#+:debug
	(assert (check-rb tree) () "Failed red-black tree check.")
	(and z item)))))

(defmethod tree-delete-fixup ((tree interval-tree) (x it-node))
  (while (and (not (eq x (root tree)))
	      (not (it-red? x)))
    (cond ((eq x (it-left (it-parent x)))
	   (let ((w (it-right (it-parent x))))
	     (when (it-red? w)
	       (make-black w)		; color[w]=black ; Case 1
	       (make-red (it-parent x)) ; color[p[x]]=red
	       (rb-left-rotate tree (it-parent x))
	       (setq w (it-right (it-parent x))))
	     (cond ((and (not (it-red? (it-left w)))
			 (not (it-red? (it-right w))))
		    (make-red w)	;Case 2
		    (setq x (it-parent x)))
		   (t (when (not (it-red? (it-right w)))
			(make-black (it-left w)) ; Case 3
			(make-red w)
			(rb-right-rotate tree w)
			(setq w (it-right (it-parent x))))
		      (setf (it-red? w) (it-red? (it-parent x)))
		      (make-black (it-parent x))
		      (make-black (it-right w))
		      (rb-left-rotate tree (it-parent x))
		      (setq x (root tree))))))
	  ((eq x (it-right (it-parent x)))
	   (let ((w (it-left (it-parent x))))
	     (when (it-red? w)
	       (make-black w)		; color[w]=black
	       (make-red (it-parent x))		; color[p[x]]=red
	       (rb-right-rotate tree (it-parent x))
	       (setq w (it-left (it-parent x))))
	     (cond ((and (not (it-red? (it-right w)))
			 (not (it-red? (it-left w))))
		    (make-red w)
		    (setq x (it-parent x)))
		   (t (when (not (it-red? (it-left w)))
			(make-black (it-right w))
			(make-red w)
			(rb-left-rotate tree w)
			(setq w (it-left (it-parent x))))
		      (setf (it-red? w) (it-red? (it-parent x)))
		      (make-black (it-parent x))
		      (make-black (it-left w))
		      (rb-right-rotate tree (it-parent x))
		      (setq x (root tree))))))))
  (make-black x))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Debugging code
;;; Exhaustively tests internal consistency of the tree-search code
;;; against the definitions of the Allen relations. Use as:
;;; 1. (test-tree)
;;; 2. (test-random)
;;; If any assertions fail, something is wrong.  Repeat tests might
;;; raise confidence.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *t* t "Test tree")

(defvar *n* 0 "Max range of values in *t*")

(defun mint (m &optional (z nil))
  "Make an interval of its two arguments, or a random interval in
  [0,m] if given only one argument."
  (let ((x (if z m (random m)))
	(y (or z (random m))))
    (make-instance 'interval :start (min x y) :end (max x y))))

(defun test-tree (&optional (m 100))
  "Creates a test tree of m random intervals, each in [0,m]."
  (setq *t* (make-instance 'interval-tree)
	*n* m)
  (dotimes (i m)
    (tree-insert *t* (mint m)))
  *t*)

(defun find-rels (tree int)
  "Returns a property list of all the basic and augmented Allen
  relationships found among elements of tree for int."
  (labels ((iter (rest ans)
	     (if (null rest) ans
	       (iter (cdr rest)
		     (list* (car rest)
			    (tree-search tree int (car rest))
			    ans)))))
    (iter (append *allen-augmented* *allen-relations*) nil)))

(defun test-random (&optional (m 100))
  "Tests m randomly generated intervals against those stored in *t* to
  make sure that the intervals retrieved by tree traversal for the
  various allen relations actually have that relation to the random
  interval. This is empirical testing at its finest.  Ugh."
  (dotimes (k m)
    (let* ((int (mint *n*))
	   (all (find-rels *t* int)))
      (do-tree (node *t*)
	;;for each i in tree, check that it has the found relations to
	;;int and does not have the unfound relations; i.e., exhaustive
	(let* ((i (it-item node))
	       (r (allen i int))
	       (x
		(list
		 'in (allenr-in (start i) (end i)
			       (start int) (end int))
		 'ar (allenr-ar (start i) (end i)
			       (start int) (end int))
		 '<m (allenr-<m (start i) (end i)
			       (start int) (end int))
		 '>mi (allenr->mi (start i) (end i)
				 (start int) (end int)))))
	  (do ((l all (cddr l)))
	      ((null l))
	    (cond ((member (car l) *allen-relations*)
		   (cond ((member i (cadr l)) ;found among r=(car l)
			  (assert (eq r (car l)) ()
			    "~a was found to be ~a ~a, but allen = ~a"
			    i (car l) int r))
			 (t (assert (not (eq r (car l))) ()
			      "~a was NOT found to be ~a ~a, but allen = ~a"
			      i (car l) int r))))
		  (t ;;extended relations
		   (cond ((member i (cadr l))
			  ;; i in extended relation with int
			  (assert (getf x (car l)) ()
			    "~a was in extended rel'n ~a to ~a, but test was false"
			    i (car l) int))
			 (t (assert (not (getf x (car l))) ()
			      "~a was NOT found to be in extended rel'n ~a to ~a, but test was true"
			      i (car l) int)))))))))))
|#

#|
;;; I don't think this is needed, because we are currently in the user
package. If we put all utilities into a util package, then yes.
(export '(interval
	  initialize-instance
	  int<
	  int=
	  interval-tree
	  it-node
	  make-red
	  make-black
	  +nilnode-key+
	  make-it-nilnode
	  nilnode?
	  initialize-instance
	  print-object
	  ))
|#
