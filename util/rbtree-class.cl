;;; -*- Mode: Lisp; Package: util; -*-

#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation

This is an implementation of red-black trees (balanced binary trees)
after the presentation in Cormen, Leiserson and Rivest's "Introduction
to Algorithms." It is functionally equivalent to rbtree.cl, but uses
CLOS classes rather than structures, which should make it easier to
extend.

The properties of a red-black tree (from CLR) are:
1. Every node is red or black
2. Every leaf is black
3. If a node is red, then both its children are black
4. Every simple path from a node to a descendant leaf contains the
same number of black nodes.
These properties assure that all basic operations can be done in
2 log(n+1) time.

The current implementation uses classes to represent a tree and its
nodes.  The constructors for these should not be called directly.
Instead, the following procedures are supported:

new-tree (&key (eq-test #'=) (less-test #'<))
Creates a new, empty tree.  The optional testing predicates apply to the keys of nodes
that will be inserted into the tree.  Any key value inserted must be a valid argument
to the equality and less-than predicates.

tree-insert (tree key &optional val)
Creates a new node with key KEY and val VAL and inserts it into TREE.  Val is optional,
for cases in which the key itself is the only value of interest.

tree-search (tree key)
Returns two values, the key and val associated with KEY.  If the key returned is NIL,
then it is not found in the tree.  If KEY occurs several times, this returns the first.

tree-range-query (tree fn key-low key-high)
Applies the function FN to each pair of KEY and VAL whose KEY is in the closed interval
[key-low, key-high].

tree-list (tree &optional min max)
Returns a list of the elements of tree, each as a (key . value) pair,
in increasing order. Min and max are bounding key values, defaulting
to the tree-minimum and tree-maximum.

tree-list-keys (tree &optional min max)
Like tree-list, but yields only the keys.

tree-delete (tree key)
Deletes the node whose key is KEY from TREE.  Returns KEY and its val if the node
was found and deleted, NIL if the node was not found.

tree-minimum (tree)
Returns the minimum KEY and its VAL from the tree, or NIL if the tree is empty.

tree-maximum (tree)
Returns the maximum KEY and its VAL from the tree, or NIL if the tree is empty.

tree-show (tree &optional depth)
Prints an indentated tree showing structure from left to right as top to bottom.
Keys of the nodes are printed, followed optionally by a non-NIL val, and a * if
the node is colored black. Depth, if given, limits the print-out to
that many levels of the tree.

tree-size (tree)
Returns the number of elements stored in TREE.

tree-depth (tree)
Returns two values: the maximum depth of the tree, and the number of black nodes along
each tree path.

do-tree ((var tree) steps...)
Iterates over the keys of tree, performing steps for each.
If var is of the form (k v), then k is bound to successive keys and v
to corresponding values. do-tree-rev is similar, but in the opposite
order.

For other methods of searching the binary tree, the procedures used internally are, of
course, available.  However, it is inadvisable to expose directly the node structure
of trees.

The potentially useful procedures that manipulate node structures include:

tree-search-node (tree key)
Returns the (first) node whose key is KEY, or NIL if not found.

tree-successor (node)
Returns the next (on ordering by key) node in the tree, or NIL if no more.

tree-predecessor (node)
Returns the previous (on ordering by key) node in the tree, or NIL if no more.

tree-search-> (tree key)
Returns the first node > key, or NIL if none.

tree-search->= (tree key)
Returns the first node >= key, or NIL if none.

The code also contains some assertions to check for internal
consistency, which are/were useful in debugging. They will be seen
only if the feature :debug is present in *features*. This must be true
at the time the file is compiled!

=====================================================================

Copyright 1997 and 2009, by MIT and Peter Szolovits (psz@mit.edu). 
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

To see a copy of the GNU Lesser General Public License, see
<http://www.gnu.org/licenses/>.

|#

(defpackage :util
  (:use :common-lisp)
  (:export "rb-tree" "new-tree"
	   "inorder-tree-walk" "inorder-tree-walk-rev" "do-tree" "do-tree-rev"
	   "tree-search" "tree-insert" "tree-delete"
	   "tree-minimum" "tree-maximum" "tree-list" "tree-list-keys" "tree-show"
	   "tree-range-query" "tree-depth" "b-depth" "tree-size"))

(in-package :util)

;;(declaim (optimize (speed 3) (safety 0)))


(defclass rb-node ()
  ((key :accessor rb-node-key :initarg :key)
   (val :accessor rb-node-val :initarg :val)
   (left :accessor rb-node-left :initarg :left)
   (right :accessor rb-node-right :initarg :right)
   (red? :accessor rb-node-red? :initarg :red? :initform nil)
   (parent :accessor rb-node-parent :initarg :parent)
   ))

(defclass rb-tree ()
  ((root :accessor rb-tree-root :initarg :root)
   (nilnode :accessor rb-tree-nilnode :initarg :nilnode)
   (eq-test :accessor rb-tree-eq-test :initarg :eq-test :initform #'=)
   (less-test :accessor rb-tree-less-test :initarg :less-test
	      :initform #'<)
   (node-type :reader node-type
	      :initarg :node-type
	      :initform 'rb-node
	      :allocation :class)))

;; This is just to make it easier to dispatch on the special case
;; where eq-test is = and less-test is <.
(defclass rb-tree-basic (rb-tree)
  ()
  (:default-initargs :eq-test #'= :less-test #'<)
  (:documentation "A subtype of rb-tree in which the keys are
numbers. This is specialized simply for efficiency.")
  )

(declaim (inline make-red make-black nilnode?))

(defmethod make-red ((node rb-node))
  "Make the color of rb-node RED. Be sure it's not the sentinel!"
  #+:debug
  (assert (not (nilnode? node))
      ()
    "Trying to make ~s red!" node)
  (setf (rb-node-red? node) t))

(defun make-black (rb-node)
  "Make the color of rb-node BLACK. No effect on the sentinel."
  #+:debug
  (assert (and (typep rb-node 'rb-node) (not (nilnode? rb-node)))
      ()
    "Trying to make ~s black!" rb-node)
  (setf (rb-node-red? rb-node) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sentinels
;;;
;;; Instead of testing for boundary conditions (i.e., the parent, left
;;; or right branch of a node being NIL, we create a recognizable
;;; pseudo-node, the nilnode, to represent each of these null links.
;;; This simplifies various algorithms because we don't have to test
;;; for the null cases. Note that the color of the nilnode should
;;; always be black, so turning it red is an error. 
;;;
;;; In particular, the code for tree-delete relies on being able to
;;; store vital information in a sentinel during its processing.
;;; Other algorithms sometimes store into the sentinel, but that
;;; should not matter. Because there is only one sentinel per tree, it
;;; is not in general possible to overlap multiple threads running
;;; code on ane tree at the same time. 
;;;
;;; We can test for a node being a nilnode? without reference to the
;;; tree by choosing such nodes to have a unique key that cannot be
;;; stored in a tree (except as the nilnode).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar +nilnode-key+ (list 'NILNODE))

(defun nilnode? (node)
  "Tests whether NODE is a sentinel for a null."
  #+:debug
  (assert (typep node 'rb-node) ()
    "Cannot test if non-node ~s is nilnode?" node)
  (eq +nilnode-key+ (rb-node-key node)))

(defmacro make-nilnode (&optional (type ''rb-node))
  "Creates a sentinel for a null.  Rb-nodes with null keys are not otherwise valid."
  `(make-instance ,type
    :key +nilnode-key+ :val nil :left nil :right nil :red? nil :parent nil))

(defmethod print-object ((node rb-node) stream)
  "Prints a compact representation of a node of a red-black tree, as
  #<NODE*/\ key=val>
  The * appears if the node is black, / and \ if it has left and right
  branches, and =val only if it has a value."
  (if (nilnode? node)
      (format stream "#<NILNODE~:[*~;~] p=~s>"
	      (rb-node-red? node)
	      (rb-node-parent node))
    (format stream
	    "#<NODE~:[*~;~]~:[/~;~]~:[\\~;~]~:[~;=~] ~s~@[:~s~]>"
	    (rb-node-red? node)
	    (nilnode? (rb-node-left node))
	    (nilnode? (rb-node-right node))
	    (nilnode? (rb-node-parent node))
	    (rb-node-key node)
	    (rb-node-val node))))

(defmethod print-object ((tree rb-tree) stream)
  "Prints a summary of the tree, including the number of elements it
  contains, and its total and black depths."
  (multiple-value-bind (dep bdep)
      (tree-depth tree)
    (format stream "#<~a: ~a items, depth=~d (~d black)>"
	    (type-of tree) (tree-size tree) dep bdep)))

(defun new-tree (&key (eq-test #'=) (less-test #'<) (type nil))
  (let* ((tree-type (or type
		       (if (and (eq eq-test #'=) (eq less-test #'<))
			   'rb-tree
			 'rb-tree-basic)))
	 (tree (make-instance tree-type
		 :eq-test eq-test :less-test less-test))
	 (nn (make-nilnode (node-type tree))))
    (setf (rb-tree-root tree) nn)
    (setf (rb-tree-nilnode tree) nn)
    tree))

(defmethod inorder-tree-walk (f (tree rb-tree))
  (labels ((walk (node)
             (unless (nilnode? node)
               (walk (rb-node-left node))
               (funcall f node)
               (walk (rb-node-right node)))))
    (walk (rb-tree-root tree))))

(defmethod inorder-tree-walk-rev (f (tree rb-tree))
  (labels ((walk (node)
             (unless (nilnode? node)
               (walk (rb-node-right node))
               (funcall f node)
               (walk (rb-node-left node)))))
    (walk (rb-tree-root tree))))

(defmacro do-tree ((nv tree) &rest steps)
  "Iterates over elements of TREE in order, performing STEPS for each, with NV bound
   to the keys.  If NV is of the form (KV V) then the two variables are bound to
   the key and val, respectively."
  (let ((nodev (gensym))
        (val-v-present? (and (consp nv) (cdr nv) (consp (cdr nv)) (null (cddr nv))
                             (symbolp (car nv)) (symbolp (cadr nv)))))
    `(inorder-tree-walk 
      #'(lambda (,nodev)
          (let ((,(if val-v-present? (car nv) nv) (rb-node-key ,nodev))
                ,@(and val-v-present? `((,(cadr nv) (rb-node-val ,nodev)))))
            ,@steps))
      ,tree)))

(defmacro do-tree-rev ((nv tree) &rest steps)
  "Iterates over elements of TREE in order, performing STEPS for each, with NV bound
   to the keys.  If NV is of the form (KV V) then the two variables are bound to
   the key and val, respectively."
  (let ((nodev (gensym))
        (val-v-present? (and (consp nv) (cdr nv) (consp (cdr nv)) (null (cddr nv))
                             (symbolp (car nv)) (symbolp (cadr nv)))))
    `(inorder-tree-walk-rev
      #'(lambda (,nodev)
          (let ((,(if val-v-present? (car nv) nv) (rb-node-key ,nodev))
                ,@(and val-v-present? `((,(cadr nv) (rb-node-val ,nodev)))))
            ,@steps))
      ,tree)))

(defmethod tree-search ((tree rb-tree) k)
  (let ((found-node (tree-search-node tree k)))
    (and found-node
	 (values (rb-node-key found-node) (rb-node-val found-node)))))

(defmethod tree-search-node ((tree rb-tree-basic) k)
  "Searches for key K in binary tree TREE.  Returns K and VAL
  associated with it, if found.  If not found, returns NIL. This
  version is specialized to numeric keys."
  (declare (optimize (speed 3) (safety 0)))
  (labels
    ((iter (node)
       (if (nilnode? node)
         nil
         (let ((kv (rb-node-key node)))
           (if (= k kv)
             node
             (if (< k kv)
               (iter (rb-node-left node))
               (iter (rb-node-right node))))))))
    (iter (rb-tree-root tree))))

(defmethod tree-search-node ((tree rb-tree) k)
  "Searches for key K in binary tree TREE.  Returns K and VAL
  associated with it, if found.  If not found, returns NIL."
  (declare (optimize (speed 3) (safety 0)))
  (let ((=fn (rb-tree-eq-test tree))
        (<fn (rb-tree-less-test tree)))
    (labels
      ((iter (node)
         (if (nilnode? node)
           nil
           (let ((kv (rb-node-key node)))
             (if (funcall =fn k kv)
               node
               (if (funcall <fn k kv)
                 (iter (rb-node-left node))
                 (iter (rb-node-right node))))))))
      (iter (rb-tree-root tree)))))

(defmethod tree-insert-simple ((tree rb-tree) key &optional val)
  "Inserts a new NODE into TREE, with val VAL.  Duplicate keys are
  allowed. This does not maintain the red-black properties."
  (declare (optimize (speed 3) (safety 0)))
  (let* ((parent
          (tree-search-i tree key))
         (nn (rb-tree-nilnode tree))
         (new (make-instance 'rb-node
		:key key
		:val val 
		:left nn
		:right nn
		:parent nn)))
    (cond ((null parent)                ; tree root
           (setf (rb-tree-root tree) new))
          ((funcall (rb-tree-less-test tree) key (rb-node-key parent))
           (setf (rb-node-left parent) new))
          (t (setf (rb-node-right parent) new)))
    (setf (rb-node-parent new) (or parent nn))
    new))

#|
The following two procedures implement the search for the place at
which to insert a new node in a tree.  They differ only in that the
first has built-in knowledge that the equality test is = and the
less-than test is <.  They return the PARENT where the new node needs
to be inserted.  Note that these are similar to tree-search, above,
but differ in the information they return.  Returns NIL if the tree is
empty.
|#

(defmethod tree-search-i ((tree rb-tree-basic) k)
  (declare (optimize (speed 3) (safety 0)))
  (labels
    ((iter (node par)
       (if (nilnode? node)
         par
         (if (< k (rb-node-key node))
               (iter (rb-node-left node) node)
               (iter (rb-node-right node) node)))))
    (iter (rb-tree-root tree) nil)))

(defmethod tree-search-i ((tree rb-tree) k)
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((<fn (rb-tree-less-test tree)))
    (labels
      ((iter (node par)
         (if (nilnode? node)
           par
           (if (funcall <fn k (rb-node-key node))
             (iter (rb-node-left node) node)
             (iter (rb-node-right node) node)))))
      (iter (rb-tree-root tree) nil))))

#-allegro
(defmacro while (condition &rest steps)
  `(loop (unless ,condition (return)) ,@steps))

(defmethod tree-insert ((tree rb-tree) key &optional val)
  "Inserts KEY/VAL into TREE, and fixes up the red-black properties."
  (declare (optimize (speed 3) (safety 0)))
  (let ((x (tree-insert-simple tree key val)))
    (make-red x)			; color[x]=red
    (while (and (not (eq x (rb-tree-root tree)))
		(rb-node-red? (rb-node-parent x))) ; color[p[x]]=red
      ;; The rest of the code is symmetric based on whether x is its parent's right or
      ;; left child.
      (let* ((px (rb-node-parent x))	; temp for parent of x
	     (ppx (rb-node-parent px))	; temp for grandparent of x
	     (y))			; uncle of x
	(cond ((eq px (rb-node-left ppx)) ; x's parent is left child of grandparent
	       (setq y (rb-node-right ppx)) ; thus uncle is right child
					; of grandparent
	       (cond ((rb-node-red? y)	; Case 1: parent of x and
					; uncle y are both red.
		      (make-black px)	; Color them both black, and
					; grandparent red.
		      (make-black y)
		      (make-red ppx)
		      (setq x ppx))	; continue looking for red/red violations
		     (t			; Case 2, falls into 3. x's uncle is black. 
		      (when (eq x (rb-node-right px)) ; if x is the
					; right child of its parent,
					; we rotate left
			(setq x px)
			(rb-left-rotate tree x)
			(setq px (rb-node-parent x)) ; fix px, ppx after rotation
			(setq ppx (rb-node-parent px)))
		      (make-black px)	; Case 3
		      (make-red ppx)
		      (rb-right-rotate tree ppx))))
	      (t ;; This case is symmetric with the above, but when x's
	       ;; parent is the right child of grandparent 
	       (setq y (rb-node-left ppx))
	       (cond ((rb-node-red? y)
		      (make-black px)	; Case 1
		      (make-black y)
		      (make-red ppx)
		      (setq x ppx))
		     (t (when (eq x (rb-node-left px)) ; Case 2, falls into 3
			  (setq x px)
			  (rb-right-rotate tree x)
			  (setq px (rb-node-parent x)) ; fix px, ppx after rotation
			  (setq ppx (rb-node-parent px)))
			(make-black px)	; Case 3
			(make-red ppx)
			(rb-left-rotate tree ppx)))))))
    (make-black (rb-tree-root tree))
    #+:debug
    (assert (check-rb tree) () "Failed red-black tree check.")
    t))

(defmethod rb-left-rotate ((tree rb-tree) (x rb-node))
  "Given a node X with a non-null right branch, this operation lifts that branch
   to be the top of the local sub-tree and makes X its left branch.  In pictures,
         x                             y
      a     y        becomes        x     c
          b   c                   a   b
   "
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((y (rb-node-right x)))
    (setf (rb-node-right x) (rb-node-left y))
    (unless (nilnode? (rb-node-left y))
      (setf (rb-node-parent (rb-node-left y)) x))
    (setf (rb-node-parent y) (rb-node-parent x))
    (cond ((nilnode? (rb-node-parent x))
	   (setf (rb-tree-root tree) y))
	  ((eq x (rb-node-left (rb-node-parent x)))
	   (setf (rb-node-left (rb-node-parent x)) y))
	  (t (setf (rb-node-right (rb-node-parent x)) y)))
    (setf (rb-node-left y) x)
    (setf (rb-node-parent x) y)
    t))

(defmethod rb-right-rotate ((tree rb-tree) (x rb-node))
  "Given a node X with a non-null left branch, this operation lifts that branch
   to be the top of the local sub-tree and makes X its right branch.  In pictures,
         x                             y
      y     c        becomes        a     x
    a   b                               b   c
   "
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((y (rb-node-left x)))
    (setf (rb-node-left x) (rb-node-right y))
    (unless (nilnode? (rb-node-right y))
      (setf (rb-node-parent (rb-node-right y)) x))
    (setf (rb-node-parent y) (rb-node-parent x))
    (cond ((nilnode? (rb-node-parent x))
	   (setf (rb-tree-root tree) y))
	  ((eq x (rb-node-right (rb-node-parent x)))
	   (setf (rb-node-right (rb-node-parent x)) y))
	  (t (setf (rb-node-left (rb-node-parent x)) y)))
    (setf (rb-node-right y) x)
    (setf (rb-node-parent x) y)
    t))

(defmethod tree-delete (tree key)
  "Deletes the (first found) node with key KEY in TREE and fixes up the red-black
   properties.  Returns the key and value of the deleted node, or NIL if the key
   was not in the tree."
  (declare (optimize (speed 3) (safety 0)))
  #+:debug
  (assert (typep tree 'rb-tree)
      ()
    "Cannot delete ~s from non-tree ~s" key tree)
  (let ((z (tree-search-node tree key)))
    (if (null z)
	nil
      (let* ((y (if (or (nilnode? (rb-node-left z))
                        (nilnode? (rb-node-right z)))
		    z
                  (tree-successor z)))
	     (x (if (nilnode? (rb-node-left y))
		    (rb-node-right y)
		  (rb-node-left y))))
        (setf (rb-node-parent x) (rb-node-parent y))	; remember, x may be sentinel!
        (if (nilnode? (rb-node-parent y))		; link x into its parent (or root)
	    (setf (rb-tree-root tree) x)
          (if (eq y (rb-node-left (rb-node-parent y)))
	      (setf (rb-node-left (rb-node-parent y)) x)
            (setf (rb-node-right (rb-node-parent y)) x)))
        (unless (eq y z)		; copy info to replaced node
          (setf (rb-node-key z) (rb-node-key y))
          (setf (rb-node-val z) (rb-node-val y)))
	;; If y is black, then we must fix up the tree after the
	;; deletion done above.
        (when (not (rb-node-red? y))
	  (tree-delete-fixup tree x))
	#+:debug
	(assert (check-rb tree) () "Failed red-black tree check.")
	(values (rb-node-key z) (rb-node-val z))))))

(defmethod tree-delete-fixup ((tree rb-tree) (x rb-node))
  (while (and (not (eq x (rb-tree-root tree)))
	      (not (rb-node-red? x)))
    (cond ((eq x (rb-node-left (rb-node-parent x)))
	   (let ((w (rb-node-right (rb-node-parent x))))
	     (when (rb-node-red? w)
	       (make-black w)		; color[w]=black ; Case 1
	       (make-red (rb-node-parent x)) ; color[p[x]]=red
	       (rb-left-rotate tree (rb-node-parent x))
	       (setq w (rb-node-right (rb-node-parent x))))
	     (cond ((and (not (rb-node-red? (rb-node-left w)))
			 (not (rb-node-red? (rb-node-right w))))
		    (make-red w)	;Case 2
		    (setq x (rb-node-parent x)))
		   (t (when (not (rb-node-red? (rb-node-right w)))
			(make-black (rb-node-left w)) ; Case 3
			(make-red w)
			(rb-right-rotate tree w)
			(setq w (rb-node-right (rb-node-parent x))))
		      (setf (rb-node-red? w) (rb-node-red? (rb-node-parent x)))
		      (make-black (rb-node-parent x))
		      (make-black (rb-node-right w))
		      (rb-left-rotate tree (rb-node-parent x))
		      (setq x (rb-tree-root tree))))))
	  ((eq x (rb-node-right (rb-node-parent x)))
	   (let ((w (rb-node-left (rb-node-parent x))))
	     (when (rb-node-red? w)
	       (make-black w)		; color[w]=black
	       (make-red (rb-node-parent x))		; color[p[x]]=red
	       (rb-right-rotate tree (rb-node-parent x))
	       (setq w (rb-node-left (rb-node-parent x))))
	     (cond ((and (not (rb-node-red? (rb-node-right w)))
			 (not (rb-node-red? (rb-node-left w))))
		    (make-red w)
		    (setq x (rb-node-parent x)))
		   (t (when (not (rb-node-red? (rb-node-left w)))
			(make-black (rb-node-right w))
			(make-red w)
			(rb-left-rotate tree w)
			(setq w (rb-node-left (rb-node-parent x))))
		      (setf (rb-node-red? w) (rb-node-red? (rb-node-parent x)))
		      (make-black (rb-node-parent x))
		      (make-black (rb-node-left w))
		      (rb-right-rotate tree (rb-node-parent x))
		      (setq x (rb-tree-root tree))))))))
  (make-black x))

(defmethod tree-minimum ((tree rb-tree))
  "Returns the minimum key (and its value) in TREE. NIL if tree is empty."
  (let ((root (rb-tree-root tree)))
    (if (nilnode? root)
	nil
      (let ((node (tree-minimum-n root)))
	(and node
	     (values (rb-node-key node) (rb-node-val node)))))))

(defmethod tree-minimum-n ((node rb-node))
  (declare (optimize (speed 3) (safety 0)))
  (let ((l (rb-node-left node)))
    (if (nilnode? l)
      node
      (tree-minimum-n l))))

(defmethod tree-maximum ((tree rb-tree))
  "Returns the maximum key (and its value) in TREE. NIL if tree is empty."
  (let ((root (rb-tree-root tree)))
    (if (nilnode? root)
	nil
      (let ((node (tree-maximum-n root)))
	(and node
	     (values (rb-node-key node) (rb-node-val node)))))))

(defmethod tree-maximum-n ((node rb-node))
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((r (rb-node-right node)))
    (if (nilnode? r) 
      node
      (tree-maximum-n r))))

(defmethod tree-list ((tree rb-tree) &optional low high)
  "Returns a list of the elements of tree, in order. Each element is a
  pair containing the key and value."
  (let ((ans nil))
    (tree-range-query tree
		      #'(lambda (k v) (push (cons k v) ans))
		      (or low (tree-minimum tree))
		      (or high (tree-maximum tree)))
    (nreverse ans)))

(defmethod tree-list-keys ((tree rb-tree) &optional low high)
  "Returns a list of the keys in a tree, in order."
  (let ((ans nil))
    (tree-range-query tree
		      #'(lambda (k v)
			  (declare (ignore v))
			  (push k ans))
		      (or low (tree-minimum tree))
		      (or high (tree-maximum tree)))
    (nreverse ans)))

(defmethod tree-show ((tree rb-tree) &optional (depth nil))
  "Prints a hierarchical display, by indentation, of tree. Only the
  keys are printed, and an indication (*) if the node is black."
  (rb-node-show (rb-tree-root tree) 0 depth))

(defmethod rb-node-show ((tr rb-node) &optional (level 0) (depth nil))
  (when (and (not (nilnode? tr)) (or (null depth) (plusp depth)))
    (rb-node-show (rb-node-left tr) (+ level 3) (and depth (1- depth)))
    (format t "~%~vT~s~a~@[~a~]" 
	    level
	    (rb-node-key tr)
	    (if (rb-node-red? tr) " " "*")
	    (rb-node-val tr))
    (rb-node-show (rb-node-right tr) (+ level 3) (and depth (1- depth)))))

(defmethod tree-range-query ((tree rb-tree) fn low high)
  "Applies fn to every key and val whose key fall within the
  (inclusive) range [low, high]."
  ;; The algorithm is after Chapter 5 of deBerg, et al., Geometric Algorithms, 
  ;; Springer 1997, but deBerg's trees have all content only at the
  ;; leaves.
  (let ((<test (rb-tree-less-test tree)))
    (labels ((it (node)
               (let ((k (rb-node-key node))
                     (v (rb-node-val node))
                     (l (rb-node-left node))
                     (r (rb-node-right node)))
                 (cond ((nilnode? node)
                        'done)
                       ((funcall <test high k)
                        (it l))
                       ((funcall <test k low)
                        (it r))
                       (t (it l)
                          (funcall fn k v)
                          (it r))))))
      (it (rb-tree-root tree)))))

#|
The successor algorithm depends on the availability of parent links in the tree.
If the node has a right branch, then the successor is just the minimum
along this branch.  If it does not, then we seek to find the first among
its parents that was reached by heading up from a left branch.  (Right branches
taken up lead to lower values, but the first left branch taken up from any
of its ancestors must lead to a node that is the first higher than the
starting node.  The situation is exactly symmetric for predecessor.
|#

(defmethod tree-successor ((node rb-node))
  (declare (optimize (speed 3) (safety 0)))
  #+:debug
  (assert (not (nilnode? node))
      ()
    "Trying to take successor of a null node ~s" node)
  (let ((r (rb-node-right node)))
    (if (nilnode? r)
      (labels ((climb (higher lower)
		 (if (or (null higher) (nilnode? higher))
                   higher
		   (if (eq (rb-node-right higher) lower)
                     (climb (rb-node-parent higher) higher)
		     higher))))
	(climb (rb-node-parent node) node))
      (tree-minimum-n r))))

(defmethod tree-predecessor ((node rb-node))
  (declare (optimize (speed 3) (safety 0)))
  #+:debug
  (assert (not (nilnode? node))
      ()
    "Trying to take predecessor of a null node ~s" node)
  (let ((r (rb-node-left node)))
    (if (nilnode? r)
      (labels ((climb (higher lower)
		 (if (or (null higher) (nilnode? higher))
                   higher
		   (if (eq (rb-node-left higher) lower)
                     (climb (rb-node-parent higher) higher)
		     higher))))
	(climb (rb-node-parent node) node))
      (tree-maximum-n r))))

(defmethod tree-depth ((tree rb-tree))
  "Returns the maximum depth of tree and its black-depth.  Officially, the empty
   nodes at the leaves of the tree are black, but these count neither in depth or
   black-depth."
  (b-depth (rb-tree-root tree)))

(defmethod b-depth ((node rb-node))
  ;; returns two values: tree depth and black depth
  (declare (optimize (speed 3) (safety 0)))
  (if (nilnode? node)
    (values 0 0)
    (multiple-value-bind (ld lbd) (b-depth (rb-node-left node))
      (multiple-value-bind (rd rbd) (b-depth (rb-node-right node))
        (values (+ (max ld rd) 1)
                (+ (max lbd rbd) (if (rb-node-red? node) 0 1)))))))

(defmethod tree-size ((tree rb-tree))
  (declare (optimize (speed 3) (safety 0)))
  (labels ((inner (node)
             (if (nilnode? node)
               0
               (+ 1 (inner (rb-node-left node)) (inner (rb-node-right node))))))
    (inner (rb-tree-root tree))))

(defmethod check-rb ((tree rb-tree))
  "Returns the black-depth of the tree if condition 4 of red-black
  trees is satisfied (i.e., if each branch has an equal number of
  black nodes to a leaf).  Otherwise, returns nil.
  It does not check the other conditions."
  (labels ((ch (node)
             (if (nilnode? node)
               0
               (let ((l (ch (rb-node-left node)))
                     (r (ch (rb-node-right node))))
                 (if (= l r)
                   (+ l (if (rb-node-red? node) 0 1))
                   ;;(error "red-black violation in ~s" tree)
		   (return-from check-rb nil)
		   )))))
    (ch (rb-tree-root tree))))


#|
The code below is for testing purposes.

;; For experiments, we construct a list of 500,000 numbers.
(defparameter *nums*
    (let ((res nil))
      (dotimes (i 100000) (push (random 1000000) res))
      res))

(defparameter test1 (new-tree))

(defun addn (&optional (lim 1000000000))
  (do ((nn *nums* (cdr nn))
       (i 0 (1+ i)))
      ((or (null nn) (>= i lim)))
    (tree-insert test1 (car nn))))

;; To test deletes

(defparameter *20*
  '(15 6 18 3 7 17 20 2 4 13 9))

(defparameter test3 (new-tree))
(defun set20 ()
  (dolist (i *20*) 
     (tree-insert test3 i)))
;; (set20)
(tree-show test3)
(defun del (n)
  (tree-delete test3 n)
  (tree-show test3))

(defun del1 (n)
  (do ((nl *nums* (cdr nl))
       (i n (1- i)))
      ((or (zerop i) (null nl)))
    (tree-delete test1 (car nl))
    (tree-size test1)))

|#
