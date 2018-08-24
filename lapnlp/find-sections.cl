;;; -*- Mode: Lisp; Package: cl-user; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  - 03/31/2010 creation
|#
;;; 3/31/2010 psz
;;; Code to grovel over a corpus of documents and try to find
;;; commonly-occuring lines that might correspond to section
;;; labels. The intution is that for many corpora there will be such
;;; labels.  This approach is way too simplistic to deal with other
;;; section labeling conventions, such as subheadings embedded within
;;; lines.  This is just experimental code to see if we can make the
;;; process of building grammars for findstruct simpler.
#|
(defparameter *line-hash* (make-hash-table :test #'equal)
  "The hash that holds each unique line.")

(defmethod find-sections-scan ((corp corpus) &key (reset nil))
  (when reset (clrhash *line-hash*))
  (dolist (d (documents corp))
    (find-sections-scan (document d) :reset nil))
  (hash-table-count *line-hash*))

(defmethod find-sections-scan ((doc document) &key (reset nil))
  (let* ((text (content doc))
	 (posn 0)
	 (end (length text))
	 (line-end-finder (compile-re "\\n"
				      :return :index
				      :single-line t)))
    (when reset (clrhash *line-hash*))
    (loop
      (multiple-value-bind (m? whole)
	  (match-re line-end-finder text :start posn)
	(when (null m?)
	  (when (< posn end)
	    (incf (gethash (subseq text posn) 0)))
	  (return))
	(incf (gethash (subseq text posn (car whole)) *line-hash* 0))
	(setq posn (cdr whole)))))
  (hash-table-count *line-hash*))

(defun prune-line-hash (&optional (minimum 2))
  (maphash #'(lambda (k v)
	       (when (< v minimum)
		 (remhash k *line-hash*)))
	   *line-hash*)
  (hash-table-count *line-hash*))

(defun show-line-hash (&key (min 2) (match nil))
  (let ((kvs nil))
    (maphash #'(lambda (k v)
		 (when (and (>= v min)
			    (or (null match)
				(match-re match k :case-fold t)))
		   (push (cons k v) kvs)))
	     *line-hash*)

    (sort kvs #'> :key #'cdr)))

(defparameter *section-scan-hash* (make-hash-table :test #'equal)
  "Hash to hold unique items.")

(defmethod section-scan ((corp corpus)
			 &key
			 (reset nil)
			 (pattern nil))
  (when reset (clrhash *section-scan-hash*))
  (dolist (d (documents corp))
    (section-scan (document d) :reset nil :pattern pattern))
  (hash-table-count *section-scan-hash*))

(defmethod section-scan ((doc document)
			 &key
			 (reset nil)
			 (pattern nil))
  (when reset (clrhash *section-scan-hash*))
  (let* ((text (content doc))
	 (posn 0)
	 (pat (compile-re pattern :return :index :single-line t)))
    (loop
      (multiple-value-bind (m? whole part1)
	  (match-re pat text :start posn)
	(when (null m?)
	  (return))
	(let* ((bounds (or part1 whole))
	       (item (subseq text (car bounds) (cdr bounds))))
	  (incf (gethash item *section-scan-hash* 0))
	  (setq posn (cdr bounds))))))
  (hash-table-count *section-scan-hash*))

(defun section-scan-counts (&optional (minimum 2))
  (let ((kvs nil))
    (maphash #'(lambda (k v)
		 (when (>= v minimum) (push (cons k v) kvs)))
	     *section-scan-hash*)
    (sort kvs #'> :key #'cdr)))

(defun rid-dups (&optional (pre "^") (post "$"))
  "Searches for every item in section-scan-hash (with pre and post
  additions) in items of *line-hash* and reduces the counts of those
  that match against the line-hash."
  (maphash
   #'(lambda (k v)
       (let ((pat (compile-re (concatenate 'string pre k post)
			      :return :index :single-line t)))
	 (declare (ignorable v))
	 (format t "~%Testing ~s:" k)
	 (maphash
	  #'(lambda (lk lv)
	      ;;(format t "~%   try ~s:" lk)
	      (multiple-value-bind (m? whole)
		  (match-re pat lk)
		(declare (ignorable whole))
		(when m?
		  (format t "~%  decr ~s for ~s" lv lk)
		  (decf (gethash k *section-scan-hash* 0)
			lv))))
	  *line-hash*)))
   *section-scan-hash*))
|#

;;; 4/16/2010
;;; Here is a second try.  This time, we keep track of the relative
;;; positions of each distinct possible section heading line, and then
;;; aggregate over the corpus to see what are the typical relations
;;; among them.
;;;
;;; Our approach is first to find all distinct lines of text that
;;; appear in an entire corpus, and to record for each the set of
;;; documents in which it appears and the specific place(s) in those
;;; documents where it appears. We filter out those lines that appear
;;; below some threshold number of times, because they are very
;;; unlikely to indicate section heading.  It is possible, as in the
;;; case of the CHB ED notes, that subsection headings do not appear
;;; on distinct lines but instead are simply capitalized words,
;;; followed by a colon, inside text.  We can extend to this as well,
;;; but would need to eliminate duplicate counting, as suggested in
;;; rid-dups, above.
;;;
;;; Using that information, we can then compute, for each pair of
;;; distinct possible headers, the number of times each precedes the
;;; other within the documents in which they jointly appear.  If
;;; header lines are ordered consistently, then we should get a
;;; partial order among them, which would then suggest the appropriate
;;; grammar to recognize sections.  This leaves several issues:
;;; 1. Inconsistency may lead to circular ordering structures, in
;;; which each cycle must be broken.
;;; 2. We will generally not get a total order because some possible
;;; headings will not appear jointly in enough documents.  (We
;;; eliminate ordering information that comes from too few instances.)
;;; 3. I don't have a good algorithm for creating the partial order,
;;; so the one here may be terribly inefficient.

(defparameter *distinct-line-hash* (make-hash-table :test #'equal)
  "The hash that holds each unique line. The value is an a-list
  indexed by document id, holding a list of all the match info for
  that line in that document. E.g., an alist element might look like
  (1234 ((30 . 40) (32 . 33)) ((100 . 110) (102 . 103)))
  if this line occurred twice in document 1234, once starting at
  location 30 and once at 100.
")

(defparameter *distinct-line-pair-hash*
    (make-hash-table :test #'equal)
  "Sparse representation of an array that counts how often one of a
    pair of distinct lines precedes another. The value holds a pair,
    of the number of times one precedes/succeeds the other.")

(defparameter *distinct-line-po-hash* (make-hash-table :test #'equal)
  "We create an entry for each possible header line, and each entry
  lists those that typically come somewhere after it in text, as
  determined by analyze-line-pairs.")
    
(defparameter *distinct-word-hash*
    (make-hash-table :test #'equalp))

(defmethod analyze-sections
    ((c corpus) &key (min 0.1) (file nil))
  "Finds the common potential section headers in a corpus. Min is
    either an integer or fraction of the corpus size that limits
    analysis to those potential headers that appear at least min times
    in the corpus.  File, if not nil, is the name of an output file
    into which the resulting draft XML structure (as used by
    findstruct) is stored; otherwise, it is simply returned."
  (clrhash *distinct-line-hash*)
  (dolist (d-id (documents c))
    (analyze-sections-in-doc (document d-id)))
  ;; Now get rid of any lines that did not occur in at least min
  ;; documents
  (let ((minn (if (floatp min)
		  (floor (* min (length (documents c))))
		min)))
    (maphash #'(lambda (str appearances)
		 (when (< (length appearances) minn)
		   (remhash str *distinct-line-hash*)))
	     *distinct-line-hash*)
    (analyze-line-pairs)
    (maphash #'(lambda (str-pair counts)
		 (when (< (+ (car counts) (cdr counts))
			  minn)
		   (remhash str-pair
			    *distinct-line-pair-hash*)))
	     *distinct-line-pair-hash*)
    (analyze-po)
    (let* ((lxml `((report prefix "" suffix "" case_insensitive "no")
		   ,@(mapcar #'(lambda (node)
				 (make-xml-name (po-node-item node)))
			     (po-linear)))))
      ;; print-xml of the structure created from lin
      ;; need to fix items that are not valid xml-names!!!
      (if (null file)
	  lxml
	(with-open-file (f file :direction :output
			 :if-exists :supersede)
	  (print-xml lxml f))))))




(defun make-xml-name (s)
  (let ((s_ (replace-re s "\\s+" "_")))
    (if (xml-name? s_)
	(list s_)
      `((,(xmlize-name s_) alt ,s_)))))

(defparameter *non-xml-namechar-re*
    (compile-re
     (concatenate 'string
       "[^" *xml-namechars* "]"))
  "A regexp2 pattern that matches any invalid characters in an xml-name.")

(defparameter *non-xml-namestartchar-re*
    (compile-re
     (concatenate 'string
       "[^" *xml-namechars* "]"))
  "A regexp2 pattern that matches any invalid starting character in an
    xml name.")

(defun xmlize-name (s)
  "Turns a string into a set of valid characters that can be an XML
  name. The first character, if not valid, becomes an _. Others become
  .. This works for matching, because the matcher interprets _ as
  space(s) and . as any character."
  (when (and (> (length s) 0)
	     (match-re *non-xml-namestartchar-re*
		       s
		       :start 0 :end 1))
    (setq s (concatenate 'string "_" (subseq s 1))))
  (replace-re s *non-xml-namechar-re* "."))
	      

(defun analyze-sections-in-doc (d)
  (let* ((text (content d))
	 (id (id d))
	 (lines (util:find-all-re "^.+$" text
				 :multiple-lines t
				 :return :index)))
    ;;(format t "~%lines=~s" lines)
    (dolist (line lines)
      (distinct-line-record (subseq text (caar line) (cdar line))
			    id
			    line))))

(defun distinct-line-record (text doc-id data)
  "Records the fact that a pattern matched text in this document, and
  returned data.  This is according to the design described for
  *distinct-line-hash*"
  (let ((existing (gethash text *distinct-line-hash*)))
    (cond
     (existing				;have seen this text before
      (let ((doc-data (assoc doc-id existing :test #'=)))
	(cond (doc-data			;in this document
	       (push data (cdr doc-data)))
	      (t (push (list doc-id data)
		       (gethash text *distinct-line-hash*))))))
      (t 				;novel
       (setf (gethash text *distinct-line-hash*)
	 (list (list doc-id data)))))))

(defun distinct-line-data ()
  (let ((ans nil))
    (maphash #'(lambda (k v) (push (list k v) ans))
	     *distinct-line-hash*)
    (nreverse ans)))

(defun distinct-line-pair-record (line-a line-b before after)
  "Increments or decrements that appropriate count in
  *distinct-line-pair-hash*"
  (let ((counts (gethash (cons line-a line-b)
			 *distinct-line-pair-hash*
			 (cons 0 0))))
    (incf (car counts) before)
    (incf (cdr counts) after)
    (setf (gethash (cons line-a line-b) *distinct-line-pair-hash*)
      counts)
    counts))

(defun analyze-line-pairs ()
  (clrhash *distinct-line-pair-hash*)
  (maphash
   #'(lambda (line-a appear-a)
       (maphash
	#'(lambda (line-b appear-b)
	    (when (not (equal line-a line-b)) ;don't compare a line
					;against itself
	      ;; find common documents and see how often line-a appears
	      ;; before line-b (or vice versa)
	      (dolist (a appear-a)
		(let ((bs (assoc (car a) (cdr appear-b)))
		      (before 0)
		      (after 0))
		  (when bs
		    (dolist (pos-a (cdr a))
		      (dolist (pos-b (cdr bs))
			(cond ((< (caar pos-a) (caar pos-b))
			       (incf before))
			      ((> (caar pos-a) (caar pos-b))
			       (decf after))))))
		  (when (or (not (zerop before)) (not (zerop after)))
		    (distinct-line-pair-record line-a line-b before after))))))
	*distinct-line-hash*))
   *distinct-line-hash*))

(defun analyze-line-pair-data ()
  "Returns a list of elements corresponding to every pair of distinct
  items in *distinct-line-hash* that have an ordering relation between
  them, as expressed in *distinct-line-pair-hash*. Each element is a
  cons of (a) a cons of the two strings, and (b) a cons of the number
  of times the first precedes the second and vice versa. The result is
  sorted by how many more times the first precedes the second."
  (let ((ans nil))
    (maphash #'(lambda (pair count)
		 (push (cons pair count) ans))
	     *distinct-line-pair-hash*)
    (sort ans #'> :key #'(lambda (x) (+ (cadr x) (cddr x))))))

(defun po-node (string)
  (gethash string *distinct-line-po-hash*))

(defun po-nodes ()
  (let ((nodes nil))
    (maphash #'(lambda (str node)
		 (declare (ignore str))
		 (push node nodes))
	     *distinct-line-po-hash*)
    nodes))

(defun po-counts ()
  (let ((n-succ 0) (n-pred 0))
    (maphash #'(lambda (str node)
		 (declare (ignore str))
		 (incf n-succ (length (po-node-succ node)))
		 (incf n-pred (length (po-node-pred node))))
	     *distinct-line-po-hash*)
    (list n-succ n-pred)))

(defstruct po-node
  item
  (succ nil)
  (pred nil))

(defun analyze-po ()
  (clrhash *distinct-line-po-hash*)
  ;; First, for each line, create a list of all the lines that often
  ;; follow it. We make po-node entries for each distinct line, and
  ;; then link them together so that every entry in
  ;; *distinct-line-po-hash* creates a 2-way link between the po-nodes
  ;; for its before and after components.  Except for possible
  ;; circularities, this creates a partial-order lattice.
  (maphash #'(lambda (k v)
	       (declare (ignore v))
	       (setf (gethash k *distinct-line-po-hash*)
		 (make-po-node :item k)))
	   *distinct-line-hash*)
  (let ((pairs (analyze-line-pair-data)))
    (dolist (pairdata pairs)
      (push (gethash (cdar pairdata)
		     *distinct-line-po-hash*)
	    (po-node-succ
	     (gethash (caar pairdata)
		      *distinct-line-po-hash*)))
      (push (gethash (caar pairdata)
		     *distinct-line-po-hash*)
	    (po-node-pred
	     (gethash (cdar pairdata)
		      *distinct-line-po-hash*))))
    ;; Reduce the partial order lattice to hold only the minimal
    ;; number of necessary links.  I.e., if A<B and B<C and A<C, then
    ;; we eliminate A<C because it is implied by the others. We do
    ;; this by a depth-first traversal of the lattice, starting with
    ;; only those po-nodes that have no predecessors.  We also check
    ;; for circularity while we are at it.
    (maphash
     #'(lambda (item node)
	 (declare (ignore item))
	 (when (null (po-node-pred node))
	   (simplify-node node nil)
	   ))
     *distinct-line-po-hash*)))

(defun simplify-node (node stack)
  "Checks for circularity, and eliminates redundant edges in the
  lattice representing a partial order.  Stack is the list of nodes
  visited on this traversal, in reverse order. Note that any link from
  a node on the stack other than the latest one is redundant."
  ;; If we detect a circularity, just cut the link that led to this
  ;; node. This is simplistic, but might be a start.
  (let ((found (member node stack)))
    (when found
;;;      (format t "~%Circularity: ~s"
;;;	      (mapcar #'po-node-item (cons node stack)))
      (setf (po-node-succ (car found))
	(delete node (po-node-succ (car found))))
      (setf (po-node-pred node)
	(delete (car found) (po-node-pred node)))))
  ;; Now remove any redundant links from further up the stack.
  (dolist (s (and stack (cdr stack)))
    (when (find node (po-node-succ s))
      #|(format t "~%Flush redundant link from ~s to ~s"
	      (po-node-item s) (po-node-item node))|#
      (setf (po-node-succ s)
	(delete node (po-node-succ s))))
    (when (find s (po-node-pred node))
      #|(format t "~%Flush reverse link from ~s to ~s"
	      (po-node-item node) (po-node-item s))|#
      (setf (po-node-pred node)
	(delete s (po-node-pred node)))))
  ;; After each recursion, again find the next successor, in case some
  ;; got deleted.
  (do ((n (let ((succs (po-node-succ node)))
	    (and succs (car succs)))
	  (let* ((succs (po-node-succ node))
		 (tail (member n succs)))
	    (and tail (cadr tail)))))
      ((null n))
    (simplify-node n (cons node stack)))
  ;;(format t "~%Done ~s ~s" (po-node-item node) (po-counts))
  )

(defun show-po ()
  (maphash
   #'(lambda (item node)
       (declare (ignore item))
       (when (null (po-node-pred node))
	 (show-po-node node )))
   *distinct-line-po-hash*))

(defun show-po-node (node &aux (visited nil))
  (labels
      ((inner (n i)
	 ;;(format t "~%~vt~a" i (po-node-item n))
	 (push n visited)
	 (dolist (s (po-node-succ n))
	   (cond ((member s visited)
		  (format t "~%~vt [~a]*" i (po-node-item s)))
		 (t (format t "~%~vt ~a" i (po-node-item s))
		    (inner s (+ i 2)))))))
    (inner node 0)))

(defun po-linear ()
  "Linearize the partial order in*distinct-line-po-hash*. Chooses an
  arbitrary linearization, because in general many linear orders are
  consistent with a partial order."
  ;; Perform essentially a breadth-first search starting at all the
  ;; nodes with no predecessors.  Each time, choose a minimum node
  ;; (i.e., one that does not have a successor link to any of the
  ;; others on the boundary) as the next to emit, and then replace
  ;; it with its successors on the boundary.
  (do ((boundary (remove-if #'po-node-pred (po-nodes))
		 (delete chosen
			 (dolist (n (po-node-succ chosen) boundary)
			   (unless (or (member n result)
				       (member n boundary))
			     (push n boundary)))))
       (chosen nil)
       (result nil (push chosen result)))
      ((null boundary) (nreverse result))
    (setq chosen (min-po boundary))))

(defun min-po (items)
  "Returns a minimal element of the set of items in a partial order."
  (dolist (item items)
    (when (notany #'(lambda (s) (member s items))
		  (po-node-succ item))
      (return item))))

(defun cluster-by-words ()
  "Clusters each of the possible header lines by words they have in
  common."
  (clrhash *distinct-word-hash*)
  (dolist (h (mapcar #'car (distinct-line-data)))
    (dolist (part (delete "" (split-re "\\s|,|\\.|:|;" h) :test #'equal))
      (pushnew h (gethash part *distinct-word-hash* nil)
	       :test #'equalp)))
  (let ((common nil))
    (maphash #'(lambda (k v)
		 (when (cdr v)
		   (push (list (length v) k v)
			 common)))
	   *distinct-word-hash*)
    (sort common #'> :key #'car)))

(defun show-headers-by-word-0 (&optional (reverse nil))
  (dolist (l (cluster-by-words))
    (format t "~%~a" (cadr l))
    (let ((hs (sort (append (caddr l) nil)
		    #'(lambda (x y)
			(let ((comparison
			       (gethash (cons x y)
					*distinct-line-pair-hash*)))
			  (and comparison
			       (plusp (+ (car comparison)
					 (cdr comparison)))))))))
      (do ((h1l hs (cdr h1l)))
	  ((null h1l))
	(do ((h2l (cdr h1l) (cdr h2l)))
	    ((null h2l))
	  (let ((comparison (gethash (cons (car h1l) (car h2l))
				     *distinct-line-pair-hash*)))
	    (when (if reverse (not comparison) comparison)
	      (format t "~%   ~s vs. ~s: ~@[~d~]"
		      (car h1l) (car h2l)
		      (and comparison (+ (car comparison) (cdr comparison)))))))))))
	   
(defun show-headers-by-word ()
  (dolist (l (cluster-by-words))
    (format t "~%~a" (cadr l))
    (let ((hs (sort (append (caddr l) nil)
		    #'(lambda (x y)
			(let ((comparison
			       (gethash (cons x y)
					*distinct-line-pair-hash*)))
			  (and comparison
			       (plusp (+ (car comparison)
					 (cdr comparison)))))))))
      (do ((h1l hs (cdr h1l)))
	  ((null h1l))
	(format t "~%   ~s:" (car h1l))
	(do ((h2l (cdr h1l) (cdr h2l)))
	    ((null h2l))
	  (let ((comparison (gethash (cons (car h1l) (car h2l))
				     *distinct-line-pair-hash*)))
	    ;; heuristic: if two headers appear in the same document,
	    ;; they are probably not equivalent
	    (unless comparison
	      (format t " ~w," (car h2l)))
	    ))))))
