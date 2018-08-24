;;; -*- Mode: Lisp; Package: late -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities for making it easier to live with the API to the Link
;;; Grammar Parser, which is defined in link-interface.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parse options
;;; We provide programs that make it relatively easier to set and
;;; retrieve various parse options, to display them, and to compare
;;; settings of several options.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following constants define compile-in limitations of LP:
;;; MAX_SENTENCE (250; must be <= 254)
;;; MAX_LINKS (=2*MAX_SENTENCE-3)
;;; MAX_WORD (60)
;;; MAX_TOKEN_LENGTH (50)
;;; MAX_LINE (1500)
;;; MAX_MEMORY_DEFAULT (128000000)
;;; MAX_PARSE_TIME (-1)
;;; MAX_DISJUNCT_COST (1000)

(defpackage :link
  (:use :common-lisp)
  (:export 
   "*gettable-parse-options-width*"
   "*panic-options*"
   "*parse-options-manipulators*"
   "+MAX_MEMORY_DEFAULT+"
   "+MAX_SENTENCE+"
   "<span"
   "=span"
   "combine-vals"
   "get-parse-options"
   "intersect-links"
   "l-lindex" "l-lword" "l-llab" "l-lab"
   "l-numdom" "l-copies"
   "l-rlab" "l-rword" "l-rindex" "l-len"
   "l="
   "line-break-char?"
   "linearize-sentence"
   "link-diff"
   "link-table"
   "linkrep"
   "lp-tokens"
   "set-parse-options"
   "set-parse-options-panic"
   "show-options-diff"
   "show-parse-options"
   "token-length"
   "union-links"
   ))
(in-package :link)



(defconstant +MAX_SENTENCE+ 250
  "LP defines the maximum space allocated for sentence tokens at
  compile time, so this limit cannot be exceeded.")

(defconstant +MAX_MEMORY_DEFAULT+ 128000000)

(defparameter *parse-options-manipulators*
  (mapcar #'(lambda (option)
	      (let ((name (symbol-name option)))
		(list option
		      (symbol-function
		       (intern
			(format nil "parse_options_get_~a" name)))
		      (symbol-function
		       (intern
			(format nil "parse_options_set_~a" name))))))
	  ;; The entries below that are commented out are because of
	  ;; an exchange of email with Linas Vepstas. He says they
	  ;; are only used by the command-line interface to control
	  ;; display of parsed sentences.
	  '(
	    :verbosity
	    :linkage_limit
	    :disjunct_cost
	    :min_null_count
	    :max_null_count
	    :null_block
	    :islands_ok
	    :short_length
	    :max_memory
	    ;;:max_sentence_length
	    :max_parse_time
	    :cost_model_type
	    :screen_width
	    :allow_null
	    :display_walls
	    :all_short_connectors
	    ;;:batch_mode
	    ;;:panic_mode
	    ;;:display_on
	    ;;:display_postscript
	    ;;:display_constituents
	    ;;:display_bad
	    ;;:display_links
	    ;;:display_union
	    ;;:display_senses
	    ;;:display_disjuncts
	    ;;:echo_on
	    ))
  "Parameter names that can be set or get from a parse-options. The
    ones commented out are not meaningful except in the command line
    interface.")

(defparameter *gettable-parse-options-width*
  (+ 1
     (reduce #'max (mapcar
		    #'(lambda (o)
			(length (symbol-name (car o))))
		    *parse-options-manipulators*)))
  "1+ the max width of the gettable parse-options, for formatting.")

(defun show-parse-options (po)
  "Displays the settings within a parse-options, which is a
  :foreign-address."
  (dolist (opt *parse-options-manipulators*)
    (format t "~%~a~vt = ~a"
	    (symbol-value (car opt))
	    *gettable-parse-options-width*
	    (funcall (cadr opt) po))))

(defun show-options-diff (&rest pos)
  (dolist (opt *parse-options-manipulators*)
    (let* ((fn (cadr opt))
	   (vs (mapcar #'(lambda (po) (funcall fn po)) pos)))
      (unless (every #'(lambda (x) (eql (car vs) x)) (cdr vs))
	(format t "~%~a~vt = ~{~a~^~1,8@t~}"
		(symbol-value (car opt))
		*gettable-parse-options-width*
		vs)))))

(defun get-parse-options (po &rest keys)
  "Returns a Lisp property list associating the given keys with their
  values in the parse-options po. The keys should be in the Lisp
  keyword package. If no keys are given, we retrieve all of them."
  (let ((ans nil))
    (mapcar #'(lambda (k)
		(let ((entry (assoc k *parse-options-manipulators*)))
		  (assert entry ()
			  "~s is not a valid get/set parse option."
			  k)
		  (push k ans)
		  (push (funcall (cadr entry) po) ans)))
	    (or keys (mapcar #'car *parse-options-manipulators*)))
    (nreverse ans)))

(defun set-parse-options (po &rest options)
  "Sets parse_options po to have the options specified by options, a
  rest argument. These should be an alternating list of keywords and
  values."
  ;; (apply #'set-parse-options po (get-parse-options po))
  ;; should be a no-op.
  (do ((l options (cddr l)))
      ((null l))
    (assert (cdr l) ()
	    "Ill-formed options to set-parse-options: odd-length  key/value list: ~s"
	    options)
    (let ((entry (assoc (car l) *parse-options-manipulators*)))
      (assert entry ()
	      "~s is not a valid get/set parse option."
	      (car l))
      (funcall (caddr entry) po (cadr l))))
  po)

(defparameter *panic-options*
    '(:disjunct-cost 3
      :min-null-count 1
      :max-null-count +MAX_SENTENCE+
      :max_parse_time 60
      :islands_ok 1
      :short_length 6
      :all_short_connectors 1
      :linkage_limit 100)
  "These are the parse_options that LP uses when in 'panic mode'.")

(defun set-parse-options-panic (po)
  (apply #'set-parse-options po *panic-options*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We use a Lisp structure to represent the information in a link in
;;; a link-parser linkage.
;;; The utilities below combine links from different linkages in two
;;; potentially useful different ways, each of which loses some
;;; information but leads to more compact representations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (linkrep
	    (:type vector)
	    (:conc-name l-))
  lindex lword llab lab rlab rword rindex len numdom (copies 1))

(defun link-table (linkage)
  "Extracts a representation of the links in a linkage, as a list of
  lists. Each entry contains information on a single link:
  1. left-word-number
  2. left-word (with pos marking)
  3. link-left-label
  4. link-label (intersection of left and right labels)
  5. link-right-label
  6. right-word (with pos marking)
  7. right-word-number
  8. link length
  9. number of domains
"
  (let ((lines nil))
    (dotimes (i (linkage_get_num_links linkage))
      (push
       (let ((l (linkage_get_link_lword linkage i))
	     (r (linkage_get_link_rword linkage i)))
	 (make-linkrep
	  :lindex l
	  :lword (linkage_get_word linkage l)
	  :llab (linkage_get_link_llabel linkage i)
	  :lab (linkage_get_link_label linkage i)
	  :rlab (linkage_get_link_rlabel linkage i)
	  :rword (linkage_get_word linkage r)
	  :rindex r
	  :len (linkage_get_link_length linkage i)
	  :numdom (linkage_get_link_num_domains linkage i)))
       lines))
    (nreverse lines)))

(defun l= (linkage1 linkage2)
  "Two linkages are l= if they have the same content, except for
  copies."
  (and (= (l-lindex linkage1) (l-lindex linkage2))
       (equal (l-lword linkage1) (l-lword linkage2))
       (equal (l-llab linkage1) (l-llab linkage2))
       (equal (l-lab linkage1) (l-lab linkage2))
       (equal (l-rlab linkage1) (l-rlab linkage2))
       (equal (l-rword linkage1) (l-rword linkage2))
       (= (l-rindex linkage1) (l-rindex linkage2))
       (= (l-len linkage1) (l-len linkage2))
       (= (l-numdom linkage1) (l-numdom linkage2))))

(defun =span (l1 l2)
  (and (= (l-lindex l1) (l-lindex l2))
       (= (l-rindex l1) (l-rindex l2))))

(defun <span (l1 l2)
  (or (< (l-lindex l1) (l-lindex l2))
      (and (= (l-lindex l1) (l-lindex l2))
	   (< (l-rindex l1) (l-rindex l2)))))

(defun combine-vals (new olds)
  ;; If new=old or among olds, do nothing. Otherwise, make
  ;; a list of both values.
  (cond ((listp olds)
	 (if (member new olds :test #'equalp)
	     olds
	   (cons new olds)))
	((equalp new olds) olds)
	(t (list new olds))))

(defun link-diff (lt1 lt2)
  "Computes the difference between two link-tables. First we find
  links that exist in both between the same word numbers. For these,
  we return common data, but where there are differences, we give the
  list if different values in these shared links.  A second value is
  links that exist in lt1 but not lt2, and a third value is the
  converse."
  (let ((common nil)
	(only-1 nil)
	(only-2 nil))
    (map nil
      #'(lambda (link)
	  (let ((m (remove-if-not #'(lambda (l) (=span link l)) lt2)))
	    (if m
		(let ((c (copy-linkrep link)))
		  (map nil
		    #'(lambda (x)
			(setf (l-llab c)
			  (combine-vals (l-llab x) (l-llab c)))
			(setf (l-rlab c)
			  (combine-vals (l-rlab x) (l-rlab c)))
			(setf (l-lab c)
			  (combine-vals (l-lab x) (l-lab c)))
			(setf (l-numdom c)
			  (combine-vals (l-numdom x) (l-numdom c))))
		    m)
		  (push c common))
	      (push link only-1))))
      lt1)
    (setq only-2
      (remove-if #'(lambda (link)
		     (find link common :test #'=span))
		 lt2))
    (values (nreverse common) (nreverse only-1) only-2)))

(defun intersect-links (list-of-link-tables)
  "Uses link-diff to combine a number of link tables, each
  representing a different parse (linkage) of a sentence. We return
  two values. The first is a link table showing all the variants for
  labels among linkages for those links that occur in all of them. The
  second value is a list of the same length as the input, holding
  those links that are not in the common set, for each linkage."
  ;; The utility of this function comes from the fact the LP often
  ;; assigns links among the same words but with different labels in
  ;; different linkages, leading to a combinatorial number of
  ;; linkages. Furthermore, these alternatives are usually not
  ;; coordinated, so the compact representation we adopt here loses no
  ;; real information.  However, when links occur among different
  ;; words in different linkages, we retain those separately. Thus,
  ;; effectively we return something like the intersection and then,
  ;; for each linkage, the set difference given by removing elements
  ;; of the intersection from the links in each linkage.
  (cond ((null list-of-link-tables) nil)
	((null (cdr list-of-link-tables))
	 (values (car list-of-link-tables) nil))
	(t (let ((ans (car list-of-link-tables)))
	     (dolist (lt (cdr list-of-link-tables))
	       (setq ans (link-diff ans lt)))
	     (values ans
		     (mapcar
		      #'(lambda (lt)
			  (remove-if
			   #'(lambda (link)
			       (find link ans :test #'=span))
			   lt))
		      list-of-link-tables))))))

(defun union-links (list-of-link-tables)
  "Computes a combination of all the links in the linkages represented
  by list-of-link-tables. This is a more compact representation than
  that returned by intersect-links, but loses more information, and
  results in a combined table that is not a valid linkage. In fact, if
  multiple linkages represent different phrase attachments, for
  example, the union form will look like the phrase attaches to all
  places where it attaches in any of the linkages.  We speculate that
  this may nevertheless be useful for certain applications."
  (let ((h (make-hash-table :test #'equal))
	(combined nil))
    (dolist (lt list-of-link-tables)
      (dolist (link lt)
	(let* ((key (cons (l-lindex link) (l-rindex link)))
	       (val (gethash key h nil)))
	  (setf (gethash key h)
	    (cond (val
		   (setf (l-llab val)
		     (combine-vals (l-llab link)
				   (l-llab val)))
		   (setf (l-rlab val)
		     (combine-vals (l-rlab link)
				   (l-rlab val)))
		   (setf (l-lab val)
		     (combine-vals (l-lab link)
				   (l-lab val)))
		   (setf (l-numdom val)
		     (combine-vals (l-numdom link)
				   (l-numdom val)))
		   val)
		  (t (copy-linkrep link)))))))
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v combined)) h)
    (sort combined #'<span)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility to turn text that includes newlines into single-line text.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun line-break-char? (c)
  (or (char= c #\Linefeed)
      (char= c #\Return)
      (char= c #\Newline)))

(defun linearize-sentence (text)
  "Returns a copy of text with all line breaks removed, and a list of
  the positions in the original text where each line break character
  was removed. This will allow reconstruction of the original
  character positions from positions in the copied string."
  (do ((deleteds nil)
       (line "")
       (s 0)
       (e (length text))
       (lf 0))
      ((null lf) (values line (nreverse deleteds)))
    (setq lf (position-if #'line-break-char? text :start s))
    (cond (lf
	   (push lf deleteds)
	   (setq line (concatenate 'string
			line (subseq text s lf) " "))
	   (setq s (1+ lf)))
	  (t
	   (setq line (concatenate 'string line (subseq text s e)))))))

(defun lp-tokens (a)
  (cond
   (a
    (annotations 
     a 
     :type 'lp-token 
     :filter #'(lambda (a) 
		 (not (typep a 'alp-token)))))
   (t
    nil)))

