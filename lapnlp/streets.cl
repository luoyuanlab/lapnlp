;;; -*- Mode: Lisp; Package: late; -*-

#|
yluo - 08/16/2018 clean and reorganization
psz  -            creation
|#

#| 

This is some code to help massage data from the Tiger 2000 database
into a form useful for LATE. We have abstracted a data set that lists
every street name in every city in every state, in the streets
table. Each street name includes indicators of the type of street it
is, so we may have Smith Street, Smith Avenue, Smith Boulevard,
etc. Assuming that for the purpose of feature generation, we don't
actually want to look for such complete names, we create a gazette of
the core parts of a street name, which excludes the common street type
indicators, as well as direction indicators that come at the end of a
street's name. We also separate root names that are followed by
numbers, such as "county road", whose instances are like "country road
9940".

The gazette entries we create are:
tiger-city -- names of cities
tiger-street-direction -- suffixes that identify compass points
tiger-street-type -- street suffixes that identify type of street
tiger-street -- street name with suffixes stripped
tiger-street-num -- street name with pure numeric suffix stripped
tiger-street-token -- word that appears in a street name

Each entry type also includes distributional data indicating its
commonality in the Tiger database.  Note that this is not actually a
good estimate of how commonly these names appear in actual
addresses, because we only count how often a city or street name
appears in the data, but we have no complete database of addresses.

|#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :mysql)
  (require :regexp2))

(defpackage :late
  (:use :common-lisp :util :dbi.mysql :excl)
  (:export "*street-titles*"
	   "*street-directions*"
	   "import-streets"))

(in-package :late)

(defparameter *street-titles*
  ;; This choice of designation is not very definitive. The list
  ;; that is commented out includes additional endings such as
  ;; "cove" that show up frequently in the data, but correspond more
  ;; to geographical and natural features: woods, oaks, bluff, isle,
  ;; lake, spring, ...  We have chosen to exclude these (and thus
  ;; leave them as part of the root street name).  Or perhaps the
  ;; Tiger database also explicitly includes the names of such
  ;; features, in which case they are not meant to be street names
  ;; at all.
  #|'("road" "drive" "street" "lane" "court" "avenue" "place"
  "circle" "way" "trail" "boulevard" "terrace" "cove" "loop"
  "parkway" "highway" "point" "alley" "tr" "path" "run" "ridge"
  "square" "park" "hill" "plaza" "crossing" "walk" "pass" "pike"
  "hollow" "row" "heights" "creek" "bend" "view" "crescent"
  "estates" "landing" "spur" "glen" "acres" "lake" "close" "vista"
  "bay" "manor" "hts" "branch" "pointe" "village" "grove" "turn"
  "cmn" "green" "woods" "turnpike" "center" "bluff" "chase"
  "crest" "oaks" "meadows" "knoll" "farm" "oak" "gate" "valley"
  "oval" "mall" "overlook" "spring" "mews" "cutoff" "wood"
  "bypass" "curve" "arch" "gardens" "expressway" "real" "broadway"
  "fork" "canyon" "pk" "mnr" "shores" "commons" "isle" "ramp"
  "bridge" "island" "peak" "mountain" "cross" "line" "corner"
  "mesa" "station" "shore" "falls")|#
  '("road" "drive" "street" "lane" "court" "avenue" "place"
    "circle" "way" "trail" "boulevard" "terrace" "loop"
    "parkway" "highway" "alley" "tr" "path" "run"
    "square" "plaza" "crossing" "walk" "pass" "pike" "row" "crescent"
    "spur" "close" "branch" "turn" "cmn" "turnpike" "chase" "gate"
    "oval" "mall" "mews" "cutoff" "bypass" "curve" "arch" "expressway"
    "broadway" "fork" "pk" "mnr" "commons" "ramp" "bridge" "cross"
    "line" "corner"))

(defparameter *street-directions*
  '("north" "south" "west" "east" "northeast" "northwest"
    "southeast" "southwest"))

(defparameter *street-suffix-parser*
  (compile-re
   `(:sequence
     :start-anchor
     (:register
      (:non-greedy-repetition 1 nil :everything))
     (:register
      (:greedy-repetition
       0 1
       (:sequence
	:whitespace-char-class
	(:alternation
	 (:register (:greedy-repetition 1 nil :digit-class))
	 (:sequence
	  (:alternation ,@*street-titles*)
	  (:greedy-repetition
	   0 1
	   (:sequence
	    :whitespace-char-class
	    (:alternation ,@*street-directions*))))))))
     :end-anchor)))

(defun import-streets ()
  (let ((streets-counts nil)
	(nstreets 0)
	(street-direction-counter (make-hash-table :test #'equal))
	(street-type-counter (make-hash-table :test #'equal))
	(street-root-counter (make-hash-table :test #'equal))
	(street-number-root-counter (make-hash-table :test #'equal))
	(street-token-counter (make-hash-table :test #'equal))
	(city-counter (make-hash-table :test #'equal))
	(committed nil)
	(spacer (compile-re "\\s+"))
	(digits (compile-re "^\\d+$")))
    (unwind-protect
	(progn
	  ;; Pull the relevant data from the streets table.
	  (open-gazette-database)
	  ;; retrieve the cities and counts of number of times each city appears with different
	  ;; states and streets; (rough estimate of commonness of city name)
	  ;; put this into the common counter format to make it easier to save data
	  (dolist (e (sql "select city,count(*) c from streets group by city order by c desc"
			  :db *gazette-db*))
	    (setf (gethash (car e) city-counter) (cadr e)))
	  (setq streets-counts
		(sql "select street,count(*) c from streets group by street"
		     :db *gazette-db*))
	  ;; parse street names into their token parts, count total
	  (dolist (e streets-counts)
	    (setf (cddr e)
		  (list (split-re spacer (car e))))
	    (incf nstreets (cadr e)))
	  ;; count the occurrences of each street direction and type, recognize
	  ;; street names that end in numbers, and determine the root names
	  (dolist (e streets-counts)
	    (let ((street (car e))
		  (count (cadr e))
		  (tokens (reverse (caddr e))))
	      (cond ((or (null tokens) (null (cdr tokens)))
		     (incf (gethash street street-root-counter 0) count))
		    ((match-re digits (car tokens))
		     (incf (gethash (join (reverse (cdr tokens)))
				    street-number-root-counter
				    0)
			   count))
		    (t (when (member (car tokens) *street-directions*
				     :test #'string=)
			 (incf (gethash (pop tokens) street-direction-counter 0)
			       count))
		       (cond ((member (car tokens) *street-titles*
				      :test #'string=)
			      (incf (gethash (pop tokens) street-type-counter 0)
				    count))
			     (t		;no recognized suffix; include full name
			      ))
		       (incf (gethash (join (reverse tokens))
				      street-root-counter
				      0)
			     count)))
	      (dolist (tk tokens)
		(incf (gethash tk street-token-counter 0) count))))
	  ;; save the information gathered to the LATE gazette table
	  ;; Do all this within a transaction, to make sure that it happens atomically
	  (sql "start transaction" :db *gazette-db*)
	  ;; first, delete all the existing geog data in the table
	  (sql "delete from gazette where type in ('tiger-city','tiger-street-type','tiger-street-direction','tiger-street-type','tiger-street','tiger-street-num','tiger-street-token')"
	       :db *gazette-db*)
	  (labels ((record (ht gaz)
			   (let ((total 0)
				 (cum 0.0d0)
				 (items nil)
				 (index 0)
				 (gazq (sq gaz)))
			     (maphash #'(lambda (k v)
					  (push (list k v) items)
					  (incf total v))
				      ht)
			     (setq items (sort items #'> :key #'cadr))
			     (setq total (float total 0.0d0))
			     (dolist (e items)
			       (let ((frac (/ (cadr e) total)))
				 (sql (format
				       nil
				       "insert into ~
                                  gazette(type,entry,rank,frequency,cum) ~
                                  values(~a,~a,~d,~24f,~24f)"
				       gazq
				       (sq (car e))
				       (incf index)
				       frac
				       (incf cum frac))
				      :db *gazette-db*))))))
	    (record city-counter "tiger-city")
	    (record street-direction-counter "tiger-street-direction")
	    (record street-type-counter "tiger-street-type")
	    (record street-root-counter "tiger-street")
	    (record street-number-root-counter "tiger-street-num")
	    (record street-token-counter "tiger-street-token"))
	  (sql "commit" :db *gazette-db*)
	  (setq committed t)
	  )
      (when (and *gazette-db* (not committed))
	(format t "~%*** import-streets failed to commit; roll back! ***")
	(sql "rollback" :db *gazette-db*)))))

