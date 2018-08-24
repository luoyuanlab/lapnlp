;;; -*- Mode: Lisp; Package: late -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We provide code to import static data into the gazettes database.
;;; This database is normally accessed with the same credentials as
;;; the UMLS, and is similarly read-only with those credentials.  To
;;; load data into it, by default we assume that the program runs on
;;; the database server, where root may do anything to any database
;;; without a password.  This would need to be altered for other
;;; circumstances. We also assume that the gazettes database already
;;; exists before we try to import into it.
;;; The database is specified by the usual LATE configuration methods,
;;; see config.cl.
;;; 
;;; Import-known-gazettes imports into the gazette table, whose
;;; entries each have a type and entry, and possibly a rank,
;;; frequency, and cumulative probability for those data for which
;;; such statistics are available. I found that when the filename of a
;;; file such as dist.male.first.txt had multiple dots, I was unable
;;; to get the Lisp logical pathnames to work, so the import
;;; failed. Thus, I have renamed the files for male, female and last
;;; names.
;;;
;;; Import-nickname-gazette imports a table of associations between
;;; names and nicknames, and these do not fit into the gazette format.
;;;
;;; Import-provider-gazette imports the names and titles of staff
;;; members from a listing from Partners Healthcare (Boston) into the
;;; gazette-names table. Again, the format is idiosyncratic and these
;;; data also cannot fall into the gazette table.
;;;
;;; Import-streets assumes that the Tiger 2000 data already exists in
;;; the table streets, and computes a number of other gazette types
;;; from these: tiger-street, which is the base name of a
;;; street with street type and direction as suffix stripped off;
;;; tiger-street-num, which lists the root names of streets that end
;;; in a number, such as "State Route 9"; tiger-street-token lists
;;; every unique token that appears as part of a street root name;
;;; tiger-street-direction and tiger-street-type simply record the
;;; (few) cardinal directions that show up in street names and the
;;; type of street, e.g., street, avenue, boulevard, ... .
;;; It also creates tiger-city entries, which give the names of all
;;; cities/town/... in the country.
;;; Importantly, all of these include rank, frequency and cumulative
;;; frequency estimates as a computation from the streets table. Note
;;; that these frequencies are not genuine, but only represent the
;;; frequency of occurrence of a street name, type, etc. in the
;;; data. This is because we don't know how many addresses actually
;;; stand on each street.  For example, Jones Circle, which may have
;;; only two houses, counts the same as Francis Street, which may have
;;; 1000.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :late
  (:use :common-lisp #+allegro :excl :util)
  (:export "import-gazette"
	   "import-known-gazettes"
	   "*provider-titles*"
	   "check-title"
	   "import-provider-gazette"
	   "import-nickname-gazette"
	   "*gazette-db*"
	   "open-gazette-db"
	   "close-gazette-db"
	    "*street-titles*"
	    "*street-directions*"
	    "import-streets"))

(in-package :late)

(defun import-gazette (filespec &key (name nil) (comma "\\s*\\t\\s*"))
  "Imports a gazette into the LATE database. These are useful for
  identifying specific known entities such as names, honorifs, cities,
  hospitals, etc."
  ;; These are used by various annotation programs to annotate text.
  (open-gazette-database)
  (let* ((pn (pathname filespec))
	 (d (pathname-directory pn))
	 (n (pathname-name pn))
	 (e (pathname-type pn)))
    (when (and n (not (eq n :wild)) (null e))
      (setq d (append d (list n))
	    n nil
	    pn (make-pathname :directory d :name n :type e)))
    (let* ((gn (or name
		   (and d (find-if-not #'(lambda (x) (eq x :wild-inferiors))
				       d
				       :from-end t))
		   (symbol-name (gensym "gazette-")))))
      (dolist (f (directory pn))
	(let ((table (read-csv f :comma comma)))
	  (dolist (line table)
	    (unless (equal line '(""))	;blank line
	      (sql
	       (format
		nil
		"insert into gazette(type,entry,rank,frequency,cum) value (~a,~a,~a,~a,~a)"
		(sq gn)
		(sq (car line))
		(sq (and (> (length line) 3) (cadddr line)))
		(sq (and (cdr line) (cadr line)))
		(sq (and (> (length line) 2) (caddr line))))
	       :db *gazette-db*))))))))

(defun import-known-gazettes ()
  (open-gazette-database)
  (sql "drop table if exists gazette" :db *gazette-db*)
  (sql "create table if not exists gazette (
  id integer not null auto_increment primary key,
  type varchar(32) not null,
  entry varchar(255) not null,
  rank integer,
  frequency real,
  cum real,
  index (type, entry))" :db *gazette-db*)
  (import-gazette "late:;gazettes;male.txt" :name "male")
  (import-gazette "late:;gazettes;female.txt" :name "female")
  (import-gazette "late:;gazettes;last.txt" :name "last")
  (import-gazette "late:;gazettes;last-prefix.txt" :name "last-prefix")
  (import-gazette "late:;gazettes;honorifics-prefix.txt" :name "honorific-pre")
  (import-gazette "late:;gazettes;honorifics-suffix.txt" :name "honorific-post")
  (import-gazette "late:;gazettes;colleges.txt" :name "college")
  (import-gazette "late:;gazettes;countries.txt" :name "country")
  (import-gazette "late:;gazettes;counties.txt" :name "county")
  (import-gazette "late:;gazettes;companies.txt" :name "company")
  (import-gazette "late:;gazettes;ethnicities.txt" :name "ethnicity") ;includes nationalities
  (import-gazette "late:;gazettes;ma-hospitals.txt" :name "ma-hospital")
  (import-gazette "late:;gazettes;hospitals.txt" :name "hospital")
  (import-gazette "late:;gazettes;cities.txt" :name "city")
  (import-gazette "late:;gazettes;streets.txt" :name "street")
  (import-gazette "late:;gazettes;states.txt" :name "state")
  (import-gazette "late:;gazettes;provider-titles.txt" :name "provider-title")
  t)

;;; Another importer is import-mimic-corpus, defined in mimic.cl.

(defparameter *provider-titles*
    '("MD" "M.D." "Ph.D." "DO" "DMD" "MPH" "S.S." "MBBS" "NP" "R.N."
      "RN" "MB" "DDS" "MS" "MA" "PA-C" "PA" "DPM" "PSYD" "Psyd" "EDD" "CNM"
      "N.P." "MSC" "BA" "R.D." "MBA" "M.B.A." "Rn" "DSC" "MBBCH" "MBCHB" "SCD"
      "Np" "Dr" "M.B.B.S." "BS" "BSC" "DVM" "DPHIL" "D.M.D." 
      "PHD" "Phd" "PhD" "Ccc-S" "JD" "MBBC" "Ms" "M.S." "Mph" "M. D." "Md" "M.Ed."
      "Msc" "MDCM" "LPN" "BDS" "MSW" "Lpn" "A" "Pa" "Ed.M." "MSN"
      "Pa-C" "FACS" "FRCPC" "Ocs" "CNP" "BSN" "MHA" "Ldn" "Rd" "A.N.P."
      "(Fellow)" "(Fellows)" "Crn" "CRNA" "Crna" "Crns" "Dds" "Do"
      "Dpm" "Edd" "Licsw" "MD(Fellow)" "MD." "MP" "Nd" "RD" "Res"
      ))

(defun title? (str)
  (member str *provider-titles* :test #'equal))

(defun dot-if-needed (str)
  (if (> (length str) 1)
      str
    (concatenate 'string str ".")))

(defun import-provider-gazette (&optional
				(file "late:;gazettes;Providers.txt")
				(type "partners-provider")
				(keep nil))
  "The gazette of provider names does not quite fit the pattern for
  general gazettes used above. Therefore, we create another table,
  gazette_names, to hold these data. The structure of these separates
  last names, first names and middle names, and saves titles as
  well. There are many peculiarities of the provider list we received
  from Partners Healthcare (Boston), which accounts for some of the
  peculiarities of the processing here."
  ;; For the Partners staff list, I also hand-edited the source file
  ;; to get rid of a handful of entries like "Practitioner, Nurse",
  ;; which certainly seem like a position rather than a name.
  (open-gazette-database)
  (cond ((member "gazette_names"
		 (sql "show tables" :db *gazette-db*)
		 :test #'equalp
		 :key #'car)
	 (unless keep (sql "delete from gazette_names" :db *gazette-db*)))
	(t
	 (sql "create table gazette_names (
id integer not null auto_increment primary key,
type varchar(255) default null,
last varchar(255) not null,
first varchar(255) default null,
givens varchar(255) default null,
initials varchar(15) default null,
titles varchar(255) default null,
key x_last(last),
key x_first(first)) character set utf8"
	      :db *gazette-db*)))
  (labels
      ((title? (str)
	 (member str *provider-titles* :test #'equal))
       (dot-if-needed (str)
	 (if (> (length str) 1)
	     str
	   (concatenate 'string str "."))))
    (let* (;; read the name list as a CSV, to separate out components
	   (names (read-csv file))
	   ;; get rid of blank components, of which there are many
	   (names-no-blank (mapcar #'(lambda (n) (delete "" n :test #'equal))
				   names))
	   ;; split each name component in order to further process it
	   (names-split (mapcar
			 #'(lambda (n)
			     (mapcar #'(lambda (str) (split-re "\\s+" str)) n))
			 names-no-blank))
	   (type-sq (sq type)))
      (dolist (n names-split)
	;; a name may have titles attached to it in three ways, exemplified here:
	;; 1. "Watkins MD,Timothy"
	;; 2. "Watkins,MD,Timothy"
	;; 3. "Watkins,Timothy, MD, PA, Ph.D."
	;; In (1), the title is the last word or words in the last name.
	;; In (2), I don't think there are ever multiple titles.
	;; In (3), multiple titles may/may not be separated by commas
	(let ((last nil) (given nil) (titles nil))
	  ;; Run backward through last name components, accumulate titles;
	  ;; remainder is the (possibly multi-word) last name. Handles (1).
	  (dolist (lnc (car n))
	    (if (title? lnc)
		(pushnew lnc titles :test #'equal)
	      (push lnc last)))
	  (setq last (nreverse last))	;fix order because backwards 
	  ;; Run through successive top-level elements of the name, and
	  ;; so long as every part of one of those elements is a title,
	  ;; push it onto the titles. This risks interpreting a first or
	  ;; middle name or a set of initials as a title, but that
	  ;; should happen only very rarely. It does properly take care
	  ;; of both cases (2) and (3) or mixtures thereof.
	  (dolist (lnl (cdr n))
	    (if (every #'title? lnl)
		(dolist (tit lnl) (pushnew tit titles :test #'equal))
	      (setq given (append given lnl))))
	  (setq titles (nreverse titles)) ;fix order to the one given in text
	  ;; Normalize names and initials by splitting on periods,
	  ;; because some names include things like "D.N." or "L.Albeck".
	  (setq given
	    (mapcan #'(lambda (n)
			(delete "" (split-re "\\." n) :test #'equal))
		    given))
	  ;; Unfortunately, there are a modest number of name entries
	  ;; that do not follow the above formats. Instead, they simply
	  ;; list a person's name, first-name first, possible followed
	  ;; by a comma and titles.  These need to be parsed quite
	  ;; differently. Here we just assume that there are either no
	  ;; commas in the name or there is one and all the elements
	  ;; after it are titles.
	  (when (or (null (cdr n))
		    (and (= (length n) 2) (every #'title? (cadr n))))
	    (let ((namerev (reverse (car n))))
	      (setq last (list (car namerev)))
	      (setq given (nreverse (cdr namerev)))
	      (setq titles (and (cdr n) (cadr n)))))
	  ;;(format t "~%~s, ~s, ~s" last given titles)
	  (let* ((first
		  (dot-if-needed
		   (or (find-if #'(lambda (n) (>= (length n) 2))
				given)
		       (car given))))
		 (givens (join (mapcar #'dot-if-needed given) " "))
		 (ini (apply #'concatenate 'string
			     (mapcar #'(lambda (n) (subseq n 0 1)) given)))
		 (ttls (join titles ", ")))
	    (unless
		(sql
		 (format
		  nil
		  "select id from gazette_names where type=~a and last=~a and first=~a and givens=~a and titles=~a"
		  type-sq
		  (sq (join last " "))
		  (sq first)
		  (sq givens)
		  (sq ttls))
		 :db *gazette-db*)
	      (sql
	       (format
		nil
		"insert into gazette_names(type,last,first,givens,initials,titles) values(~a,~a,~a,~a,~a,~a)"
		type-sq
		(sq (join last " "))
		(sq first)
		(sq givens)
		(sq ini)
		(sq ttls))
	       :db *gazette-db*))))))))

(defun import-nickname-gazette (&optional (file "late:;gazettes;nicknames.txt"))
  "Imports a set of nicknames into the nickname table in the gazette
  database. One must connect via a username/password that permits
  changes to this normally read-only database. The code here assumes
  it is running on the same machine as the database server and can
  connect as root with no password; it also assumes a gazettes
  database already exists. The source file comes
  from MITRE's distribution its de-identification system used in the
  2006 i2b2/AMIA NLP challenge, and they attribute the source to
  \"http://www.usgenweb.org/researchers/nicknames.html, which is no
  longer available. Located via the Wayback machine at
  http://web.archive.org.\" That file contains some #-delimited
  comment lines at the beginning, and then each line is a
  comma-separated set of names, where all but the first are nicknames
  for the first.  The list is undoubtedly far from comprehensive, but useful."
  (open-gazette-database)
  (let ((lines (read-lines file))
	(splitter (compile-re "\\s*,\\s*")))
    (sql "drop table if exists nicknames" :db *gazette-db*)
    (sql "create table nicknames (
id integer not null auto_increment primary key,
name varchar(255) not null,
nickname varchar(255) not null,
key nick(nickname)) character set utf8"
	 :db *gazette-db*)
    (dolist (line lines)
      (unless (or (null line)
		  (zerop (length line))
		  (char= #\# (char line 0)))
	(let ((names (split-re splitter line)))
	  (dolist (nn (cdr names))
	    (sql (format nil
			 "insert into nicknames(name,nickname) values(~a,~a)" 
			 (sq (car names))
			 (sq nn))
		 :db *gazette-db*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Import Tiger 2000 streets data into the gazette, in a somewhat
;;; more useful form.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
	  (setq *gazette-db* (open-mysql-db "GAZETTE_DB"))
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

