;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/12/2015 rewrite using asdf framework
psz  - 12/17/2012 fix to allow nil analysis, for compatibility with past
psz  - 12/15/2012 fixed bug in (corpora nil)
yluo - 07/20/2012 added sigsub-covered-pnodes
yluo - 05/02/2012 added socket-error binding to latesql, changed with-db-rows
       to latesql
yluo - 4/12/2012 added add-analysis, analyzedp, del-analysis to handle analysis
       bit uniformly; adjusted storage utility to accommodate (sw-ver ann) and
       (setting ann); added register-analysis to 
yluo - 4/5/2012 added dirty bit to annotations to avoid unnecessary database
       accesses.
yluo - 3/3/2012 when saving document, sync the update_at time
psz  - 12/8/2010 changed database tables to use UTF8 character
       encoding, as it should have been all along. See note about also
       shortening some indexed fields because of MySQL's limit of
       indexed fields to 1000 bytes and the larger number of bytes
       possibly needed to store UTF8 characters.
yluo - 09/12/2010 add get-linkage-graph to retreive linkage graph given sentence
       annotation, more query utilities to come later.
yluo - 09/11/2010 add utilities to save graph to database, these utilities need
       to be finalized.
yluo - 09/10/2010 added tables graph, linkage_graph, sig_subgraph, and
       lg_sigsub
psz  - 9/3/2010 added split-corpus as a utility
yluo - 06/02/2010 added instances table to late database
yluo - 05/23/2010 added corpus-attach-doc corpus-attach-docs
       corpus-detach-doc corpus-detach-docs      
yluo - 05/10/2010 added :db *late-db* to db operations where missing 
psz  - 08/19/2009 changed mind about the saving of umls-annotations,
       replacing the July mechanism with simply storing multiple
       annotations for each previous instance of a umls-annotation. This
       also got rid of the codings table.
psz  - 07/04/2009 added transactions to save and delete corpora and
       document, added saving of umls-annotation codings

This file provides a Mysql persistent storage mechanism for LATE, as
an alternative to the Allegrocache implementation.

We represent the following types of objects in a persistent way:
1. Documents
2. Corpora
3. Annotations
4. Gazettes
5. Annotation type hierarchy
... and eventually we will need models, which will be challenging.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :mysql))

(defpackage :late
  (:use :common-lisp :util :dbi.mysql)
  (:export 
   "*ann-delayed*"
   "*job-id*"
   "*job-table*"
   "*late-db*"
   "*output-sql*"
   "*mysql-create-commands*"
   "*mysql-drop-commands*"
   "add-analysis"
   "analyzedp"
   "changed-in-store?"
   "close-gazette-database"
   "close-late-database"
   "compact-late-database"
   "complete-annotation"
   "corp-attach-doc"
   "corp-attach-docs"
   "corp-detach-doc"
   "corp-detach-docs"
   "corpora"
   "corpus"
   "create-late-database"
   "docid->name"
   "del"
   "del-analysis"
   "delete-annotations"
   "delete-corpus"
   "delete-document"
   "drop-job-tables"
   "link-label-index"
   "document"
   "encode"
   "fetch-corpora"
   "find-annotation"
   "flush-deleted-annotations"
   "flush-orphan-annotations"
   "flush-orphan-documents"
   "flush-volatile-annotations"
   "get-annotation"
   "get-corpus"
   "get-document"
   "get-graph-name"
   "get-graphs"
   "get-mapped-linkage-graphs"
   "get-sigsub-support"
   "late-transaction"
   "latesql"
   "open-gazette-database"
   "open-late-database"
   "open-mysql-db"
   "persist-link-supp-dict-hash"
   "register-analysis"
   "save"
   "save-phrase-longest-umls"
   "save-wn-synset"
   "split-corpus"
   "sq"
   "sql-matcher"
   "sqs"
   "syns-freq"
   "umls-freq"
   "unsq"
   "unsq-read"
   "update-description"
   "update-wn-synset"
   "wn-synset-freq"
   "ann-doc-id"
   ))

(in-package :late)

(defparameter *save-ann-time* 0)
(defparameter *save-doc-time* 0)
(defparameter *ann-delayed* nil)
(defparameter *job-id* nil)
(defparameter *job-table* nil)
(defparameter *output-sql* nil)
(defparameter *max-packet* 4194000)
(defparameter *bulk-insert-ann-cmd* nil)
(defparameter *data-sql* (outstr-init))
(defparameter *ft-sql* (outstr-init))


(defun open-mysql-db (config-name 
		      &key (external-format :utf8)
		      &aux (db nil))
  "Opens a Mysql database specified by the configuration parameter
  config-name. Applying get-envs on this should yield a list of
  connection specifications, which are tried in order until one
  succeeds. Each one should be either a 4-list of (host, database,
  username, password) or a string containing the same four elements
  separated by vertical bars. A fifth element may optionally be given,
  as the port number (which defaults to 3306).  Returns the open
  connection unless opening all the specified connections fail, in
  which case it returns NIL."
  (dolist (spec (get-envs config-name))
    (assert (or (stringp spec)
		(and (listp spec)
		     (>= (length spec) 4)))
	    ()
	    "Error: ~s does not specify valid Mysql connection data."
	    spec)
    (let ((s (or (and (stringp spec)
		      (split-re "\\|" spec))
		 spec)))
      (when (setq db 
		  (ignore-errors
		    (connect :host (elt s 0)
			     :database (elt s 1)
			     :user (elt s 2)
			     :password (elt s 3)
			     :port (or (and (> (length s) 4)
					    (elt s 4))
				       3306)
			     :external-format external-format)))
	(format t "~%;;; ~a connected to ~s on ~a"
		config-name (elt s 1) (or (elt s 0) "localhost"))
	(return))))
  (assert db (db) "Unable to open database ~s!" config-name)
  db)

(defparameter *late-db* nil
  "The connection to the Mysql database holding LATE's persistent store.")

(defun open-late-database ()
  "Opens connection to the Mysql database used by LATE."
  ;;  (format t "~&;;; Openning LATE database~%")
  (when (and *late-db* (not (mysql-connected *late-db*)))
    (setq *late-db* nil))
  (unless *late-db*
    ;; :external-format nil is a hack for avoiding double conversion
    (setq *late-db* (open-mysql-db "LATE_DB")) ;;   :external-format nil
    )
  (when (and *job-id* (not *job-table*))
    (sql (format nil "DROP TABLE IF EXISTS annotations_~a" *job-id*)
	 :db *late-db*)
    (sql (format nil "CREATE TABLE IF NOT EXISTS annotations_~a (
  id INTEGER NOT NULL PRIMARY KEY,
  type VARCHAR(80) NOT NULL,
  document_id INTEGER NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
  start INTEGER NOT NULL,
  end INTEGER NOT NULL,
  data VARCHAR(255),
  sw_ver VARCHAR(20),
  setting VARCHAR(220),
  other MEDIUMTEXT,
  up BIGINT REFERENCES annotations(id),
  INDEX hierarchy (up),
  INDEX ind_sw_ver (sw_ver),
  INDEX ind_setting (setting),
  INDEX owner (document_id, type),
  INDEX x_ann_doc (document_id),
  INDEX x_data (data))
  ENGINE=MyISAM DEFAULT CHARACTER SET UTF8" *job-id*) 
	 :db *late-db*)
    
    (setf *job-table* t))
  *late-db*)

(defparameter *gazette-db* nil)

(defun drop-job-tables (corpn range)
  (dotimes (i range)
    (when (> i 0)
      (latesql "INSERT INTO annotations SELECT * FROM annotations_~a_~a"
	       corpn i)
      (latesql "DROP TABLE IF EXISTS annotations_~a_~a" corpn i))))

(defun open-gazette-database ()
  "Opens connection to the Mysql database holding Gazettes."
  ;;  (format t "~&;;; Openning Gazette database~%")
  (when (and *gazette-db* (not (mysql-connected *gazette-db*)))
    (setq *gazette-db* nil))
  (unless *gazette-db*
    (setq *gazette-db* (open-mysql-db "GAZETTE_DB"))))

(defun close-gazette-database ()
  "Terminate connection to the database used by LATE for gazettes.
  However, before closing the connection, we commit all transactions
  and update any changes in the type hierarchy."
  (cond ((null *gazette-db*) (format t "~%Gazette database already closed."))
	((mysql-connected *gazette-db*)
	 (ignore-errors (disconnect :db *gazette-db*))
	 (setq *gazette-db* nil))
	(t (format t "~%Gazette database was already disconnected.")))
  *gazette-db*)

(defun astr (analysis sw-ver setting)
  "analysis should be root form verb, we add -d here to indicate completion."
  (assert analysis
	  ()
	  "Must specify an analysis.")
  (format nil "|~a~@[|~a~]~@[|~a~]|" analysis sw-ver setting))

;; unregister an analysis is tricky, needs to do a full database scan,
;; may not worth it to do it automatically
(defun register-analysis (analysis sw-ver setting annotation)
  (latesql "INSERT INTO analyses_annotations 
            (analysis, annotation, sw_ver, setting)
            VALUES (~a, ~a, ~a, ~a)"
	   (sq analysis) (sq annotation) (sq sw-ver) (sq setting)))

(defmethod add-analysis ((doc document)
			 &key 
			 (ganalysis nil)
			 (analysis (ganalysis ganalysis))
			 (sw-ver (gsw-ver ganalysis))
			 (setting (gsetting ganalysis)))
  
  (when analysis
    (pushnew (astr analysis sw-ver setting) (analyses doc) :test #'equalp)
    (setf (dirty doc) t)))


(defmethod analyzedp ((doc document)
		      &key 
		      (ganalysis nil)
		      (analysis (ganalysis ganalysis))
		      (sw-ver (gsw-ver ganalysis))
		      (setting (gsetting ganalysis)))
  "The defaults for sw-ver and setting are contingent on the fact that keywords
are assigned sequentially"
  (and analysis (member (astr analysis sw-ver setting) (analyses doc) :test #'equalp)))

(defmethod del-analysis ((doc document)
			 &key (analysis nil)
			 (sw-ver nil)
			 (setting nil))
  (cond 
   ((equalp "all" analysis)
    (setf (analyses doc) nil))
   (t
    (setf (analyses doc) (remove (astr analysis sw-ver setting) 
				 (analyses doc) :test #'equalp))))
  (setf (dirty doc) t))

(defun sq (item)
  "Creates a SQL-friendly representation of item. Nil -> NULL, a
  number remains itself, a string is surrounded by single-quotes,
  which are also doubled if they appear within it. The expectation is
  that these will be used as the argument for an ~a directive in a
  format statement that constructs a SQL query."
  (cond ((null item) "NULL")
	((numberp item) (format nil "~a" item))
	((stringp item) (sqs item))
	((symbolp item) (sqs (symbol-name item)))
	(t (sqs (format nil "~s" item)))))

(defun sqs (string)
  "Replaces ' by \', \ by \\, and surrounds string with '. This is to
  provide input to a sql query."
  (setf string (replace-re string "\\\\" "\\\\\\\\"))
  (setf string (replace-re string "'" "\\'"))
  (concatenate 'string
	       "'"
					;(replace-re 
	       string
					;"(?<!~)~" "~~")
	       "'"))

(defun unsq (item)
  "Translates :null to NIL in items retrieved from sql."
  (if (eq item :null) nil item))

(defun unsq-read (item)
  "Turns a string returned from sql into a Lisp data item or
  structure. If it cannot be read as such, then it just returns the
  string." 
  (if (eq item :null)
      nil
    (or (let ((*package* (find-package :late)))
	  (ignore-errors
	    (multiple-value-bind (thing length)
		(read-from-string item)
	      (if (= length (length item)) thing item))))
	item)))

(defun sql-matcher (stuff)
  "Yields a string suitable for inclusion in a sql select where
  statement. If stuff is a string or a single-element list of strings,
  the result is ='element'. Otherwise it's in ('el1', 'el2', 'elk')."
  (when (and (consp stuff) (null (cdr stuff)))
    ;; eliminate single-element lists
    (setq stuff (car stuff)))
  (if (atom stuff)
      (concatenate 'string "=" (sq stuff))
    (format nil " in (~{~a~^,~})"
	    (mapcar #'sq stuff))))

(defun latesql (template &rest args)
  "Constructs and runs a mysql command. The error handler is for the
  case where after inactivity, mysql disconnects but Lisp only finds
  out when an error occurs trying to use the connection. In that case,
  we re-establish it."
  (open-late-database)
  (let ((result
	 (catch 'mysql-retry
	   ;; This catch will be thrown to within the error handler for the
	   ;; mysql-protocol-error that is signalled when an apparently
	   ;; connected mysql connection has actually lost its mysql
	   ;; end. The handler tries to re-open the database, whose Lisp
	   ;; status has changed as a result of the error, and then throws
	   ;; :retry in order to retry the call.  If no error occurs, the catch
	   ;; simply receives the answer, 
	   (handler-bind ((dbi.mysql:mysql-protocol-error
			   #'(lambda (condition)
			       (declare (ignore condition))
			       (when (open-late-database)
				 (format t "~&Warning: reopen late on mysql protocol error~%")
				 (throw 'mysql-retry :retry))))
			  (excl:socket-error
			   #'(lambda (condition)
			       (declare (ignore condition))
			       (when (open-late-database)
				 (format t "~&Warning: reopen late on socket error~%")
				 (throw 'mysql-retry :retry))))
			  (excl:errno-stream-error
			   #'(lambda (condition)
			       (declare (ignore condition))
			       (when (open-late-database)
				 (format t "~&Warning: reopen late on errno stream error~%")
				 (throw 'mysql-retry :retry)))))
	     (sql (apply #'format nil template args)
		  :db *late-db*)))))
    (if (eq result :retry)
	(apply #'latesql template args)
      result)))

(defmacro late-transaction (&body body)
  `(prog2 (progn (open-late-database)
		 (latesql "start transaction" :db *late-db*))
       (progn ,@body)
     (latesql "commit" :db *late-db*)))

(defun n2-biject-n (i j)
  "Defines a bijection from n2 to n, this is used to generate a key from 
combining two keys while avoid collision"
  (round (+ j (/ (* (+ i j -2) (+ i j -1)) 2))))

(defun n-biject-n2-i (n i)
  "Based on the bijection, calculates the mapping from n and i to j."
  (if (> n 0)
      (round (/ (+ (- 1 (* 2 i)) (sqrt (+ (* 8 i) (* 8 n) -7))) 2))
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Structure of the LATE database, and utilities to create it.
;;; Note that with UTF8 character set, a varchar(255) could be as many
;;;as 765 bytes long. Because indexes are limited to 1000 bytes in
;;;MySQL, we have had to change some fields (annotations.type,
;;;gazette.gazette) from varchar(255) to varchar(50) to allow the
;;;joint indexes to be created.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: put them in .sql inscript instead of hard code here
(defparameter *mysql-create-commands*
  '(
    "CREATE TABLE IF NOT EXISTS documents (
  id INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(255) UNIQUE,
  data TEXT,
  source VARCHAR(255),
  description TEXT,
  size INTEGER,
  updated_at TIMESTAMP,
  content LONGTEXT,
  analyses TEXT,
  INDEX name_ind (name))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS corpora (
  id INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(255) NOT NULL UNIQUE,
  description TEXT)
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS corpora_documents(
  corpus_id INTEGER NOT NULL REFERENCES corpora(id) ON DELETE CASCADE,
  document_id INTEGER NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
  PRIMARY KEY (corpus_id, document_id),
  INDEX x_cd_doc (document_id))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    

;;;  INDEX ind_type (type),
;;; UNIQUE KEY (id, document_id),
;;;   PARTITION BY LINEAR KEY(document_id)
;;;  PARTITIONS 10	  
    "CREATE TABLE IF NOT EXISTS annotations (
  id BIGINT NOT NULL PRIMARY KEY,
  type VARCHAR(80) NOT NULL,
  document_id INTEGER NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
  start INTEGER NOT NULL,
  end INTEGER NOT NULL,
  data VARCHAR(255),
  sw_ver VARCHAR(20),
  setting VARCHAR(220),
  other MEDIUMTEXT,
  up BIGINT REFERENCES annotations(id),
  
  INDEX hierarchy (up),
  INDEX ind_sw_ver (sw_ver),
  INDEX ind_setting (setting),
  INDEX owner (document_id, type),
  INDEX x_ann_doc (document_id),
  INDEX x_data (data))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM
"
      ;;; psz: Addition of fields sw_ver and setting to annotations requires the following changes
      ;;; to existing annotations tables:
      ;;; alter table annotations add column sw_ver varchar(20) after data;
      ;;; alter table annotations add column setting varchar(220) after sw_ver;
      ;;; alter table annotations add index ind_sw_ver (sw_ver);
      ;;; alter table annotations add index ind_setting (setting);
      ;;; alter table annotations drop index owner;
      ;;; alter table annotations add index owner (document_id, type);
      ;;; see (fix-maimonides) below.
    
;;; yluo - changed the column name of gazette to type to be consistent with 
;;; the database and the rest of the code: e.g., feature-extract.cl
    "CREATE TABLE IF NOT EXISTS gazette (
  id INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY,
  type VARCHAR(50) NOT NULL,
  entry VARCHAR(255) NOT NULL,
  rank INTEGER,
  frequency REAL,
  cum REAL,
  INDEX (type, entry))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS gazette_names (
  id INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY,
  type VARCHAR(255) DEFAULT NULL,
  last VARCHAR(255) NOT NULL,
  first VARCHAR(255) DEFAULT NULL,
  givens VARCHAR(255) DEFAULT NULL,
  initials VARCHAR(15) DEFAULT NULL,
  titles VARCHAR(255) DEFAULT NULL,
  INDEX (last),
  INDEX (first))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS types (
  type VARCHAR(255) NOT NULL PRIMARY KEY,
  super VARCHAR(255))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
;;; these tables are needed to merge multiple documents or annotations or a mix
;;; of documents and annotations into one training instance.
    "CREATE TABLE IF NOT EXISTS instance_sets (
  id INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(255) NOT NULL UNIQUE,
  updated_at TIMESTAMP NOT NULL,
  description VARCHAR(255) NOT NULL,
  grouping_rule VARCHAR(35) NOT NULL,
  INDEX name_ind (name),
  INDEX grouping_rule_ind (grouping_rule)) 
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS instances (
  id INTEGER AUTO_INCREMENT NOT NULL PRIMARY KEY,
  inst_type VARCHAR(30) NOT NULL,
  name VARCHAR(30) NOT NULL,
  INDEX name_ind (name),
  INDEX inst_type_ind (inst_type)) 
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS doc_attrs (
  did INTEGER NOT NULL PRIMARY KEY,
  empi VARCHAR(20) NOT NULL,
  mrn_type VARCHAR(10) NOT NULL,
  mrn VARCHAR(20) NOT NULL,
  rep_num VARCHAR(20) NOT NULL,
  mid VARCHAR(20) NOT NULL,
  rep_dt VARCHAR(30) NOT NULL,
  rep_desc VARCHAR(40) NOT NULL,
  rep_stat VARCHAR(20) NOT NULL,
  rep_type VARCHAR(10) NOT NULL,
  INDEX mrn_ind (mrn),
  INDEX rep_num_ind (rep_num),
  INDEX rep_dt_ind (rep_dt),
  FOREIGN KEY (did) REFERENCES documents(id)
    ON DELETE CASCADE) 
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS sets_instances (
  set_id INTEGER NOT NULL,
  inst_id INTEGER NOT NULL,
  PRIMARY KEY (set_id, inst_id),
  INDEX set_id_ind (set_id),
  INDEX inst_id (inst_id))
DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"	  
    
;;; the index on doc_id is probably useful in debug.
    "CREATE TABLE IF NOT EXISTS instances_content (
  inst INTEGER NOT NULL,
  content BIGINT NOT NULL,
  content_type VARCHAR(30) NOT NULL,
  PRIMARY KEY (inst, content),
  INDEX inst_ind (inst),
  INDEX content_ind (content),
  INDEX content_type_ind (content_type),
  FOREIGN KEY (content) REFERENCES documents(id)
    ON DELETE CASCADE,
  FOREIGN KEY (content) REFERENCES annotations(id)
    ON DELETE CASCADE,
  FOREIGN KEY (content) REFERENCES instances(id)
    ON DELETE CASCADE) 
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
;;; stores features of instances
    "CREATE TABLE IF NOT EXISTS instances_features (
  inst INTEGER NOT NULL PRIMARY KEY,
  features LONGTEXT NOT NULL,
  FOREIGN KEY (inst) REFERENCES instances(id)
    ON DELETE CASCADE)
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"

;;; feature list without duplicates
    "CREATE TABLE IF NOT EXISTS feature_list (
  type VARCHAR(255) NOT NULL,
  feature TEXT,
  docname VARCHAR(255) NOT NULL,
  instname VARCHAR(255) NOT NULL,
  INDEX type_ind (type),
  INDEX feature_ind (feature (255)))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"	  
    
;;; below are used to create graph related tables
    "CREATE TABLE IF NOT EXISTS graph (
  gid INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY,
  gname VARCHAR(255) NOT NULL,
  gtype VARCHAR(255) NOT NULL,
  INDEX gname_ind (gname),
  INDEX gtype_ind (gtype))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS sig_subgraph (
  sub_id INTEGER NOT NULL,
  n1 INTEGER NOT NULL,
  n2 INTEGER,
  lab VARCHAR(255),
  significance NUMERIC NOT NULL DEFAULT 0,
  type VARCHAR(20) NOT NULL,
  FOREIGN KEY (sub_id) REFERENCES graph(gid)
    ON DELETE CASCADE,
  INDEX sub_id_ind (sub_id),
  INDEX significance_ind (significance)) 
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"

    "CREATE TABLE IF NOT EXISTS redundant_sigsub (
  sub_id INTEGER NOT NULL,
  FOREIGN KEY (sub_id) REFERENCES graph(gid)
    ON DELETE CASCADE) 
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS sigsub_property (
  sub_id INTEGER NOT NULL,
  npname VARCHAR(255),
  npval DOUBLE NOT NULL,
  FOREIGN KEY (sub_id) REFERENCES graph(gid)
    ON DELETE CASCADE,
  INDEX npname_ind (npname),
  INDEX sub_id_ind (sub_id)) 
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS sigsub_class_mutual_information (
  sub_id INTEGER NOT NULL,
  class VARCHAR(255) NOT NULL,
  mutual_information DOUBLE NOT NULL,
  FOREIGN KEY (sub_id) REFERENCES graph(gid)
    ON DELETE CASCADE,
  PRIMARY KEY (sub_id, class))
DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"	  
    
    "CREATE TABLE IF NOT EXISTS linkage_graph (
  lg_id INTEGER NOT NULL,
  pn1 BIGINT NOT NULL,
  pn2 BIGINT,
  n1 INTEGER NOT NULL,
  n2 INTEGER,
  lab VARCHAR(255),
  type VARCHAR(50) NOT NULL,
  sent BIGINT NOT NULL, 
  FOREIGN KEY (lg_id) REFERENCES graph(gid)
    ON DELETE CASCADE,
  FOREIGN KEY (sent) REFERENCES annotations(id)
    ON DELETE CASCADE,
  INDEX lg_id_ind (lg_id),
  INDEX pn1_ind (pn1),
  INDEX type_ind (type),
  INDEX sent_ind (sent))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS lg_sigsub (
  sub_id INTEGER NOT NULL,
  lg_id INTEGER NOT NULL,
  lg_node INTEGER NOT NULL,
  sub_node INTEGER NOT NULL,
  mapping_i INTEGER NOT NULL,
  FOREIGN KEY (sub_id) REFERENCES graph(gid)
    ON DELETE CASCADE,
  FOREIGN KEY (lg_id) REFERENCES graph(gid)
    ON DELETE CASCADE,
  INDEX lg_id_ind (lg_id),
  INDEX sub_id_ind (sub_id))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"

;;; Assumes sub1 is a subisomorphic to sub2
    "CREATE TABLE IF NOT EXISTS sigsub_sigsub (
  sub1_id INTEGER NOT NULL,
  sub2_id INTEGER NOT NULL,
  sub1_node INTEGER NOT NULL,
  sub2_node INTEGER NOT NULL,
  mapping_i INTEGER NOT NULL,
  FOREIGN KEY (sub1_id) REFERENCES graph(gid)
    ON DELETE CASCADE,
  FOREIGN KEY (sub2_id) REFERENCES graph(gid)
    ON DELETE CASCADE,
  INDEX sub1_id_ind (sub1_id),
  INDEX sub2_id_ind (sub2_id))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    
    "CREATE TABLE IF NOT EXISTS groundtruth (
  inst_id INTEGER NOT NULL,
  label VARCHAR(50) NOT NULL,
  PRIMARY KEY (label, inst_id))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS link_supp_dict (
  file VARCHAR(50) NOT NULL,
  entry VARCHAR(255) NOT NULL,
  INDEX entry_ind (entry),
  INDEX file_ind (file))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS phrase_longest_umls (
  phid BIGINT NOT NULL,
  umlsid BIGINT NOT NULL,
  content VARCHAR(50) NOT NULL,
  type VARCHAR(30) NOT NULL,
  PRIMARY KEY (umlsid, type),
  FOREIGN KEY (phid) REFERENCES annotations(id)
    ON DELETE CASCADE,
  FOREIGN KEY (umlsid) REFERENCES annotations(id)
    ON DELETE CASCADE,
  INDEX phid_ind (phid),
  INDEX type_ind (type),
  INDEX content_ind (content))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS wordnet_synset_stat (
  synset VARCHAR(200) NOT NULL,
  invoked_by VARCHAR(50) NOT NULL,
  pos VARCHAR(10) NOT NULL,
  freq INTEGER NOT NULL,
  PRIMARY KEY (invoked_by, pos, synset),
  INDEX invoked_by_ind (invoked_by),
  INDEX pos_ind (pos),
  INDEX synset_ind (synset))
  DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS decoder (
  ann_id BIGINT NOT NULL PRIMARY KEY,
  code TEXT NOT NULL,
  decoded MEDIUMTEXT,
  type VARCHAR(30) NOT NULL,
  FOREIGN KEY (ann_id) REFERENCES annotations(id)
    ON DELETE CASCADE,
  INDEX type_ind (type))
DEFAULT CHARACTER SET UTF8 ENGINE=MyISAM"
    
    "CREATE TABLE IF NOT EXISTS analyses_annotations (
    analysis VARCHAR(255) NOT NULL,
    annotation VARCHAR(80) NOT NULL,
    sw_ver VARCHAR(80),
    setting VARCHAR(160),
    PRIMARY KEY (annotation, sw_ver, setting),
    INDEX analysis_ind(analysis))
  DEFAULT CHARACTER SET UTF8  ENGINE=MyISAM"	  
    )
  "SQL commands to create the tables for persistent storage of LATE data.")

(defparameter *mysql-drop-commands*
  '("DROP TABLE IF EXISTS documents"
    "DROP TABLE IF EXISTS corpora"
    "DROP TABLE IF EXISTS corpora_documents"
    "DROP TABLE IF EXISTS annotations"
    "DROP TABLE IF EXISTS gazette"
    "DROP TABLE IF EXISTS gazette_names"
    "DROP TABLE IF EXISTS types"
    "DROP TABLE IF EXISTS instance_sets"
    "DROP TABLE IF EXISTS instances"
    "DROP TABLE IF EXISTS sets_instances"
    "DROP TABLE IF EXISTS doc_attrs"
    "DROP TABLE IF EXISTS instances_content"
    "DROP TABLE IF EXISTS instances_features"
    "DROP TABLE IF EXISTS feature_list"
    "DROP TABLE IF EXISTS graph"
    "DROP TABLE IF EXISTS sig_subgraph"
    "DROP TABLE IF EXISTS redundant_sigsub"
    "DROP TABLE IF EXISTS sigsub_property"
    "DROP TABLE IF EXISTS sigsub_class_mutual_information"
    "DROP TABLE IF EXISTS linkage_graph"
    "DROP TABLE IF EXISTS lg_sigsub"
    "DROP TABLE IF EXISTS sigsub_sigsub"
    "DROP TABLE IF EXISTS groundtruth"
    "DROP TABLE IF EXISTS link_supp_dict"
    "DROP TABLE IF EXISTS phrase_longest_umls"
    "DROP TABLE IF EXISTS wordnet_synset_stat"
    "DROP TABLE IF EXISTS decoder"
    "DROP TABLE IF EXISTS analyses_annotations")
  "This list should parallel those in *mysql-create-commands*.")

(defun create-late-database (&optional (confirm nil))
  "Deletes and re-creates an empty LATE database.  The optional
  argument must be given and not NIL, as a safety measure."
  (assert confirm
	  (confirm)
	  "If you REALLY want to delete all data in the LATE database, you
  must provide an optional argument to confirm. Consider instead using
  flush-volatile-annotations, which preserves all documents, corpora
  and persistent-annotations.")
  (open-late-database)
  (dolist (cmd *mysql-drop-commands*)
    (latesql cmd :db *late-db*))
  (dolist (cmd *mysql-create-commands*)
    (latesql cmd :db *late-db*)))

(defun close-late-database ()
  "Terminate connection to the database used by LATE.
  However, before closing the connection, we commit all transactions
  and update any changes in the type hierarchy."
  
  (cond ((null *late-db*) (format t "~%LATE database already closed."))
	((mysql-connected *late-db*)
;;;	 (record-type-hierarchy)
;;;	 (latesql "commit")
	 (ignore-errors (disconnect :db *late-db*))
	 (setq *late-db* nil))
	(t (format t "~%LATE database was already disconnected.")))
  *late-db*)

(defun compact-late-database (&key keep)
  "This was defined for an Allegrocache database, but is not needed
  for a Mysql one."
  (declare (ignore keep))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods to fetch and save and delete persistent data for LATE
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CORPORA

(defmethod corpora ((dummy-nil (eql nil)))
  "Retrieves a list of all the corpora in the LATE database."
  (let ((ans nil))
    (open-late-database)
    (dolist (corp-res (latesql "SELECT id, name, description FROM corpora"))
      (destructuring-bind (id name description)
	  corp-res
	(let* ((desc (unsq-read description))
	       (c (make-instance 'corpus
				 :id id :name name
				 :description desc)))
	  (dolist
	      (doc-res (latesql
			"SELECT document_id FROM corpora_documents WHERE corpus_id=~a" id))
	    (destructuring-bind (document_id)
		doc-res
	      (push document_id (documents c))))
	  (push c ans))))
    ans))

(defmethod corpora ((doc document))
  "Retrieves the corpora with which a document is associated."
  (open-late-database)
  (mapcar #'(lambda (row) (corpus (car row)))
	  (latesql "select distinct corpus_id from corpora_documents where document_id=~d"
		   (id doc))))

(defmethod corpus ((corp corpus))
  corp)

(defmethod corpus ((id integer))
  (get-corpus (format nil "id=~d" id)))

(defmethod corpus ((name string))
  (get-corpus (format nil "name=~a" (sq name))))

(defun get-corpus (where-spec)
  "Retrieves the corpus identified by a where clause (for ID or name)."
  (open-late-database)
  (let*
      ((res (latesql (concatenate 'string
				  "select id,name,description from corpora where "
				  where-spec)
		     :db *late-db*))
       (c

	(progn 	(when (null res)
		  (format t "~&no corpora available.~%")	  
		  (return-from get-corpus))
		(assert (and (consp res) (null (cdr res))) ()
			"Error in retrieving corpus ~a" where-spec)
		(make-instance 'corpus
			       :id (caar res)
			       :name (unsq (cadar res))
			       :description (unsq-read (caddar res))))))
    (dolist (doc-res (latesql "SELECT document_id FROM corpora_documents WHERE corpus_id=~a" (id c)))
      (destructuring-bind (document_id)
	  doc-res
	(push document_id (documents c))))
;;;    (with-db-rows ((document_id)
;;;		   :db *late-db* :table "corpora_documents"
;;;		   :where (= corpus_id (id c))))
    c))

(defun update-description (corpus spec-file)
  "Updates the description of corpus from the content of the XML
  specification in spec-file. Symbols are interned in the :late package."
  (assert (probe-file spec-file) ()
	  "Specification file does not exist: ~s" spec-file)
  (setf (description corpus) (car (read-xml spec-file :package :late)))
  (save corpus))

(defmethod save ((corp corpus))
  "Saves a corpus to the LATE data store."
  (late-transaction
   (cond ((null (id corp))
	  ;; This is newly created.
	  (latesql
	   "insert into corpora(name,description) values(~a,~a)"
	   (sq (name corp))
	   (sq (description corp)))
	  (setf (id corp) (mysql-insert-id *late-db*)))
	 (t (latesql
	     "update corpora set name=~a,description=~a where id=~d"
	     (sq (name corp))
	     (sq (description corp))
	     (id corp))))
   (latesql "delete from corpora_documents where corpus_id=~d"
	    (id corp))
   (when (documents corp)
     (latesql "insert into corpora_documents values~{(~d,~d)~^,~}"
	      (mapcan #'(lambda (doc-id) (list (id corp) doc-id))
		      (documents corp)))))
  corp)

(defmethod del ((c corpus))
  (delete-corpus (id c)))

;;; yluo - 07/02/2010 changed del-docs to del-cas, if t, cascadingly delete 
;;; annotations and documents
;;; yluo - 05/25/2010 added del-docs option, because flush-orphan-documents is
;;; join, and join in mysql takes longer than I can bear with.
(defmethod delete-corpus ((corpus-id integer)
			  &optional
			  (del-cas nil))
  "Deletes the given corpus (given by its ID) from
  the LATE database. The documents in the corpus are NOT deleted, but
  their association with the corpus is.  See flush-documents for
  clearning out documents that are no longer associated with any corpus."
  (late-transaction
   (when del-cas
     (dolist (doc-id (documents (corpus corpus-id)))
       (latesql "DELETE FROM annotations WHERE document_id=~d" doc-id)
       (latesql "delete from documents where id=~d" doc-id)))
   (latesql "delete from corpora where id=~d" corpus-id)
   (latesql "delete from corpora_documents where corpus_id=~d" corpus-id)))

(defmethod delete-corpus ((corp corpus)
			  &optional (del-cas nil))
  (delete-corpus (id corp) del-cas))

(defmethod delete-corpus ((corp-n string)
			  &optional (del-cas nil))
  (delete-corpus (corpus corp-n) del-cas))

(defmethod split-corpus ((c corpus) (n fixnum))
  "Creates n new corpora from an existing corpus.  Useful for
  parallelization of processing and cross-validation studies."
  (let* ((doc-ids (documents c))
	 (k (ceiling (length doc-ids) n)))
    (dotimes (i n)
      (let ((new doc-ids)
	    (temp (nthcdr doc-ids (1- k))))
	(setq doc-ids (cdr temp))
	(setf (cdr temp) nil)
	(let ((new-c
	       (make-instance 'corpus
			      :description (description c)
			      :name (format nil "~a-~d" (name c) i)
			      :documents new)))
	  (save new-c))))))

;;; DOCUMENTS
(defun docid->name (docid)
  (caar (latesql "select name from documents where id=~a" (sq docid))))

(defmemo ann-doc-id (ann-id)
  (caar (latesql "select document_id from annotations where id=~a" ann-id)))


(defmethod document ((doc-id integer))
  "Retrieves the document object whose id is doc-id from the LATE
  persistent store.  This also retrieves all its annotation."
  (get-document (format nil "id=~d" doc-id)))

(defmethod document ((doc-name string))
  "Retrieves the (first) document object whose name is doc-name from
  the LATE persistent store, including all its annotations."
  (get-document (format nil "name=~a" (sq doc-name))))

(defmethod document ((doc document))
  doc)

(defun get-document (where-spec)
  "Retrieves a document specified by where-spec and all the
  annotations stored on that document. The spec is expected to be of
  the form id=123 or name='foobar'."
  (open-late-database)
  (let ((docs (latesql "select id, name, data, source, description, size, content, updated_at, analyses from documents where ~a" where-spec)))
    (and docs
	 (destructuring-bind
	     (id name data source description size content updated_at analyses)
	     (car docs)
	   (let ((d (and docs
			 (make-instance 'document
					:id id
					:name (unsq name)
					:data (unsq data) ;prop list of other data
					:source (unsq source)
					:description (unsq-read description)
					:size size
					:dirty nil
					:dirty-annotations nil
					:updated_at updated_at
					:analyses (unsq-read analyses)
					:content (if (eq content :null) "" content))))
		 (ann-cache (make-hash-table)))
	     ;;(format t "~%Created ~s" d)
	     (dolist (ann-res (latesql "SELECT id, type, start, end, data, sw_ver, setting, other, up FROM annotations WHERE document_id=~a" id))
	       (destructuring-bind (id type start end data sw_ver setting other up)
		   ann-res
		 (let ((a (make-instance (intern type)
					 :id id
					 :document d
					 :start start
					 :end end
					 ;;perhaps should just be unsq --psz
					 :data (unsq data)
					 :sw-ver (unsq sw_ver)
					 :setting (unsq setting)
					 :h-up (unsq up) ;temporary, id for now!
					 :dirty nil
					 :new nil
					 )))
		   ;; retrieve other fields from other
		   (complete-annotation a (unsq-read other))
		   ;;(format t "~%Annotation ~s, h-up=~a" a (h-up a))
		   ;; Prepare to fix up the h-up and h-down pointers
		   (setf (gethash id ann-cache) a)
		   ;; Save annotations in the rb-tree
		   (tree-insert (annotations-tree d) a))))
	     
	     ;; (with-db-rows ((id type start end data sw_ver setting other up)
	     ;; 		    :db *late-db* :table "annotations"
	     ;; 		    :where (= document_id id)))
	     ;; Fix up h-up and h-down links among annotations
	     (maphash #'(lambda (id ann)
			  (declare (ignore id))
			  (when (h-up ann)
			    (let ((above (gethash (h-up ann) ann-cache)))
			      (assert above ()
				      "Annotation ~a has h-up=~a, but that annotation does not exist!" ann (h-up ann))
			      (push ann (h-down above))
			      (setf (h-up ann) above))))
		      ann-cache)
	     d)))))

(defun doc-ann-cnt (doc &aux ids)
  "Returns the highest cnt ever reached."
  (setf ids (or (mapcar #'id (annotations doc)) (list 0)))
  (cond 
   (ids
    (n-biject-n2-i (car (stable-sort ids '>)) (id doc)))
   (t
    0)))

(defmethod save ((doc document)
		 &aux time-start)
  "Writes back any changes to the document to the LATE data store. We
  use the time stamp of document entry in the data store to detect
  asynchronous conflicting attempts to change it.  Thus, every
  document object has both a dirty flag and a time stamp from the
  store. When it is dirty, we try to write it back to the store. If,
  however, the store's timestamp is inconsistent with the object's,
  then we have detected a conflict."
  (setf *save-doc-time* 0
	*save-ann-time* 0)
  
  ;; (late-transaction
  (setf time-start (get-internal-real-time))
  (cond ((null (id doc))
	 ;; This is a newly-created document, not yet saved to the
	 ;; datastore. We need to save it.
	 (latesql "insert into documents(name,data,source,description,size,~
                   content, analyses) values(~a,~a,~a,~a,~d,~a,~a)"
		  (sq (name doc))
		  (sq (data doc))
		  (sq (source doc))
		  (sq (description doc))
		  (sq (size doc))
		  (sq (content doc))
		  (sq (analyses doc)))
	 (setf (id doc) (mysql-insert-id *late-db*))
	 (setf (dirty doc) nil)
	 ;; if updating was done by me, then sync the update_at time.
	 (setf (updated_at doc) 
	       (caar (latesql "SELECT updated_at FROM documents WHERE id=~d" 
			      (id doc)))))
	
	((not (dirty doc))
	 ;; The document has not changed, so no need to save it.
	 )
	((and (changed-in-store? doc)
	      ;; Note: cerror returns nil if continued, hence we drop
	      ;; through to the next cond clause to do the save.
	      (cerror "Save it anyway!!!"
		      "Need to save ~s, but the version in the LATE
  store has changed since it was last retrieved."
		      doc)))
	(t 
	 ;; The document is dirty, and (fortunately) has not
	 ;; changed in the store. Thus, we can just update it.
	 ;; For now, we avoid the overhead of locking as we do
	 ;; this, though it does create the possibility of a race
	 ;; condition if another process tries to update
	 ;; simultaneously.
	 (latesql "update documents set name=~a, data=~a, source=~a,~
  description=~a, size=~a, content=~a, analyses=~a where id=~d"
		  (sq (name doc)) (sq (data doc)) (sq (source doc))
		  (sq (description doc)) (sq (size doc))
		  (sq (content doc)) (sq (analyses doc)) (id doc))
	 (setf (dirty doc) nil)
	 ;; if updating was done by me, then sync the update_at time
	 (setf (updated_at doc) 
	       (caar (latesql "SELECT updated_at FROM documents WHERE id=~d" 
			      (id doc))))))

  ;; (format t "~{~a~%~}" (annotations doc :filter #'(lambda (a) (dirty a))))
  (setf *save-doc-time* (- (get-internal-real-time) time-start))
  ;; Now we save annotations.  If any have been deleted, get rid of
  ;; them. There is an ordering problem with new annotations because
  ;; the hierarchy among them is represented by a pointer from the
  ;; lower to the higher annotation. Because in the data store this is
  ;; represented by an ID, we must assure that higher annotations are
  ;; stored first (i.e., have an ID) before lower ones. Therefore, we
  ;; first walk the annotation hierarchy, save them in top-to-bottom
  ;; order, remember which ones we have saved, and then save the rest,
  ;; which are not hierarchically organized.
  ;; (sql (format nil "LOCK TABLES annotations~@[_~a~] WRITE" *job-id*)
  ;; 	 :db *late-db*)
  (setf *bulk-insert-ann-cmd* nil)
  (when (dirty-annotations doc)
    (when (annotations-deleted doc)
      (dolist (a (annotations-deleted doc))
	;; (format t "~&deleting ~a~%" a)
	(del a))
      ;; (format t "~&# anns in DB: ~a~%" (caar (latesql "select count(*) from annotations where document_id=~a" (id doc))))
      (setf (annotations-deleted doc) nil))
    ;; (format t "~&size of anns to save: ~a~%" (length (annotations doc :type 'annotation)))
    (let* ((doc-anns (annotations doc :type 'document-annotation))
	   (doc-ann (and doc-anns (car doc-anns)))
	   (saved-anns nil))
      (labels ((iter (ann)
		     (when ann
		       (bulk-save ann)
		       (push ann saved-anns)
		       (dolist (a (h-down ann))
			 (iter a)))))
	(iter doc-ann))
      (do-tree (ann (annotations-tree doc))
	       (unless (member ann saved-anns)
		 (bulk-save ann)))
      ;; if *output-sql*, just need to dump in the end.
      (when (and *bulk-insert-ann-cmd* (not *output-sql*))
	;; (format t "~&~a~%" *bulk-insert-ann-cmd*)
	(setf *bulk-insert-ann-cmd* (replace-re *bulk-insert-ann-cmd* "~" "~~"))
	(latesql *bulk-insert-ann-cmd*)
	(setf *bulk-insert-ann-cmd* nil))
      (setf (dirty-annotations doc) nil)))
  ;; (sql "UNLOCK TABLES" :db *late-db*)
  (format t "~&save doc time: ~f secs, save anns time: ~f secs~%"
	  (/ *save-doc-time* 1000) (/ *save-ann-time* 1000)))

(defun changed-in-store? (doc)
  "True if the updated_at time in the LATE store for this document is
  not the same as the time it remembers from when it was read in. We
  consider new documents, which have no remembered time, not to have
  changed in the store."
  (and (updated_at doc)
       (let ((old (latesql 
		   "select updated_at from documents where id=~d"
		   (id doc))))
	 (not (string-equal (caar old) (updated_at doc))))))

(defmethod del ((doc document))
  (delete-document (id doc))
  ;; destroy the useful information in this document instance.
  (setf (id doc) nil)
  (setf (annotations-deleted doc) nil)
  (setf (dirty doc) nil)
  (setf (dirty-annotations doc) nil)
  (setf (annotations-tree doc) nil)
  doc)

(defun delete-document (document-id)
  "Deletes the given document (given by its ID) from
  the LATE database. The annotations associated with this document are
  also deleted, as well as its membership in all corpora."
  (assert (numberp document-id) ()
	  "Cannot delete document identified by ~s" document-id)
  (late-transaction
   (latesql "delete from documents where id=~d" document-id)
   (latesql "delete from corpora_documents where document_id=~d"
	    document-id)
   ;; This could use (del each-annotation), but this is faster.
   (latesql "delete from annotations where document_id=~d" document-id)))


(defun documents-ids (doc-or-doc-list)
  "Returns a list of document ids from a single specification or a
  list of them.  A document may be specified by
  an integer, its id
  a string, its name, or
  the document itself"
  ;; The computation here is trivial unless a name is given, in which
  ;; case we need to consult the database to find the corresponding id.
  (let* ((docs (if (listp doc-or-doc-list) doc-or-doc-list
		 (list doc-or-doc-list)))
	 (names (delete-if-not #'stringp docs))
	 (name-map (latesql
		    "select name,id from documents where name~a"
		    (sql-matcher names))))
    (mapcar #'(lambda (d &aux entry)
		(cond ((integerp d) d)
		      ((typep d 'document) (id d))
		      ((and (stringp d)
			    (setq entry (assoc d name-map)))
		       (cadr entry))
		      (t (error "No document ~s is known" d))))
	    docs)))

(defun corp-attach-doc (doc-s corp)
  ;; Attaches a document or list of documents to a corpus.
  ;; The documents may be given by id, name or the document itself
  ;; The corpus may be given by id, name or the corpus itself
  (let* ((docids (documents-ids doc-s))
	 (corpid (id (corpus corp))))
    (latesql "insert into corpora_documents(corpus_id,document_id) values ~{(~a,~a)~^,~}"
	     (mapcan #'(lambda (d) (list corpid d)) all))))

(defun corp-detach-doc (doc-s corp)
  ;; Detaches a document or list of documents from a corpus
  (let* ((docids (documents-ids doc-s))
	 (corpid (id (corpus corp))))
    (latesql
     "delete from corpora_documents where corpus_id=~a and document_id~a"
     corpid (sql-matcher docids))))





(defun save-phrase-longest-umls (phid umlsid content type)
  (latesql "INSERT INTO phrase_longest_umls (phid, umlsid, content, type) 
            VALUES (~d, ~d, ~s, ~s)" phid umlsid content type))

(defmemo umls-freq (content type)
  (car (car (latesql "SELECT COUNT(*) FROM phrase_longest_umls 
                      WHERE content~a AND type~a"
		     (sql-matcher content) (sql-matcher type)))))

(defmemo syns-freq (synsense pos)
  (car (car (latesql "SELECT COUNT(*) FROM wordnet_synset_stat 
                      WHERE synset~a AND pos~a"
		     (sql-matcher synsense) (sql-matcher pos)))))

(defun save-wn-synset (synset invoked-by freq pos)
  (latesql "INSERT INTO wordnet_synset_stat (synset, invoked_by, freq, pos)
            VALUES (~a, ~a, ~a, ~a)"
	   (sq synset)
	   (sq invoked-by)
	   (sq freq)
	   (sq pos)))

(defun update-wn-synset (synset invoked-by freq pos)
  (latesql "UPDATE wordnet_synset_stat SET freq=~a WHERE
            synset~a AND invoked_by~a AND pos~a"
	   (sq freq)
	   (sql-matcher synset)
	   (sql-matcher invoked-by)
	   (sql-matcher pos)))

(defun wn-synset-freq (synset &optional (invoked-by nil) (pos nil))
  (let* (freq)
    (cond 
     ((and invoked-by pos)
      (setf freq (caar (latesql "SELECT freq FROM wordnet_synset_stat WHERE
                                synset~a AND invoked_by~a AND pos~a"
				(sql-matcher synset) 
				(sql-matcher invoked-by)
				(sql-matcher pos)))))
     (t
      (setf freq (caar (latesql "SELECT freq FROM wordnet_synset_stat WHERE
                                synset~a"
				(sql-matcher synset))))))
    freq))




;;; ANNOTATIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Annotations of different types are stored in the same table. Some
;;;subclasses of annotations contain additional slots, which are
;;;serialized into the 'other' field as a Lisp s-expression
;;;representing a property list (retrieved by read from the string).
;;;
;;; We adopt a general protocol for retrieving and saving
;;;annotations. For retrieval, we first construct a class instance of
;;;the appropriate type, and then invoke the generic function
;;;complete-annotation on it.  This can read additional tables or sort
;;;the contents of data into the fields where it belongs.  For saving,
;;;we call save, which simply stores all common components into their
;;;appropriate database fields.  This can be overridden or extended to
;;;store additional data in a serialized data field or in other
;;;tables. We use the generic encode to package up such other data
;;;into the data field (as serialized text).

;;; In an initial implementation, we required defining encode and
;;;decode functions for each type of annotation to assemble the data
;;;going into the other field and to distribute it.  Now we use the
;;;Meta-Object introspective capabilities of CLOS to do this
;;;generically for any subtype of annotation.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-annotation (id doc)
  (car (annotations doc :filter #'(lambda (a) (and (id a) (= (id a) id))))))

(defmethod get-annotation ((id integer) (doc document))
  "Base method to retrieve an annotation from the late database, by
  its id.  We require the root document as well, but do not link the
  annotation into the annotations-tree, assuming that the caller will
  do so.  We assume that the h-up pointers will be fixed when the
  entire set of annotations for a document are retrieved. We also
  expect the generic function complete-annotation to allocate the
  serialized content of the other field to possibly other slots."
  (destructuring-bind (id type doc_id start end data sw_ver setting other up)
      ;; We make sure we get one and only one right annotation.
      (let ((anns (latesql
		   "select * from annotations where id='~d'" id)))
	(assert anns ()
		"Get-annotation failed to find annotation with id=~a"
		id)
	(assert (null (cdr anns)) ()
		"Get-annotation found ~d annotations with id=~a; there ~
             should only be one."
		(length anns))
	(car anns))
    (assert (= doc_id (id doc)) ()
	    "Get-annotation finds a doc_id (~a) that does not match the id of its document (~a)"
	    doc_id (id doc))
    (let ((ann (make-instance (intern type)
			      :id id
			      :document doc
			      :start start
			      :end end
			      :h-up (unsq up)
			      :data (unsq data)
			      :sw-ver sw_ver
			      :setting setting
			      :dirty nil
			      :new nil
			      )))
      ;; fill in other fields, if any
      (complete-annotation ann (unsq-read other))
      ann)))

(defmethod get-annotation ((id integer) (dummy-doc (eql nil)))
  "Retrieves an annotation AND the document to which it belongs from
  the LATE database.  This is only for debugging convenience, because
  it forces a database read of the entire document."
  (let* ((docids (latesql
		  "select document_id from annotations where id='~d'"
		  id))
	 (docid (and docids (null (cdr docids)) (caar docids)))
	 (doc (and docid (document docid))))
    (find id (annotations doc) :key #'id :test #'equal)))


(defmethod complete-annotation ((ann annotation) other)
  "This is responsible for distributing information about the
  annotation that had been serialized in the 'other' field into the
  relevant fields of the annotation."
  ;; There is a bug/feature in ACL such that CLOS does not finalize
  ;; class definitions until they are actually used. Therefore,
  ;; class-slots will fail in a newly loaded LATE the first time we
  ;; access the slots of a first-used class.  The following will take
  ;; care of the problem:
  (let ((annotation-class (find-class 'annotation)))
    #+allegro
    (unless (clos:class-finalized-p annotation-class)
      (clos:finalize-inheritance annotation-class))
    (unless (clos:class-finalized-p annotation-class)
      (format t "~a not finalized~%" annotation-class))
    (let ((annotation-slotnames
	   (mapcar #'clos:slot-definition-name
		   (clos:class-slots annotation-class)))
	  (slotnames (mapcar #'clos:slot-definition-name
			     (clos:class-slots (class-of ann)))))
      (map nil #'(lambda (slotname)
		   (unless (member slotname annotation-slotnames)
		     (setf (slot-value ann slotname)
			   ;; Note: A slot not mentioned in other is assumed
			   ;; to be nil.
			   (getf other slotname nil))))
	   slotnames)))
  ann)

(defmethod encode ((ann annotation))
  "Encode all but the slots of annotation into a property list that is
  to be stored in the data table's other field. Slots with null values
  are not explicitly stored."
  ;; Both encode and complete-annotation treat all slots as if they
  ;; were instance slots. It might be better to omit the values of
  ;; class slots from the serialization of instances; however, then
  ;; they would not get serialized at all.
  ;; The function clos:slot-definition-allocation returns :class for
  ;; those, and :instance for instances slots.
  ;; See comment in complete-annotation about finalize-inheritance.
  (let ((annotation-class (find-class 'annotation)))
    #+allegro
    (unless (clos:class-finalized-p annotation-class)
      (clos:finalize-inheritance annotation-class))
    (let ((annotation-slotnames
	   (mapcar #'clos:slot-definition-name
		   (clos:class-slots annotation-class)))
	  (slotnames (mapcar #'clos:slot-definition-name
			     (clos:class-slots (class-of ann)))))
      (mapcan #'(lambda (slotname)
		  (if (member slotname annotation-slotnames)
		      nil
		    (let ((slotval (slot-value ann slotname)))
		      (and slotval (list slotname slotval)))))
	      slotnames))))


(defun bulk-save (ann
		  &aux time-start time-elapsed)
  (setf time-start (get-internal-real-time))
  ;; (format t "~&*bulk-insert-ann-cmd*:~%~a~%" *bulk-insert-ann-cmd*)
  (when (dirty ann)
    (let ((*print-pretty* nil)
	  (docid (id (document ann)))
	  ;; omit extra newlines and spaces in the serialization of Lisp
	  ;; data that would be there simply to make it read nicely.
	  (*package* (find-package :late))
	  ;; use the late package when printing lisp content; this is
	  ;; especially important for the OTHER field, which has the
	  ;; form of a LISP property list; the indicators there are in
	  ;; the late: package, and would print as late::foo without
	  ;; this. Note that unsq-read, which inverts this, also binds
	  ;; *package*, to make things consistent.
	  )
      
      (cond ((new ann)
	     ;; This is a new annotation, not yet stored in the LATE
	     ;; data store.
	     (setf (new ann) nil)
	     (cond 
	      (*output-sql*
	       (format *data-sql* "~d|~a|~d|~d|~d|~a|~a|~a|~a|~a||~%"
		       (id ann)
		       (sq (type-of ann))
		       docid
		       (start ann)
		       (end ann)
		       (sq (data ann))
		       (sq (sw-ver ann))
		       (sq (setting ann))
		       (sq (encode ann))
		       (sq (and (h-up ann) (id (h-up ann))))))
	      ((null *bulk-insert-ann-cmd*)
	       (setf *bulk-insert-ann-cmd* (outstr-init))
	       (format *bulk-insert-ann-cmd* "insert into annotations~@[_~a~](id, type,document_id,start,end,data,sw_ver,setting,other,up) values (~d,~a,~d,~d,~d,~a,~a,~a,~a,~a)" 
		       *job-id*
		       (id ann)
		       (sq (type-of ann))
		       docid
		       (start ann)
		       (end ann)
		       ;; Annotations in general carry only a single 'data'
		       ;; field, which is sql indexed to make retrieval
		       ;; efficient. In addition, subclasses of annotations may
		       ;; have other fields as well, which are not stored
		       ;; directly into the database but are instead serialized
		       ;; to the 'other' field of the database as a text
		       ;; representation of a Lisp property list.
		       (sq (data ann))
		       (sq (sw-ver ann))
		       (sq (setting ann))
		       (sq (encode ann))
		       ;; This assumes that any h-up annotation already
		       ;; has an ID before ann is saved. This should be
		       ;; assured by the order in which the
		       ;; annotations-tree is stored, determined by
		       ;; annotation-lessp. Longer annotations that start
		       ;; before or at ann are saved earlier.
		       (sq (and (h-up ann) (id (h-up ann)))))
	       ;;(format t "~&*bulk-insert-ann-cmd*2:~%~a~%" *bulk-insert-ann-cmd*)
	       )
	      (t
	       (format *bulk-insert-ann-cmd* ",~% (~d,~a,~d,~d,~d,~a,~a,~a,~a,~a)"
		       (id ann)
		       (sq (type-of ann))
		       docid
		       (start ann)
		       (end ann)
		       (sq (data ann))
		       (sq (sw-ver ann))
		       (sq (setting ann))
		       (sq (encode ann))
		       (sq (and (h-up ann) (id (h-up ann)))))
	       (when (> (length *bulk-insert-ann-cmd*) *max-packet*)
		 ;; (format t "~&~a~%" *bulk-insert-ann-cmd*)
		 (setf *bulk-insert-ann-cmd* (replace-re *bulk-insert-ann-cmd* 
							 "~" "~~"))
		 (latesql *bulk-insert-ann-cmd*)
		 (setf *bulk-insert-ann-cmd* nil)))))
	    (t ;; We assume that the document cannot change.
	     (latesql
	      "update annotations~@[_~a~] set start=~d,end=~d,data=~a,other=~a,up=~a where id=~d"
	      *job-id* (start ann) (end ann) (sq (data ann)) (sq (encode ann))
	      (sq (and (h-up ann) (id (h-up ann)))) (id ann)))))
    (setf (dirty ann) nil)
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *save-ann-time* time-elapsed)))

(defmethod save ((ann annotation)
		 &aux time-start time-elapsed)
  (setf time-start (get-internal-real-time))
  (when (dirty ann)
    (let* ((*print-pretty* nil)
	   (docid (id (document ann)))
	   ;; omit extra newlines and spaces in the serialization of Lisp
	   ;; data that would be there simply to make it read nicely.
	   (*package* (find-package :late))
	   ;; use the late package when printing lisp content; this is
	   ;; especially important for the OTHER field, which has the
	   ;; form of a LISP property list; the indicators there are in
	   ;; the late: package, and would print as late::foo without
	   ;; this. Note that unsq-read, which inverts this, also binds
	   ;; *package*, to make things consistent.
	   insert-cmd)
      (cond 
       (*ann-delayed*
	(setf insert-cmd "insert delayed into annotations~@[_~a~](id,type,document_id,start,end,data,sw_ver,setting,other,up) values(~d,~a,~d,~d,~d,~a,~a,~a,~a,~a)"))
       (t
	(setf insert-cmd "insert into annotations~@[_~a~](id,type,document_id,start,end,data,sw_ver,setting,other,up) values(~d,~a,~d,~d,~d,~a,~a,~a,~a,~a)")))
      (cond ((new ann)
	     ;; This is a new annotation, not yet stored in the LATE
	     ;; data store.
	     (setf (new ann) nil)
	     (sql
	      (format 
	       nil
	       insert-cmd
	       *job-id*
	       (id ann)
	       (sq (type-of ann))
	       docid
	       (start ann)
	       (end ann)
	       ;; Annotations in general carry only a single 'data'
	       ;; field, which is sql indexed to make retrieval
	       ;; efficient. In addition, subclasses of annotations may
	       ;; have other fields as well, which are not stored
	       ;; directly into the database but are instead serialized
	       ;; to the 'other' field of the database as a text
	       ;; representation of a Lisp property list.
	       (sq (data ann))
	       (sq (sw-ver ann))
	       (sq (setting ann))
	       (sq (encode ann))
	       ;; This assumes that any h-up annotation already
	       ;; has an ID before ann is saved. This should be
	       ;; assured by the order in which the
	       ;; annotations-tree is stored, determined by
	       ;; annotation-lessp. Longer annotations that start
	       ;; before or at ann are saved earlier.
	       (sq (and (h-up ann) (id (h-up ann)))))
	      :db *late-db*))
	    (t ;; We assume that the document cannot change.
	     (latesql
	      "update annotations~@[_~a~] set start=~d,end=~d,data=~a,other=~a,up=~a where id=~d"
	      *job-id* (start ann) (end ann) (sq (data ann)) (sq (encode ann))
	      (sq (and (h-up ann) (id (h-up ann))))
	      (id ann)))))
    (setf (dirty ann) nil)
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *save-ann-time* time-elapsed)))

;;; Because there is NOT a tight coupling between the Mysql database
;;; used to implement the LATE data store and the CLOS structures in
;;; memory, there is no attempt to make sure that structures in memory
;;; update automatically when they are changed in the database.  Thus,
;;; if I hold two document structures representing the same document,
;;; they can easily become inconsistent with each other.  We could
;;; solve this by maintaining a hash in memory of each persistent
;;; object and always referencing it indirectly.  However, we expect
;;; this situation not to be important or frequent except during
;;; debugging.
;;; I start to feel that during incremental experiments, the need for
;;; delete some annotations, and perform subsequent analysis rises more 
;;; often, hence the need to make memory copy and persistent copy consistent
;;; rises. TODO - yluo

(defmethod del ((ann annotation))
  "Delete this annotation from the persistent store."
  (delete-annotations ann))

(defmethod delete-annotations ((list-of-annotations list))
  "Deletes a given list of annotations. We assume that they are all
  annotations on the same document. Annotations with an ID of NIL are
  ignored."
  (when list-of-annotations
    (latesql "delete from annotations where id~a"
	     (sql-matcher
	      (mapcan #'(lambda (a)
			  (and (id a)
			       (list (id a))))
		      list-of-annotations)))))

(defmethod delete-annotations ((ann annotation))
  (latesql "DELETE FROM annotations WHERE id=~d" (id ann)))

(defmethod flush-deleted-annotations ((doc document))
  (delete-annotations (annotations-deleted doc))
  (setf (annotations-deleted doc) nil))

(defun find-orphan-documents ()
  "Finds all documents that are not part of any corpus."
  (open-late-database)
  (mapcar #'car
	  (latesql "select d.id from documents d left outer join corpora_documents cd on d.id=cd.document_id where cd.corpus_id is null")))

(defun flush-orphan-documents ()
  "Deletes all documents that are not part of a corpus. Thus, to
  delete all documents that belong only to a single corpus, first del
  that corpus, then call this function."
  (open-late-database)
  (latesql "delete d from documents d left outer join corpora_documents cd on d.id=cd.document_id where cd.corpus_id is null"))

(defun flush-orphan-annotations ()
  "Deletes all annotations that do not belong to a document."
  (open-late-database)
  (latesql "delete a from annotations a left outer join documents d on a.document_id=d.id where d.id is null"
	   :db *late-db*))

(defun flush-volatile-annotations (&optional (confirm nil))
  "Deletes all annotations that are subclasses of volatile-annotation,
  i.e., all annotations that have been computed by LATE
  components. This preserves persistent annotations on the database,
  but requires recomputation of all others."
  (assert confirm (confirm)
	  "If you really want to delete all the volatile annotations in
  LATE, you must provide a non-nil optional argument to
  flush-volatile-annotations.")
  (open-late-database)
  (let* ((volatile-types
	  (labels
	      ((iter (type)
		     (cons type
			   (mapcan #'iter
				   (mop:class-direct-subclasses type)))))
	    (iter (find-class 'volatile-annotation))))
	 (names (mapcar #'class-name volatile-types)))
    (latesql "delete from annotations where type~a"
	     (sql-matcher names))))

(defun fetch-corpora (&aux (ans nil))
  "Fetches all corpora and assigns them to symbols corresponding to their names."
  (dolist (c (corpora nil))
    (let ((n (intern (name c) :user)))
      (push n ans)
      (set n c)))
  ans)

(defun record-type-hierarchy ()
  "Writes the current type hierarchy under late-object to the types
  table in the LATE database. This is write-only, for the benefit of
  external users; LATE itself does not depend on this table. We also
  assume that the type hierarchy is strict; i.e., no multiple
  up-links, even though in CLOS those are allowed. We delete all
  existing content and re-write current data, in a transaction."
  (late-transaction
   (latesql "delete from types")
   (labels ((inner (type super-name)
		   (let ((type-name (class-name type)))
		     (latesql "insert into types values(~a,~a)"
			      (sq type-name) (sq super-name))
		     (dolist (subtype (mop:class-direct-subclasses type))
		       (inner subtype type-name)))))
     (inner (find-class 'late-object) nil))))

(defun find-dup-annotations ()
  "Creates the dupann database that holds probable duplicate annotations. Warning: This
   operation is phenomenally slow, taking days to complete on a large annotations table.
   This needs revisit, yluo"
  (open-late-database)
  (latesql "drop table if exists dupann")
  (latesql "create table dupann select ~
a1.id id1, a2.id id2, a1.document_id doc_id, a1.start s, a1.end s, a1.type t,a1.data d1, a2.data d2, a1.other o1, a2.other o2 ~
from annotations a1 join annotations a2 on a1.id < a2.id and a1.document_id=a2.document_id and a1.start=a2.start and a1.end=a2.end and a1.type=a2.type")
  (latesql "select count(*) from dupann"))

(defun clear-link-supp-dict-table ()
  (late-transaction
   (latesql "DELETE FROM link_supp_dict")))

(defun persist-link-supp-dict-hash (h)
  (late-transaction
   (maphash #'(lambda (fn wl)
		(dolist (w wl)
		  ;; you need the BINARY keyword here because the link grammar
		  ;; dictionary is case sensitive, and the default mysql
		  ;; string comparison is case insensitive.
		  (unless (latesql "SELECT * FROM link_supp_dict WHERE
                                      file=~a AND BINARY entry=~a"
				   (sq fn) (sq w))
		    (latesql "INSERT INTO link_supp_dict(file, entry) VALUES
                                (~a, ~a)" 
			     (sq fn) (sq w)))))
	    h)
   (clrhash h)))

(defun get-graphs (gtype)
  (mapcar #'car (latesql "SELECT gid FROM graph WHERE gtype=~a" (sq gtype))))

(defun get-mapped-linkage-graphs (sigsub-id)
  (mapcar #'car (latesql "SELECT DISTINCT lg_id FROM lg_sigsub WHERE sub_id=~a" (sq sigsub-id))))

(defun get-graph-name (gid)
  (caar (latesql "SELECT gname FROM graph WHERE gid=~a" (sq gid))))

(defun get-sigsub-support (sigsub-id)
  (caar (latesql "SELECT significance FROM sig_subgraph WHERE sub_id=~a"
		 (sq sigsub-id))))


