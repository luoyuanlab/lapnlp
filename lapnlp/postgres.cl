;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/12/2015 rewrite using asdf framework
psz  -            creation

psz - Rewrite of persistence.cl to use Postgres instead of MySQL.
This is an attempt to investigate whether PG's acid-compliance and
speed will serve us better than MySQL.

The Lisp interface is based on Postmodern, described at
http://marijnhaverbeke.nl/postmodern 
I am using Quicklisp to load Postmodern.  It is tempting to use the
Postmodern DAO's (the Data Access Objects metaclass), but to retain
parallelism with the MySQL implementation, I will not do that here.

We start with code to define the LATE data tables.  These are (very)
slightly refactored from their MySQL version, mainly to support
indexed properties on documents and annotations 
rather than simply storing Lisp property lists in text fields of those
elements. This should make those properties accessible to other
programs, though it may slow down retrieval by requiring additional
SQL calls.

The first functions are used to transfer data from an existing MySQL
implementation to Postgres. These should then no longer be useful for
future programming. 

|#


#|

This file provides a Postgres persistent storage mechanism for LATE, as
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
  (require :mysql)			;needed for transfer of data
  (ql:quickload "postmodern"))

(defpackage :late
  (:use :common-lisp :util :dbi.mysql)
  (:export "open-mysql-db"
	  "*late-db*"
	  "open-late-database"
	  "open-gazette-database"
	  "sq"
	  "sqs"
	  "unsq"
	  "unsq-read"
	  "sql-matcher"
	  "latesql"
	  "late-transaction"
	  "*mysql-create-commands*"
	  "*mysql-drop-commands*"
	  "create-late-database"
	  "close-late-database"
	  "close-gazette-database"
	  "compact-late-database"
	  "corpora"
	  "corpus"
	  "get-corpus"
	  "split-corpus"
	  "update-description"
	  "save"
	  "del"
	  "delete-corpus"
	  "document"
	  "get-document"
	  "changed-in-store?"
	  "delete-document"
	  "get-annotation"
	  "complete-annotation"
	  "encode"
	  "delete-annotations"
	  "flush-deleted-annotations"
	  "flush-orphan-documents"
	  "flush-orphan-annotations"
	  "flush-volatile-annotations"
	  "fetch-corpora"	
	  "corp-attach-doc"
	  "corp-attach-docs"
	  "corp-detach-doc"
	  "corp-detach-docs"
	  "save-phrase-longest-umls"
	  "umls-freq"
	  "syns-freq"
	  "save-wn-synset"
	  "update-wn-synset"
	  "wn-synset-freq"
	  "save-linkage-graph"
	  "save-lg-node"
	  "save-lg-edge"
	  "get-linkage-graph"
	  "get-sigsubs-of-lg"
	  "get-sigsub-ids"
	  "sigsub-size"
	  "lgnid->pnid"
	  "get-lg-node-type"
	  "get-lg-adj-nodes"
	  "persist-link-supp-dict-hash"
	  "find-pg-database"
	  "with-late-connection"
	  "latepg"
	  ))

(in-package :late)

(defun open-mysql-db (config-name &aux (db nil))
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
					    3306))))
	(format t "~%;;; ~a connected to ~s on ~a"
		config-name (elt s 1) (or (elt s 0) "localhost"))
	(return))))
  (assert db (db) "Unable to open database ~s!" config-name)
  db)

(defparameter *late-db* nil
  "The connection to the Mysql database holding LATE's persistent
  store.")

(defun open-late-database ()
  "Opens connection to the Mysql database used by LATE."
;;  (format t "~&;;; Openning LATE database~%")
  (when (and *late-db* (not (mysql-connected *late-db*)))
    (setq *late-db* nil))
  (unless *late-db*
    (setq *late-db* (open-mysql-db "LATE_DB"))))

(defparameter *gazette-db* nil)

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
	 (setq *late-db* nil))
	(t (format t "~%Gazette database was already disconnected.")))
  *gazette-db*)

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
  (concatenate 'string
    "'"
    (replace-re (replace-re (replace-re string "\\\\" "\\\\\\\\") "'" "\\'")
		"~" "~~")
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

(defun sqpg (item)
  (cond ((null item) :null)
	((symbolp item) (symbol-name item))
	((stringp item) item)
	(t (format nil "~a" item))))

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
	 (catch 'trap-mysql-protocol-err
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
				 (throw 'trap-mysql-protocol-err :retry)))))
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


(defparameter *pg-create-commands*
    '(
      "CREATE TABLE documents (
  id BIGSERIAL PRIMARY KEY,
  name VARCHAR(255),
  source VARCHAR(255),
  description TEXT,
  updated_at TIMESTAMP default now(),
  content TEXT)"
      
      "CREATE INDEX x_doc_name on documents(name)" ; not UNIQUE
      
      "CREATE TABLE document_attrs (
  document_id BIGINT references documents(id) ON DELETE CASCADE,
  attr varchar(255) not null,
  val varchar(255) not null)"
      
      "CREATE INDEX x_attrs ON document_attrs(document_id,attr)"
      
      "CREATE TABLE corpora (
  id SERIAL PRIMARY KEY,
  name VARCHAR(255) NOT NULL UNIQUE,
  updated_at TIMESTAMP default now(),
  description TEXT)"
      
      "CREATE TABLE corpora_documents(
  corpus_id INTEGER NOT NULL REFERENCES corpora(id) ON DELETE CASCADE,
  document_id BIGINT NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
  PRIMARY KEY (corpus_id, document_id))"
      
      "CREATE INDEX x_cd_doc ON corpora_documents(document_id)"
      
      "CREATE TABLE annotations (
  id BIGSERIAL PRIMARY KEY,
  type VARCHAR(255) NOT NULL,
  document_id BIGINT NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
  starting INTEGER NOT NULL,
  ending INTEGER NOT NULL,
  data VARCHAR(255),
  up BIGINT REFERENCES annotations(id))"

      "CREATE INDEX annotations_up ON annotations(up)"
      
      "CREATE INDEX annotations_owner on annotations(document_id,type)"
      
      "CREATE INDEX annotations_data on annotations(data)"
      
      "CREATE INDEX annotations_doc on annotations(document_id)"

      "CREATE TABLE annotation_attrs (
  annotation_id BIGINT references annotations(id) ON DELETE CASCADE,
  attr varchar(255) not null,
  val TEXT)"
  
      "CREATE INDEX a_attrs ON annotation_attrs(annotation_id,attr)"

      "CREATE TABLE types (
      type VARCHAR(255) NOT NULL PRIMARY KEY,
      super VARCHAR(255))"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Managing connection to PostgreSQL database
;;;
;;; Unlike for Mysql, where we keep a connection open,
;;; we opt to keep a pool of connections and open one each
;;; time it's needed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *psql-specs* (make-hash-table :test #'equalp)
  "Property list of bindings of Postgres databases and the specification
lists used to open them.")

(defun find-pg-database (config-name &optional (close? t))
  "Determines and memoizes in *psql-specs* the specification list
for pomo:connect or pomo:with-connection for the database specified
by config-name. To do so, it tries to open the list of specs retrieved
by get-envs until one succeeds in connecting. If close? is true (default),
that connection is disconnected."
  (or (gethash config-name *psql-specs* nil)
      (dolist (spec (get-envs config-name))
	(let* ((speclist
		(append (if (stringp spec) (split-re "\\|" spec) spec)
			'(:pooled-p t)))
	       (conn (ignore-errors (apply #'pomo:connect speclist))))
	  (format t "~%Connection ~s" conn)
	  (cond (conn
		 (setf (gethash config-name *psql-specs*) speclist)
		 (when close?
		   (pomo:disconnect conn)
		   (setq pomo:*database* nil))
		 (return speclist)))))))

(defmacro with-late-connection ((config-name) &body body)
  `(pomo:with-connection
       (find-pg-database ,config-name)
     ,@body))

(defmacro latepg (&body body)
  `(with-late-connection ("LATE_PG")
     ,@body))
       
(defun latesql (template &rest args)
  "Constructs and runs a mysql command. The error handler is for the
  case where after inactivity, mysql disconnects but Lisp only finds
  out when an error occurs trying to use the connection. In that case,
  we re-establish it."
  (open-late-database)
  (let ((result
	 (catch 'trap-mysql-protocol-err
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
				 (throw 'trap-mysql-protocol-err :retry)))))
	     (sql (apply #'format nil template args)
		  :db *late-db*)))))
    (if (eq result :retry)
	(apply #'latesql template args)
      result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code to copy an existing Mysql database to Postgres.
;;;
;;; This includes reorganization to store as individual rows
;;; in document_attrs and annotation_attrs the data that
;;; had been stored as textually serialized versions of 
;;; Lisp property lists in the 'data' field of documents and
;;; the 'other' field of annotations.
;;;
;;; For a database of about 30K documents and 237M annotations,
;;; this copy process takes about 3 days of real time.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pomo:defprepared copytopgdoc
    "insert into documents(id,name,source,description,content)
    values($1,$2,$3,$4,$5)")
(pomo:defprepared copytopgdocattr
    "insert into document_attrs values($1,$2,$3)")
(pomo:defprepared copytopgcorpdoc
    "insert into corpora_documents values($1, $2)")
(pomo:defprepared copytopgcorp
    "insert into corpora values($1, $2, $3)")
(pomo:defprepared copytopgann
    "insert into annotations values($1, $2, $3, $4, $5, $6, $7)")
(pomo:defprepared copytopgannattr
    "insert into annotation_attrs values($1, $2, $3)")

#|
;;; Note: When previously run, this inserted the string value "NULL"
;;; in places where it should have inserted NULL instead. This should
;;; get fixed if we ever run it again.
;
(defun transfer-mysql-to-pgsql
    (&optional (skip-create nil) &aux (count 0))
  (latepg
    (unless skip-create
      (dolist (cmd *pg-create-commands*)
	(pomo:execute cmd)))
    (late-transaction
     (pomo:with-transaction ()
       (dolist (d (latesql "select * from documents"))
       	 (copytopgdoc
       	  (elt d 0)			;id
       	  (sqpg (unsq (elt d 1)))	;name
       	  (sqpg (unsq (elt d 3)))	;source
       	  (sqpg (unsq (elt d 4)))	;description
       	  (sqpg (unsq (elt d 7)))	;content
       	  )
       	 (do ((l (unsq-read (elt d 2)) (cddr l)))
       	     ((null l))
       	   (copytopgdocattr
       	    (elt d 0) (sqpg (car l)) (sqpg (cadr l)))))
       (dolist (c (latesql "select * from corpora"))
       	 (copytopgcorp (car c) (cadr c) (caddr c)))
       (dolist (r (latesql "select * from corpora_documents"))
       	 (copytopgcorpdoc (car r) (cadr r)))
       (dolist (a (latesql "select * from annotations"))
	 (when (zerop (mod (incf count) 1000))
	   (princ "."))
	 (copytopgann (elt a 0) 	;id
		      (elt a 1)		;type
		      (elt a 2)		;document_id
		      (elt a 3)		;starting
		      (elt a 4)		;ending
		      (sqpg (unsq (elt a 5))) ;data
		      (sqpg (unsq (elt a 7))) ;up
		      )
	 (do ((l (unsq-read (elt a 6)) (cddr l)))
	     ((null l))
	   (copytopgannattr (elt a 0) (sqpg (car l)) (sqpg (cadr l)))))

       (let ((maxdocid (pomo:query "select max(id) from documents"
				   :single))
	     (maxcorpid (pomo:query "select max(id) from documents"
				    :single))) 
	 (pomo:execute
	  (format nil
		  "alter sequence documents_id_seq restart with ~d"
		  (1+ maxdocid)))
	 (pomo:execute
	  (format nil
		  "alter sequence corpora_id_seq restart with ~d"
		  (1+ maxcorpid))))
       (let ((n-cols (dbi.mysql:sql-start
		      "select * from annotations"
		      :db *late-db*))
	     (a nil))
	 (if (null n-cols)
	     (error "No annotations to copy from LATE database.")
	   (while (setq a (get-next-row :db *late-db*))
	     (when (zerop (mod (incf count) 10000))
	       (princ "."))
	     (copytopgann (elt a 0)	;id
			  (elt a 1)	;type
			  (elt a 2)	;document_id
			  (elt a 3)	;starting
			  (elt a 4)	;ending
			  (sqpg (unsq (elt a 5))) ;data
			  (sqpg (unsq (elt a 7))) ;up
			  )
	     (do ((l (unsq-read (elt a 6)) (cddr l)))
		 ((null l))
	       (copytopgannattr (elt a 0) (sqpg (car l)) (sqpg (cadr l)))))))))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Overall Design
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

The database tables represent persistent reality. In-memory data
structures are what programs manipulate. In the MySQL implementation
of persistence, users encountered problems with the fact that it was
possible (easy, in fact), to create multiple in-memory structures that
represented the same document or corpus.  Therefore, here we try to
create an "interning" cache that holds a single copy of one of these
structures. Nevertheless, it is the responsibility of programs to save
these structures back to the persistent store.

This approach does highlight a set of synchronization problems,
however. First, if we run multi-threaded, then access to the cache
must be serialized to avoid conflicts.  Second, because programs
outside LATE can simultaneously access the persistent database, that
database might change between the time an element is cached and the
time that such an updated element is written back.

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Corpora
;;;
;;; A corpus is simply a collection of documents. Any document may
;;; be part of multiple corpora.  Hence, the corpora_documents table
;;; holds the n-n mapping.
;;;
;;; A corpus has an ID, and NAME, and a DESCRIPTION, which is
;;; conventionally used to hold an XML-encoded grammar for how to parse
;;; the overall structure of documents in the corpus.
;;;
;;; A Corpus is represented by a CLOS data structure that holds the above 
;;; information plus a list of the document id's that make up the corpus.
;;; (Note: not the documents themselves.)
;;; 
;;; (corpora nil) retrieves a list of all known corpora
;;; (corpora <doc>) retrieves the list of corpora in which a document appears
;;; (corpora <doc-id>) retrieves the list of corpora in which a
;;; document with that id appears
;;; (corpus ...) retrieves a corpus by its id or name.
;;; (del <corpus>) deletes a corpus
;;; (delete-corpus <corpus-id>) deletes a corpus with that id
;;; (flush-orphan-documents) deletes any documents that are associated
;;; with no corpora.
;;; (save <corpus>) writes back any changes to the corpus to the database
;;; (update-description <corpus> <filename>) updates the 'description'
;;; field of corpus to contain the (XML) content of the given file.
;;;
;;; Changes from previous version that we should make:
;;; 1. 'description' used to hold the Lisp parsed pxml representation
;;; rather than the original XML. For interoperability with non-Lisp
;;; processors, it should just hold the XML and we should parse it when
;;; we read the corpus data structure
;;; 2. We need to figure out better whether/how deleting a corpus or
;;; document cascades to the corpora_documents table.

(defparameter *corpora-cache*
    (make-guarded-hash-table :test #'eql :values :weak)
  "Holds cache of corpora CLOS objects that have been loaded. In reality,
these are small objects that simply hold a list of their document_ids, not
the documents themselves. The cache has weak values, so they are gc'd if not
pointed to by anything else.")

(pomo:defprepared get-corp-docs-by-id
    "select document_id from corpora_documents where corpus_id=$1"
  :column)

(defmethod corpora ((dummy-nil (eql nil)))
  "Retrieves a list of all the corpora in the LATE database."
  (latepg
    (let ((ans nil))
      (dolist (cspec (pomo:query "select id,name,description from corpora"))
	(let ((existing
	       (get-hash (elt cspec 0) *corpora-cache*)))
	  (when (not existing)
	    (setq existing
	      (make-instance 'corpus
		:id (elt cspec 0)
		:name (elt cspec 1)
		:description (elt cspec 2)
		:documents (get-corp-docs-by-id (elt cspec 0))))
	    (setf (get-hash (id existing) *corpora-cache*) existing))
	  (push existing ans)))
      (nreverse ans))))

(pomo:defprepared get-corp-ids-from-doc
    "select distinct corpus_id from corpora_documents where document_id=$1"
  :column)

(defmethod corpora ((doc document))
  "Retrieves the corpora with which a document is associated."
  (mapcar #'(lambda (row) (corpus (car row)))
	  (latepg
	    (get-corp-ids-from-doc (id doc)))))

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
		  :description (unsq-read (caddar res))))
	))
    (with-db-rows ((document_id)
		   :db *late-db* :table "corpora_documents"
		   :where (= corpus_id (id c)))
      (push document_id (documents c)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Documents
;;; 
;;; A document in the database has:
;;; id, a numeric unique id
;;; name, a string that gives a short text name. Not required to be unique,
;;;   but if not, then retrieval by name is ambiguous; first one is gotten
;;; source, a string describing where this document came from, e.g., a
;;;   file name
;;; description, a text field to describe the document; not used
;;; updated_at, the timestamp of last update
;;; content, a text field holding the content
;;; 
;;; In addition, annotations link to documents, and when read in, they
;;; are held in an interval-tree.
;;;
;;; As we did for corpora, the document data structures that are read
;;; in get cached.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *documents-cache*
    (make-guarded-hash-table :test #'eql :values :weak)
  "Holds cache of document CLOS objects that have been loaded.
   The cache has weak values, so they are gc'd if not
   pointed to by anything else.")

(pomo:defprepared getdocbyid
    "select id,name,data,source,description,content,analyses,updated_at from documents where id=$1")
(pomo:defprepared getdocbyname
    "select id,name,data,source,description,content,analyses,updated_at from documents where name=$1 limit 1")

(defmethod document ((doc-id integer))
  "Retrieves the document object whose id is doc-id from the LATE
  persistent store.  This also retrieves all its annotation. If the
  document is already cached, we just return that."
  (or (get-hash doc-id *documents-cache*)
      (latepg
	(get-document (getdocbyid doc-id)))))

(defmethod document ((doc-name string))
  "Retrieves the (first) document object whose name is doc-name from
  the LATE persistent store, including all its annotations. We would also
  like to return cached values, but we need to retrieve the document id from
  the name before we can do so."
  (latepg
    (let ((docs-data (getdocbyname doc-name)))
      (or (and docs-data (get-hash (caar docs-data) *documents-cache*))
	  (get-document docs-data)))))

(defmethod document ((doc document))
  doc)

(pomo:defprepared getannotationsbydocid
    "select id,type,starting,ending,data,other,up from annotations where document_id=$1")

(defun get-document (docs)
  "Performs the construction of a document object from the return values from the database
   passed in by (document ...). We get a list, but it should have only one element."
  (and docs
       (destructuring-bind
	   (id name data source description size content analyses updated_at)
	   (car docs)
	 (let ((d (and docs
		       (make-instance 'document
			 :id id
			 :name (unsq name)
			 :data (unsq-read data) ;prop list of other data
			 :source (unsq source)
			 :description (unsq description)
			 :size size
			 :dirty nil
			 :dirty-annotations nil
			 :updated_at updated_at
			 :analyses (unsq-read analyses)
			 :content (if (eq content :null) "" content))))
	       (ann-cache (make-hash-table)))
	   (dolist (ann (getannotationsbydocid id))
	     (destructuring-bind (id type start end data other up) ann
	       (let ((a (make-instance (intern type)
			  :id id
			  :document d
			  :start start
			  :end end
			  ;;perhaps should just be unsq --psz
			  :data (unsq-read data)
			  :h-up (unsq up) ;temporary, id for now!
			  )))
		 ;; retrieve other fields from other
		 (complete-annotation a (unsq-read other))
		 ;;(format t "~%Annotation ~s, h-up=~a" a (h-up a))
		 ;; Prepare to fix up the h-up and h-down pointers
		 (setf (gethash id ann-cache) a)
		 ;; Save annotations in the rb-tree
		 (tree-insert (annotations-tree d) a)))
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

(defmethod save ((doc document))
  "Writes back any changes to the document to the LATE data store. We
  use the time stamp of document entry in the data store to detect
  asynchronous conflicting attempts to change it.  Thus, every
  document object has both a dirty flag and a time stamp from the
  store. When it is dirty, we try to write it back to the store. If,
  however, the store's timestamp is inconsistent with the object's,
  then we have detected a conflict."
  (late-transaction
   (cond ((null (id doc))
		  ;; This is a newly-created document, not yet saved to the
		  ;; datastore. We need to save it.
		  (latesql (format
					nil
					"insert into documents(name,data,source,description,size,~
                 content, analyses) values(~a,~a,~a,~a,~d,~a,~a)"
					(sq (name doc))
					(sq (data doc))
					(sq (source doc))
					(sq (description doc))
					(sq (size doc))
					(sq (content doc))
					(sq (analyses doc))) :db *late-db*)
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
		  (latesql (format
					nil
					"update documents set name=~a, data=~a, source=~a,~
  description=~a, size=~a, content=~a, analyses=~a where id=~d"
					(sq (name doc)) (sq (data doc)) (sq (source doc))
					(sq (description doc)) (sq (size doc))
					(sq (content doc)) (sq (analyses doc)) (id doc))
				   :db *late-db*)
		  (setf (dirty doc) nil)
		  ;; if updating was done by me, then sync the update_at time
		  (setf (updated_at doc) 
			(caar (latesql "SELECT updated_at FROM documents WHERE id=~d" 
						   (id doc))))))
   ;; Now we save annotations.  If any have been deleted, get rid of
   ;; them. There is an ordering problem with new annotations because
   ;; the hierarchy among them is represented by a pointer from the
   ;; lower to the higher annotation. Because in the data store this is
   ;; represented by an ID, we must assure that higher annotations are
   ;; stored first (i.e., have an ID) before lower ones. Therefore, we
   ;; first walk the annotation hierarchy, save them in top-to-bottom
   ;; order, remember which ones we have saved, and then save the rest,
   ;; which are not hierarchically organized.
   (when (dirty-annotations doc)
     (when (annotations-deleted doc)
       (dolist (a (annotations-deleted doc))
		 (del a))
       (setf (annotations-deleted doc) nil))
     (let* ((doc-anns (annotations doc :type 'document-annotation))
			(doc-ann (and doc-anns (car doc-anns)))
			(saved-anns nil))
       (labels ((iter (ann)
				  (when ann
					(save ann)
					(push ann saved-anns)
					(dolist (a (h-down ann))
					  (iter a)))))
		 (iter doc-ann))
       (do-tree (ann (annotations-tree doc))
		 (unless (member ann saved-anns)
		   (save ann)))
       (setf (dirty-annotations doc) nil)))))



;;; incomplete
