;;; -*- Mode: Lisp; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 08/01/2009 creation
|#

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (require :mysql)
  (require :print-table)
  (import '(dbi.mysql:sql dbi.mysql:with-db-rows dbi.mysql:*mysql*)))

(defun open-mysql (db
		   &optional (user "root")
			     (pwd nil)
			     (host nil)
			     (port 3306))
  "Opens a Mysql database specified the arguments. The first should be
 the name of a database (as string or symbol), and subsequent ones are
 user, password, host and port number. Any that are not given default
 to access to a localhost database server with default login root."
  (let ((conn
	 (dbi.mysql:connect :host host
			    :database (if (symbolp db) (symbol-name db) db)
			    :user user
			    :password pwd
			    :port port)))
    (format t "~%~a connected to ~s on ~a"
		user db (or host "localhost"))
    (assert conn (conn) "Unable to open database ~s!" db)
    conn))

(defun get-databases (&optional (conn *mysql*))
  (mapcar #'car (sql "show databases" :db conn)))

(defun use-database (db &optional (conn *mysql*))
  (sql (format nil "use ~a" db) :db conn))

(defun get-tables (&optional (database nil) (conn *mysql*))
  "Gets the names of tables from the database via the (open)
  connection conn."
  (when database (use-database database conn))
  (mapcar #'car (sql "show tables" :db conn)))

(defun get-cols (table &optional (database nil) (conn *mysql*))
  (when database (use-database database conn))
  (sql (format nil "describe ~a" table) :db conn))

(defun show-cols (table &optional (database nil) (conn *mysql*))
  (multiple-value-bind (rows headers)
      (get-cols table database conn)
    (print-table
     (cons headers rows))))

(defun show-db (&optional (database nil) (conn *mysql*))
  (dolist (tbl (get-tables database conn))
    (format t "~2%~a (~d rows)"
	    tbl
	    (caar (sql (format nil "select count(*) from ~a" tbl)
		       :db conn)))
    (show-cols tbl database conn))
  t)
