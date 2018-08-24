;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation
|#
;;; This code looks up tokens in the gazettes in the LATE database and
;;; creates gazette annotations for matching tokens.

(defpackage :late
  (:use :common-lisp #+allegro :excl :util)
  (:export "gazettize-tokens"
	   "gazette-lookup"
	   "init-gazette-data"
	   "gazettize-strings"
	   "in-stoplist?"
	   "h-in-stoplist?"))

(in-package :late)

(defmemo gazette-lookup (token)
  (latesql "select gazette,rank,frequency,cum ~
from gazette where entry=~a"
	   (sq token)))

(defmemo in-stoplist? (token)
  "Use customized stoplist for lymphoma project."
  (when (getf *ml-features* 'use-stoplist)
    (let* ((qtokm (sq (concatenate 'string "%" token "%"))))
      (or (sql (format nil "select id from gazette where type='lymphoma-stoplist' and entry=~a limit 1"
		       (sq token))
	       :db *gazette-db*)
	  (sql (format nil "select id from gazette where type in ('day', 'month', 'month-abbrev', 'day-abbrev') and entry like ~a limit 1"
		       qtokm)
	       :db *gazette-db*)))))

(defparameter *h-stoplist* (make-hash-table :test #'equalp))

(defun load-stoplist (&key (in-fn "late:;gazettes;lymphoma_stoplist.txt"))
  (when (= 0 (hash-table-count *h-stoplist*))
    (let* (ln time-start time-elapsed)
      (setf time-start (get-internal-real-time))
      (with-open-file (in-f in-fn :direction :input :external-format :utf-8)
		      (loop (unless (setf ln (read-line in-f nil nil)) (return))
			    (setf (gethash ln *h-stoplist*) 1)))
      (setf time-elapsed (- (get-internal-real-time) time-start))
      (format t "~&load stoplist time: ~f secs" (/ time-elapsed 1000)))))

(defun h-in-stoplist? (token)
  "Use customized stoplist for lymphoma project."
  (when (getf *ml-features* 'use-stoplist)
    (load-stoplist)
    (gethash token *h-stoplist*)))

(defmethod gazettize-tokens ((doc document))
  "Adds annotations to existing annotations of type (default tokens)
  in doc. Note that by default this will not find multi-token gazette
  entries. For that, see string-gazettize."
  (dolist (token (annotations-spec doc :type (gtype 'gtoken-type)))
    (dolist (match (gazette-lookup (content token)))
      (add-annotation
       doc
       (make-instance 'gazette-annotation
		      :start (start token)
		      :end (end token)
		      :data (elt match 0)
		      :rank (elt match 1)
		      :frequence (elt match 2)
		      :cum (elt match 3)))))
  (save doc))

(defmethod gazettize ((corp corpus))
  (do-corpus (doc corp)
	     (gazettize doc)))

(defvar *gazette-pattern* nil)

(defvar *gazette-hash* (make-hash-table :test #'equalp))

(defun init-gazette-data (&optional (force nil)
				    &aux (alist nil) (strings nil))
  (when (or force
	    (null *gazette-pattern*)
	    (zerop (hash-table-count *gazette-hash*)))
    (setq alist (latesql "select entry,gazette from gazette"))
    (clrhash *gazette-hash*)
    (dolist (e alist)
      (push (cadr e) (gethash (car e) *gazette-hash* nil)))
    (maphash #'(lambda (k v)
		 (declare (ignore v))
		 (push k strings))
	     *gazette-hash*)
    (setq *gazette-pattern*
	  (compile-re
	   `(:alternation
	     ,@(mapcar
		#'(lambda (e)
		    (let ((parts (split-re " " e)))
		      `(:sequence
			:word-boundary
			,@(if (null (cdr parts))
			      (list e)
			    (cdr (mapcan
				  #'(lambda (part)
				      `((:greedy-repetition
					 1 nil :whitespace-char-class)
					,part))
				  parts)))
			:word-boundary)))
		strings))))))

(defmethod gazettize-strings ((doc document))
  (init-gazette-data)
  (let ((text (content doc)))
    (map-match-re 
     #'(lambda (match)
	 (add-annotation
	  doc
	  (make-instance 'gazette-annotation
			 :start (car match)
			 :end (cdr match)
			 :data (gethash (subseq text (car match) (cdr match))
					*gazette-hash*))))
     *gazette-pattern*
     text))
  ;;(save doc)
  )

(defmethod gazettize-strings ((corp corpus))
  (do-corpus (doc corp)
	     (gazettize-strings doc)))
