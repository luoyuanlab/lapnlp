;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
|#
;;; Programs to fix existing problems as they have been discovered in
;;; a LATE system. These do not require rebuilding the entire database
;;; and re-doing all the analyses from scratch.


(defun fix-annotations-fields ()
;;; This is a procedure that modifies a LATE database from the time
;;; before the 'other' field was added to annotations.  It inserts the
;;; new field, copies over any data that had previously been encoded
;;; in the 'data' field, then modifies the data field to be of type
;;; varchar(255) instead of text, and creates an index on it.  This is
;;; only a transitional step, not necessary for databases created
;;; after this change.

  (open-late-database)
  (let ((ann-fields (latesql "describe annotations"))
	(i 0))
    (assert (null (assoc "other" ann-fields :test #'equalp))
	() "Fix-annotations-fields has already been done; no need to ~
            do it again.")
    (latesql "alter table annotations add column other mediumtext ~
  after data")
    (dolist (row (latesql "select id,data from annotations"))
      (when (zerop (mod (incf i) 1000)) (princ "."))
      (let* ((d (unsq-read (cadr row)))
	     (data (getf d 'data)))
	(remf d 'data)
	(latesql "update annotations set data=~a, other=~a ~
 where id=~d"
		 (sq data)
		 (sq d)
		 (car row))))
    (latesql "alter table annotations change data data varchar(255)")
    (latesql "alter table annotations add index x_data (type,data)")
    t))

