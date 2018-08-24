;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 04/18/2012 just use string valued type.
yluo - creation

This file is customizable for different needs to merge documents or annotations
or a mixture of them into a instance.
|#

(defpackage :late
  (:use :common-lisp :util :dbi.mysql)
  (:export 
   "ANNOTATION"
   "DOCUMENT"
   "INSTANCE"
   "inst-content"
   "inst-id"
   "instances"
   "mrn->docids"
   "new-inst-set"
   "new-inst"
   "related-content"
   "related-inst"
   "inst-sets-overlap"
   "mrns"
   "add-mrn"
   "del-mrn"
   "associate-mrns"
   "associate-mrn"
   "disassociate-mrns"
   "update-inst-corpus"
   ))

(in-package :late)


(defun new-inst-set (name desc grp-rule &aux inst-id)
  "the grouping rule could be: by mrn, window 2"

  (late-transaction
   (latesql "INSERT INTO instance_sets(name, description, grouping_rule) VALUES(~a, ~a, ~a)"
	    (sq name) (sq desc) (sq grp-rule))
   (setf inst-id (mysql-insert-id *late-db*)))
  inst-id)

(defmethod del-inst-set ((name string))
  "Not rely on ON DELETE CASCADE to delete the entries in instances and 
instances_contents"
  (late-transaction
   (latesql "DELETE FROM instances_content WHERE inst IN 
             SELECT inst_id FROM sets_instances WHERE set_id IN
             SELECT id FROM instance_sets WHERE name~a" (sql-matcher name))
   (latesql "DELETE FROM instances WHERE inst IN
             SELECT inst_id FROM sets_instances WHERE set_id IN
             SELECT id FROM instance_sets WHERE name~a" (sql-matcher name))
   (latesql "DELETE FROM sets_instances WHERE set_id IN
             SELECT id FROM instance_sets WHERE name~a" (sql-matcher name))
   (latesql "DELETE FROM instance_sets WHERE name=~a" (sq name))))

(defmethod instances ((set-id integer))
  "return id's and names of instances"
  (latesql "SELECT ti.id, ti.name FROM instances ti
                    JOIN sets_instances tsi ON ti.id=tsi.inst_id
                    WHERE tsi.set_id=~d"
	   (sq set-id)))

(defun inst-set-id (set-name)
  (caar (latesql "SELECT id FROM instance_sets WHERE name=~a" (sq set-name))))


(defun associate-instance (set-id inst-id)
  (latesql "INSERT INTO sets_instances(set_id, inst_id) VALUES (~a, ~a)"
	   set-id inst-id))

(defun disassociate-instance (set-id inst-id)
  (latesql "DELETE FROM sets_instances where set_id=~a and inst_id=~a"
	   set-id inst-id))

(defmethod instances ((set-name string))
  (let* ((set-id (inst-set-id set-name)))
    (instances set-id)))

(defun mrns (set-name)
  (mapcar #'second (instances set-name)))

(defun new-inst (&key (contents nil) 
		      (set-name nil) 
		      (inst-name nil) 
		      (inst-type nil)
		      (content-check? nil))
  "Persist a newly merged instance.
Input
======
contents : should be a list of cons (content-id content-type)
set-name : name of a set.
inst-name : name of the instance to be inserted.
ctype : content type, e.g. documetn, annotation etc. here it is for sanity 
        check.
"
  (let* ((set-id (inst-set-id set-name))
	 inst-id cid ctype)
    (assert set-id () "no instance set with name ~a" set-name)
    (late-transaction
     (latesql "INSERT INTO instances(name, inst_type) VALUES(~a, ~a)" 
	      (sq inst-name) (sq inst-type))
     (setf inst-id (mysql-insert-id *late-db*))
     (latesql "INSERT INTO sets_instances(set_id, inst_id) VALUES(~a, ~a)"
	      set-id inst-id)
     (dolist (content contents)
       (setf cid (car content)
	     ctype (cdr content))
       (when content-check?
	 (cond 
	  ((equalp "document" ctype)
	   (assert (latesql "SELECT id FROM documents WHERE id=~a" (sq cid))
		   () "no document with name ~a" cid))
	  ((equalp "annotation" ctype)
	   (assert (latesql "SELECT id FROM annotations WHERE id=~a" (sq cid))
		   () "no annotation with name ~a" cid))
	  ((equalp "instance" ctype)
	   (assert (latesql "SELECT id FROM instances WHERE id=~a" (sq cid))
		   () "no instance with name ~a" cid))
	  (t
	   (error "unknown type ~a" ctype))))
       (latesql "INSERT INTO instances_content(inst, content, content_type) VALUES (~d, ~d, ~a)" 
		(sq inst-id) (sq cid) (sq ctype))))))

(defun inst-id (inst-name inst-type)
  "Assume that inst name and inst type uniquely determines an entry"
  (caar (latesql "SELECT id FROM instances WHERE name=~a AND inst_type=~a"
		 (sq inst-name) (sq inst-type))))

(defun associate-mrn (set-name mrn)
  (let* ((set-id (inst-set-id set-name))
	 (inst-id (inst-id mrn "mrn")))
    (associate-instance set-id inst-id)))

(defun associate-mrns (set-name mrns)
  (let* ((set-id (inst-set-id set-name))
	 inst-id)
    (dolist (mrn mrns)
      (setf inst-id (inst-id mrn "mrn"))
      (associate-instance set-id inst-id))))

(defun disassociate-mrns (set-name mrns)
  (let* ((set-id (inst-set-id set-name))
	 inst-id)
    (dolist (mrn mrns)
      (setf inst-id (inst-id mrn "mrn"))
      (disassociate-instance set-id inst-id))))

(defun del-mrn (mrn)
  (let* ((inst-id (inst-id mrn "mrn")))
    ;; delete from instance sets
    (latesql "DELETE FROM sets_instances where inst_id=~a" inst-id)
    ;; delete instance content
    (latesql "DELETE FROM instances_content where inst=~a" inst-id)
    ;; delete from instance table
    (latesql "DELETE FROM instances where id=~a" inst-id)
    
    ))

(defun update-inst-corpus (corpn)
  "update corpus that are derived according to instances"
  (let* ((corp (corpus corpn)))
    (setf (documents corp) nil)
    (dolist (mrn (mrns corpn))
      (dolist (docid (mrn->docids mrn))
	(push docid (documents corp))))
    (save corp)))

(defun add-mrn (mrn docs setns)
  (new-inst :contents (mapcar #'(lambda (a) 
				  (cons (id (document a)) "document"))
			      docs)
	    :set-name "MGH MRN instance set"
	    :inst-name mrn
	    :inst-type "mrn")
  (dolist (setn setns)
    (associate-mrn setn mrn)))




(defun mrn-correct (&aux mrn-old mrn-cor)
  "deprecate"
  (dolist (mrn (mapcar #'car 
		       (latesql "SELECT DISTINCT mrn FROM instances_content")))
    (setf mrn-old mrn)
    (setf mrn-cor (replace-re mrn "(\\s|\\n)" ""))
    (latesql "UPDATE instances_content SET mrn=~a WHERE mrn=~a"
	     (sq mrn-cor) (sq mrn-old))))


(defun mrn->docids2 (mrn)
  "Deprecated: do not use unless you have doc_attrs"
  (let* ((res (latesql "SELECT rep_num FROM doc_attrs
                        WHERE mrn=~a" (sq mrn))))
    (mapcar #'(lambda (x) (id (document (car x)))) res)))

(defun mrn->docids (mrn)
  "Assumes there is only one id corresponding to a mrn, need better enforcement
 in database."
  (let* ((inst-id (caar (latesql "SELECT id FROM instances WHERE inst_type='mrn' AND name=~a" (sq mrn)))))
    (inst-content inst-id :ctype "document")))

;;; returns a list of lists (corresponds to row content of result)
(defmethod inst-content ((inst integer)
			 &key (ctype nil))
  (mapcar #'car
	  (latesql "SELECT content FROM instances_content 
                    WHERE inst=~d~@[ AND content_type='~a'~]" 
		   inst ctype)))

;;; Input: a content name, and type
;;; Output: a list of related contents' ids all with the same type
(defmethod related-content ((cid integer)
			    &key
			    (ctype nil))
  (let* ((insts (related-inst cid))
	 ans)
    (mapcar #'(lambda (inst) 
		(dolist (cid (inst-content inst :ctype ctype))
		  (pushnew cid ans :test #'=))) 
	    insts)
    ans))

(defmethod related-inst ((cid integer))
  (mapcar #'car (latesql "SELECT inst FROM instances_content WHERE content=~d"
			 cid)))



(defun inst-sets-overlap (inst-set-names &aux h-inst ans)
  (setf h-inst (make-hash-table :test #'equalp))
  (dolist (inst-set-name inst-set-names)
    (let* ((insts (instances inst-set-name)))
      (dolist (inst insts)
	(incf (gethash inst h-inst 0)))))
  (maphash #'(lambda (inst cnt)
	       (when (> cnt 1)
		 (pushnew inst ans :test #'equalp)))
	   h-inst)
  ans)
