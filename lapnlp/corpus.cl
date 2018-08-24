;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 04/13/2012 adjusted annotations to take on global specifications
yluo - 03/09/2012 modified panalyze-corpus to handle non excl-tuis-fn
yluo - 12/17/2011 deleted populate-excl-secs 
yluo - 04/20/2011 add reports_groupby_mrn
yluo - 03/23/2011 add inclusion-check
yluo - 12/27/2010 add populate-excl-secs
yluo - 11/26/2010 creation 

Utility for corpus/corpora level processings.
we expecte the users to come up with their own populate-corpus
|#

(defpackage :late
  (:use :common-lisp :excl :excl.osi :mnegex :cg :wordnet
	:opennlp :javatools.jlinker :jc)
  (:export
   "*debug-corpus*"
   "*max-cores*"
   "*min-sc-size*"
   "analyze-corpus"
   "analyze-docs"
   "combine-corpora"
   "final-diagnosis-scan"
   "group-documents"
   "inclusion-check"
   "load-pconfig"
   "panalyze-corpus"
   "panalyze-subcorpus"
   "pconcept-graph-map"
   "populate-doc-attrs"
   "populate-excl-tuis"
   "populate-inst-mrn"
   "populate-instance-set-corpus"
   "ppivot-analysis"
   "reports-by-mrn"
   "p-featuregen"
   "p-featuregen-subset"
   "output-mallet-dir"
   "output-data-sql"
   "output-ft-sql"
   "psg-pn-matrix"
   "populate-docname-corpus"
   "update-corpus-doc-content"
   "find-sen-in-corp"
   "lymphoma-class"
   "lymphoma-classes"
   "find-doc-in-corp"
   "multi-lymphoma-mrns"
   "lymphoma-split-sanity"
   "output-mrn-insts"
   "case-length-check"
   "train-test-cnt"
   "output-gt-csv"
   "mrn-date"
   "embedding-corpus-gen"
   ;; "populate-corpus"
   ))

(in-package :late)

(defparameter *debug-corpus* nil)
(defparameter *max-cores* 2000)
(defparameter *min-sc-size* 10)


(defun populate-doc-attrs (corpora)
  (format t "~&Populate doc_attrs table ...~%")
  (let* (mrn_type mrn rep_num rep_dt rep_desc rep_stat rep_type sanns)
    (dolist (ncorp corpora)
      (format t "~&Scanning corpus ~a~%" ncorp)
      (dolist (docid (documents (corpus ncorp)))
	(unless (latesql "SELECT * FROM doc_attrs WHERE did=~d" (sq docid))
	  (setf sanns 
		(annotations (document docid) :type 'section-annotation))
	  
	  (setf mrn_type
		(replace-re
		 (content
		  (find-if #'(lambda (a) 
			       (and (string-equal "MRN_Type"
						  (data a))
				    (not (typep a 
						'section-head-annotation))))
			   sanns))
		 "(^\\s+|\\s+$)" ""))
	  
	  ;; extract MRN
	  (setf mrn 
		(replace-re
		 (content
		  (find-if #'(lambda (a) 
			       (and (string-equal "MRN"
						  (data a))
				    (not (typep a 
						'section-head-annotation))))
			   sanns))
		 "(^\\s+|\\s+$)" ""))
	  
	  (setf rep_num
		(replace-re
		 (content
		  (find-if #'(lambda (a) 
			       (and (string-equal "Report_Number"
						  (data a))
				    (not (typep a 
						'section-head-annotation))))
			   sanns))
		 "(^\\s+|\\s+$)" ""))
	  
	  ;; extract Report_Date_Time
	  (setf rep_dt 
		(replace-re
		 (content
		  (find-if #'(lambda (a) 
			       (and (string-equal "Report_Date_Time"
						  (data a))
				    (not (typep a 
						'section-head-annotation))))
			   sanns))
		 "(^\\s+|\\s+$)" ""))

	  (setf rep_desc
		(replace-re
		 (content
		  (find-if #'(lambda (a) 
			       (and (string-equal "Report_Description"
						  (data a))
				    (not (typep a 
						'section-head-annotation))))
			   sanns))
		 "(^\\s+|\\s+$)" ""))
	  
	  (setf rep_stat
		(replace-re
		 (content
		  (find-if #'(lambda (a) 
			       (and (string-equal "Report_Status"
						  (data a))
				    (not (typep a 
						'section-head-annotation))))
			   sanns))
		 "(^\\s+|\\s+$)" ""))
	  
	  (setf rep_type
		(replace-re
		 (content
		  (find-if #'(lambda (a) 
			       (and (string-equal "Report_Type"
						  (data a))
				    (not (typep a 
						'section-head-annotation))))
			   sanns))
		 "(^\\s+|\\s+$)" ""))


	  (latesql "INSERT INTO doc_attrs(did, mrn_type, mrn, rep_num,
                                        rep_dt, rep_desc, rep_stat,
                                        rep_type) VALUES
                    (~d, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a)"
		   (sq docid) (sq mrn_type) (sq mrn) (sq rep_num) 
		   (sq rep_dt) (sq rep_desc) (sq rep_stat) 
		   (sq rep_type)))))))

(defun reports-by-mrn (folder corpora)
  "output reports to a folder with those having same mrn grouped in one file"
  (format t "~&Reports Group by MRN ...~%")
  (let* ((h-corpora (make-hash-table :test #'equalp))
	 mrn dt)
    (dolist (ncorp corpora)
      (dolist (docid (documents (corpus ncorp)))
	;; extract MRN
	(setf mrn 
	      (replace-re
	       (content
		(car
		 (annotations 
		  (document docid) 
		  :type 'section-annotation 
		  :filter #'(lambda (a) 
			      (and (string-equal "MRN"
						 (data a))
				   (not (typep a 
					       'section-head-annotation)))))))
	       "(^\\s+|\\s+$)" ""))
	
	;; extract Report_Date_Time
	(setf dt 
	      (replace-re
	       (content
		(car
		 (annotations 
		  (document docid) 
		  :type 'section-annotation 
		  :filter #'(lambda (a) 
			      (and (string-equal "Report_Date_Time"
						 (data a))
				   (not (typep a 
					       'section-head-annotation)))))))
	       "(^\\s+|\\s+$)" ""))
	(setf dt (date-ut? (replace-re dt "(^\\s+|\\s+.*$)" "")))
	(cond 
	 ((gethash mrn h-corpora)
	  (pushnew docid (gethash dt (gethash mrn h-corpora) nil)))
	 (t
	  (let* ((h-dt (make-hash-table :test #'equalp)))
	    (pushnew docid (gethash dt h-dt nil))
	    (setf (gethash mrn h-corpora) h-dt)))
	 )))
    
    (maphash #'(lambda (mrn h-dt)
		 (let* (dts)
		   (maphash #'(lambda (dt docid)
				(declare (ignore docid))
				(pushnew dt dts))
			    h-dt)
		   (setf dts (sort dts #'<))
		   
		   (with-open-file (f-mrn (format nil "~a;mrn_~a" folder mrn)
					  :direction :output
					  :if-exists :supersede
					  :if-does-not-exist :create)
				   (dotimes (i (length dts))
				     (dolist (docid (gethash (elt dts i) h-dt))
				       (format f-mrn "~&~a~%~%" (content (document docid))))
				     ))))
	     h-corpora)
    ))


(defun output-mrn-insts (folder)
  (dolist (mrn (mrns "MRN instance set"))
    (with-open-file (f-mrn (format nil "~a;~a" folder mrn)
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
		    ;; sort docs according to date
		    (let* ((docids (mrn->docids mrn))
			   (docs (mapcar #'document docids))
			   (h-dates (make-hash-table :test #'equalp)))
		      (dolist (doc docs)
			(let* ((date (doc-date doc)))
			  (if (gethash date h-dates)
			      (pushnew doc (gethash date h-dates))
			    (setf (gethash date h-dates) (list doc)))))
		      (let* ((dates (sort (hash-keys h-dates) #'<)))
			(dolist (date dates)
			  (dolist (doc (gethash date h-dates))
			    (format f-mrn "~&~a~%~%" (content doc)))))))))


(defun output-gt-csv (fncsv)
  (with-open-file (fcsv fncsv
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
		  (dolist (mrn (mrns "MRN instance set"))
		    ;; sort docs according to date
		    (let* ((docids (mrn->docids mrn))
			   (docs (mapcar #'document docids))
			   (h-dates (make-hash-table :test #'equalp)))
		      (dolist (doc docs)
			(let* ((date (doc-date doc)))
			  (if (gethash date h-dates)
			      (pushnew doc (gethash date h-dates))
			    (setf (gethash date h-dates) (list doc)))))
		      (let* ((dates (sort (hash-keys h-dates) #'<))
			     (docns nil))
			(dolist (date dates)
			  (dolist (doc (gethash date h-dates))
			    (push (name doc) docns)))
			(format fcsv "~a, \"~{~a~^, ~}\"~%" mrn (nreverse docns)))))))

(defun inclusion-check (fn-rep mode corpora)
  "Check wether reports of certain criteria exist in corpus
Input
======
fn-rep: report list file, can be of mrn or report number
mode: either 'MRN' or 'Report_Number'
corpora: hash of corpus to be checked upon
Output
======
A list of reports' contents depending on mode, which are not present in the 
corpora
"
  (format t "~&Inclusion check ...~%")
  (let* ((h-corpora (make-hash-table :test #'equalp))
	 res rkey l-rep)
    (with-open-file (f-rep fn-rep :direction :input)
		    (let* (line rep)
		      (loop (unless (setf line (read-line f-rep nil nil)) (return))
			    (setf rep (replace-re line "[- ]" ""))
			    (when (< 0 (length rep))
			      (pushnew rep l-rep)))))
    
    ;; populate corpora hash table.
    (dolist (ncorp corpora)
      (dolist (docid (documents (corpus ncorp)))
	(setf rkey 
	      (replace-re
	       (content
		(car
		 (annotations 
		  (document docid) 
		  :type 'section-annotation 
		  :filter #'(lambda (a) 
			      (and (string-equal mode
						 (data a))
				   (not (typep a 
					       'section-head-annotation)))))))
	       "(^\\s+|\\s+$)" ""))
	(incf (gethash rkey h-corpora 0))))
    (format t "~&corpora hash table poped~%")
    
    (dolist (rep l-rep)
      (unless (gethash rep h-corpora)
	(format t "~a: ~a~%" mode rep)
	(pushnew rep res)))
    res))



(defun populate-doc-inst-set (inst-set-name inst-set-desc grp-rule fn-inst)
  "Read .inst file and populate corpus whose documents are listed as lists of
reports grouped at user's discretion.
Input
======
corp-name: corpus name
fn-inst: .inst file, line conforms to <inst-name, doc-name1, doc-name 2, ...>
         format.
"
  (del-inst-set inst-set-name)
  (new-inst-set inst-set-name inst-set-desc grp-rule)
  (format t "~&Populating instance set ~a~%" inst-set-name)

  (with-open-file (f-inst fn-inst :direction :input)
		  (let* (line doc-names contents inst-name)
		    (loop (unless (setf line (read-line f-inst nil nil)) (return))
			  (setf doc-names (split-re ", *" line))
			  (setf inst-name (pop doc-names))
			  (setf contents (mapcar #'(lambda (x) (id (document x))) doc-names))
			  (new-inst contents inst-set-name inst-name "document")))))

(defun populate-instance-set-corpus (set-name corp-name fn-csv 
					      &key (set-desc nil)
					      (set-grp-rule nil)
					      (inst-type "mrn")
					      &aux h-csv corp set-id)
  "Populate the instance set and associated corpus"
  ;; sample usage:
  ;; (populate-instance-set-corpus "burkitts_negative_test" "burkitts_negative_test" "data:;groundtruth;burkitts_negative_test" :set-desc "derived from burkitts_negative_test" :set-grp-rule "by MRN with no window" :inst-type "mrn")
  (setf h-csv (make-hash-table :test #'equalp))
  (if (setf corp (corpus corp-name))
      (del corp))
  (setf set-id (caar (latesql "SELECT id FROM instance_sets WHERE name=~a"
			      (sq set-name))))
  
  (when set-id
    (format t "deleting old instance set ~a~%" set-name)
    (latesql "DELETE FROM sets_instances WHERE set_id=~a" set-id)
    (latesql "DELETE FROM instance_sets WHERE id=~a" set-id)
    (format t "done~%"))
  (format t "~&Populating instance set & corpus ~a~%" corp-name)
  
  (setf corp (make-instance 'corpus :name corp-name
			    :description "derived from same name instance set"))
  (setf set-id (new-inst-set set-name set-desc set-grp-rule))
  
  (with-open-file (f-csv fn-csv :direction :input)
		  (let* (line)
		    (loop (unless (setf line (read-line f-csv nil nil)) (return))
			  (multiple-value-bind (match? whole mrn)
			      (match-re "^(\\d+)," line)
			    (declare (ignore whole))
			    (when match? (setf (gethash mrn h-csv) 1))))))
  
  ;; link related files from instance_sets
  (with-open-file (f-rel (concatenate 'string fn-csv "-related-insts")
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)    
		  (maphash #'(lambda (mrn v)
			       (declare (ignore v))
			       (let* ((inst-id (inst-id mrn inst-type)))
				 (cond 
				  (inst-id
				   (associate-instance set-id inst-id)
				   (format f-rel "~&MRN ~a: " mrn)
				   (dolist (docid (inst-content inst-id))
				     (push docid (documents corp))
				     (format f-rel "~a " (name (document docid))))
				   (format f-rel "~%"))
				  (t
				   (format t "~&MRN: ~a not in corpora~%" mrn)))))
			   h-csv))
  (save corp))

(defun final-diagnosis-scan (corpn expr fnout)
  (let* ((hhit (make-hash-table :test #'equalp))
	 doc mrn docn sa-text fout)
    (dolist (docid (documents (corpus corpn)))
      (setf doc (document docid))
      (dolist (sa (annotations doc :type 'section-annotation))
	(setf sa-text (replace-re (content sa) "(^\\s+|\\s+$)" ""))
	(when (and (not (typep sa 'section-head-annotation))
		   (equalp "MRN" (data sa)))
	  (setf mrn sa-text))
	(when (and (not (typep sa 'section-head-annotation))
		   (match-re "final_diagnosis" (data sa) :case-fold t)
		   (match-re expr sa-text :case-fold t))
	  (setf docn (name doc))
	  (setf sa-text (replace-re sa-text "\\s+" " "))
	  (push (list docn sa-text) (gethash mrn hhit)))))
    (setf fout (open fnout :direction :output :if-exists :supersede
		     :if-does-not-exist :create))
    (dolist (kv (hash-table-alist hhit))
      (setf mrn (car kv))
      (format fout "~a~%~{~a~^~%~}~%" mrn (cdr kv)))
    (close fout)))

(defun populate-docname-corpus (corpn fn)
  (let* ((corp (make-instance 'corpus :name corpn))
	 (f (open fn :direction :input))
	 ln doc docid)
    (loop (unless (setf ln (read-ne-line f)) (return))
	  (setf doc (document ln))
	  (setf docid (id doc))
	  (push docid (documents corp)))
    (save corp)
    (close f)))


(defun combine-corpora (corpora-names new-corp-name &aux corp new-corp desc)
  (setf desc (format nil "combine ~{~a~^, ~}" corpora-names))
  (setf new-corp (make-instance 'corpus :name new-corp-name
				:description desc))
  (dolist (corpn corpora-names)
    (setf corp (corpus corpn))
    (dolist (docid (documents corp))
;;;      (when (member docid (documents new-corp) :test #'equalp)
;;;		(format t "~&duplicate doc ~a~%" (name (document docid))))
      (pushnew docid (documents new-corp))))
  (save new-corp))


(defun within-window? (date1 date2 window)
  "
Input
=====
date1 - Allegro's Date representation.
window - in days"
  (or (not window)
      (= date1 date2)
      (and (> date1 date2)
	   (<= (- date1 date2) (* 3600 24 window)))
      (and (< date1 date2)
	   (<= (- date2 date1) (* 3600 24 window)))))

(defun sort-merge-dates (mrn hash-date inst-set-name
			     &key (logfile nil)
			     (window nil)) 
  "Sort keys of hash tables B; scan B using sliding window, merge as necessary 
in a linear sweep. Operates in place of a hash table.
Input
======
window - if not supplied, merge all dates"
  (let* (dates merged-dates rep-nums contents inst-name)
    (maphash #'(lambda (date val) (declare (ignore val)) (pushnew date dates))
	     hash-date)
    ;; we add a dummy dates in the end to avoid special handling of last item
    (setf dates (append (sort dates #'<) '(0)))

    (format logfile "~&after sort, mrn is ~a, dates is ~a~%" mrn dates)
    (dotimes (i (length dates))
      (cond 
       ((null merged-dates)
	(setf merged-dates (cons i i)))
       ;; merged-dates is not null
       ((and (< i (1- (length dates)))
	     (within-window? (elt dates i) (elt dates (cdr merged-dates))
			     window))
	(setf (cdr merged-dates) i))
       (t
	(mapcar #'(lambda (a)
		    (setf rep-nums (append (gethash a hash-date) rep-nums)))
		(subseq dates (car merged-dates) (1+ (cdr merged-dates))))
	(setf contents (mapcar #'(lambda (a) 
				   (cons (id (document a)) "document")) 
			       rep-nums))
	(format logfile "~&rep-nums: ~{~a~^, ~}~%" rep-nums)
	(setf inst-name (format nil "~a" mrn))
	(new-inst :contents contents 
		  :set-name inst-set-name 
		  :inst-name inst-name 
		  :inst-type "mrn")
	(setf rep-nums nil
	      contents nil)
	(setf merged-dates (cons i i)))))))


(defun doc-date (doc)
  (let* ((date (section-content doc "Report_Date_Time") ))
    (setf date (date-ut? (replace-re date "(^\\s+|\\s+.*$)" "")))))

(defun mrn-date (mrn &aux mrn-date)
  (dolist (docid (mrn->docids mrn))
    (let* ((doc (document docid))
	   (ddate (doc-date doc)))
      (if (or (not mrn-date) (> mrn-date ddate))
	  (setf mrn-date ddate))))
  mrn-date)

(defmethod group-documents ((corpora list)
			    (inst-set-name string)
			    &key
			    (inst-set-desc nil)
			    (window nil)
			    (fn-log nil))
  "Produces instance that has documents as its content and has MRN as name."
  ;; sample usage:
  ;; (late::group-documents '("corpusA" "corpusB" "corpusC") "MRN instance set" :inst-set-desc "group by MRN with no window req" :fn-log "late:;collapse1.log")
  ;; make hash table A with MRN as key, hash table B as val
  (let* ((hash-MRN (make-hash-table :test #'equalp))
	 mrn date rep-num doc corp)
    (format t "~&Group documents in corpora~%")
    ;; hash table B has date as key, list of report numbers as val
    ;; scan corp-C, add into hash table
    (dolist (corp-name corpora)
      (time-profiling
       (setf corp (corpus corp-name))
       (dolist (doc-id (documents corp))
	 (setf doc (document doc-id))
	 (setf mrn (section-content doc "MRN"))
	 (setf mrn (replace-re mrn "(^\\s+|\\s+$|\\n)" ""))
	 
	 (setf date (doc-date doc))
	 
	 (setf rep-num (section-content doc "Report_Number"))
	 (setf rep-num (replace-re rep-num "(^\\s+|\\s+$)" ""))

	 (cond 
	  ((gethash mrn hash-MRN)
	   (pushnew rep-num (gethash date (gethash mrn hash-MRN) nil)))
	  (t
	   (let* ((hash-date (make-hash-table :test #'equalp)))
	     (pushnew rep-num (gethash date hash-date nil))
	     (setf (gethash mrn hash-MRN) hash-date)))))
       (format t "~&corpus ~a scanned~%" (name corp))))
    
    (new-inst-set inst-set-name inst-set-desc "by mrn")
    (format t "~&sort & merge dates~%")
    (time-profiling
     (with-open-file (logfile fn-log :direction :output 
			      :if-exists :supersede)
		     ;; sort keys of hash tables B
		     ;; scan B using sliding window, merge as necessary in a linear sweep
		     (maphash #'(lambda (mrn h-date) 
				  (sort-merge-dates mrn h-date inst-set-name
						    :logfile logfile 
						    :window window))
			      hash-MRN)))))

(defmethod populate-exclude-cui-hash ((msh-uids-fn string))
  " filter out all mentions of lymphomas
 Bill's approach: search UMLS MRXW_ENGS table for all concepts that contain
 all the words of interest, say 'follicular' and 'lymphoma', in either the 
 preferred name or the synonyms. There are false positives such as 'Diffuse 
 large follicular center-cell lymphoma' actually means 'DLBCL' rather than 
 'FL', then he filtered out all those preferred names and synonyms. 
 I am taking a different approach, starting from, say follicular lymphoma,
 find the unique MeSH ID for it ('D008224'), then find all CUIs mapping into
 this MeSH ID, filter out all preferred name and synonyms for all CUIs.
 Note that for follicular lymphoma with different MeSH Tree Number, the MeSH 
 ID are still the same.
 Whether we want to generate the excluding CUI list, and filter against CUIs ;;; or to find MeSH Tree Number and filtered against a small list of MeSH Tree 
 Numbers is a trade off considering the size of the corpora and the relative 
 size of the portion of the MeSH tree we want to exclude, as well as whether
 it is necessary to generate the MeSH

 input: a list of mesh tree numbers 
 output: populate exclude-cui-hash with UIDs of nodes in subtrees of MeSH 
 rooted at MeSH Tree numbers in the input list"
  (let* ((exclude-cui-hash (make-hash-table :test #'equalp)) 
	 subtree msh-tree-nums msh-uids line)
    (with-open-file (msh-uids-f msh-uids-fn :direction :input)
		    (loop (unless (setf line (read-line msh-uids-f nil nil)) (return))
			  (pushnew line msh-uids :test #'equalp)))
    (setf msh-tree-nums (umlssql1 "SELECT DISTINCT ATV FROM MRSAT
                                     WHERE SAB='MSH' AND ATN='MN' AND CODE~a"
				  (sql-matcher msh-uids)))
    (dolist (mn msh-tree-nums)
      ;; return the MeSH UIDs to be excluded
      (setf subtree
	    (umlssql1 "SELECT DISTINCT CODE FROM MRSAT
                   WHERE SAB='MSH' AND ATN='MN' AND ATV LIKE ~a"
		      (sq (concatenate 'string mn "%"))))
      ;; return the CUIs associated with the UIDs
      (mapcar #'(lambda (cui)
		  (setf (gethash cui exclude-cui-hash) t))
	      (umlssql1 "SELECT DISTINCT CUI FROM MRCONSO
                           WHERE SAB='MSH' AND CODE~a"
			(sql-matcher subtree)))

      )
    exclude-cui-hash))

(defun populate-excl-tuis (docn)
  "Exclude sub tuis also, not tested."
  (when docn
    (populate-tui-stn-hash t)
    (let* (ans tui pstn)
      (with-open-file (doc docn :direction :input)
		      (loop (unless (setf tui (read-line doc nil nil)) (return))
			    (pushnew tui ans :test #'equalp)
			    (setf pstn (tui->stn tui))
			    (maphash #'(lambda (ctui cstn) 
					 (when (and (search pstn cstn)
						    (= 0 (search pstn cstn)))
					   (pushnew ctui ans :test #'equalp)))
				     *tui-stn-hash*)))
      ans)))









(defmethod analyze-corpus ((corp-name string)
			   &key
			   (line-splits? (gkey 'gsentencize 'split))
			   (excl-tuis nil)
			   (pre-lp? t))
  (analyze-docs (documents (corpus corp-name)) (corpus corp-name)
		:line-splits? line-splits? 
		:excl-tuis excl-tuis 
		:pre-lp? pre-lp?))

(defmethod analyze-docs ((docs list)
			 (corp corpus)
			 &key
			 (line-splits? (gkey 'gsentencize 'split))
			 (excl-tuis nil)
			 (pre-lp? t))
  "Analyze a corpus by performing sectionization, sentencization, tokenization,
pos-tagging, chunking, umlsization, hierarchical link parser analysis, 
item-content pair extraction.
Parameters
==========
corp-name       : Name of the corpus to be analyzed.
line-splits?     : Whether to use newline as sentence breakers.
excl-tuis: Exclude the tui segments from being analyzed, whose tuis are 
in excl-tuis.
"
  (let* ((cnt 300)
	 doc)
    (dolist (doc-id docs)
      (when (zerop cnt)
	(format t "free jlinker memory~%")
	(discard-in-java :flush)
	(setq cnt 300))
      (setf doc (document doc-id))
      (cond
       (pre-lp?
	(analyze-doc-pre-lp doc corp :line-splits? line-splits?))

       (t
	(analyze-doc-lp doc excl-tuis))))))

(defun panalyze-subcorpus (corp-name start end excl-tuis
				     &key 
				     (pre-lp? t) 
				     (line-splits? nil)
				     (redo? nil))
  (let* ((corp (corpus corp-name))
	 (docs (documents corp))
	 sub-docs)
    (setf sub-docs (mapcar #'document (subseq docs start end)))
    (dolist (doc sub-docs)
      (cond
       (redo? 
	(re-import-doc (name doc) corp-name))
       (pre-lp?
	(analyze-doc-pre-lp doc corp :line-splits? line-splits?))
       (t 
	(analyze-doc-lp doc excl-tuis))))
    (if pre-lp?
	(persist-link-supp-dict-hash *h-dict-supp*))))

(defun load-pconfig (fnpcfg)
  (let* ((*package* (find-package :late))
	 (spec-plist (read-from-string (read-file fnpcfg))))
    (when (getf spec-plist 'late-db)
      (setf (gethash "LATE_DB" *late-prefs*) (getf spec-plist 'late-db)))
    (when (getf spec-plist 'gazette-db)
      (setf (gethash "GAZETTE_DB" *late-prefs*) (getf spec-plist 'gazette-db)))
    (when (getf spec-plist 'umls-db)
      (setf (gethash "UMLS_DB" *late-prefs*) (getf spec-plist 'umls-db)))
    (setf *output-sql* (getf spec-plist 'output-sql))
    (setf *job-id* (getf spec-plist 'job-id))
    (setf *ann-delayed* (getf spec-plist 'ann-delayed))
    (setf link::*link-work* (getf spec-plist 'link-work-dir))
    (setf *excl-tuis* (populate-excl-tuis (getf spec-plist 'excl-tuis-fn))))
  (format t "~&Loaded parallel config file ~a.~%" fnpcfg))

(defun panalyze-corpus (corp-name 
			&key (pre-lp? nil) 
			(qtype "short")
			(fn-pcfg "late:;lymph-pconfig.cl")
			(fn-exp nil)
			&aux cnt cl-ldir)
  "Generate scripts to parallel process corpus. This version uses template.
Input
======
excl-tuis: name of the hash table for excluded tuis
qtype: normal, medium or long
Note
======
You need to call (load-pcnofig fn) before calling this."
  (setf cl-ldir (format nil "late:;parallel;~a" corp-name))
  (create-dir (namestring (translate-logical-pathname cl-ldir)))
  (cmd-verbose (format nil "rm -rf panalyze_~a_~a*" 
		       (if pre-lp? "pre_lp" "lp") corp-name) 
	       :dir cl-ldir)
  (create-dir (namestring (translate-logical-pathname "jobs:;panalyze_corpus")))
  (cmd-verbose (format nil "rm -rf panalyze_~a_~a*" 
		       (if pre-lp? "pre_lp" "lp") corp-name) 
	       :dir "jobs:;panalyze_corpus")

  ;; generate the .cl file
  (let* ((corp (corpus corp-name))
	 (docs (documents corp))
	 (c-size (length docs))
	 (subc-size (max *min-sc-size* (ceiling c-size *max-cores*)))
	 (idx 0)
	 line fnjob fnlsf fnsh)
    (setf cnt 0)
    
    (loop (unless (< idx c-size) (return))
	  (incf cnt)
	  ;; output .cl file
	  (setf fnjob (format nil "late:;parallel;~a;panalyze_~a_~a_~a.cl" 
			      corp-name (if pre-lp? "pre_lp" "lp") corp-name cnt))
	  (with-open-file
	   (fpjob fnjob
		  :direction :output :if-exists :supersede
		  :if-does-not-exist :create)
	   ;; read .cl template
	   (with-open-file
	    (fpheader "late:;parallel;panalyze_corpus_tmp.cl"
		      :direction :input)
	    (loop (unless (setf line (read-line fpheader nil nil)) (return))
		  (cond
		   ((equalp "[exe]" line)
		    (format fpjob "#! ~a/mlisp -#D~%" (get-env "ACL_HOME")))
		   ((equalp "[setting]" line)
		    (format fpjob "(setf corp-name ~s)~%" corp-name)
		    (format fpjob "(setf job-id ~a)~%" cnt)
		    (format fpjob "(setf subc-start ~a)~%" idx)
		    (format fpjob "(setf subc-end ~a)~%" 
			    (min (+ idx subc-size) c-size))
		    (format fpjob "(setf pre-lp? ~a)~%" pre-lp?)
		    (format fpjob "(setf fn-pcfg ~s)~%" fn-pcfg)
		    (format fpjob "(setf fn-exp ~s)~%" fn-exp))
		   (t
		    (format fpjob "~a~%" line))))))
	  
	  ;; output individual .lsf file that accompanies the .cl file
	  (setf fnlsf (format nil "jobs:;panalyze_corpus;panalyze_~a_~a_~a.lsf" 
			      (if pre-lp? "pre_lp" "lp") corp-name cnt))
	  (with-open-file
	   (fplsf fnlsf
		  :direction :output :if-exists :supersede
		  :if-does-not-exist :create)
	   ;; read .lsf template
	   (with-open-file
	    (fptmp "lsf_templates:;panalyze_corpus_tmp.lsf"
		   :direction :input)
	    (loop (unless (setf line (read-line fptmp nil nil)) (return))
		  (cond
		   ((equalp "[setting]" line)
		    (format fplsf "cname=~a~%" corp-name)
		    (format fplsf "jobid=~a~%" cnt)
		    (format fplsf "balp=~a~%" (if pre-lp? "pre_lp" "lp"))
		    (format fplsf "workdir=~a~%" 
			    (translate-logical-pathname cl-ldir)))
		   (t
		    (format fplsf "~a~%" line))))))
	  (setf idx (min (+ idx subc-size) c-size)))
    
    (format t "~&~a has ~a jobs~%" corp-name cnt)
    
    ;; output .sh file
    (setf fnsh (format nil "jobs:;panalyze_~a_~a.sh" 
		       (if pre-lp? "pre_lp" "lp") corp-name))
    (with-open-file
     (fpsh fnsh
	   :direction :output :if-exists :supersede
	   :if-does-not-exist :create)
     ;; read .sh tmplate
     (with-open-file
      (fptmp "job_sh_templates:;panalyze_corpus_tmp.sh"
	     :direction :input)
      (loop (unless (setf line (read-line fptmp nil nil)) (return))
	    (cond
	     ((equalp "[setting]" line)
	      (format fpsh "csize=~a~%" cnt)
	      (format fpsh "cname=~a~%" corp-name)
	      (format fpsh "balp=~a~%" (if pre-lp? "pre_lp" "lp"))
	      (format fpsh "qtype=~a~%" qtype))
	     (t
	      (format fpsh "~a~%" line))))))
    
    (format t "chmod a+x *.cl in ~a~%" cl-ldir)
    (cmd-verbose "chmod a+x *.cl" :dir cl-ldir)
    (format t "chmod a+x *.sh in jobs:;")
    (cmd-verbose "chmod a+x *.sh" :dir "jobs:;")))

(defun output-data-sql (fnsql)
  (with-open-file (fsql fnsql :direction :output :if-exists :supersede
			:if-does-not-exist :create :external-format :default)
		  (format fsql "~a" *data-sql*)))

(defun output-ft-sql (fnsql)
  (with-open-file (fsql fnsql :direction :output :if-exists :supersede
			:if-does-not-exist :create)
		  (format fsql "~a" *ft-sql*)))

(defun psg-pn-matrix (nsg ; number of subgraphs in a batch
		      &key
		      (job-ldir "jobs:;psg_pn_matrix;")
		      (sg-ldir "gaston:;lgcorp_train_hier;")
		      (qtype "short")
		      &aux
		      work-ldir mat-ldir)
  (setf work-ldir "late:;psg_pn_matrix;")
  (setf mat-ldir (format nil "~amatrix;" sg-ldir))
  (let* ((idx 0)
	 (cnt 0)
	 (gstr (outstr-init))
	 (fnsg (format nil "~algcorp_train_hier.out" sg-ldir))
	 fncl fnlsf line fnsh  fnpsg)
    
    (format t "deleting old job files~%")
    (cmd-verbose "rm -rf psg_pn_*" :dir job-ldir)
    (format t "deleting old .cl files~%")
    (cmd-verbose "rm -rf psg_pn_* *core*" :dir work-ldir)
    (format t "deleting old output files~%")
    (cmd-verbose "rm -rf *.mat *.vsg *.sg" :dir mat-ldir)
    
    (with-open-file
     (fsg fnsg :direction :input)
     (loop (unless (setf line (read-ne-line fsg)) (return))
	   (when (match-re "^#" line)
	     (when (and (= 0 (rem cnt nsg)) (< 0 (length gstr)))
	       (incf idx)
	       (setf fnpsg (format nil "~a~a.sg" mat-ldir idx))
	       (with-open-file
		(fpsg fnpsg :direction :output
		      :if-exists :supersede :if-does-not-exist :create)
		(format fpsg "~a" gstr))
	       (setf gstr (outstr-init)))
	     (incf cnt))
	   
	   (format gstr "~a~%" line))
     
     (when (> (length gstr) 0)
       (incf idx)
       (setf fnpsg (format nil "~a~a.sg" mat-ldir idx))
       (with-open-file (fpsg fnpsg :direction :output
			     :if-exists :supersede :if-does-not-exist :create)
		       (format fpsg "~a" gstr))))
    (setf cnt idx)
    (setf idx 0)
    (loop (unless (<= idx cnt) (return))
	  (incf idx)
	  (setf fncl (format nil "~apsg_pn_matrix_~a.cl" work-ldir idx))
	  (setf fnlsf (format nil "~apsg_pn_matrix_~a.lsf" job-ldir idx))
	  
	  ;; output .cl files
	  (with-open-file
	   (fpcl fncl
		 :direction :output :if-exists :supersede
		 :if-does-not-exist :create)
	   ;; read .cl tmplate.
	   (with-open-file
	    (fptmp "late:;psg_pn_matrix_tmp.cl" :direction :input)
	    (loop (unless (setf line (read-line fptmp nil nil)) (return))
		  (cond
		   ((equalp "[exe]" line)
		    (format fpcl "#! ~a/mlisp -#D~%" (get-env "ACL_HOME")))
		   ((equalp "[setting]" line)
		    (format fpcl "(defvar idx ~a)~%" idx)
		    (format fpcl "(defvar mat-ldir \"~a\")~%" mat-ldir)
		    (format fpcl "(defvar sg-ldir \"~a\")~%" sg-ldir))
		   (t
		    (format fpcl "~a~%" line))))))
	  
	  ;; output .lsf files
	  (with-open-file
	   (fplsf fnlsf
		  :direction :output :if-exists :supersede
		  :if-does-not-exist :create)
	   ;; read lsf tmplate.
	   (with-open-file
	    (fptmp "lsf_templates:;psg_pn_matrix_tmp.lsf" 
		   :direction :input)
	    (loop (unless (setf line (read-line fptmp nil nil)) (return))
		  (cond
		   ((equalp "[setting]" line)
		    (format fplsf "workdir=~a~%" 
			    (translate-logical-pathname work-ldir))
		    (format fplsf "jid=~a~%" idx))
		   (t
		    (format fplsf "~a~%" line))))))
	  
	  (cmd-verbose (format nil "chmod a+x *.cl") :dir work-ldir))
    
    ;; read .sh tmplate and output .sh script
    (setf fnsh (format nil "jobs:;psg_pn_matrix.sh"))
    (setf fnsh (translate-logical-pathname fnsh))
    (with-open-file
     (fpsh fnsh
	   :direction :output :if-exists :supersede
	   :if-does-not-exist :create)
     (with-open-file (fptmp "job_sh_templates:;psg_pn_matrix_tmp.sh"
			    :direction :input)
		     (loop (unless (setf line (read-line fptmp nil nil)) (return))
			   (cond
			    ((equalp "[setting]" line)
			     (format fpsh "nscripts=~a~%" idx)
			     (format fpsh "qtype=~a~%" qtype))
			    (t
			     (format fpsh "~a~%" line))))))
    
    (format t "chmod a+x ~a" fnsh)
    (cmd-verbose (format nil "chmod a+x ~a" fnsh))))


(defun ppivot-analysis (sigsub-type
			&key 
			(job-dirln "jobs:;ppivot_analysis;")
			(pivot-dirln "late:;ppivot_analysis;")
			(qtype "medium")
			&aux job-dirn pivot-dirn work-ldir)
  (setf work-ldir "late:;ppivot_analysis;")
  (latesql "delete from sigsub_property")
  (let* ((sigsub-ids (get-graphs sigsub-type))
	 (c-size (length sigsub-ids))
	 (subc-size (max *min-sc-size* (ceiling c-size *max-cores*)))
	 (njobs (ceiling c-size subc-size))
	 istart iend sg-batch line fnlsf fncl fnsh)
    (setf job-dirn (translate-logical-pathname job-dirln))
    (setf pivot-dirn (translate-logical-pathname pivot-dirln))
    (format t "deleting old job files~%")
    (cmd-verbose (format nil "rm -rf ppivot_*") :dir job-dirn)
    (format t "deleting old .cl files~%")
    (cmd-verbose (format nil "rm -rf ppivot_*") :dir pivot-dirn)
    (format t "deleting old .piv files~%")
    (cmd-verbose (format nil "rm -rf *.piv") :dir pivot-dirn)
    (format t "deleting old core files~%")
    (cmd-verbose (format nil "rm -rf core.*") :dir pivot-dirn)	
    (format t "deleting old .log files~%")
    (cmd-verbose (format nil "rm -rf *.log") :dir pivot-dirn)	
    
    
    (dotimes (cnt njobs)
      (setf istart (* subc-size cnt))
      (setf iend (min (+ istart subc-size) c-size))
      (setf sg-batch (subseq sigsub-ids istart iend))
      (setf fncl (format nil "~appivot_~a.cl" pivot-dirn (1+ cnt)))
      (setf fnlsf (format nil "~appivot_~a.lsf" job-dirn (1+ cnt)))
      
      ;; output .cl file
      (with-open-file (fpcl fncl
			    :direction :output :if-exists :supersede
			    :if-does-not-exist :create)
		      ;; read .cl tmplate.
		      (with-open-file (fptmp "late:;ppivot_tmp.cl" 
					     :direction :input)
				      (loop (unless (setf line (read-line fptmp nil nil)) (return))
					    (cond
					     ((equalp "[exe]" line)
					      (format fpcl "#! ~a/mlisp -#D~%" (get-env "ACL_HOME")))
					     ((equalp "[job]" line)
					      (dolist (sigsub-id sg-batch)
						(format fpcl "(pivot-analysis ~a \"~a~a.piv\")~%" 
							sigsub-id pivot-dirln sigsub-id)))
					     (t
					      (format fpcl "~a~%" line))))))
      

      ;; output .lsf file
      (with-open-file (fplsf fnlsf
			     :direction :output :if-exists :supersede
			     :if-does-not-exist :create)
		      ;; read lsf tmplate.
		      (with-open-file (fptmp "lsf_templates:;ppivot_tmp.lsf" 
					     :direction :input)
				      (loop (unless (setf line (read-line fptmp nil nil)) (return))
					    (cond
					     ((equalp "[setting]" line)
					      (format fplsf "workdir=~a~%" 
						      (translate-logical-pathname work-ldir))
					      (format fplsf "jid=~a~%" (1+ cnt)))
					     (t
					      (format fplsf "~a~%" line)))))))
    
    
    (cmd-verbose (format nil "chmod a+x ~a*.cl" pivot-dirn))	  

    ;; read .sh tmplate and output .sh script
    (setf fnsh (format nil "jobs:;ppivot.sh"))
    (setf fnsh (translate-logical-pathname fnsh))
    (with-open-file (fpsh fnsh
			  :direction :output :if-exists :supersede
			  :if-does-not-exist :create)
		    (with-open-file (fptmp "job_sh_templates:;ppivot_tmp.sh"
					   :direction :input)
				    (loop (unless (setf line (read-line fptmp nil nil)) (return))
					  (cond
					   ((equalp "[setting]" line)
					    (format fpsh "nscripts=~a~%" njobs)
					    (format fpsh "qtype=~a~%" qtype))
					   (t
					    (format fpsh "~a~%" line))))))
    
    (format t "chmod a+x ~a" fnsh)
    (cmd-verbose (format nil "chmod a+x ~a" fnsh))))

(defun pconcept-graph-map (corp-name 
			   &key 
			   (job-dirln "jobs:;pconcept_graph_map;")
			   (cg-dirln "late:;lgcorp_test_hier;")
			   (qtype "short") 
			   (gtype "plain_graph|parse-node-stanford-hier-tagged")
			   &aux job-dirn cg-dirn work-ldir)
  "Generate scripts to parallel process corpus.
Input
======
excl-tuis: name of the hash table for excluded tuis
qtype: normal, medium or long"
  (setf work-ldir "late:;")
  (setf job-dirn (translate-logical-pathname job-dirln))
  ;; translate from logical pathname to physical name, cross platform
  (setf cg-dirn (translate-logical-pathname cg-dirln))
  (cmd-verbose (format nil "rm -rf *") :dir job-dirn)
  (cmd-verbose (format nil "rm -rf *") :dir cg-dirn)
  
  (let* ((c-size (length (documents (corpus corp-name))))
	 (subc-size (max *min-sc-size* (ceiling c-size *max-cores*)))
	 (idx 0)
	 (cnt 0)
	 line fnlg fnlsf fnsh fnsql)
    (loop (unless (< idx c-size) (return))
	  (incf cnt)
	  (setf fnlg (format nil "~a~a_~a.txt" cg-dirn corp-name cnt))
	  (setf fnsql (format nil "~a~a_~a.sql" cg-dirn corp-name cnt))
	  (setf fnlsf (format nil "~apcg_map_~a_~a.lsf" job-dirn corp-name cnt)) 
	  ;; output individual .lsf file that calls python scripts
	  (with-open-file (fplsf fnlsf
				 :direction :output :if-exists :supersede
				 :if-does-not-exist :create)
			  ;; read lsf tmplate.
			  (with-open-file (fptmp "lsf_templates:;pconcept_graph_map_tmp.lsf" 
						 :direction :input)
					  (loop (unless (setf line (read-line fptmp nil nil)) (return))
						(cond
						 ((equalp "[setting]" line)
						  (format fplsf "workdir=~a~%" 
							  (translate-logical-pathname work-ldir))
						  (format fplsf "fnin=~a~%" fnlg)
						  (format fplsf "fnout=~a~%" fnsql))
						 (t
						  (format fplsf "~a~%" line))))))
	  (output-corpus-concept-graphs fnlg corp-name :gtype gtype :cstart idx
					:cend (min (+ idx subc-size) c-size))
	  (setf idx (min (+ idx subc-size) c-size)))
    
    ;; read .sh tmplate and output .sh script
    (setf fnsh (format nil "jobs:;pcg_map_~a.sh" corp-name))
    (setf fnsh (translate-logical-pathname fnsh))
    (with-open-file (fpsh fnsh
			  :direction :output :if-exists :supersede
			  :if-does-not-exist :create)
		    (with-open-file (fptmp "job_sh_templates:;pconcept_graph_map_tmp.sh"
					   :direction :input)
				    (loop (unless (setf line (read-line fptmp nil nil)) (return))
					  (cond
					   ((equalp "[setting]" line)
					    (format fpsh "nscripts=~a~%" cnt)
					    (format fpsh "qtype=~a~%" qtype)
					    (format fpsh "corpus=~a~%" corp-name))
					   (t
					    (format fpsh "~a~%" line))))))

    (format t "chmod a+x ~a" fnsh)
    (cmd-verbose (format nil "chmod a+x ~a" fnsh))))

(defun p-featuregen-subset (instset-name start end
					 &key (mode nil))
  (let* ((l-inst (instances instset-name)))
    (dolist (inst-id-name (subseq l-inst start end))
      (let* ((inst-id (first inst-id-name))
	     (inst-name (second inst-id-name))
	     fv)
	(unless (inst-featuregen-p inst-id)
	  (setf fv (convert-feature-vector-db inst-name :mode mode))
	  (if fv
	      (save-file-inst-feature inst-id fv)
	    (error "no fv for ~a: ~a" mode inst-name)))))))

;; just call it multiple times for pos-train, neg-train, pos-test, neg-test
;; of course, you need to combine a train corpus, test corpus
(defun p-featuregen (corp-name 
		     &key 
		     (mode nil) (qtype "short")
		     (job-dirln "jobs:;p_featuregen;")
		     (ftgen-dirln "late:;p_featuregen;")
		     (ft-dirln "late:;tmp_ft_sql;")
		     (fn-pcfg "late:;lymph-pconfig.cl")
		     (ml-spec "late:;ml-spec")
		     &aux 
		     job-dirn ftgen-dirn work-ldir ft-dirn)
  "corpus really means instance set here."
  (setf job-dirn (translate-logical-pathname job-dirln))
  (setf ftgen-dirn (translate-logical-pathname ftgen-dirln))
  (setf ft-dirn (translate-logical-pathname ft-dirln))
  ;; remove previous job scripts 
  (cmd-verbose (format nil "rm -rf p_featuregen_~a*" corp-name) :dir job-dirn)
  (cmd-verbose (format nil "rm -rf *") :dir ft-dirn)
  (setf work-ldir "late:;p_featuregen;")
  (cmd-verbose (format nil "rm -rf p_featuregen_~a*" corp-name) :dir work-ldir)
  
  
  (format t "~&feature gen scripts: ~a~%" corp-name)
  (let* ((l-inst (instances corp-name))
	 (c-size (length l-inst))
	 (subc-size (max *min-sc-size* (ceiling c-size *max-cores*)))
	 (idx 0)
	 (cnt 0)
	 line fnsh fncl fnlsf)
    (loop (unless (< idx c-size) (return))
	  ;; only do feature gen on new instance
	  (incf cnt)
	  (setf fncl (format nil "~ap_featuregen_~a_~a.cl" 
			     ftgen-dirn corp-name cnt))
	  (setf fnlsf (format nil "~ap_featuregen_~a_~a.lsf" 
			      job-dirn corp-name cnt))
	  ;; output .cl file
	  (with-open-file (fpcl fncl
				:direction :output :if-exists :supersede
				:if-does-not-exist :create)
			  ;; read .cl tmplate.
			  (with-open-file (fptmp "late:;p_featuregen_tmp.cl" 
						 :direction :input)
					  (loop (unless (setf line (read-line fptmp nil nil)) (return))
						(cond
						 ((equalp "[exe]" line)
						  (format fpcl "#! ~a/mlisp -#D~%" (get-env "ACL_HOME")))
						 ((equalp "[setting]" line)
						  (format fpcl "(defparameter ftgen-corp-name ~s)~%" corp-name)
						  (format fpcl "(setf job-id ~a)~%" cnt)
						  (format fpcl "(setf fn-pcfg ~s)~%" fn-pcfg)
						  (format fpcl "(setf ml-spec ~s)~%" ml-spec)
						  (format fpcl "(defparameter ftgen-mode ~s)~%" mode)
						  (format fpcl "(defparameter ftgen-start ~s)~%" idx)
						  (format fpcl "(defparameter ftgen-end ~s)~%" 
							  (min (+ idx subc-size) c-size)))
						 (t
						  (format fpcl "~a~%" line))))))
	  ;; output .lsf file
	  (with-open-file (fplsf fnlsf
				 :direction :output :if-exists :supersede
				 :if-does-not-exist :create)
			  ;; read lsf tmplate.
			  (with-open-file (fptmp "lsf_templates:;p_featuregen_tmp.lsf" 
						 :direction :input)
					  (loop (unless (setf line (read-line fptmp nil nil)) (return))
						(cond
						 ((equalp "[setting]" line)
						  (format fplsf "workdir=~a~%" 
							  (translate-logical-pathname work-ldir))
						  (format fplsf "corpus=~a~%" corp-name)
						  (format fplsf "jid=~a~%" cnt))
						 (t
						  (format fplsf "~a~%" line))))))
	  (setf idx (min (+ idx subc-size) c-size)))
    (cmd-verbose (format nil "chmod a+x ~a*.cl" ftgen-dirn))
    
    ;; read .sh tmplate and output .sh script
    (setf fnsh (format nil "jobs:;p_featuregen_~a.sh" corp-name))
    (setf fnsh (translate-logical-pathname fnsh))
    (with-open-file (fpsh fnsh
			  :direction :output :if-exists :supersede
			  :if-does-not-exist :create)
		    (with-open-file (fptmp "job_sh_templates:;p_featuregen_tmp.sh"
					   :direction :input)
				    (loop (unless (setf line (read-line fptmp nil nil)) (return))
					  (cond
					   ((equalp "[setting]" line)
					    (format fpsh "nscripts=~a~%" cnt)
					    (format fpsh "qtype=~a~%" qtype)
					    (format fpsh "corpus=~a~%" corp-name))
					   (t
					    (format fpsh "~a~%" line))))))
    
    (format t "chmod a+x ~a" fnsh)
    (cmd-verbose (format nil "chmod a+x ~a" fnsh))))




(defun filtered-corpus-gen (fn-filter corpn old-dir new-dir 
				      &aux ln h-filter)
  (setf h-filter (make-hash-table :test #'equalp))
  (with-open-file (f-filter fn-filter :direction :input)
		  (loop (unless (setf ln (read-line f-filter nil nil)) (return))
			(setf (gethash ln h-filter) 1)))
  
  (dolist (docid (documents (corpus corpn)))
    (let* ((doc (document docid))
	   (docn (name doc))
	   fromn ton)
      (format t "scanning ~a, " docn)
      (when (gethash docn h-filter)
	(setf fromn (format nil "~a~a" old-dir docn))
	(setf ton (format nil "~a~a" new-dir docn))
	(format t "copying ~a~%" docn)
	(excl.osi:command-output (format nil "cp ~a ~a" fromn ton))))))



(defun output-mallet-dir (instset mallet-dir)
  "Output instances as text files into directories (dir name as label) for mallet."
  (let* ((l-inst (instances instset))
	 mrn docs fnout)
    (dolist (inst-id-name l-inst)
      (setf mrn (second inst-id-name))
      (setf docs (mapcar #'document (mrn->docids mrn)))
      (setf fnout (format nil "~amrn_~a" mallet-dir mrn))
      (with-open-file (fout fnout :direction :output
			    :if-exists :supersede :if-does-not-exist :create)
		      (dolist (doc docs)
			(dolist (sen (annotations doc :type 'sentence-annotation))
			  (dolist (pnode (annotations sen 
						      :type (gtype 'ghier-parse-node-type)))
			    (unless (maskedp pnode)
			      (format fout "~a " (content pnode)))
			    (format fout "~%"))))))))

;;; debugging utility 
(defun re-import-mrn (mrn &aux doc cname)
  (dolist (doc-id (mrn->docids mrn))
    (setf doc (document doc-id))
    (setf cname (name (car (stable-sort (corpora doc) #'< :key #'id))))
    (re-import-doc (name doc) cname)))

(defun update-corpus-doc-content (corpn)
  (let* ((docids (documents (corpus corpn))))
    (mapcar #'update-doc-content docids)))




(defun find-sen-in-corp (corpn pat &aux doc ans)
  ;; (setf h-stmp (read-hash "late:;sent.templ"))
  ;; used to detect template pattern
  (dolist (mrn (mapcar #'second (instances corpn)))
    (dolist (docid (mrn->docids mrn))
      (setf doc (document docid))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(when (and (match-re pat (content sen))
		   (annotations-spec sen :type (gtype 'gtoken-type))
		   ;; (not (gethash (id sen) h-stmp))
		   )
	  (format t "~&mrn: ~a ~a(~a): ~a~%" mrn (name doc) (id sen) (content sen))
	  (push sen ans)))))
  (format t "~%~%total: ~a~%" (length ans))
  ans)

(defun find-doc-in-corp (corpn pats &aux doc ans)
  ;; (setf h-stmp (read-hash "late:;sent.templ"))
  (dolist (mrn (mapcar #'second (instances corpn)))
    (dolist (docid (mrn->docids mrn))
      (setf doc (document docid))
      (let* ((m? t))
	(dolist (pat pats)
	  (unless (match-re pat (content doc))
	    (setf m? nil)))
	(when m?
	  (format t "~&mrn: ~a ~a~%" mrn (name doc) )
	  (pushnew mrn ans :test #'equalp)))))
  (format t "~%~%total: ~a~%" (length ans))
  ans)


(defun lymphoma-class (mrn)
  ;; shows how one can organize train and test data
  (let* ((bptr (mrns "burkitts_positive_train"))
	 (bpte (mrns "burkitts_positive_test"))
	 (dptr (mrns "dlbcl_positive_train"))
	 (dpte (mrns "dlbcl_positive_test"))
	 (fptr (mrns "follicular_positive_train"))
	 (fpte (mrns "follicular_positive_test"))
	 (hptr (mrns "hodgkins_positive_train"))
	 (hpte (mrns "hodgkins_positive_test"))
	 (bp (union bptr bpte :test #'equalp))
	 (dp (union dptr dpte :test #'equalp))
	 (fp (union fptr fpte :test #'equalp))
	 (hp (union hptr hpte :test #'equalp))
	 class)
    
    (when (member mrn bp :test #'equalp)
      (push "burkitt" class))
    (when (member mrn dp :test #'equalp)
      (push "dlbcl" class))
    (when (member mrn fp :test #'equalp)
      (push "follicular" class))
    (when (member mrn hp :test #'equalp)
      (push "hodgkins" class))
    class))

(defun train-test-cnt ()
  (let* ((bptr (mrns "burkitts_positive_train"))
	 (bpte (mrns "burkitts_positive_test"))
	 (dptr (mrns "dlbcl_positive_train"))
	 (dpte (mrns "dlbcl_positive_test"))
	 (fptr (mrns "follicular_positive_train"))
	 (fpte (mrns "follicular_positive_test"))
	 (hptr (mrns "hodgkins_positive_train"))
	 (hpte (mrns "hodgkins_positive_test"))
	 (tr-mrns (multisets-union (list bptr dptr fptr hptr) :test #'equalp))
	 (te-mrns (multisets-union (list bpte dpte fpte hpte) :test #'equalp)))
    (assert (not (intersection tr-mrns te-mrns :test #'equalp))
	    ()
	    "train and test intersection ~a~%" 
	    (intersection tr-mrns te-mrns :test #'equalp))
    (format t "train: ~a; test ~a~%" (length tr-mrns) (length te-mrns))))

(defun lymphoma-split (mrn)
  (let* ((bptr (mrns "burkitts_positive_train"))
	 (bpte (mrns "burkitts_positive_test"))
	 (dptr (mrns "dlbcl_positive_train"))
	 (dpte (mrns "dlbcl_positive_test"))
	 (fptr (mrns "follicular_positive_train"))
	 (fpte (mrns "follicular_positive_test"))
	 (hptr (mrns "hodgkins_positive_train"))
	 (hpte (mrns "hodgkins_positive_test"))
	 (tr-mrns (multisets-union (list bptr dptr fptr hptr) :test #'equalp))
	 (te-mrns (multisets-union (list bpte dpte fpte hpte) :test #'equalp)))
    (assert (not (intersection tr-mrns te-mrns :test #'equalp))
	    ()
	    "train and test intersection ~a~%" 
	    (intersection tr-mrns te-mrns :test #'equalp))
    (cond
     ((member mrn tr-mrns :test #'equalp)
      "train")
     ((member mrn te-mrns :test #'equalp)
      "test")
     (t
      "missing"))))

(defun lymphoma-split-sanity ()
  (let* ((bptr (mrns "burkitts_positive_train"))
	 (bpte (mrns "burkitts_positive_test"))
	 (dptr (mrns "dlbcl_positive_train"))
	 (dpte (mrns "dlbcl_positive_test"))
	 (fptr (mrns "follicular_positive_train"))
	 (fpte (mrns "follicular_positive_test"))
	 (hptr (mrns "hodgkins_positive_train"))
	 (hpte (mrns "hodgkins_positive_test"))
	 ;; negative
	 (bntr (mrns "burkitts_negative_train"))
	 (bnte (mrns "burkitts_negative_test"))
	 (dntr (mrns "dlbcl_negative_train"))
	 (dnte (mrns "dlbcl_negative_test"))
	 (fntr (mrns "follicular_negative_train"))
	 (fnte (mrns "follicular_negative_test"))
	 (hntr (mrns "hodgkins_negative_train"))
	 (hnte (mrns "hodgkins_negative_test"))
	 ;; train
	 (btr (union bptr bntr :test #'equalp))
	 (dtr (union dptr dntr :test #'equalp))
	 (ftr (union fptr fntr :test #'equalp))
	 (htr (union hptr hntr :test #'equalp))
	 ;; test
	 (bte (union bpte bnte :test #'equalp))
	 (dte (union dpte dnte :test #'equalp))
	 (fte (union fpte fnte :test #'equalp))
	 (hte (union hpte hnte :test #'equalp)))
    
    (format t "~&train sizes: btr(~a), dtr(~a), ftr(~a), htr(~a)~%"
	    (length btr) (length dtr) (length ftr) (length htr))
    (format t "~&test sizes: bte(~a), dte(~a), fte(~a), hte(~a)~%"
	    (length bte) (length dte) (length fte) (length hte))))

(defun lymphoma-classes (mrns)
  (let* ((h-class (make-hash-table :test #'equalp))
	 class)
    (dolist (mrn mrns)
      (setf class (lymphoma-class mrn))
      (format t "~&~a: ~a~%" mrn class)
      (incf (gethash class h-class 0)))
    (dolist (kv (hash-table-val-desc-alist h-class))
      (let* ((class (car kv))
	     (cnt (cdr kv)))
	(format t "~&~a: ~a~%" class cnt)))))


(defun multi-lymphoma-mrns (&aux hmulti)
  (setf hmulti (make-hash-table :test #'equalp))
  (dolist (mrn (mrns "MRN instance set"))
    (let* ((class (lymphoma-class mrn)))
      (when (< 1 (length class))
	(format t "~&~a (~a): ~a~%" mrn (lymphoma-split mrn) class)
	(incf (gethash class hmulti 0)))))
  (dolist (kv (hash-table-val-desc-alist hmulti))
    (format t "~a: ~a~%" (car kv) (cdr kv))))



(defun case-length-check (window)
  "window in days"
  (dolist (mrn (mrns "MRN instance set"))
    
    (let* (min-date max-date)
      (dolist (docid (mrn->docids mrn))
	(let* ((doc (document docid))
	       date)
	  (when (annotations doc :type 'section-annotation)
	    (setf date (doc-date doc))
	    (when (or (not min-date)
		      (> min-date date))
	      (setf min-date date))
	    (when (or (not max-date)
		      (< max-date date))
	      (setf max-date date)))))
      (unless (within-window? max-date min-date window)
	(format t "~&~a ~a ~a spans over ~a days~%" 
		(lymphoma-split mrn) (lymphoma-class mrn) mrn window)))))



