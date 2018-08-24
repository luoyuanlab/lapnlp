;;; -*- Mode: Lisp; Package: concept-graph; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 02/27/2013 creation
The purpose is to isolate peripheral utilities
Package to handle concept graph stats calculation and accessing.
|#

(defpackage :concept-graph
  (:nicknames :cg)
  (:use :common-lisp :late :link :wordnet :util :norm :umlsize 
	#+allegro :excl :dbi.mysql)
  (:export 
   "get-sigsub-entropy"
   "pnode-unit-stats"
   "sigsub-size"
   "pivot-analysis-batch"
   "entropy"
   "pivot-analysis"
   "subgraph-coverage"
   "corpus-longest-umls-stats"
   "clusters-coreset"
   "corpus-head-synsense-stats"
   "popular-stn"
   "popular-syns"
   "get-uncovered-pnode-head-cuis"
   "get-uncovered-pnode-content"
   "subgraph-tf-idf"
   "noun-pnode->cui-stats"
   ))
(in-package :concept-graph)

(defun entropy (p)
  (let* ((sum (reduce '+ p))
	 (p (mapcar #'(lambda (a) (/ a sum)) p))
	 (p (mapcar #'(lambda (a) (* a (log (/ 1 a) 2))) p)))
    (reduce '+ p)))

(defparameter *h-sg-entropy* (make-hash-table :test #'equalp))

(defun load-sub-entropy ()
  ;;; entropy-based feature selection, only for exploration
  (let* ((l-sub-ent (latesql "SELECT sub_id, npname, npval FROM sigsub_property"))
	 (hprop (make-hash-table :test #'equalp))
	 (bpnpt (length (mrns "burkitts_positive_train")))
	 (dpnpt (length (mrns "dlbcl_positive_train")))
	 (fpnpt (length (mrns "follicular_positive_train")))
	 (hpnpt (length (mrns "hodgkins_positive_train")))
	 lsg)
    (dolist (sub-n-v l-sub-ent)
      (let* ((sub (first sub-n-v))
	     (name (second sub-n-v))
	     (val (third sub-n-v))
	     (pkey (format nil "~a_~a" sub name)))
	(setf (gethash pkey hprop) val)
	
	(pushnew sub lsg)))

    (dolist (sgid lsg)
      (dolist (class '("burkitts" "dlbcl" "follicular" "hodgkins"))
	(let* ((pcnt (1+ (gethash (format nil "~a_~a_pos_cnt" sgid class) hprop)))
	       (ncnt (1+ (gethash (format nil "~a_~a_neg_cnt" sgid class) hprop)))
	       ent)
	  (cond
	   ((equalp class "burkitts")
	    (setf pcnt (* pcnt (/ (+ dpnpt fpnpt hpnpt) bpnpt))))
	   ((equalp class "dlbcl")
	    (setf ncnt (* ncnt (/ dpnpt (+ bpnpt fpnpt hpnpt)))))
	   ((equalp class "follicular")
	    (setf pcnt (* pcnt (/ (+ bpnpt dpnpt hpnpt) fpnpt))))
	   ((equalp class "hodgkins")
	    (setf pcnt (* pcnt (/ (+ bpnpt dpnpt fpnpt) hpnpt)))))
	  
	  (setf ent (entropy (list pcnt ncnt)))
	  (setf (gethash (format nil "~a_~a" sgid class) *h-sg-entropy*) ent))))))

(defun get-sigsub-entropy (sub-id class)
  (when (= 0 (hash-table-count *h-sg-entropy*))
    (load-sub-entropy))
  (gethash (format nil "~a_~a" sub-id class) *h-sg-entropy*))



(defmemo sigsub-size (sub-id)
  (caar (latesql "SELECT count(*) FROM sig_subgraph WHERE sub_id=~d AND type='node'" sub-id)))

(defun pnode-unit-stats (out-fn
			 &key (thold 200))
  (with-open-file (out-f out-fn
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
		  (dolist (iter (latesql "SELECT DISTINCT lab, COUNT(*) FROM linkage_graph WHERE type='node' GROUP BY lab HAVING COUNT(*)>=~a ORDER BY COUNT(*) DESC" thold))
		    (let* ((lab (first iter))
			   (cnt (second iter))
			   nsets pn-str sty-rl)
		      (dolist (pnid (mapcar #'car (latesql "SELECT pn1 FROM linkage_graph WHERE lab=~a" (sq lab))))
			(setf pn-str (caar (latesql "SELECT data FROM annotations WHERE id=~a" pnid)))
			(pushnew (string-downcase pn-str) nsets :test #'equalp))
		      (cond 
		       ((match-re "^(A|B)\\d\." lab)
			(setf sty-rl (mapcar #'stn->sty-rl (split-re "_" lab))))
		       (t
			(setf sty-rl nil)))
		      (format out-f "~a ~@[(~{~a~^| ~})~] || ~a || ~{~a~^| ~}~%" lab sty-rl cnt nsets)))))


(defun pivot-analysis-batch (sigsub-type 
			     &aux (h-sen (make-hash-table :test #'equalp)))
  (dolist (sigsub-id (get-graphs sigsub-type))
    (let* ((sigsub-supp (get-sigsub-support sigsub-id))
	   (sigsub-size (sigsub-size sigsub-id)))
      (clrhash h-sen)
      (dolist (lg-id (get-mapped-linkage-graphs sigsub-id))
	(let* ((gname (get-graph-name lg-id))
	       (doc-sen (split-re "_" gname))
	       (sen-id (parse-integer (second doc-sen)))
	       (sen (get-annotation sen-id nil))
	       (sen-str (content sen))
	       (sen-str (replace-re sen-str "\\s+" " ")))
	  (incf (gethash sen-str h-sen 0))))
      (format t "~&pivot analysis for significant subgraph ~a (size: ~a, support: ~a):~%" 
	      sigsub-id sigsub-size sigsub-supp)
      (dolist (sen-cnt (hash-table-val-desc-alist h-sen))
	(format  t "~&sen (~a): ~a~%" (cdr sen-cnt) (car sen-cnt)))
      (format t "~&---------------------------------------~%~%"))))

(defun add-sg-prop (sg-id name val)
  (latesql "INSERT INTO sigsub_property(sub_id, npname, npval) VALUES (~a, ~a, ~a)"
	   (sq sg-id) (sq name) (sq val)))

(defun pivot-analysis (sigsub-id fnout
				 &aux (h-sen (make-hash-table :test #'equalp)))
  "TODO: add entropy calculation"
  (with-open-file (fout fnout
			:direction :output :if-exists :supersede
			:if-does-not-exist :create)
		  (let* ((sigsub-supp (get-sigsub-support sigsub-id))
			 (sigsub-size (sigsub-size sigsub-id))
			 (bptr (mrns "burkitts_positive_train"))
			 (dptr (mrns "dlbcl_positive_train"))
			 (fptr (mrns "follicular_positive_train"))
			 (hptr (mrns "hodgkins_positive_train"))
			 (ltr-mrn (union bptr dptr :test #'equalp))
			 (ltr-mrn (union ltr-mrn fptr :test #'equalp))
			 (ltr-mrn (union ltr-mrn hptr :test #'equalp))
			 ;; (bpnpt (length bptr))
			 ;; (dpnpt (length dptr))
			 ;; (fpnpt (length fptr))
			 ;; (hpnpt (length hptr))
			 bpl dpl fpl hpl bpcnt dpcnt fpcnt hpcnt 
			 bnl dnl fnl hnl bncnt dncnt fncnt hncnt
			 l-sen-cnt)
		    (clrhash h-sen)
		    (dolist (lgid-name (sigsub-mapped-lgid-name sigsub-id))
		      (let* ((gname (second lgid-name))
			     (doc-sen (split-re "_" gname))
			     (docid (parse-integer (first doc-sen)))
			     (sen-id (parse-integer (second doc-sen)))
			     (doc (document docid))
			     (mrn (doc-mrn doc))
			     (sen (find-annotation sen-id doc))
			     (sen-str (content sen))
			     (sen-str (replace-re sen-str "\\s+" " ")))
			(incf (gethash sen-str h-sen 0))
			
			(cond
			 ((member mrn bptr :test #'equalp)
			  (pushnew mrn bpl :test #'equalp))
			 ((member mrn ltr-mrn :test #'equalp)
			  (pushnew mrn bnl :test #'equalp)))

			(cond
			 ((member mrn dptr :test #'equalp)
			  (format t "dlbcl mrn: ~a~%" mrn)
			  (pushnew mrn dpl :test #'equalp))
			 ((member mrn ltr-mrn :test #'equalp)
			  (pushnew mrn dnl :test #'equalp)))
			
			(cond
			 ((member mrn fptr :test #'equalp)
			  (pushnew mrn fpl :test #'equalp))
			 ((member mrn ltr-mrn :test #'equalp)
			  (pushnew mrn fnl :test #'equalp)))
			
			(cond
			 ((member mrn hptr :test #'equalp)
			  (format t "hodgkin mrn: ~a~%" mrn)
			  (pushnew mrn hpl :test #'equalp))
			 ((member mrn ltr-mrn :test #'equalp)
			  (pushnew mrn hnl :test #'equalp)))))

		    (setf bpcnt (length bpl) bncnt (length bnl))
		    (setf dpcnt (length dpl) dncnt (length dnl))
		    (setf fpcnt (length fpl) fncnt (length fnl))
		    (setf hpcnt (length hpl) hncnt (length hnl))
		    

		    (format fout "~&pivot analysis for significant subgraph ~a (size: ~a, support: ~a):~%" 
			    sigsub-id sigsub-size sigsub-supp)
		    
		    (format t "~&burkitts: ~a; dlbcl: ~a; follicular: ~a; hodgkins ~a~%"
			    bpcnt dpcnt fpcnt hpcnt)
		    (format t "~&!burkitts: ~a; !dlbcl: ~a; !follicular: ~a; !hodgkins ~a~%"
			    bncnt dncnt fncnt hncnt)

		    (setf l-sen-cnt (hash-table-val-desc-alist h-sen))

		    (latesql "delete from sigsub_property where sub_id=~a"
			     (sq sigsub-id))
		    (add-sg-prop sigsub-id "burkitts_pos_cnt" bpcnt)
		    (add-sg-prop sigsub-id "burkitts_neg_cnt" bncnt)

		    (add-sg-prop sigsub-id "dlbcl_pos_cnt" dpcnt)
		    (add-sg-prop sigsub-id "dlbcl_neg_cnt" dncnt)
		    
		    (add-sg-prop sigsub-id "follicular_pos_cnt" fpcnt)
		    (add-sg-prop sigsub-id "follicular_neg_cnt" fncnt)

		    (add-sg-prop sigsub-id "hodgkins_pos_cnt" hpcnt)
		    (add-sg-prop sigsub-id "hodgkins_neg_cnt" hncnt)
		    
		    (dolist (sen-cnt l-sen-cnt)
		      (format fout "~&sen (~a): ~a~%" (cdr sen-cnt) (car sen-cnt)))
		    (format fout "~&---------------------------------------~%~%"))))


(defun subgraph-coverage (fnsubs)
  (let* (subid lgs ln coverage fsubs)
    (setf fsubs (open fnsubs :direction :input))
    (loop (unless (setf ln (read-ne-line fsubs)) (return))
	  (setf subid (subname->id ln))
	  (setf lgs (get-lgs-of-sigsub subid))
	  (setf coverage (union coverage lgs :test #'equalp)))
    (format t "covers ~a sentences~%" (length coverage))
    (close fsubs)))

(defun corpus-longest-umls-stats (ncorp type)
  (dolist (doc-id (documents (corpus ncorp)))
    (format t "obtain stats on longest umls'es for doc ~a~%"
	    (name (document doc-id)))
    (dolist (ph (annotations-spec (document doc-id) 
				  :type (gtype 'gphrase-type)
				  :filter #'(lambda (a) 
					      (equalp (format nil "~a" (data a))
						      "NP"))))
      (phrase-longest-umlss ph :type type))))


(defun clusters-coreset (fncid fnsub fncore)
  (let* ((hcluster (make-hash-table :test #'equalp))
	 fcid fsub fcore cid substr)
    
    (setf fcid (open fncid :direction :input))
    (setf fsub (open fnsub :direction :input))
    (loop (unless (setf substr (read-ne-line fsub)) (return))
	  (setf cid (parse-integer (read-ne-line fcid)))
	  (push substr (gethash cid hcluster)))
    (close fcid)
    (close fsub)
    
    (setf fcore (open fncore :direction :output :if-exists :supersede
		      :if-does-not-exist :create))
    (maphash 
     #'(lambda (cid sgs)
	 (format fcore "cluster ~a~%" cid)
	 (let* (lgs core-sgs subname sub-id sub-lgs)
	   (dolist (substr sgs)
	     (let* ((m (match-re "^#(?<name>\\S+)\\s" substr :return :match)))
	       (setf subname (re-submatch m nil nil "name")))
	     (setf sub-id (subname->id subname))
	     (setf sub-lgs (get-lgs-of-sigsub sub-id))
	     (unless (subsetp sub-lgs lgs :test #'equalp)
	       (push substr core-sgs))
	     (setf lgs (union lgs sub-lgs :test #'equalp)))
	   
	   (format fcore "~{~a~%~}" core-sgs)))
     hcluster)
    (close fcore)))


(defun corpus-head-synsense-stats (ncorp &aux doc)
  (dolist (doc-id (documents (corpus ncorp)))
    (setf doc (document doc-id))
    (format t "obtain stats on ph head synsense for doc ~a~%"
	    (name doc))
    (dolist (ph (annotations-spec doc :type (gtype 'gphrase-type)
				  :filter #'(lambda (a) 
					      (member (format nil "~a" (data a))
						      (list "ADJP" "ADVP" "VP")
						      :test #'equalp))))
;;;      (when (member (content ph) (list "]" "}" ")") :test #'equalp)
;;;	(format t "~a problematic in ~a~%" ph (name (document doc-id))))
      (phrase-head-synsenses ph))))


(defun popular-stn (stns)
  ;; (format t "stns: ~a~%" stns)
  (let* ((sorted-stns (stable-sort stns 
				   #'(lambda (x y) (>= (umls-freq x "stn-7") 
						       (umls-freq y "stn-7"))))))
    (car sorted-stns)))

(defun popular-syns (word pos)
  (format t "word: ~a, pos: ~a, " word pos)
  (let* ((synsenses (stable-sort (synsensedef-ff word pos)
				 #'(lambda (x y) (>= (syns-freq x pos)
						     (syns-freq y pos))))))
    (format t "sense: ~a~%" (car synsenses))
    (car synsenses)))

(defmethod get-uncovered-pnode-head-cuis ((sen sentence-annotation)
					  &key (lg-type "plain_graph|parse-node-stanford-hier-tagged"))
  (let* ((lgid (get-linkage-graph sen lg-type))
	 (h-sigsubs (and lgid (get-sigsubs-of-lg lgid)))
	 (pnodes (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	 (cnt -1)
	 nids covered-nids uncovered-nids ans)
    (when h-sigsubs
      (setf nids (mapcar #'(lambda (a) (declare (ignorable a)) (incf cnt)) 
			 pnodes))
      (maphash #'(lambda (ls-map-id ls-map)
		   (declare (ignorable ls-map-id))
		   ;; (format t "~&ls-map-id: ~a~%" ls-map-id)
		   (maphash #'(lambda (subn lgn)
				(declare (ignorable subn))
				;; (format t "~&subn ~a; lgn ~a~%" subn lgn)
				(pushnew lgn covered-nids))
			    ls-map))
	       h-sigsubs)
      (setf uncovered-nids (set-difference nids covered-nids))
      ;; (format t "~&nids ~a~%covered: ~a~%uncovered: ~a~%" nids covered-nids uncovered-nids)
      (dolist (nid uncovered-nids)
	(let* ((pnode (elt pnodes nid))
	       (cuis (pnode-long-umlss pnode :type 'cui-annotation))
	       (head-cuis (last-anns cuis)))
	  (when (and (not (maskedp pnode))
		     (member (pnode-pos-tag pnode) *ptb-noun-list* :test #'equalp))
	    (dolist (cui head-cuis)
	      ;; (format t "~&~a~%" cui)
	      (push (pname (data cui)) ans)))))
      ans)))

(defmethod get-uncovered-pnode-content ((sen sentence-annotation)
					&key (lg-type "plain_graph|parse-node-stanford-hier-tagged"))
  (let* ((lgid (get-linkage-graph sen lg-type))
	 (h-sigsubs (and lgid (get-sigsubs-of-lg lgid)))
	 (pnodes (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	 (cnt -1)
	 nids covered-nids uncovered-nids ans)
    (when h-sigsubs
      (setf nids (mapcar #'(lambda (a) (declare (ignorable a)) (incf cnt)) 
			 pnodes))
      (maphash #'(lambda (ls-map-id ls-map)
		   (declare (ignorable ls-map-id))
		   ;; (format t "~&ls-map-id: ~a~%" ls-map-id)
		   (maphash #'(lambda (subn lgn)
				(declare (ignorable subn))
				;; (format t "~&subn ~a; lgn ~a~%" subn lgn)
				(pushnew lgn covered-nids))
			    ls-map))
	       h-sigsubs)
      (setf uncovered-nids (set-difference nids covered-nids))
      
      (dolist (nid uncovered-nids)
	(let* ((pnode (elt pnodes nid)))
	  (when (and (not (maskedp pnode))
		     (match-re "\\w" (content pnode)))
	    (push (replace-re (content pnode) "\\s+" " ") ans))))
      (nreverse ans))))

(defmethod get-uncovered-pnode-head-cuis 
  ((doc document)
   &key (lg-type "plain_graph|parse-node-stanford-hier-tagged"))
  (let* ((sens (annotations doc :type 'sentence-annotation)))
    (mapcan #'(lambda (s) (get-uncovered-pnode-head-cuis s :lg-type lg-type)) 
	    sens)))

(defmethod get-uncovered-pnode-content 
  ((doc document)
   &key (lg-type "plain_graph|parse-node-stanford-hier-tagged"))
  (let* ((sens (annotations doc :type 'sentence-annotation))
	 uncovered-pns sen-str)
    (dolist (sen sens)
      (setf uncovered-pns (get-uncovered-pnode-content sen :lg-type lg-type))
      (when uncovered-pns
	(setf sen-str (content sen))
	(setf sen-str (replace-re sen-str "\\s+" " "))
	(format t "~a {~a}:~% ~{[~a]~^ ~}~%~%" (id sen) sen-str uncovered-pns)))))

(defmethod get-uncovered-pnode-content 
  ((mrn string)
   &key (lg-type "plain_graph|parse-node-stanford-hier-tagged"))
  (let* ((docs (mapcar #'document (mrn->docids mrn))))
    (dolist (doc docs)
      (format t "doc ~a:~%" (name doc))
      (get-uncovered-pnode-content doc :lg-type lg-type))))

(defmethod get-uncovered-pnode-head-cuis ((corp corpus)
					  &key (lg-type "plain_graph|parse-node-stanford-hier-tagged"))
  (let* ((h-ucui (make-hash-table :test #'equalp))
	 doc)
    (dolist (docid (documents corp))
      (setf doc (document docid))
      (dolist (cui (get-uncovered-pnode-head-cuis doc :lg-type lg-type))
	(incf (gethash cui h-ucui 0))))
    (dolist (kv (hash-table-val-desc-alist h-ucui))
      (format t "~&~a: ~a~%" (cdr kv) (car kv)))))


(defun subgraph-tf-idf 
    (corpn
     &key (lg-type "plain_graph|parse-node-stanford-hier-tagged"))
  (let* ((h-sg-doc (make-hash-table))
	 (h-sg-tf (make-hash-table))
	 (h-sg-idf (make-hash-table))
	 (docids (documents (corpus corpn)))
	 (dtot (length docids))
	 doc lgid sg-map-ids sg-ids dtnum)
    (dolist (docid docids)
      (setf doc (document docid))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(setf lgid (get-linkage-graph sen lg-type))
	(when lgid
	  (setf sg-map-ids (hash-keys (get-sigsubs-of-lg lgid)))
	  (setf sg-ids (mapcar #'(lambda (a) 
				   (parse-integer (car (split-re "-" a))))
			       sg-map-ids))
	  ;; (setf sg-ids (remove-duplicates sg-ids))
	  (dolist (sg-id sg-ids)
	    (pushnew docid (gethash sg-id h-sg-doc))
	    (unless (gethash sg-id h-sg-tf)
	      (setf (gethash sg-id h-sg-tf) (make-hash-table)))
	    (incf (gethash docid (gethash sg-id h-sg-tf) 0))))))
    (dolist (kv (hash-table-alist h-sg-doc))
      (let* ((sg-id (car kv)))
	(setf dtnum (length (cdr kv)))
	(setf (gethash sg-id h-sg-idf) (log (/ dtot dtnum)))))
    (dolist (kv (hash-table-val-ascd-alist h-sg-idf))
      (let* ((sg-id (car kv)))
	(format t "~&~a: ~a~%" (cdr kv) (db-sgid->str sg-id))))

    (list h-sg-doc h-sg-tf h-sg-idf)))

(defun noun-pnode->cui-stats (corpn fncsv)
  #||hcui: key cui, val number of string
  hcui-unique: key cui, val number of unique string||#
  (let* ((hcui (make-hash-table :test #'equalp))
	 (hcui-unique (make-hash-table :test #'equalp))
	 doc pn-tag cuis head-cuis head-cui pnstr)
    (dolist (docid (documents (corpus corpn)))
      (setf doc (document docid))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(dolist (pn (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	  (setf pn-tag (get-pnode-pos-tag pn))
	  (when (ptb-nounp pn-tag)
	    (setf cuis (pnode-long-umlss pn :type 'cui-annotation))
	    (setf head-cuis (last-anns cuis))
	    (when head-cuis
	      (setf head-cui (data (car head-cuis)))
	      (incf (gethash head-cui hcui 0))
	      (setf pnstr (string-downcase (content pn)))
	      (setf pnstr (replace-re pnstr "\\s+" " "))
	      (pushnew pnstr (gethash head-cui hcui-unique) 
		       :test #'equalp))))))
    (let* ((f (open-new fncsv)))
      (dolist (kv (hash-table-val-desc-alist hcui))
	(let* ((cui (car kv))
	       (cnt (cdr kv))
	       (unique-cnt (length (gethash cui hcui-unique))))
	  ;; (mapcar #'stn->sty-rl (mapcar #'tui->stn (cui->tui cui)))
	  (format f "~&~a, ~a, ~a, ~a~%" cnt unique-cnt cui (pname cui))))
      (close f))))
