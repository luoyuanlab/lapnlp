;;; -*- Mode: Lisp; Package: concept-graph; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 02/27/2013 creation
The purpose is to isolate peripheral utilities
Package to handle concept graph reading.
|#

(defpackage :concept-graph
  (:nicknames :cg)
  (:use :common-lisp :late :link :wordnet :util :norm :umlsize 
	#+allegro :excl :dbi.mysql)
  (:export 
   "get-lg-adj-nodes"
   "get-lg-node-type"
   "get-lg-nodes"
   "get-lg-nonpunc-nodes"
   "get-lgs-of-sigsub"
   "get-linkage-graph"
   "get-nontrivial-sigsubs-of-lg"
   "get-sg-edges"
   "get-sg-nodes"
   "get-sigsub-entropy"
   "get-sigsub-ids"
   "get-sigsubs-of-lg"
   "get-singleton-lab"
   "get-singleton-lgs"
   "get-singleton-n1"
   "singleton?"
   "covered-by-non-singleton?"
   "get-subsub-ancester"
   "get-training-lgs-of-sigsub"
   "get-uncovered-pnode-head-cuis"
   "get-subsub-ancester"
   "h-get-sigsubs-of-lg"
   "*h-lg-sigsub*"
   "load-lg-sigsub"
   "lgn-type"
   "lgnid->pnid"
   "lgnid->row"
   "lg-modn-lab"
   "sigsub-covered-pnode-ids"
   "uncovered-lgs"
   "uncovered-sents"
   "import-rand-sg-pairs"
   "read-ef-sen-list"
   "get-subids-of-lg"
   "sg-freq"
   "subid->name"
   "*node-lab->norm*"
   "populate-node-lab->norm"
   "get-lg-senid"
   "noun-singleton?"
   ))
(in-package :concept-graph)

(defparameter *h-lg-sigsub* (make-hash-table :test #'equalp))
(defparameter *node-type* '("node" "node-modifier" "node-modifier-noun" "node-list-noun"))
(defparameter *mod-node-type* '("node-modifier" "node-modifier-noun"))

;;; nodes reader

(defun get-lg-adj-nodes (nid lgid)
  (union
   (mapcar #'car (latesql "SELECT n2 FROM linkage_graph WHERE
                          n1=~d AND lg_id=~d AND type='edge'"
			  (sq nid) (sq lgid)))
   (mapcar #'car (latesql "SELECT n1 FROM linkage_graph WHERE
                          n2=~d AND lg_id=~d AND type='edge'"
			  (sq nid) (sq lgid)))
   :test #'equalp))

(defun get-lg-node-type (nid lgid)
  (caar (latesql "SELECT type FROM linkage_graph WHERE
                    n1=~d AND lg_id=~d AND type like '%node'"
		 (sq nid) (sq lgid))))

(defmemo get-lg-nodes (lgid)
  (latesql "select n1, lab from linkage_graph where lg_id=~a and type ~a" (sq lgid) (sql-matcher *node-type*)))


(defun get-lg-nonpunc-nodes (lgid)
  "get non-punctuation nodes for linkage graph."
  (let* ((nodes (get-lg-nodes lgid)))
    (remove-if #'(lambda (a) (match-re "^\\W*$" (second a))) nodes)))

(defun lgn-type (nid lgid)
  (caar (latesql "SELECT type FROM linkage_graph WHERE n1=~d AND lg_id=~d AND type like 'node%'" nid lgid)))


(defmemo lgnid->pnid (nid lgid)
  ;; (format t "~&nid: ~a, lgid: ~a~%" nid lgid)
  (caar (latesql "SELECT pn1 FROM linkage_graph WHERE
                    n1=~d AND lg_id=~d AND type ~a"
		 nid lgid (sql-matcher *node-type*))))


(defun lgnid->row (nid lgid)
  (car (latesql "SELECT pn1, lab, type FROM linkage_graph WHERE
                    n1=~d AND lg_id=~d AND type ~a"
		nid lgid (sql-matcher *node-type*))))

(defun lg-modn-lab (nid lgid)
  (caar (latesql "SELECT lab FROM linkage_graph WHERE
                    n1=~d AND lg_id=~d AND type ~a"
		 nid lgid (sql-matcher *mod-node-type*))))

(defun sigsub-covered-pnode-ids (sen lg-type)
  "Covered nodes include both nodes in the sig subgraph and nodes masked."
  (let* ((doc (document sen))
	 (ans (make-hash-table :test #'equalp))
	 (pnodes (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	 lg-id lg-nodes)
    (setf lg-id (caar (latesql "SELECT gid FROM graph 
                                WHERE gname='~a_~a' AND gtype='~a'" 
			       (id doc) (id sen) lg-type)))
    (setf lg-nodes (latesql "SELECT DISTINCT lg_node FROM lg_sigsub 
                             WHERE lg_id='~a'"
			    lg-id))
    (setf lg-nodes (mapcar #'car lg-nodes))
    (dolist (nid lg-nodes)
      (setf (gethash (lgnid->pnid nid lg-id) ans) 1))
    (dolist (pnode pnodes)
      (when (maskedp pnode)
	(setf (gethash (id pnode) ans) 1)))
    ans))

;;; graph edges reader

(defmemo get-lg-edges (lgid)
  (latesql "select n1, n2, lab from linkage_graph where lg_id=~a and type='edge'" (sq lgid)))

;;; subgraph edges reader
(defun get-sg-nodes (subid)
  (latesql "select n1, lab from sig_subgraph where sub_id=~a and type like 'node%' order by n1" subid))

(defparameter *node-lab->norm* (make-hash-table :test #'equalp))
(defun populate-node-lab->norm ()
  (when (= 0 (hash-table-count *node-lab->norm*))
    (let* ((did-sen-n-labs (latesql "select document_id, sent, n1, lab from linkage_graph as lg join annotations as a on lg.sent=a.id where lg.type='node' order by document_id"))
	   (hlab-cnt (make-hash-table :test #'equalp))
	   cur-did doc pns nstr)
      (dolist (did-sen-n-lab did-sen-n-labs)
	(destructuring-bind (did sen n lab) did-sen-n-lab
	  (unless (equalp cur-did did)
	    (setf cur-did did)
	    (setf doc (document cur-did)))
	  (setf sen (find-annotation sen doc))
	  (setf pns (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	  (setf n (elt pns n))
	  (setf nstr (content n))
	  (if (match-re "[+\\-/]" nstr)
	      (setf nstr (string-downcase nstr))
	    (setf nstr (medg-norm nstr)))
	  (when nstr
	    (setf nstr (replace-re nstr "\\s+" "_"))
	    (unless (gethash lab hlab-cnt)
	      (setf (gethash lab hlab-cnt) (make-hash-table :test #'equalp)))
	    (incf (gethash nstr (gethash lab hlab-cnt) 0)))))
      (maphash #'(lambda (lab hcnt)
		   (when (match-re "^[A-Z]" lab)
		     (setf nstr (caar (hash-table-val-desc-alist hcnt)))
		     ;; (format t "~&lab: ~a, nstr: ~a~%" lab nstr)
		     (setf (gethash lab *node-lab->norm*) nstr)))
	       hlab-cnt))))

(defun get-sg-edges (subid)
  (latesql "select n1, n2, lab from sig_subgraph where sub_id=~a and type='edge'" subid))

(defmemo sg-edges (subid)
  (let* ((nodes (get-sg-nodes subid))
	 (edges (get-sg-edges subid))
	 (hnlab (make-hash-table :test #'equalp))
	 ans)
    (dolist (node nodes)
      (setf (gethash (first node) hnlab) (second node)))
    (dolist (edge edges)
      (let* ((n1id (first edge))
	     (n2id (second edge))
	     (elab (third edge))
	     (n1 (gethash n1id hnlab))
	     (n2 (gethash n2id hnlab)))
	(push (format nil "~a|~a|~a" elab n1 n2) ans)))
    ans))

;;; graph reader
(defun get-linkage-graph (sa &optional (type "plain_graph|parse-node-stanford-hier-tagged"))
  "Retrieve a linakge graph id given a sentence annotation"
  (caar (latesql "SELECT DISTINCT LG.lg_id FROM linkage_graph LG JOIN graph G ON
                  G.gid=LG.lg_id
                  WHERE LG.sent=~d AND gtype=~a" 
		 (id sa) (sq type))))

(defmemo lgid->sen (lgid)
  (let* ((senid (caar (latesql "select distinct sent from linkage_graph where lg_id=~a" lgid))))
    (get-annotation senid nil)))

(defun get-lgs-of-sigsub (sub-id)
  (let* ((lgs (latesql "SELECT DISTINCT lg_id FROM lg_sigsub WHERE sub_id=~a"
		       sub-id)))
    (mapcar #'car lgs)))

(defmemo get-sg-mapped-lgn (lgid)
  (let* ((sg-lgns (latesql "select sub_id, lg_node from lg_sigsub where lg_id=~a" lgid)))
    sg-lgns))

(defmemo get-1-sg-mapped-lgn (lgid sgid)
  (let* ((lgns (latesql "select lg_node from lg_sigsub where lg_id=~a and sub_id=~a" lgid sgid)))
    (mapcar #'car lgns)))

(defun get-training-lgs-of-sigsub (sub-id)
  (let* ((lgs (get-lgs-of-sigsub sub-id)))
    (remove-if-not #'lg-in-training? lgs)))

(defun uncovered-lgs (size)
  (let* ((lgs (latesql "select distinct lg_id from linkage_graph, graph where type='node' and gtype='plain_graph|parse-node-stanford-hier-tagged' and gid=lg_id and lg_id not in (select distinct lg_id from lg_sigsub) group by lg_id having count(*)=~a" size)))
    (setf lgs (mapcar #'car lgs))))

(defun uncovered-sents (size 
			&aux 
			(h-sen (make-hash-table :test #'equalp))	
			l-sen-cnt)
  (dolist (lg-sent (latesql "select distinct lg_id, sent from linkage_graph, graph where type='node' and gtype='plain_graph|parse-node-stanford-hier-tagged' and gid=lg_id and lg_id not in (select distinct lg_id from lg_sigsub) group by lg_id having count(*)=~a" size))
    (let* ((sen-id (second lg-sent))
	   (sen (get-annotation sen-id nil))
	   (sen-str (content sen))
	   (sen-str (replace-re sen-str "\\s+" " ")))
      (incf (gethash sen-str h-sen 0))))
  (setf l-sen-cnt (hash-table-val-desc-alist h-sen))
  (dolist (sen-cnt l-sen-cnt)
    (format t "~&sen (~a): ~a~%" (cdr sen-cnt) (car sen-cnt))))

(defun all-uncovered-sents (&aux 
			    (h-sen (make-hash-table :test #'equalp))	
			    l-sen-cnt)
  (dolist (lg-sent (latesql "select distinct lg_id, sent from linkage_graph, graph where type='node' and gtype='plain_graph|parse-node-stanford-hier-tagged' and gid=lg_id and lg_id not in (select distinct lg_id from lg_sigsub)"))
    (let* ((sen-id (second lg-sent))
	   (sen (get-annotation sen-id nil))
	   (sen-str (content sen))
	   (sen-str (replace-re sen-str "\\s+" " ")))
      (incf (gethash sen-str h-sen 0))))
  (setf l-sen-cnt (hash-table-val-desc-alist h-sen))
  (dolist (sen-cnt l-sen-cnt)
    (format t "~&sen (~a): ~a~%" (cdr sen-cnt) (car sen-cnt))))


;;; subgraph reader
(defmemo subname->id (subname)
  (caar (latesql "select gid from graph where gname=~a" (sq subname))))

(defmemo subid->name (subid)
  (caar (latesql "select gname from graph where gid=~a" (sq subid))))

(defun get-sigsub-ids (gtype)
  (mapcar #'car (latesql "SELECT gid FROM graph WHERE gtype=~a 
                          ORDER BY gid ASC" (sq gtype))))

(defmemo get-sigsubs-of-lg (lgid)
  "Given a linkage graph id, return a hash table whose key is a conjunction of 
subgraph ids and mapping ids and whose value is a hash table from subgraph node
 to linkage graph node."
  (let* (;; (time-start (get-internal-real-time))
	 (h-sigsubs (make-hash-table :test #'equalp))
	 (lg-sigsub-rows (latesql 
			  "SELECT mapping_i, sub_id, lg_node, sub_node 
                           FROM lg_sigsub 
                           WHERE lg_id=~d"
			  lgid))
	 sub-lg-map sub-map-id) ; time-elapsed
    (dolist (lg-sigsub-row lg-sigsub-rows)
      (destructuring-bind (map-id sub-id lg-node sub-node)
	  lg-sigsub-row
	(setf sub-map-id (format nil "~a-~a" sub-id map-id))
	(unless (gethash sub-map-id h-sigsubs)
	  (setf (gethash sub-map-id h-sigsubs) 
		(make-hash-table :test #'equalp)))
	(setf sub-lg-map (gethash sub-map-id h-sigsubs))
	(when (gethash sub-node sub-lg-map)
	  (error "duplicate mapping in lg: ~a, sub: ~a, map: ~a, sub-node: ~a, lg-node: ~a, ~a" 
		 lgid sub-id map-id sub-node lg-node 
		 (gethash sub-node sub-lg-map)))
	(setf (gethash sub-node sub-lg-map) lg-node)))
    ;; (setf time-elapsed (- (get-internal-real-time) time-start))
    ;; (format nil "~&get-sigsubs-of-lg for ~a: ~f secs~%" lgid (/ time-elapsed 1000))
    h-sigsubs))

(defmemo get-subids-of-lg (lgid)
  (mapcar #'car (latesql "select distinct sub_id from lg_sigsub where lg_id=~a"
			 lgid)))


(defmethod h-get-sigsubs-of-lg ((lgid integer))
  "Given a linkage graph id, return a hash table whose key is a conjunction of 
subgraph ids and mapping ids and whose value is a hash table from subgraph node
 to linkage graph node."
  (load-lg-sigsub)
  (let* ((h-sigsubs (make-hash-table :test #'equalp))
	 (lg-sigsub-rows (gethash lgid *h-lg-sigsub*))
	 sub-lg-map
	 sub-map-id)
    (dolist (lg-sigsub-row lg-sigsub-rows)
      (destructuring-bind (sub-id map-id lg-node sub-node)
	  lg-sigsub-row
	(setf sub-map-id (format nil "~a-~a" sub-id map-id))
	(unless (gethash sub-map-id h-sigsubs)
	  (setf (gethash sub-map-id h-sigsubs) 
		(make-hash-table :test #'equalp)))
	(setf sub-lg-map (gethash sub-map-id h-sigsubs))
	(when (gethash sub-node sub-lg-map)
	  (error "duplicate mapping in lg: ~a, sub: ~a, map: ~a, sub-node: ~a, lg-node: ~a, ~a" 
		 lgid sub-id map-id sub-node lg-node 
		 (gethash sub-node sub-lg-map)))
	(setf (gethash sub-node sub-lg-map) lg-node)))
    h-sigsubs))

(defmemo get-container (sub-id)
  "get all significant subgraphs to which the graph denoted by sub-id is subisomorphic"
  (mapcar #'car (latesql "SELECT DISTINCT sub2_id FROM sigsub_sigsub WHERE sub1_id=~d" sub-id)))


(defmemo has-neg-container (sgid lgid)
  (let* ((mlgns (get-1-sg-mapped-lgn lgid sgid)) ; mapped lgns
	 (lges (get-lg-edges lgid))
	 (lgns (get-lg-nodes lgid))
	 (hlgn (make-hash-table :test #'equalp))
	 negs poses clgn)
    (dolist (lgn lgns)
      (setf (gethash (first lgn) hlgn) (second lgn)))
    (dolist (mlgn mlgns)
      (dolist (lge lges)
	(cond 
	 ((= mlgn (first lge))
	  (setf clgn (second lge)))
	 ((= mlgn (second lge))
	  (setf clgn (first lge)))
	 (t
	  (setf clgn nil)))
	(when (and (not (member clgn mlgns))
		   (not (member (gethash mlgn hlgn) '("posit" "neg") :test #'equalp))
		   (member (gethash clgn hlgn) '("not" "neg" "cannot" "no") :test #'equalp))
	  (pushnew mlgn negs))
	(when (and (not (member clgn mlgns))
		   (member (gethash clgn hlgn) '("posit") :test #'equalp))
	  (pushnew mlgn poses))))
    (set-difference negs poses)))


(defun load-sub-id2name (&aux h)
  (setf h (make-hash-table :test #'equalp))
  (dolist (kv (latesql "SELECT DISTINCT gid, gname FROM graph WHERE gtype='sig_subgraph|hier'"))
    (setf (gethash (first kv) h) (second kv)))
  h)

(defmemo sg-freq (subid)
  (caar (latesql "select significance from sig_subgraph where sub_id=~a" subid)))

;;; mapping


(defun load-lg-sigsub (&key (in-fn "late:;graph_cache;lg_sigsub"))
  (when (= 0 (hash-table-count *h-lg-sigsub*))
    (format t "~&loading lg sigsub~%")
    (let* (ln time-start time-elapsed)
      (setf time-start (get-internal-real-time))
      (with-open-file (in-f in-fn :direction :input :external-format :utf-8)
		      (loop (unless (setf ln (read-line in-f nil nil)) (return))
			    (destructuring-bind (lgid subid mi lgn subn)
				(split-re "\\|" ln)
			      (setf lgid (parse-integer lgid))
			      (setf subid (parse-integer subid))
			      (setf mi (parse-integer mi))
			      (setf lgn (parse-integer lgn))
			      (setf subn (parse-integer subn))
			      (pushnew '(subid mi lgn subn) 
				       (gethash lgid *h-lg-sigsub*) 
				       :test #'equalp))))
      (setf time-elapsed (- (get-internal-real-time) time-start))
      (format t "~&load mrconso_mesh time: ~f secs~%" (/ time-elapsed 1000)))))


(defmemo get-subsub-ancester (sub-id)
  "Get ancester wrt to subisomorphism between sig subgraphs"
  (mapcar #'car (latesql "SELECT DISTINCT sub2_id FROM sigsub_sigsub WHERE sub1_id=~d" sub-id)))

(defun get-container-or-self (sub-id)
  "get all significant subgraphs to which the graph denoted by sub-id is subisomorphic"
  (or (get-container sub-id)
      (list sub-id)))


;;; misc
(defun import-rand-sg-pairs (fnpair fnsg fnout)
  (let* ((hsg (make-hash-table :test #'equalp))
	 (idx 0) 
	 fpair fsg fout ln)
    (setf fsg (open fnsg :direction :input))
    (loop (unless (setf ln (read-ne-line fsg)) (return))
	  (setf (gethash (incf idx) hsg) ln))
    (close fsg)
    (format t "~&total sgs: ~a~%" (hash-table-count hsg)) 
    
    (setf fpair (open fnpair :direction :input))
    (setf fout (open fnout :direction :output :if-exists :supersede
		     :if-does-not-exist :create))
    (setf idx 0)
    (loop (unless (setf ln (read-ne-line fpair)) (return))
	  (let* ((m (match-re "^\\s*(?<p1>\\d+)\\s+(?<p2>\\d+)" ln :return :match))
		 (p1 (parse-integer (re-submatch m nil nil "p1")))
		 (p2 (parse-integer (re-submatch m nil nil "p2")))
		 (sg1 (gethash p1 hsg))
		 (sg2 (gethash p2 hsg))
		 (m1 (match-re "^#(?<name>\\S+)\\s" sg1 :return :match))
		 (m2 (match-re "^#(?<name>\\S+)\\s" sg2 :return :match))
		 (subname1 (re-submatch m1 nil nil "name"))
		 (subname2 (re-submatch m2 nil nil "name"))
		 (sub-id1 (subname->id subname1))
		 (sub-id2 (subname->id subname2))
		 (lgs1 (get-lgs-of-sigsub sub-id1))
		 (lgs2 (get-lgs-of-sigsub sub-id2))
		 (sen1 (lgid->sen (car lgs1)))
		 (sen2 (lgid->sen (car lgs2))))
	    ;; (format "~&lgs1: ~a~%lgs2: ~a~%" lgs1 lgs2)
	    (unless (intersection lgs1 lgs2 :test #'equalp)
	      (format fout "~&~a~%[~a]~%~a~%[~a]~%[~a-~a]~%~%~%" 
		      sg1 (content sen1) sg2 (content sen2) p1 p2)
	      (incf idx))))
    (format t "~&total pairs: ~a~%" idx)
    (close fpair)
    (close fout)))


(defun read-ef-sen-list (in-fn &aux line h-sen)
  "Read sentence list from concept graph files"
  (setf h-sen (make-hash-table :test #'equalp))
  (with-open-file (in-f in-fn :direction :input)
		  (loop (unless (setf line (read-line in-f nil nil)) (return))
			(setf line (replace-re line "#" ""))
			(incf (gethash line h-sen 0))))
  (mapcar #'car (hash-table-alist h-sen)))

(defmemo singleton? (subid)
  (let* ((nc (caar (latesql "select count(*) from sig_subgraph where sub_id=~a and type='node'" (sq subid)))))
    (= 1 nc)))

(defmemo noun-singleton? (subid)
  (when (singleton? subid)
    (let* ((lgid-node (car (latesql "select lg_id, lg_node from lg_sigsub where sub_id=~a limit 1" subid)))
	   (lgid (first lgid-node))
	   (node (second lgid-node))
	   (pnid (caar (latesql "select pn1 from linkage_graph where lg_id=~a and n1=~a and type='node' limit 1" lgid node)))
	   (pn (get-annotation pnid nil)))
      (noun-pn? pn))))

(defmemo covered-by-non-singleton? (lgid lgn)
  (let* ((subids (latesql "select distinct sub_id from lg_sigsub where lg_id=~a and lg_node=~a" (sq lgid) (sq lgn)))
	 (subids (mapcar #'car subids)))
    (remove-if #'singleton? subids)))


(defun get-singleton-lgs ()
  "The following sql query return real singletons, while we use generalized 
singletons (may contain an extra .):
select distinct lg_id from linkage_graph, graph where type='node' and gtype='plain_graph|parse-node-stanford-hier-tagged' and gid=lg_id group by lg_id having count(*)=1"
  (let* ((singletons (latesql "select distinct lg_id from linkage_graph, graph where type='node' and gtype='plain_graph|parse-node-stanford-hier-tagged' and lab !='.' and gid=lg_id group by lg_id having count(*)=1")))
    (setf singletons (mapcar #'car singletons))))

(defun get-singleton-lab (lg-id)
  (caar (latesql "select lab from linkage_graph where lab !='.' and type ~a and lg_id=~a" (sql-matcher *node-type*) lg-id)))

(defun get-singleton-n1 (lg-id)
  (caar (latesql "select n1 from linkage_graph where lab !='.' and type ~a and lg_id=~a" (sql-matcher *node-type*) lg-id)))

(defun get-lgs ()
  "The following sql query return real singletons, while we use generalized 
singletons (may contain an extra .):
select distinct lg_id from linkage_graph, graph where type='node' and gtype='plain_graph|parse-node-stanford-hier-tagged' and gid=lg_id group by lg_id having count(*)=1"
  (let* ((lgs (latesql "select distinct lg_id from linkage_graph, graph where gtype='plain_graph|parse-node-stanford-hier-tagged' and gid=lg_id")))
    (setf lgs (mapcar #'car lgs))))

(defun get-lg-senid (lgid)
  (caar (latesql "select sent from linkage_graph where lg_id=~a" (sq lgid))))
