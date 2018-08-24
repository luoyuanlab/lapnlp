;;; -*- Mode: Lisp; Package: concept-graph; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 02/27/2013 creation
The purpose is to isolate peripheral utilities package to handle concept graph writing.
|#

(defpackage :concept-graph 
  (:nicknames :cg)
  (:use :common-lisp :late :link :wordnet :util :norm :umlsize 
	#+allegro :excl :dbi.mysql)
  (:export 
   "output-doc-concept-graphs"
   "output-concept-graphs"
   "output-concept-graphs-nel"
   "output-pnode-vocab"
   "sgcluster2vector"
   "h-subgraph-pnode-mallet"
   "h-subgraph-pnode-matrix"
   "h-doc-subgraph-matrix"
   "h-doc-subgraph-pnode-train-tensor"
   "h-doc-subgraph-pnode-test-tensor"
   "h-pt-sg-wd-tensor-matrix"
   "save-linkage-graph"
   "save-lg-node"
   "output-corpus-concept-graphs"
   "output-corpus-concept-graphs-nel"
   "give-sg-examples"
   "flush-concept-graph"
   "tui-network"
   "tui-coloc"
   "immunologic-tui-network"
   "db-subgraph->str"
   "h-sg-lg-matrix"
   "db-sgid->str"
   "h-doc-pn-matrix"
   "h-doc-full-pn-matrix"
   "h-doc-wd-matrix"
   "print-all-sg"
   "h-subgraph-word-matrix"
   "db-subgraph->norm-str"
   "h-pt-sg-wd-tensor-matrix-context"
   "subnames->sgstr"
   "topic-list-simplify"
   "get-redundant-sigsubs"
   "find-sg-specific-sents"
   "sg-sent-assoc"
   ))
(in-package :concept-graph)

(defun find-sg-specific-sents (ssgid)
  "get sents that are associated with subgraph but not its containing subgraph"
  (let* ((lgids-of-ssg (get-lgs-of-sigsub ssgid))
	 (lsgids (get-container ssgid))
	 lgids-of-lsg)
    (dolist (lsgid lsgids)
      (setf lgids-of-lsg (union lgids-of-lsg (get-lgs-of-sigsub lsgid))))
    (dolist (lgid (set-difference lgids-of-ssg lgids-of-lsg))
      (let* ((sen (lgid->sen lgid))
	     (doc (document sen)))
	(format t "~a (sen ~a): ~a~%" (name doc) (id sen) (content sen))))))

(defmemo tensor-exclude-sg? (sub-id)
  (let* ((nlabs (latesql "select lab from sig_subgraph 
                         where sub_id=~a and type='node'" 
			 (sq sub-id)))
	 (nlabs (mapcar #'car nlabs))
	 (redundant-sgs (get-redundant-sigsubs))
	 )
    (or (sigsub-be-trivial? sub-id)
	(member "that" nlabs :test #'equalp))
    
    (or (sigsub-trivialp sub-id)
	(> (length nlabs) 6)
	(member sub-id redundant-sgs)
	)))

;;; database writer
(defun save-linkage-graph (gname gtype &aux gid)
  "Add concept graph to graph table"
  (setf gid (car (latesql "SELECT gid FROM graph WHERE gname=~a AND gtype=~a" 
			  (sq gname) (sq gtype))))
  (cond
   (gid 
    nil)
   (t
    (latesql "INSERT INTO graph (gname, gtype) VALUES (~a, ~a)" 
	     (sq gname) (sq gtype))
    (mysql-insert-id *late-db*))))


(defun save-lg-node (efid type n1 pn1 lab sent &optional (other nil))
  "Add node to concept graph graph."
  (when efid
    (cond 
     (other
      (latesql "INSERT INTO linkage_graph (lg_id, pn1, lab, type, n1, sent, 
                                         other)
                VALUES (~d, ~d, ~a, ~a, ~d, ~d, ~a)"
	       efid pn1 (sq lab) (sq type) n1 sent (sq other)))
     (t 
      (latesql "INSERT INTO linkage_graph (lg_id, pn1, lab, type, n1, sent) 
                VALUES (~d, ~d, ~a, ~a, ~d, ~d)"
	       efid pn1 (sq lab) (sq type) n1 sent)))))

(defun save-lg-edge (efid type n1 pn1 n2 pn2 lab sent
			  &optional (other nil))
  "Add edge to concept graph graph."
  (when efid
    (cond
     (other
      (latesql "INSERT INTO linkage_graph(lg_id, type, n1, pn1, 
                                      n2, pn2, lab, sent, other)
                VALUES (~d, ~a, ~d, ~d, ~d, ~d, ~a, ~d, ~a)"
	       efid (sq type) n1 pn1 n2 pn2 (sq lab) sent (sq other)))
     (t
      (latesql "INSERT INTO linkage_graph(lg_id, type, n1, pn1, n2, pn2, 
                                        lab, sent)
                VALUES (~d, ~a, ~d, ~d, ~d, ~d, ~a, ~d)"
	       efid (sq type) n1 pn1 n2 pn2 (sq lab) sent)))))

(defun sg-sent-assoc (fn)
  (let* ((f (open-new fn))
	 (l-sg-lg (latesql "select distinct sub_id, lg_id from lg_sigsub"))
	 (l-lg-sent (latesql "select distinct lg_id, sent from linkage_graph"))
	 (hlg-sent (make-hash-table :test #'equalp)))
    (dolist (lg-sent l-lg-sent)
      (setf (gethash (first lg-sent) hlg-sent) (second lg-sent)))
    (dolist (sg-lg l-sg-lg)
      (format f "~a ~a~%" (first sg-lg) (gethash (second sg-lg) hlg-sent)))
    (close f)))


;;; file writer
(defun output-doc-concept-graphs (out-dirn docn
					   &key (gtype "plain_graph")
					   &aux doc out-fn)
  "Output the concept graphs within a specified corpus (e.g., train or test 
corpora) from database to user specified file."
  (concept-graph-init)
  (setf out-fn (format nil "~a~a" out-dirn docn)) ;; docn: name_hier.txt
  (setf doc (document docn))
  (with-open-file (out-f out-fn
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
		  (dolist (sen (annotations doc :type 'sentence-annotation))
		    (dolist (iter (latesql "SELECT distinct gid, gname 
                                FROM graph, linkage_graph
                                WHERE gtype=~a and sent=~a and gid=lg_id" 
					   (sq gtype) (id sen)))
		      (let* ((gid (first iter))
			     (gname (second iter))
			     nodes edges)
			(format out-f "~&#~a~%" gname)
			(setf nodes (mapcar #'car (latesql "SELECT lab FROM linkage_graph WHERE lg_id=~a AND type ~a ORDER BY n1" gid (sql-matcher *node-type*))))
			(format out-f "~a~%" (length nodes))
			(dolist (node nodes)
			  (format out-f "~a~%" node))
			
			(setf edges (latesql "SELECT n1, n2, lab FROM linkage_graph WHERE lg_id=~a AND type='edge' ORDER BY n1" gid))
			(format out-f "~%~a~%" (length edges))
			(dolist (edge edges)
			  (let* ((fn (first edge))
				 (tn (second edge))
				 (lab (replace-re (third edge) "-+" "_")) ;; ? 
				 (lab-id (gethash lab *hash-llab*)))
			    (unless lab-id
			      (format t "~&~a not found in syn_deps~%" lab)
			      (setf lab-id (gethash "dep" *hash-llab*)))
			    (format out-f "~a ~a ~a~%" fn tn lab-id)))
			(format out-f "~%"))))))



(defun output-corpus-concept-graphs (out-fn corpn
					    &key (gtype "plain_graph|parse-node-stanford-hier-tagged")
					    (cstart nil)
					    (cend nil)
					    (filter-corpn nil)
					    (if-exists :supersede)
					    (l-secn nil)
					    (l-excl-secn nil)
					    &aux corpus doc sas doc-ids)
  "Output the concept graphs within a specified corpus or a portion of a corpus (e.g., train or test corpora) from database to user specified file."
  (setf corpus (corpus corpn))
  (unless cstart
    (setf cstart 0))
  (unless cend
    (setf cend (length (documents corpus))))
  (concept-graph-init)
  (setf doc-ids (subseq (documents corpus) cstart cend))
  (when filter-corpn
    (setf doc-ids (intersection doc-ids (documents (corpus filter-corpn))
				:test #'equalp)))
  (with-open-file (out-f out-fn
			 :direction :output
			 :if-exists if-exists
			 :if-does-not-exist :create)
		  (dolist (docid doc-ids)
		    (setf doc (document docid))
		    ;; (format t "~&~a~%" (name doc))
		    (dolist (sen (annotations doc :type 'sentence-annotation))
		      (setf sas (annotations-spanning sen :type 'section-annotation))
		      ;; (format t "~&sen: ~a~%" (id sen))
		      (when (and (sec-check l-secn l-excl-secn sas)
				 (annotations-spec sen :type (gtype 'gtoken-type)))
			(dolist (iter (latesql "SELECT distinct gid, gname 
                                FROM graph, linkage_graph
                                WHERE gtype=~a and sent=~a and gid=lg_id" 
					       (sq gtype) (id sen)))
			  (let* ((gid (first iter))
				 (gname (second iter))
				 nodes edges)
			    (setf nodes (mapcar #'car (latesql "SELECT lab FROM linkage_graph WHERE lg_id=~a AND type ~a ORDER BY n1" gid (sql-matcher *node-type*))))
			    (when (> (length nodes) 1)
			      (format out-f "~&#~a~%" gname)
			      (format out-f "~a~%" (length nodes))
			      (dolist (node nodes)
				(format out-f "~a~%" node))
			      (setf edges (latesql "SELECT n1, n2, lab FROM linkage_graph WHERE lg_id=~a AND type='edge' ORDER BY n1" gid))
			      (format out-f "~%~a~%" (length edges))
			      (dolist (edge edges)
				(let* ((fn (first edge))
				       (tn (second edge))
				       (lab (replace-re (third edge) "-+" "_")) ;; ? 
				       (lab-id (gethash lab *hash-llab*)))
				  (unless lab-id
				    (format t "~&~a not found in syn_deps~%" lab)
				    (setf lab-id (gethash "dep" *hash-llab*)))
				  (format out-f "~a ~a ~a~%" fn tn lab-id)))
			      (format out-f "~%")))))))))

(defun output-concept-graphs (out-fn corpn
			      &key (gtype "plain_graph")
			      &aux h-stmp)
  "Output from database to user specified file."
  (concept-graph-init)
  (setf h-stmp (sent-templ corpn))
  (with-open-file (out-f out-fn
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
		  (dolist (iter (latesql "SELECT gid, gname FROM graph WHERE gtype=~a" 
					 (sq gtype)))
		    (let* ((gid (first iter))
			   (gname (second iter))
			   (sen-id (parse-integer (replace-re gname "^\\d+_" "")))
			   (sen (get-annotation sen-id nil))
			   nodes edges)
		      
		      (setf nodes (mapcar #'car (latesql "SELECT lab FROM linkage_graph WHERE lg_id=~a AND type ~a  ORDER BY n1" gid (sql-matcher *node-type*))))
		      (when (and (> (length nodes) 1)
				 (not (maskedp sen))
				 (not (gethash sen-id h-stmp)))
			(format out-f "~&#~a~%" gname)
			(format out-f "~a~%" (length nodes))
			(dolist (node nodes)
			  (format out-f "~a~%" node))
			
			(setf edges (latesql "SELECT n1, n2, lab FROM linkage_graph WHERE lg_id=~a AND type='edge' ORDER BY n1" gid))
			(format out-f "~%~a~%" (length edges))
			(dolist (edge edges)
			  (let* ((fn (first edge))
				 (tn (second edge))
				 (lab (replace-re (third edge) "-+" "_")) 
				 (lab-id (gethash lab *hash-llab*)))
			    (unless lab-id
			      (format t "~&~a not found in syn_deps~%" lab)
			      (setf lab-id (gethash "dep" *hash-llab*)))
			    (format out-f "~a ~a ~a~%" fn tn lab-id)))
			(format out-f "~%"))))))

(defun output-concept-graphs-nel (out-fn 
				  &key (gtype "plain_graph"))
  "Output from database to user specified file."
  (concept-graph-init)
  (with-open-file (out-f out-fn
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
		  (dolist (iter (latesql "SELECT gid, gname FROM graph WHERE gtype=~a" 
					 (sq gtype)))
		    (let* ((gid (first iter))
			   (gname (second iter))
			   (ncnt 0)
			   nodes edges)
		      
		      (setf nodes (mapcar #'car (latesql "SELECT lab FROM linkage_graph WHERE lg_id=~a AND type ~a  ORDER BY n1" gid (sql-matcher *node-type*))))
		      (dolist (node nodes)
			(format out-f "v ~a ~a~%" (incf ncnt) node))
		      
		      (setf edges (latesql "SELECT n1, n2, lab FROM linkage_graph WHERE lg_id=~a AND type='edge' ORDER BY n1" gid))
		      (dolist (edge edges)
			(let* ((fn (first edge))
			       (tn (second edge))
			       (lab (replace-re (third edge) "-+" "_")))
			  
			  (format out-f "e ~a ~a ~a~%" (1+ fn) (1+ tn) lab)))
		      (format out-f "~&g ~a~%" gname)
		      (format out-f "~%")))))

(defun give-sg-examples (fnsg fnout)
  #|| Usage:
  (cg:give-sg-examples "late:;subgraphs_f20_filtered.txt" "late:;subgraphs_f20_filtered_examples.txt")
  ||#
  (let* (fsg fout ln m subname sub-id lgs sen)
    (setf fsg (open fnsg :direction :input))
    (setf fout (open fnout :direction :output :if-exists :supersede
		     :if-does-not-exist :create))
    (loop (unless (setf ln (read-ne-line fsg)) (return))
	  (setf m (match-re "^#(?<name>\\S+)\\s" ln :return :match))
	  (setf subname (re-submatch m nil nil "name"))
	  (setf sub-id (subname->id subname))
	  (setf lgs (get-lgs-of-sigsub sub-id))
	  (setf sen (lgid->sen (car lgs)))
	  (format fout "~&~a~%[~a]~%~%" ln (content sen)))
    (close fsg)
    (close fout)))

(defun output-pnode-vocab (corpn fnvocab
				 &aux (h (make-hash-table :test #'equalp)) fvocab)
  (dolist (docid (documents (corpus corpn)))
    (let* ((doc (document docid))
	   pnstr)
      (dolist (pnode (annotations-spec doc :type (gtype 'ghier-parse-node-type)))
	(setf pnstr (medg-norm (content pnode)))
	(when (and pnstr (match-re "[a-zA-Z]" pnstr))
	  (incf (gethash pnstr h 0))))))
  (setf fvocab (open fnvocab :direction :output :if-exists :supersede
		     :if-does-not-exist :create))
  (dolist (kv (hash-table-val-desc-alist h))
    (format fvocab "~a~%" (car kv)))
  (close fvocab))


(defun sgcluster2vector (fnsgc fncv fnsgv fnsgall fnsgidx)
  "read subgraph cluster text files, output two vector files, one for subgraph names and one for corresponding cluster ids."
  (let* ((hsgc (make-hash-table :test #'equalp))
	 (hsgall (make-hash-table :test #'equalp))
	 (idx 0)
	 fsgc fcv fsgv fsgall fsgidx ln cid m1 m2 sgname)
    (setf fsgall (open fnsgall :direction :input))
    (loop (unless (setf ln (read-ne-line fsgall)) (return))
	  (setf (gethash ln hsgall) (incf idx)))
    (close fsgall)
    
    (setf fsgc (open fnsgc :direction :input))
    
    (loop (unless (setf ln (read-ne-line fsgc)) (return))
	  (setf m1 (match-re "^cluster (?<cid>\\d+)" ln :return :match))
	  (setf m2 (match-re "^#(?<name>\\S+) " ln :return :match))
	  (cond 
	   (m1
	    (setf cid (re-submatch m1 nil nil "cid")))
	   (m2
	    (setf sgname (re-submatch m2 nil nil "name"))
	    (setf (gethash sgname hsgc) (parse-integer cid)))))
    (close fsgc)
    
    
    (setf fcv (open fncv :direction :output :if-exists :supersede
		    :if-does-not-exist :create))
    (setf fsgv (open fnsgv :direction :output :if-exists :supersede
		     :if-does-not-exist :create))
    (setf fsgidx (open fnsgidx :direction :output :if-exists :supersede
		       :if-does-not-exist :create))
    (dolist (kv (hash-table-val-ascd-alist hsgc))
      (format fsgv "~a~%" (car kv))
      (format fsgidx "~a~%" (gethash (car kv) hsgall))
      (format fcv "~a~%" (cdr kv)))
    (close fcv)
    (close fsgv)
    (close fsgidx)))



(defun h-subgraph-pnode-mallet (corpn fnsig fnlg out-ldir)
  "Generate the subgraph pnode matrix in the lda-C format"
  (concept-graph-init)
  (let* (fsig fout ln nnodes
	      (row (make-hash-table :test #'equalp))
	      alg-hnlab alg hnlab hsen)
    (format t "loading sen hash for ~a~%" corpn)
    (time-profiling (setf hsen (load-sen-hash corpn)))
    
    (format t "loading gaston lg mapping~%")
    (time-profiling (setf alg-hnlab (gaston-lg-mapping fnlg)))
    (setf alg (first alg-hnlab))
    (setf hnlab (second alg-hnlab))
    
    (setf fsig (open fnsig :direction :input))
    
    (loop (unless (setf ln (read-ne-line fsig)) (return))
	  (let* ((m1 (match-re "^#(?<id>\\d+),.*$" ln :return :match))
		 (m2 (match-re "^(?<lgid>\\d+)\\s+->\\s+(?<mstr>.*?)\\s*$" 
			       ln :return :match))
		 lgid sgid mstr sen pnstr nedges pnode pnodes)
	    (declare (ignorable pnode pnodes))
	    (cond
	     ;; subgraph description part
	     (m1
	      (setf sgid (re-submatch m1 nil nil "id"))
	      (when fout
		(dolist (kv (hash-table-alist row))
		  (format fout "~a~%" (replace-re (car kv) "\\s+" "_")))
		(setf row (make-hash-table :test #'equalp))
		(close fout))
	      (setf fout (open (format nil "~asg_~a" out-ldir sgid) 
			       :direction :output :if-exists :supersede
			       :if-does-not-exist :create))
	      (format t "reading subgraph ~a~%" sgid)
	      
	      ;; read nodes
	      (setf nnodes (parse-integer (read-ne-line fsig)))		  
	      (dotimes (i nnodes) (setf ln (read-ne-line fsig)))
	      
	      ;; read edges
	      (setf nedges (parse-integer (read-ne-line fsig)))
	      (dotimes (i nedges) (setf ln (read-ne-line fsig))))
	     
	     ;; subgraph mapping part
	     (m2
	      (setf lgid (parse-integer (re-submatch m2 nil nil "lgid")))
	      (setf sen (gethash (aref alg lgid) hsen))
	      (setf pnodes (annotations-spec sen 
					     :type (gtype 'ghier-parse-node-type)))
	      (setf mstr (re-submatch m2 nil nil "mstr"))
	      (dolist (lgnid (split-re "\\s+" mstr))
		(setf lgnid (parse-integer lgnid))
		(setf pnstr (gethash (cons lgid lgnid) hnlab))
		(when (and pnstr (match-re "[a-zA-Z]" pnstr))
		  (incf (gethash pnstr row 0))))))))
    
    (when fout
      (dolist (kv (hash-table-alist row))
	(format fout "~a~%" (car kv)))
      (setf row (make-hash-table :test #'equalp))
      (close fout))
    
    (close fsig)))

(defmemo db-subgraph->str (subname)
  (let* ((subid (subname->id subname))
	 (nodes (mapcar #'second (get-sg-nodes subid)))
	 (edges (get-sg-edges subid))
	 (sgstr (outstr-init))
	 (freq (sg-freq subid))
	 (i -1))
    (format sgstr "~a| ~a| ~a| " subid subname freq)
    (format sgstr "n{~a} e{~a}| " (length nodes) (length edges))
    (dolist (node nodes)
      (format sgstr "[~a ~a] " (incf i) node))
    (dolist (edge edges)
      (format sgstr "(~a-~a ~a) " (first edge) (second edge) (third edge)))
    sgstr))

(defmemo db-subgraph->norm-str (subname)
  (populate-node-lab->norm)
  (let* ((subid (subname->id subname))
	 (nodes (mapcar #'second (get-sg-nodes subid)))
	 (edges (get-sg-edges subid))
	 (sgstr (outstr-init))
	 (freq (sg-freq subid))
	 (i -1))
    (format sgstr "~a| ~a| ~a| " subid subname freq)
    (format sgstr "n{~a} e{~a}| " (length nodes) (length edges))
    (dolist (node nodes)
      (format sgstr "[~a ~a] " 
	      (incf i) (or (gethash node *node-lab->norm*) node)))
    (dolist (edge edges)
      (format sgstr "(~a-~a ~a) " (first edge) (second edge) (third edge)))
    sgstr))

(defmemo db-sgid->str-entropy (subid)
  (let* ((nodes (mapcar #'second (get-sg-nodes subid)))
	 (edges (get-sg-edges subid))
	 (sgstr (outstr-init))
	 (subname (subid->name subid))
	 (freq (sg-freq subid))
	 (b-ent (get-sigsub-entropy subid "burkitts"))
	 (d-ent (get-sigsub-entropy subid "dlbcl"))
	 (f-ent (get-sigsub-entropy subid "follicular"))
	 (h-ent (get-sigsub-entropy subid "hodgkins"))
	 (i -1))
    (format sgstr "~a| ~a| b: ~a| d: ~a| f: ~a| h: ~a| cnt: ~a| " subid subname b-ent d-ent f-ent h-ent freq)
    (format sgstr "n{~a} e{~a}| " (length nodes) (length edges))
    (dolist (node nodes)
      (format sgstr "[~a ~a] " (incf i) node))
    (dolist (edge edges)
      (format sgstr "(~a-~a ~a) " (first edge) (second edge) (third edge)))
    sgstr))

(defmemo db-sgid->str (subid)
  (let* ((nodes (mapcar #'second (get-sg-nodes subid)))
	 (edges (get-sg-edges subid))
	 (sgstr (outstr-init))
	 (subname (subid->name subid))
	 (freq (sg-freq subid))

	 (i -1))
    (format sgstr "~a| ~a| cnt: ~a| " subid subname freq)
    (format sgstr "n{~a} e{~a}| " (length nodes) (length edges))
    (dolist (node nodes)
      (format sgstr "[~a ~a] " (incf i) node))
    (dolist (edge edges)
      (format sgstr "(~a-~a ~a) " (first edge) (second edge) (third edge)))
    sgstr))

(defmemo db-sgid->str-short (subid)
  (let* ((nodes (mapcar #'second (get-sg-nodes subid)))
	 (edges (get-sg-edges subid))
	 (sgstr (outstr-init))
	 ;; (subname (subid->name subid))
	 ;; (freq (sg-freq subid))
	 (hdeg (make-hash-table :test #'equalp)))
    ;; (format sgstr "~a| ~a| cnt: ~a| " subid subname freq)
    
    (cond 
     ((= (length nodes) 3)
      (dolist (edge edges)
	(incf (gethash (first edge) hdeg 0))
	(incf (gethash (second edge) hdeg 0)))
      (let* ((hub (caar (hash-table-val-desc-alist hdeg))))
	(unless (= 1 hub)
	  (let* ((tmp (elt nodes 1)))
	    (setf (elt nodes 1) (elt nodes hub))
	    (setf (elt nodes hub) tmp)))))
     ((> (length nodes) 3)
      (format t "warning: more than 3 nodes graph")))
    
    (format sgstr "~{[~a]~^ - ~}" nodes)
    sgstr))

(defun topic-list-simplify (fnin fnout)
  (let* ((fout (open-new fnout)) 
	 ln m p sgid sgstr)
    (with-open-file (fin fnin :direction :input)
		    (loop (unless (setf ln (read-line fin nil nil)) (return))
			  (cond
			   ((setf m (match-re "^(?<p>[\\d.e\\-]+): (?<id>\\d+)\\|" ln 
					      :return :match))
			    (setf p (re-submatch m nil nil "p" :type :string))
			    (setf sgid (re-submatch m nil nil "id" :type :string))
			    (setf sgstr (db-sgid->str-short sgid))
			    (format fout "~a| ~a| ~a~%" p sgid sgstr))
			   (t
			    (format fout "~a~%" ln)))))
    (close fout)))

(defun h-subgraph-pnode-matrix-file (corpn fnsig fnlg fnvsg fnldac fnvocab)
  "Generate the subgraph pnode matrix in the lda-C format"
  (concept-graph-init)
  (let* (fsig fldac fvsg fvocab ln sigstr nnodes
	      (hvocab (make-hash-table :test #'equalp))
	      ;; the matrix is represented as a hash hash table: (cid -> cnt)
	      (row (make-hash-table :test #'equalp))
	      alg-hnlab alg hnlab hsen )
    (format t "loading sen hash for ~a~%" corpn)
    (time-profiling (setf hsen (load-sen-hash corpn)))
    
    (format t "loading gaston lg mapping~%")
    (time-profiling (setf alg-hnlab (gaston-lg-mapping fnlg)))
    (setf alg (first alg-hnlab))
    (setf hnlab (second alg-hnlab))
    
    (setf fsig (open fnsig :direction :input))
    (setf fvsg (open fnvsg :direction :output :if-exists :supersede
		     :if-does-not-exist :create))
    (setf fldac (open fnldac :direction :output :if-exists :supersede
		      :if-does-not-exist :create))
    (loop (unless (setf ln (read-ne-line fsig)) (return))
	  (let* ((m1 (match-re "^#(?<id>\\d+),.*$" ln :return :match))
		 (m2 (match-re "^(?<lgid>\\d+)\\s+->\\s+(?<mstr>.*?)\\s*$" 
			       ln :return :match))
		 lgid sgid mstr sen pnodes pnode pnstr cid nedges)
	    (cond
	     ;; subgraph description part
	     (m1
	      (output-row-ldac row fldac)
	      (setf row (make-hash-table :test #'equalp))
	      
	      (setf sgid (re-submatch m1 nil nil "id"))
	      (format t "reading subgraph ~a~%" sgid)
	      (setf sigstr (outstr-init))
	      (format sigstr "~a" ln)
	      ;; read nodes
	      (setf ln (read-ne-line fsig))
	      (setf nnodes (parse-integer ln))
	      (format sigstr " n{~a}" nnodes)
	      
	      (dotimes (i nnodes)
		(setf ln (read-ne-line fsig))
		(format sigstr " [~a]" ln))
	      ;; read edges
	      (setf ln (read-ne-line fsig))
	      (setf nedges (parse-integer ln))
	      (format sigstr " e{~a}" nedges)
	      
	      (dotimes (i nedges)
		(setf ln (read-ne-line fsig))
		(let* ((edge (split-re "\\s+" ln))
		       (n1 (elt edge 0))
		       (n2 (elt edge 1))
		       (labid (parse-integer (elt edge 2)))
		       (lab (gethash labid  *hid2lab*)))
		  (format sigstr " (~a-~a ~a)" n1 n2 lab)))
	      
	      (format fvsg "~a~%" sigstr))
	     
	     ;; subgraph mapping part
	     (m2
	      (setf lgid (parse-integer (re-submatch m2 nil nil "lgid")))
	      (setf sen (gethash (aref alg lgid) hsen))
	      
	      (setf pnodes (annotations-spec sen 
					     :type (gtype 'ghier-parse-node-type)))
	      (setf mstr (re-submatch m2 nil nil "mstr"))
	      (dolist (lgnid (split-re "\\s+" mstr))
		(setf lgnid (parse-integer lgnid))
		;; (format t "lgnid: ~a, lgid: ~a, pnode: ~a~%" lgnid lgid pnid)
		;; this is necessary because I inserted positivity for +
		(when (< lgnid (length pnodes))
		  (setf pnode (elt pnodes lgnid))
		  (setf pnstr (medg-norm (content pnode))))
		
		(when (and pnstr (match-re "[a-zA-Z]" pnstr))
		  (setf cid (gethash pnstr hvocab))
		  (unless cid 
		    (setf cid (1+ (hash-table-count hvocab)))
		    (setf (gethash pnstr hvocab) cid))
		  (incf (gethash cid row 0)))
		
		(setf pnstr (gethash (cons lgid lgnid) hnlab))
		(when (and pnstr (match-re "[a-zA-Z]" pnstr))
		  (setf cid (gethash pnstr hvocab))
		  (unless cid 
		    (setf cid (1+ (hash-table-count hvocab)))
		    (setf (gethash pnstr hvocab) cid))
		  (incf (gethash cid row 0))))))))
    (output-row-ldac row fldac)
    (setf fvocab (open fnvocab :direction :output :if-exists :supersede 
		       :if-does-not-exist :create))
    (dolist (kv (hash-table-val-ascd-alist hvocab))
      (format fvocab "~a~%" (car kv)))
    (close fvocab)	
    
    (close fsig)
    (close fvsg)
    (close fldac)))





(defun h-doc-subgraph-pnode-train-tensor 
    (corpn fntensor-pn fntensor-wd fnpn-tid fndoc-tid fnsg-tid fnwd-tid
	   &key
	   (lg-type "plain_graph|parse-node-stanford-hier-tagged")
	   (log? nil))
  "Generate the document subgraph pnode tensor-pn in the sparse tensor-pn format"
  (concept-graph-init)
  (let* ((tensor-pn (make-hash-table :test #'equalp))
	 (tensor-wd (make-hash-table :test #'equalp))
	 (hwd-tid (make-hash-table :test #'equalp))
	 (hpn-tid (make-hash-table :test #'equalp))
	 (hdoc-tid (read-idx-hash fndoc-tid))
	 (hsg-tid (make-hash-table :test #'equalp))
	 (hsub-id2name (load-sub-id2name))
	 (ftensor-pn (open-new fntensor-pn))
	 (ftensor-wd (open-new fntensor-wd))
	 (cnt 0)
	 doc sub-id sub-name lgid pns pn sg-lgns lgn doc-tid
	 pnstr pn-tid sg-tid wd-tid)

    (dolist (did (documents (corpus corpn)))
      (incf cnt)
      (when (= 0 (mod cnt 500))
	(format t "~&processed ~a docs~%" cnt))
      (setf doc (document did))
      (setf doc-tid (gethash (name doc) hdoc-tid))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(setf lgid (get-linkage-graph sen lg-type))
	(when lgid
	  (setf pns (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	  (setf sg-lgns (get-sg-mapped-lgn lgid))
	  (setf sg-lgns (remove-if #'(lambda (a) (tensor-exclude-sg? (first a)))
				   sg-lgns))
	  (setf sg-lgns (remove-duplicates sg-lgns :test #'equalp))
	  (dolist (sg-lgn sg-lgns)
	    (setf lgn (second sg-lgn)
		  sub-id (first sg-lgn))
	    (setf sub-name (gethash sub-id hsub-id2name))
	    (setf sg-tid (cnt-gethash sub-name hsg-tid))
	    (when (< lgn (length pns))
	      (setf pn (elt pns lgn))
	      (setf pnstr (medg-norm (content pn)))
	      (when pnstr 
		(when (match-re "[a-zA-Z]" pnstr)
		  (setf pn-tid (cnt-gethash pnstr hpn-tid))
		  (tensor-add tensor-pn doc-tid sg-tid pn-tid)
		  (dolist (wd (split-re "\\s+" pnstr))
		    (setf wd-tid (cnt-gethash wd hwd-tid))
		    (tensor-add tensor-wd doc-tid sg-tid wd-tid))))))))
      
      (tensor-file-append ftensor-pn tensor-pn :log? log?)
      (tensor-file-append ftensor-wd tensor-wd :log? log?)
      (clrhash tensor-pn)
      (clrhash tensor-wd))

    (write-idx-hash hsg-tid fnsg-tid)
    (write-idx-hash hpn-tid fnpn-tid)
    (write-idx-hash hwd-tid fnwd-tid)
    (close ftensor-pn)
    (close ftensor-wd)))

(defun write-pt-gt (fnpt-gt fnpt-tid)
  (let* ((hm-pt-gt (make-hash-table :test #'equalp))
	 (hpt-tid (read-idx-hash fnpt-tid))
	 (bptr (mrns "burkitts_positive_train"))
	 (bpte (mrns "burkitts_positive_test"))
	 (l-burkitt (union bptr bpte :test #'equalp))
	 (dptr (mrns "dlbcl_positive_train"))
	 (dpte (mrns "dlbcl_positive_test"))
	 (l-dlbcl (union dptr dpte :test #'equalp))
	 (fptr (mrns "follicular_positive_train"))
	 (fpte (mrns "follicular_positive_test"))
	 (l-follicular (union fptr fpte :test #'equalp))
	 (hptr (mrns "hodgkins_positive_train"))
	 (hpte (mrns "hodgkins_positive_test"))
	 (l-hodgkin (union hptr hpte :test #'equalp))
	 mrn tid)
    (dolist (mrn-tid (hash-table-alist hpt-tid))
      (setf mrn (car mrn-tid))
      (setf tid (cdr mrn-tid))
      (when (member mrn l-burkitt :test #'equalp)
	(matrix-add hm-pt-gt tid 1))
      (when (member mrn l-dlbcl :test #'equalp)
	(matrix-add hm-pt-gt tid 2))
      (when (member mrn l-follicular :test #'equalp)
	(matrix-add hm-pt-gt tid 3))
      (when (member mrn l-hodgkin :test #'equalp)
	(matrix-add hm-pt-gt tid 4)))
    (write-matrix fnpt-gt hm-pt-gt)))

(defun h-pt-sg-wd-tensor-matrix
    (mrns fntensor fnm-pt-sg fnm-pt-wd fnm-sg-wd fnpt-tid fnsg-tid fnwd-tid
	  fnpt-gt
	  &key
	  (lg-type "plain_graph|parse-node-stanford-hier-tagged")
	  (log? nil))
  "Generate everything."
  (concept-graph-init)
  (let* ((tensor (make-hash-table :test #'equalp))
	 (hwd-tid (make-hash-table :test #'equalp))
	 (hpt-tid (read-idx-hash fnpt-tid))
	 (hsg-tid (make-hash-table :test #'equalp))
	 (hsub-id2name (load-sub-id2name))
	 (ftensor (open-new fntensor))
	 (hm-pt-sg (make-hash-table :test #'equalp))
	 (hm-sg-wd (make-hash-table :test #'equalp))
	 (hm-pt-wd (make-hash-table :test #'equalp))
	 (cnt 0)
	 doc sub-id sub-name lgid pns pn sg-lgns lgn pt-tid tas wd hsenlgs
	 sg-tid wd-tid)
    
    (dolist (mrn mrns)
      (setf pt-tid (gethash mrn hpt-tid))
      (assert pt-tid
	      ()
	      "nil pt-tid for ~a" mrn)
      (dolist (did (mrn->docids mrn))
	(incf cnt)
	(when (= 0 (mod cnt 500))
	  (format t "~&processed ~a docs~%" cnt))
	(setf doc (document did))
	(when (annotations doc)
	  (dolist (sen (annotations doc :type 'sentence-annotation))
	    (setf lgid (get-linkage-graph sen lg-type))
	    (setf hsenlgs (make-hash-table :test #'equalp))
	    (when lgid
	      (setf pns (raw-pns sen))
	      (setf sg-lgns (get-sg-mapped-lgn lgid))
	      (setf sg-lgns (remove-if #'(lambda (a) 
					   (tensor-exclude-sg? (first a)))
				       sg-lgns))
	      (setf sg-lgns (remove-duplicates sg-lgns :test #'equalp))
	      
	      (dolist (sg-lgn sg-lgns)
		(setf lgn (second sg-lgn)
		      sub-id (first sg-lgn))
		(setf sub-name (gethash sub-id hsub-id2name))
		(setf sg-tid (cnt-gethash sub-name hsg-tid))
		(unless (gethash sg-tid hsenlgs)
		  (matrix-add hm-pt-sg pt-tid sg-tid)
		  (setf (gethash sg-tid hsenlgs) 1))

		(when (< lgn (length pns))
		  (setf pn (elt pns lgn))
		  (setf tas (annotations-spec pn :type (gtype 'gtoken-type)))
		  
		  (dolist (ta tas)
		    (setf wd (late::weka-str (late::tok-ft-str ta)))
		    (when (match-re "[a-zA-Z]" wd)
		      (setf wd-tid (cnt-gethash wd hwd-tid))
		      (tensor-add tensor pt-tid sg-tid wd-tid)
		      (matrix-add hm-pt-wd pt-tid wd-tid)
		      (matrix-add hm-sg-wd sg-tid wd-tid))))))))))

    (tensor-file-append ftensor tensor :log? log?)
    (write-idx-hash hsg-tid fnsg-tid)
    (write-idx-hash hwd-tid fnwd-tid)
    (write-matrix fnm-pt-sg hm-pt-sg :log? log?)
    (write-matrix fnm-pt-wd hm-pt-wd :log? log?)
    (write-matrix fnm-sg-wd hm-sg-wd :log? log?)
    (write-pt-gt fnpt-gt fnpt-tid)
    (close ftensor)))

(defun get-redundant-sigsubs ()
  (mapcar #'car (latesql "select sub_id from redundant_sigsub")))

#| Example usage:
(cg::h-pt-sg-wd-tensor-matrix-context mrns "pt_sg_wd/train.tensor" "pt_sg_wd/train.doctensor" "pt_sg_wd/pt_sg_train.spmat" "pt_sg_wd/pt_wd_train.spmat" "pt_sg_wd/sg_wd_train.spmat" "pt_sg_wd/lymphoma_mrn_list" "pt_sg_wd/sg_train.tid" "pt_sg_wd/wd_train.tid" "pt_sg_wd/doc_train.tid" "pt_sg_wd/pt_gt_train.spmat")
(cg::subnames->sgstr "pt_sg_wd/sg_train.tid" "pt_sg_wd/sg_train.str")
where mrns can be obtained using mrns function in merge-inst.cl
|#
(defun h-pt-sg-wd-tensor-matrix-context
    (mrns fntensor fndoctensor fnm-pt-sg fnm-pt-wd fnm-sg-wd 
	  fnpt-tid fnsg-tid fnwd-tid fndoc-tid fnpt-gt
	  &key
	  (lg-type "plain_graph|parse-node-stanford-hier-tagged")
	  (log? nil)
	  (wr 2))
  "Generate everything."
  (concept-graph-init)
  (let* ((tensor (make-hash-table :test #'equalp))
	 (doctensor (make-hash-table :test #'equalp))
	 (hwd-tid (make-hash-table :test #'equalp))
	 (hpt-tid (read-idx-hash fnpt-tid))
	 (hsg-tid (make-hash-table :test #'equalp))
	 (hdoc-tid (make-hash-table :test #'equalp))
	 (hsub-id2name (load-sub-id2name))
	 (ftensor (open-new fntensor))
	 (fdoctensor (open-new fndoctensor))
	 (hm-pt-sg (make-hash-table :test #'equalp))
	 (hm-sg-wd (make-hash-table :test #'equalp))
	 (hm-pt-wd (make-hash-table :test #'equalp))
	 (hsg-cnt (make-hash-table :test #'equalp))
	 (cnt 0)
	 (hsg-pn-covered (make-hash-table :test #'equalp))
	 (hsgf (make-hash-table :test #'equalp))
	 doc sub-name lgid subids pt-tid wds sg-tid wd-tid doc-tid)

    (dolist (mrn mrns)
      (setf pt-tid (gethash mrn hpt-tid))
      (format t "~a~%" mrn)
      (assert pt-tid () "nil pt-tid for ~a" mrn)
      (dolist (did (mrn->docids mrn))
	(incf cnt)
	(when (= 0 (mod cnt 500))
	  (format t "~&processed ~a docs~%" cnt))
	(setf doc (document did))
	(setf doc-tid (cnt-gethash (name doc) hdoc-tid))
	(when (annotations doc)
	  (dolist (sen (annotations doc :type 'sentence-annotation))
	    (clrhash hsgf)
	    (when (setf lgid (get-linkage-graph sen lg-type))
	      (clrhash hsg-pn-covered)
	      (setf subids (get-subids-of-lg lgid))
	      (setf subids (remove-if #'tensor-exclude-sg? subids))
	      (setf subids (remove-if #'(lambda (subid) (has-neg-container subid lgid)) subids))
	      (incf (gethash mrn hsg-cnt 0) (length subids))
	      
	      (setf wds (late::extract-ngram 1 sen))
	      (setf wds (mapcar #'late::weka-str wds))
	      (dolist (wd wds)
		(setf wd-tid (cnt-gethash wd hwd-tid))
		(matrix-add hm-pt-wd pt-tid wd-tid))

	      (let* ((pns (raw-pns sen))
		     (tas (annotations-spec sen :type (gtype 'gtoken-type)))
		     (sg-lgns (get-sg-mapped-lgn lgid))
		     (hsg-lgns (make-hash-table :test #'equalp))
		     lgns fwds)
		(setf sg-lgns (remove-if #'(lambda (a) 
					     (not (member (first a) subids)))
					 sg-lgns))
		(setf sg-lgns (remove-duplicates sg-lgns :test #'equalp))
		(dolist (sg-lgn sg-lgns)
		  (let* ((lgn (second sg-lgn))
			 (subid (first sg-lgn)))
		    (pushnew lgn (gethash subid hsg-lgns nil))))
		
		(dolist (subid (hash-keys hsg-lgns))
		  (setf fwds nil sg-tid nil)
		  (setf lgns (gethash subid hsg-lgns))
		  (dolist (lgn lgns)
		    (when (or (not (singleton? subid)) 
			      (and ;; (noun-singleton? subid)
			       (not (covered-by-non-singleton? lgid lgn))))
		      (setf sub-name (gethash subid hsub-id2name))
		      (setf sg-tid (cnt-gethash sub-name hsg-tid))
		      (setf (gethash subid hsgf) t)
		      
		      
		      (when (>= lgn (length pns))
			(format t "wrong ~a in ~a~%" lgn sen))
		      (when (< lgn (length pns))
			(let* ((pn (elt pns lgn))
			       (tas-covered (annotations-spec pn :type (gtype 'gtoken-type)))
			       (tas-before (annotations-before-spec pn :type (gtype 'gtoken-type) :cnt wr))
			       (tas-after (annotations-after-spec pn :type (gtype 'gtoken-type) :cnt wr))
			       pn-covered?)
			  (unless (maskedp pn)
			    (dolist (ta tas-covered)
			      (unless (maskedp ta)
				(let* ((wd (late::weka-str (late::tok-ft-str ta))))
				  (when (> (length wd) 0)
				    (setf pn-covered? t)
				    (pushnew wd fwds :test #'equalp)))))
			    
			    (unless pn-covered?
			      (let* ((wd (late::weka-str (late::tok-ft-str (data pn)))))
				(cond
				 ((> (length wd) 0)
				  (pushnew wd fwds :test #'equalp))
				 (t
				  (format t "uncovered ~a in ~a by ~a~%" lgn sen subid)))))
			    
			    (dolist (ta (intersection tas-before tas :test #'annotation-id-equal))
			      (unless (maskedp ta)
				(let* ((wd (late::weka-str (late::tok-ft-str ta))))
				  (pushnew wd fwds :test #'equalp))))
			    
			    (dolist (ta (intersection tas-after tas :test #'annotation-id-equal))
			      (unless (maskedp ta)
				(let* ((wd (late::weka-str (late::tok-ft-str ta))))
				  (pushnew wd fwds :test #'equalp)))))))))
		  (when sg-tid
		    (dolist (wd fwds)
		      (when (match-re "[a-zA-Z]" wd)
			(setf wd-tid (cnt-gethash wd hwd-tid))
			(tensor-add tensor pt-tid sg-tid wd-tid 
				    :num (/ 1 (sigsub-size subid))) 
			(matrix-add hm-sg-wd sg-tid wd-tid))))))
	      
	      (dolist (subid subids)
		(when (gethash subid hsgf)
		  (setf sub-name (gethash subid hsub-id2name))
		  (setf sg-tid (cnt-gethash sub-name hsg-tid))
		  (matrix-add hm-pt-sg pt-tid sg-tid)
		  (tensor-add doctensor pt-tid sg-tid doc-tid)))))))
      (when (= 0 (gethash mrn hsg-cnt 0))
	(format t "warning: ~a has no sgs~%" mrn)))

    (tensor-file-append ftensor tensor :log? log?)
    (tensor-file-append fdoctensor doctensor :log? log?)
    (write-idx-hash hdoc-tid fndoc-tid)
    (write-idx-hash hsg-tid fnsg-tid)
    (write-idx-hash hwd-tid fnwd-tid)
    (write-matrix fnm-pt-sg hm-pt-sg :log? log?)
    (write-matrix fnm-pt-wd hm-pt-wd :log? log?)
    (write-matrix fnm-sg-wd hm-sg-wd :log? log?)
    (write-pt-gt fnpt-gt fnpt-tid)
    (close ftensor)
    (close fdoctensor)))



(defun subnames->sgstr (fnin fnout)
  (let* ((fout (open-new fnout))
	 ln subid)
    (with-open-file (fin fnin :direction :input)
		    (loop (unless (setf ln (read-ne-line fin)) (return))
			  (setf subid (subname->id ln))
			  (format fout "~a~%" (db-sgid->str subid))))
    (close fout)))

(defun h-doc-subgraph-pnode-train-tensor-sg-norm 
    (corpn fntensor fnpn fndoc fnsubid fnlg
	   &key
	   (lg-type "plain_graph|parse-node-stanford-hier-tagged"))
  "Generate the document subgraph pnode tensor in the sparse tensor format"
  (concept-graph-init)
  (let* ((corp (corpus corpn))
	 (dids (documents corp))
	 (tensor (make-hash-table :test #'equalp))
	 (tsg-sum (make-hash-table :test #'equalp))
	 (hpn-tid (make-hash-table :test #'equalp))
	 (hdoc-tid (make-hash-table :test #'equalp))
	 (hsub-tid (make-hash-table :test #'equalp))
	 (hsub-id2name (load-sub-id2name))
	 (i 0)
	 doc docn ftensor sub-id sub-name lgid h-sigsubs pns pn 
	 pnstr str-id key hnlab alg-hnlab subtid)
    (let* ((fdoc (open fndoc :direction :input)))
      (loop (unless (setf docn (read-ne-line fdoc)) (return))
	    (setf (gethash docn hdoc-tid) (incf i)))
      (close fdoc))
    
    (setf i 0)
    (let* ((fpn (open fnpn :direction :input)))
      (loop (unless (setf pn (read-ne-line fpn)) (return))
	    (setf (gethash pn hpn-tid) (incf i)))
      (close fpn))
    
    (setf i 0)
    (let* ((fsubid (open fnsubid :direction :input)))
      (loop (unless (setf sub-name (read-ne-line fsubid)) (return))
	    (setf (gethash sub-name hsub-tid) (incf i)))
      (close fsubid))

    (setf ftensor (open fntensor :direction :output :if-exists :supersede
			:if-does-not-exist :create))	
    (format t "loading gaston lg mapping~%")
    (time-profiling (setf alg-hnlab (gaston-lg-mapping fnlg)))
    (setf hnlab (second alg-hnlab))
    
    (dolist (did dids)
      (setf doc (document did))
      (setf i (gethash (name doc) hdoc-tid))
      ;; (format t "scanning doc ~a~%" (name doc))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(setf lgid (get-linkage-graph sen lg-type))
	(when lgid
	  (setf h-sigsubs (get-sigsubs-of-lg lgid))
	  (setf pns (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	  (maphash 
	   #'(lambda (sub-map-id ls-map)
	       (setf sub-id (parse-integer (car (split-re "-" sub-map-id))))
	       (unless (tensor-exclude-sg? sub-id)
		 (setf sub-name (gethash sub-id hsub-id2name))
		 
		 (setf subtid (gethash sub-name hsub-tid))
		 (when subtid
		   (maphash 
		    #'(lambda (subn lgn)
			(declare (ignorable subn))
			(when (< lgn (length pns))
			  (setf pn (elt pns lgn))
			  (setf pnstr (medg-norm (content pn))))
			(when (and pnstr (match-re "[a-zA-Z]" pnstr))
			  (setf str-id (gethash pnstr hpn-tid))
			  (when str-id 
			    (setf key (format nil "~a ~a ~a" i subtid str-id))
			    
			    (incf (gethash key tensor 0))
			    (incf (gethash subtid tsg-sum 0))))
			
			(setf pnstr (gethash (cons lgid lgn) hnlab))
			(when (and pnstr (match-re "[a-zA-Z]" pnstr))
			  (setf str-id (gethash pnstr hpn-tid))
			  (when str-id 
			    (setf key (format nil "~a ~a ~a" i subtid str-id))
			    
			    (incf (gethash key tensor 0))
			    (incf (gethash subtid tsg-sum 0)))))
		    ls-map))))
	   h-sigsubs))))
    
    (dolist (kv (hash-table-alist tensor))
      (let* ((tkey (car kv))
	     ;; need to have :count 1 here
	     (tsum-key (parse-integer (elt (split-re "\\s+" tkey) 1)))
	     (tcnt (cdr kv))
	     (tsum-cnt (gethash tsum-key tsg-sum)))
	(format ftensor "~a ~f~%" (car kv) (/ tcnt tsum-cnt))))
    (close ftensor)))



(defun h-doc-subgraph-pnode-test-tensor 
    (corpn fntensor fnpn fndoc fnsubid fnlg
	   &key
	   (lg-type "plain_graph|parse-node-stanford-hier-tagged")
	   (log? nil))
  "Generate the document subgraph pnode tensor in the sparse tensor format"
  (concept-graph-init)
  (let* ((corp (corpus corpn))
	 (dids (documents corp))
	 (tensor (make-hash-table :test #'equalp))
	 (hpn-tid (make-hash-table :test #'equalp))
	 (hdoc-tid (make-hash-table :test #'equalp))
	 (hsub-tid (make-hash-table :test #'equalp))
	 (hsub-id2name (load-sub-id2name))
	 (i 0)
	 doc docn ftensor sub-id sub-name lgid h-sigsubs pns pn 
	 pnstr str-id key hnlab alg-hnlab subtid)
    (let* ((fdoc (open fndoc :direction :input)))
      (loop (unless (setf docn (read-ne-line fdoc)) (return))
	    (setf (gethash docn hdoc-tid) (incf i)))
      (close fdoc))
    
    (setf i 0)
    (let* ((fpn (open fnpn :direction :input)))
      (loop (unless (setf pn (read-ne-line fpn)) (return))
	    (setf (gethash pn hpn-tid) (incf i)))
      (close fpn))
    
    (setf i 0)
    (let* ((fsubid (open fnsubid :direction :input)))
      (loop (unless (setf sub-name (read-ne-line fsubid)) (return))
	    (setf (gethash sub-name hsub-tid) (incf i)))
      (close fsubid))

    (setf ftensor (open fntensor :direction :output :if-exists :supersede
			:if-does-not-exist :create))	
    (format t "loading gaston lg mapping~%")
    (time-profiling (setf alg-hnlab (gaston-lg-mapping fnlg)))
    (setf hnlab (second alg-hnlab))
    
    (dolist (did dids)
      (setf doc (document did))
      (setf i (gethash (name doc) hdoc-tid))
      ;; (format t "scanning doc ~a~%" (name doc))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(setf lgid (get-linkage-graph sen lg-type))
	(when lgid
	  (setf h-sigsubs (get-sigsubs-of-lg lgid))
	  (setf pns (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	  (maphash 
	   #'(lambda (sub-map-id ls-map)
	       (setf sub-id (parse-integer (car (split-re "-" sub-map-id))))
	       (unless (tensor-exclude-sg? sub-id)
		 (setf sub-name (gethash sub-id hsub-id2name))
		 
		 (setf subtid (gethash sub-name hsub-tid))
		 (when subtid
		   (maphash 
		    #'(lambda (subn lgn)
			(declare (ignorable subn))
			(when (< lgn (length pns))
			  (setf pn (elt pns lgn))
			  (setf pnstr (medg-norm (content pn))))
			(when (and pnstr (match-re "[a-zA-Z]" pnstr))
			  (setf str-id (gethash pnstr hpn-tid))
			  (when str-id 
			    (setf key (format nil "~a ~a ~a" 
					      i subtid str-id))
			    (incf (gethash key tensor 0))))
			
			(setf pnstr (gethash (cons lgid lgn) hnlab))
			(when (and pnstr (match-re "[a-zA-Z]" pnstr))
			  (setf str-id (gethash pnstr hpn-tid))
			  (when str-id 
			    (setf key (format nil "~a ~a ~a" 
					      i subtid str-id))
			    (incf (gethash key tensor 0)))))
		    ls-map))))
	   h-sigsubs)))
      
      (dolist (kv (hash-table-alist tensor))
	(if log?
	    (format ftensor "~a ~a~%" (car kv) (log (1+ (cdr kv))))
	  (format ftensor "~a ~a~%" (car kv) (cdr kv))))
      (clrhash tensor))
    (close ftensor)))

(defun h-doc-subgraph-matrix 
    (corpn fnmatrix fndocid fnsubid
	   &key
	   (lg-type "plain_graph|parse-node-stanford-hier-tagged")
	   (log? nil))
  (concept-graph-init)
  (let* ((matrix (make-hash-table :test #'equalp))
	 (hdoc-name (make-hash-table :test #'equalp)) ;; docname -> tensor index
	 (hsub-name (make-hash-table :test #'equalp)) ;; subname -> tensor index
	 (hsub-id2name (load-sub-id2name))
	 (cnt 0)
	 doc sub-name lgid sg-lgns sgs
	 key subtid doctid doc-name)
    (format t "~&loading subgraph index~%")
    (let* ((fsubid (open fnsubid :direction :input))
	   (idx 0) ln)
      (loop (unless (setf ln (read-ne-line fsubid)) (return))
	    (setf (gethash ln hsub-name) (incf idx)))
      (close fsubid))

    (format t "~&loading document index~%")
    (let* ((fdocid (open fndocid :direction :input))
	   (idx 0) ln)
      (loop (unless (setf ln (read-ne-line fdocid)) (return))
	    (setf (gethash ln hdoc-name) (incf idx)))
      (close fdocid))

    (dolist (did (documents (corpus corpn)))
      (incf cnt)
      (when (= 0 (mod cnt 500))
	(format t "~&processed ~a docs~%" cnt))
      (setf doc (document did))
      (setf doc-name (name doc))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(when (setf lgid (get-linkage-graph sen lg-type))
	  (setf sg-lgns (get-sg-mapped-lgn lgid))
	  (setf sgs (mapcar #'first sg-lgns))
	  ;; try not to over count
	  (setf sgs (remove-duplicates sgs :test #'equalp))
	  (setf sgs (remove-if #'(lambda (a) (tensor-exclude-sg? a)) sgs))
	  (dolist (sub-id sgs)
	    (setf sub-name (gethash sub-id hsub-id2name))
	    (setf subtid (gethash sub-name hsub-name))
	    (setf doctid (gethash doc-name hdoc-name))
	    (setf key (format nil "~a ~a" subtid doctid))
	    (incf (gethash key matrix 0))))))
    
    (let* ((fmatrix (open fnmatrix :direction :output :if-exists :supersede
			  :if-does-not-exist :create)))
      (dolist (kv (hash-table-alist matrix))
	(if log?
	    (format fmatrix "~a ~a~%" (car kv) (log (1+ (cdr kv))))
	  (format fmatrix "~a ~a~%" (car kv) (cdr kv))))
      (close fmatrix))))

(defun h-doc-pn-matrix 
    (corpn fnmatrix fndoc-mid fnpn-mid
	   &key
	   (lg-type "plain_graph|parse-node-stanford-hier-tagged")
	   (binary? nil)
	   (log? nil))
  (concept-graph-init)
  (let* ((matrix (make-hash-table :test #'equalp))
	 (hdoc-mid (read-idx-hash fndoc-mid)) ;; docname -> matrix index
	 (hpn-mid (read-idx-hash fnpn-mid)) ;; pn -> matrix index
	 (hpn-cnt (make-hash-table :test #'equalp))
	 (cnt 0)
	 doc lgid pns pn pnstr str-id sg-lgns lgns
	 key doctid doc-name)
    (dolist (did (documents (corpus corpn)))
      (incf cnt)
      (setf doc (document did))
      (setf doc-name (name doc))
      (when (= 0 (mod cnt 500))
	(format t "~&processed ~a docs~%" cnt))
      (setf doctid (gethash doc-name hdoc-mid))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(when (setf lgid (get-linkage-graph sen lg-type))
	  (setf pns (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	  (setf sg-lgns (get-sg-mapped-lgn lgid))
	  (setf sg-lgns (remove-if #'(lambda (a) (tensor-exclude-sg? (first a)))
				   sg-lgns))
	  (setf lgns (mapcar #'second sg-lgns))
	  (setf lgns (remove-duplicates lgns :test #'equalp))
	  (dolist (lgn lgns)
	    (when (< lgn (length pns))
	      (setf pn (elt pns lgn))
	      (setf pnstr (medg-norm (content pn)))
	      (when (and pnstr (match-re "[a-zA-Z]" pnstr))
		(incf (gethash pnstr hpn-cnt 0))
		(setf str-id (gethash pnstr hpn-mid))
		(setf key (format nil "~a ~a" doctid str-id))
		(if binary?
		    (setf (gethash key matrix) 1)
		  (incf (gethash key matrix 0)))))))))
    
    (let* ((fpn-cnt (open "late:;matlab_data;lymphoma_train_pn_cnt"
			  :direction :output :if-exists :supersede
			  :if-does-not-exist :create)))
      (dolist (kv (hash-table-val-desc-alist hpn-cnt))
	(format fpn-cnt "~a ~a~%" (car kv) (cdr kv)))
      (close fpn-cnt))
    (write-matrix fnmatrix matrix :log? log?)))

(defun h-doc-wd-matrix 
    (corpn fnmatrix fndoc-mid fnwd-mid
	   &key
	   (lg-type "plain_graph|parse-node-stanford-hier-tagged")
	   (log? nil))
  (concept-graph-init)
  (let* ((matrix (make-hash-table :test #'equalp))
	 (hdoc-mid (read-idx-hash fndoc-mid)) ;; docname -> matrix index
	 (hwd-mid (read-idx-hash fnwd-mid)) ;; wd -> matrix index
	 (cnt 0)
	 doc lgid pns pn pnstr str-id sg-lgns lgns doctid doc-name)
    
    (dolist (did (documents (corpus corpn)))
      (incf cnt)
      (setf doc (document did))
      (setf doc-name (name doc))
      (when (= 0 (mod cnt 500))
	(format t "~&processed ~a docs~%" cnt))
      (setf doctid (gethash doc-name hdoc-mid))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(when (setf lgid (get-linkage-graph sen lg-type))
	  (setf pns (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	  (setf sg-lgns (get-sg-mapped-lgn lgid))
	  (setf sg-lgns (remove-if #'(lambda (a) (tensor-exclude-sg? (first a)))
				   sg-lgns))
	  (setf lgns (mapcar #'second sg-lgns))
	  (setf lgns (remove-duplicates lgns :test #'equalp))
	  (dolist (lgn lgns)
	    (when (< lgn (length pns))
	      (setf pn (elt pns lgn))
	      (setf pnstr (medg-norm (content pn)))
	      (when (and pnstr (match-re "[a-zA-Z]" pnstr))
		(dolist (wd (split-re "\\s+" pnstr))
		  (setf str-id (gethash wd hwd-mid))
		  (matrix-add matrix doctid str-id))))))))
    (write-matrix fnmatrix matrix :log? log?)))




(defun h-doc-full-pn-matrix (corpn fnmatrix fndocid fn-full-pnid
				   &key
				   (binary? nil)
				   (log? nil))
  (concept-graph-init)
  (let* ((matrix (make-hash-table :test #'equalp))
	 (hdoc-name (make-hash-table :test #'equalp)) ;; docname -> matrix index
	 (hpn-name (make-hash-table :test #'equalp)) ;; pn -> matrix index
	 (hpn-cnt (make-hash-table :test #'equalp))
	 doc pns pnstr str-id key doctid doc-name f-full-pnid)

    (format t "~&loading document index~%")
    (let* ((fdocid (open fndocid :direction :input))
	   (idx 0) ln)
      (loop (unless (setf ln (read-ne-line fdocid)) (return))
	    (setf (gethash ln hdoc-name) (incf idx)))
      (close fdocid))

    (setf f-full-pnid (open fn-full-pnid :direction :output 
			    :if-exists :supersede 
			    :if-does-not-exist :create))
    (dolist (did (documents (corpus corpn)))
      (setf doc (document did))
      (setf doc-name (name doc))
      (setf doctid (gethash doc-name hdoc-name))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(when (not (maskedp sen))
	  (setf pns (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	  (dolist (pn pns)
	    (setf pnstr (medg-norm (content pn)))
	    
	    (when (and pnstr (match-re "[a-zA-Z]" pnstr))
	      (incf (gethash pnstr hpn-cnt 0))
	      (setf str-id (gethash pnstr hpn-name))
	      (unless str-id 
		(setf str-id (1+ (hash-table-count hpn-name)))
		(format f-full-pnid "~a~%" pnstr)
		(setf (gethash pnstr hpn-name) str-id))
	      (setf key (format nil "~a ~a" doctid str-id))
	      (if binary?
		  (setf (gethash key matrix) 1)
		(incf (gethash key matrix 0))))))))

    (close f-full-pnid)

    (let* ((fmatrix (open fnmatrix :direction :output :if-exists :supersede
			  :if-does-not-exist :create)))
      (dolist (kv (hash-table-alist matrix))
	(if log?
	    (format fmatrix "~a ~a~%" (car kv) (log (1+ (cdr kv))))
	  (format fmatrix "~a ~a~%" (car kv) (cdr kv))))
      (close fmatrix))
    
    (let* ((fpn-cnt (open "late:;matlab_data;lymphoma_train_full_pn_cnt"
			  :direction :output :if-exists :supersede
			  :if-does-not-exist :create)))
      (dolist (kv (hash-table-val-desc-alist hpn-cnt))
	(format fpn-cnt "~a ~a~%" (car kv) (cdr kv)))
      (close fpn-cnt))))

(defun h-sg-lg-matrix 
    (corpn fnmatrix fnlgid fnsubid
	   &key
	   (lg-type "plain_graph|parse-node-stanford-hier-tagged")
	   (log? nil))
  "Row corresponds to subgraph and column graph"
  (concept-graph-init)
  (let* ((matrix (make-hash-table :test #'equalp))
	 (hlg-id (make-hash-table :test #'equalp)) ;; lg-id -> matrix index
	 (hsub-name (make-hash-table :test #'equalp)) ;; subname -> matrix index
	 (hsub-id2name (load-sub-id2name))
	 doc sub-ids sub-name lg-id lg-mid key sub-mid)
    (format t "~&loading subgraph index~%")
    (let* ((fsubid (open fnsubid :direction :input))
	   (idx 0) ln)
      (loop (unless (setf ln (read-ne-line fsubid)) (return))
	    (setf (gethash ln hsub-name) (incf idx)))
      (close fsubid))

    (dolist (did (documents (corpus corpn)))
      (setf doc (document did))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(when (setf lg-id (get-linkage-graph sen lg-type))
	  (setf sub-ids (get-subids-of-lg lg-id))
	  (setf lg-mid (gethash lg-id hlg-id))
	  (unless lg-mid
	    (setf lg-mid (1+ (hash-table-count hlg-id)))
	    (setf (gethash lg-id hlg-id) lg-mid))
	  (dolist (sub-id sub-ids)
	    (setf sub-name (gethash sub-id hsub-id2name))
	    (setf sub-mid (gethash sub-name hsub-name))
	    (when sub-mid
	      (setf key (format nil "~a ~a" sub-mid lg-mid))
	      (incf (gethash key matrix 0)))))))
    
    (let* ((flgid (open fnlgid :direction :output :if-exists :supersede
			:if-does-not-exist :create)))
      (dolist (kv (hash-table-val-ascd-alist hlg-id))
	(format flgid "~a~%" (car kv)))
      (close flgid))

    (let* ((fmatrix (open fnmatrix :direction :output :if-exists :supersede
			  :if-does-not-exist :create)))
      (dolist (kv (hash-table-alist matrix))
	(if log?
	    (format fmatrix "~a ~a~%" (car kv) (log (1+ (cdr kv))))
	  (format fmatrix "~a ~a~%" (car kv) (cdr kv))))
      (close fmatrix))))

(defun h-subgraph-pnode-matrix 
    (corpn fnmatrix fnpn-mid fnsg-mid 
	   &key
	   (binary? nil)
	   (lg-type "plain_graph|parse-node-stanford-hier-tagged")
	   (log? nil))
  "Generate the subgraph pnode matrix in the sparse matrix format.
Note that this has to be done after tensor generation, so as to keep the 
indexing of subgraphs consistent"
  (concept-graph-init)
  (let* ((matrix (make-hash-table :test #'equalp))
	 (hpn-mid (read-idx-hash fnpn-mid)) ;; word -> tensor index
	 (hsg-mid (read-idx-hash fnsg-mid)) ;; subname -> tensor index
	 (hsub-id2name (load-sub-id2name))
	 (cnt 0)
	 doc sub-id sg-mid lgid pns pn pnstr sg-lgns lgn
	 str-id key subtid)

    (dolist (did (documents (corpus corpn)))
      (incf cnt)
      (when (= 0 (mod cnt 500))
	(format t "~&processed ~a docs~%" cnt))
      (setf doc (document did))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(when (setf lgid (get-linkage-graph sen lg-type))
	  (setf sg-lgns (get-sg-mapped-lgn lgid))
	  (setf sg-lgns (remove-if #'(lambda (a) (tensor-exclude-sg? (first a)))
				   sg-lgns))
	  (setf sg-lgns (remove-duplicates sg-lgns :test #'equalp))
	  (setf pns (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	  (dolist (sg-lgn sg-lgns)
	    (setf lgn (second sg-lgn)
		  sub-id (first sg-lgn))
	    (setf sg-mid (gethash sub-id hsub-id2name))
	    (setf subtid (gethash sg-mid hsg-mid))
	    (when (< lgn (length pns))
	      (setf pn (elt pns lgn))
	      (setf pnstr (medg-norm (content pn)))
	      (when (and pnstr (match-re "[a-zA-Z]" pnstr))
		(setf str-id (gethash pnstr hpn-mid))
		(unless str-id
		  (error "~a: ~a" sg-mid pnstr))
		(setf key (format nil "~a ~a" subtid str-id))
		(if binary?
		    (setf (gethash key matrix) 1)
		  (incf (gethash key matrix 0)))))))))
    
    (let* ((fmatrix (open fnmatrix :direction :output :if-exists :supersede
			  :if-does-not-exist :create)))
      (dolist (kv (hash-table-alist matrix))
	(if log?
	    (format fmatrix "~a ~a~%" (car kv) (log (1+ (cdr kv))))
	  (format fmatrix "~a ~a~%" (car kv) (cdr kv))))
      (close fmatrix))))


(defun h-subgraph-word-matrix 
    (corpn fnmatrix fnwd-mid fnsg-mid 
	   &key
	   (lg-type "plain_graph|parse-node-stanford-hier-tagged")
	   (log? nil))
  "Generate the subgraph pnode matrix in the sparse matrix format.
Note that this has to be done after tensor generation, so as to keep the 
indexing of subgraphs consistent"
  (concept-graph-init)
  (let* ((matrix (make-hash-table :test #'equalp))
	 (hwd-mid (read-idx-hash fnwd-mid)) ;; word -> tensor index
	 (hsg-mid (read-idx-hash fnsg-mid)) ;; subname -> tensor index
	 (hsub-id2name (load-sub-id2name))
	 (cnt 0)
	 doc sub-id sg-mid lgid pns pn pnstr sg-lgns lgn wd-mid)

    (dolist (did (documents (corpus corpn)))
      (incf cnt)
      (when (= 0 (mod cnt 500))
	(format t "~&processed ~a docs~%" cnt))
      (setf doc (document did))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(when (setf lgid (get-linkage-graph sen lg-type))
	  (setf sg-lgns (get-sg-mapped-lgn lgid))
	  (setf sg-lgns (remove-if #'(lambda (a) (tensor-exclude-sg? (first a)))
				   sg-lgns))
	  (setf sg-lgns (remove-duplicates sg-lgns :test #'equalp))
	  (setf pns (annotations-spec sen :type (gtype 'ghier-parse-node-type)))
	  (dolist (sg-lgn sg-lgns)
	    (setf lgn (second sg-lgn)
		  sub-id (first sg-lgn))
	    (setf sg-mid (gethash sub-id hsub-id2name))
	    (setf sg-mid (gethash sg-mid hsg-mid))
	    (when (< lgn (length pns))
	      (setf pn (elt pns lgn))
	      (setf pnstr (medg-norm (content pn)))
	      (when (and pnstr (match-re "[a-zA-Z]" pnstr))
		(dolist (wd (split-re "\\s+" pnstr))
		  (setf wd-mid (gethash wd hwd-mid))
		  (unless wd-mid
		    (error "~a: ~a" sg-mid wd))
		  (matrix-add matrix sg-mid wd-mid))))))))
    (write-matrix fnmatrix matrix :log? log?)))


(defun flush-concept-graph (docid &aux cmd)
  (setf cmd (format nil "SELECT gid FROM graph WHERE gname LIKE '~a_%'" docid))
  (dolist (gid (mapcar #'car (latesql cmd)))
    (latesql "DELETE FROM linkage_graph WHERE lg_id=~a" gid)
    (latesql "DELETE FROM graph WHERE gid=~a" gid)))


(defun tui-coloc (corpn
		  &key 
		  (lg-type "plain_graph|parse-node-stanford-hier-tagged")
		  (fngraph "late:;corpus_coloc_graph.csv")
		  (fnnode "late:;corpus_coloc_node.csv")
		  &aux
		  (tnet (make-hash-table :test #'equalp))
		  (h-nodes (make-hash-table :test #'equalp))
		  fgraph fnode)
  (dolist (docid (documents (corpus corpn)))
    (let* ((doc (document docid))
	   lg-id nid lab)
      
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(clrhash h-nodes)
	(setf lg-id (get-linkage-graph sen lg-type))
	(dolist (nid-lab (get-lg-nodes lg-id))
	  (setf nid (first nid-lab))
	  (setf lab (second nid-lab))
	  (when (match-re "^[A-Z]" lab)
	    (setf (gethash nid h-nodes) lab)))
	
	(do* ((nodes (hash-table-alist h-nodes) (cdr nodes))
	      (nnodes (cdr nodes) (cdr nodes)))
	    ((not nnodes))
	  (let* ((n1-lab (cdar nodes))
		 (n2-lab (cdar nnodes)))
	    (cond 
	     ((string-lessp n1-lab n2-lab)
	      (incf (gethash (list n1-lab n2-lab) tnet 0)))
	     ((string-lessp n2-lab n1-lab)
	      (incf (gethash (list n2-lab n1-lab) tnet 0)))))))))
  (clrhash h-nodes)
  (setf fgraph (open fngraph :direction :output :if-exists :supersede 
		     :if-does-not-exist :create))
  
  (format fgraph "~&Source~tTarget~tCount~%")
  
  (dolist (kv (hash-table-val-desc-alist tnet))
    (let* ((n1-lab (first (car kv)))
	   (n2-lab (second (car kv)))
	   (cnt (cdr kv)))
      (when (> 100 cnt) 
	(return))
      (if (match-re "^[A-Z]" n1-lab)
	  (setf (gethash n1-lab h-nodes) "tui")
	(setf (gethash n1-lab h-nodes) "non-tui"))
      (if (match-re "^[A-Z]" n2-lab)
	  (setf (gethash n2-lab h-nodes) "tui")
	(setf (gethash n2-lab h-nodes) "non-tui"))
      (format fgraph "~&~a~t~a~t~a~%" n1-lab n2-lab cnt)))
  (close fgraph)
  
  (setf fnode (open fnnode :direction :output :if-exists :supersede 
		    :if-does-not-exist :create))
  (format fnode "~&Id~tLabel~tType~%")
  (dolist (kv (hash-table-alist h-nodes))
    (let* ((nlab (car kv))
	   (ntype (cdr kv)))
      (format fnode "~&~a~t~a~t~a~%" nlab nlab ntype)))
  (close fnode))

(defun tui-network (corpn
		    &key 
		    (lg-type "plain_graph|parse-node-stanford-hier-tagged")
		    (fngraph "late:;corpus_graph.csv")
		    (fnnode "late:;corpus_node.csv")
		    &aux
		    (tnet (make-hash-table :test #'equalp))
		    (h-nodes (make-hash-table :test #'equalp))
		    fgraph fnode)
  (dolist (docid (documents (corpus corpn)))
    (let* ((doc (document docid))
	   lg-id nid n1 n2 n1-lab n2-lab lab)
      
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(clrhash h-nodes)
	(setf lg-id (get-linkage-graph sen lg-type))
	(dolist (nid-lab (get-lg-nodes lg-id))
	  (setf nid (first nid-lab))
	  (setf lab (second nid-lab))
	  (setf (gethash nid h-nodes) lab))
	(dolist (n1-n2-lab (get-lg-edges lg-id))
	  (setf n1 (first n1-n2-lab))
	  (setf n2 (second n1-n2-lab))
	  (setf lab (third n1-n2-lab))
	  (setf n1-lab (gethash n1 h-nodes))
	  (setf n2-lab (gethash n2 h-nodes))
	  (when (and n1-lab n2-lab)
	    (if (string-lessp n1-lab n2-lab)
		(incf (gethash (list n1-lab n2-lab lab) tnet 0))
	      (incf (gethash (list n2-lab n1-lab lab) tnet 0))))))))
  (clrhash h-nodes)
  (setf fgraph (open fngraph :direction :output :if-exists :supersede 
		     :if-does-not-exist :create))
  
  (format fgraph "~&Source~tTarget~tLabel~tCount~%")
  
  (dolist (kv (hash-table-val-desc-alist tnet))
    (let* ((n1-lab (first (car kv)))
	   (n2-lab (second (car kv)))
	   (elab (third (car kv)))
	   (cnt (cdr kv)))
      (when (> 100 cnt) 
	(return))
      (if (match-re "^[A-Z]" n1-lab)
	  (setf (gethash n1-lab h-nodes) "tui")
	(setf (gethash n1-lab h-nodes) "non-tui"))
      (if (match-re "^[A-Z]" n2-lab)
	  (setf (gethash n2-lab h-nodes) "tui")
	(setf (gethash n2-lab h-nodes) "non-tui"))
      (format fgraph "~&~a~t~a~t~a~t~a~%" n1-lab n2-lab elab cnt)))
  (close fgraph)
  
  (setf fnode (open fnnode :direction :output :if-exists :supersede 
		    :if-does-not-exist :create))
  (format fnode "~&Id~tLabel~tType~%")
  (dolist (kv (hash-table-alist h-nodes))
    (let* ((nlab (car kv))
	   (ntype (cdr kv)))
      (format fnode "~&~a~t~a~t~a~%" nlab nlab ntype)))
  (close fnode))



(defun immunologic-tui-network 
    (corpn
     &key 
     (lg-type "plain_graph|parse-node-stanford-hier-tagged")
     (fngraph "late:;corpus_immunologic_graph.csv")
     (fnnode "late:;corpus_immunologic_node.csv")
     &aux
     (tnet (make-hash-table :test #'equalp))
     (h-nodes (make-hash-table :test #'equalp))
     fgraph fnode)
  (dolist (docid (documents (corpus corpn)))
    (let* ((doc (document docid))
	   lg-id nid n1 n2 n1-lab n2-lab lab)
      
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(clrhash h-nodes)
	(when (annotations sen :type 'tui-annotation
			   :filter #'(lambda (a) (equalp "T129" (data a))))
	  (setf lg-id (get-linkage-graph sen lg-type))
	  (dolist (nid-lab (get-lg-nodes lg-id))
	    (setf nid (first nid-lab))
	    (setf lab (second nid-lab))
	    (setf (gethash nid h-nodes) lab))
	  (dolist (n1-n2-lab (get-lg-edges lg-id))
	    (setf n1 (first n1-n2-lab))
	    (setf n2 (second n1-n2-lab))
	    (setf lab (third n1-n2-lab))
	    (setf n1-lab (gethash n1 h-nodes))
	    (setf n2-lab (gethash n2 h-nodes))
	    (when (and n1-lab n2-lab)
	      (if (string-lessp n1-lab n2-lab)
		  (incf (gethash (list n1-lab n2-lab lab) tnet 0))
		(incf (gethash (list n2-lab n1-lab lab) tnet 0)))))))))
  (clrhash h-nodes)
  (setf fgraph (open fngraph :direction :output :if-exists :supersede 
		     :if-does-not-exist :create))
  
  (format fgraph "~&Source~tTarget~tLabel~tCount~%")
  
  (dolist (kv (hash-table-val-desc-alist tnet))
    (let* ((n1-lab (first (car kv)))
	   (n2-lab (second (car kv)))
	   (elab (third (car kv)))
	   (cnt (cdr kv)))
      (if (match-re "^[A-Z]" n1-lab)
	  (setf (gethash n1-lab h-nodes) "tui")
	(setf (gethash n1-lab h-nodes) "non-tui"))
      (if (match-re "^[A-Z]" n2-lab)
	  (setf (gethash n2-lab h-nodes) "tui")
	(setf (gethash n2-lab h-nodes) "non-tui"))
      (format fgraph "~&~a~t~a~t~a~t~a~%" n1-lab n2-lab elab cnt)))
  (close fgraph)
  
  (setf fnode (open fnnode :direction :output :if-exists :supersede 
		    :if-does-not-exist :create))
  (format fnode "~&Id~tLabel~tType~%")
  (dolist (kv (hash-table-alist h-nodes))
    (let* ((nlab (car kv))
	   (ntype (cdr kv)))
      (format fnode "~&~a~t~a~t~a~%" nlab nlab ntype)))
  (close fnode))





(defun remove-parts-sents (&key (gtype "plain_graph"))
  (dolist (iter (latesql "SELECT gid, gname FROM graph WHERE gtype=~a" 
			 (sq gtype)))
    (let* ((gid (first iter))
	   (gname (second iter))
	   (doc-sen (split-re "_" gname))
	   (senid (parse-integer (second doc-sen)))
	   (sen (get-annotation senid nil)))
      (when (maskedp sen)
	(latesql "DELETE FROM linkage_graph WHERE lg_id=~a" gid)
	(latesql "DELETE FROM graph WHERE gid=~a" gid)
	(format t "~&deleted concept-graph for ~a~%" sen)))))

(defun output-row-ldac (row fldac)
  (when (> (hash-table-count row) 0)
    (format fldac "~a" (hash-table-count row))
    (dolist (kv (hash-table-alist row))
      (format fldac " ~a:~a" (car kv) (cdr kv)))
    (format fldac "~%")
    (setf row (make-hash-table :test #'equalp))))

(defun output-subgraph-list (fnsg
			     &key (sg-type "sig_subgraph|hier"))
  (let* ((fout (open fnsg :direction :output :if-exists :supersede
		     :if-does-not-exist :create))
	 (ans (latesql "select distinct gname from graph where gtype=~a" 
		       sg-type)))
    (format fout "~{~a~^~%~}" (mapcar #'car ans))
    (close fout)))

(defun print-all-sg (fnsgstr)
  "Given subgraph name list, print subgraph string"
  (let* ((fsgstr (open-new fnsgstr))
	 sgns)
    (setf sgns (latesql "select distinct sub_id from sig_subgraph order by significance desc"))
    (setf sgns (mapcar #'car sgns))
    (dolist (sgn sgns)
      (format fsgstr "~a~%" (db-sgid->str sgn)))
    (close fsgstr)))
