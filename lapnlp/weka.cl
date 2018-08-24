;;; -*- Mode: Lisp; Package: weka; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/10/2015 rewrite using asdf framework
yluo - 05/14/2010 changed the way wrapper are loaded, now on demand basis
yluo - 10/20/2009 creation 

See LATE-vs-cascading_file.vsd for reason to operate WEKA API directly on
in-memory or DB stored features.
TODO: 1. WEKA API operate directly on in-memory features;
      2. Store the features in DB in serialized part of the corresponding
         annotations, lisp will automatically unserialize them.
The reason that I have to write these WEKA wrappers is that WEKA in general
expects .arff files as input rather than chunks from memory
|#
(defpackage :weka
  (:use #+allegro :excl :common-lisp :javatools.jlinker :java-wrap-gen 
	:list-gen :jc :late :dbi.mysql)
  (:export "weka-init"
	   "weka-is-missing-val"
	   "make-weka-attr-list"
	   "make-weka-instance"
	   "make-weka-instances"
	   "filter-data"
	   "filter-insts"
	   "batch-filter-insts"
	   "batch-filter-data"
	   "str-to-word-vector"
	   "str-to-word-vector-insts"
	   "batch-str-to-word-vector-insts"
	   "batch-str-to-word-vector"
	   "discretize"
	   "discretize-insts"
	   "batch-discretize-insts"
	   "batch-discretize"
	   "weka-get-flag"
	   "weka-get-option"
	   "weka-get-pmml-model"
	   "eval-classifier"
	   "SMO-classify"
	   "batch-data-classify"
	   "*weka-db*"
	   "open-weka-database"
	   "weka-transaction"
	   "*weka-mysql-create-commands*"
	   "*weka-mysql-drop-commands*"
	   "create-weka-database"
	   "close-weka-database"
	   "update-threshold-q"
	   "update-object-q"
	   "update-result-q"
	   "insert-classifier-q"
	   ))

;;; in-package has effect at both compile time and load time
(in-package :weka)


(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; if jlinker checks whether jar file has loaded upon def-java-class
  ;; you need to load weka.jar first.
  

  (write-java-wrapper "java.io.ByteArrayInputStream"
		      "late:;java;io;ByteArrayInputStream.cl"
		      nil)
  (require :java.io.ByteArrayInputStream "late:;java;io;ByteArrayInputStream")
  
  (write-java-wrapper "java.util.Random"
		      "late:;java;util;Random.cl"
		      nil)
  (require :java.util.Random "late:;java;util;Random")
  
  (write-java-wrapper "java.io.PipedInputStream"
		      "late:;java;io;PipedInputStream.cl"
		      nil)
  (require :java.io.PipedInputStream "late:;java;io;PipedInputStream")

  (write-java-wrapper "java.lang.StringBuffer"
		      "late:;java;lang;StringBuffer.cl"
		      nil)
  (require :java.lang.StringBuffer "late:;java;lang;StringBuffer")
  
  (write-java-wrapper "java.io.ObjectInputStream"
		      "late:;java;io;ObjectInputStream.cl"
		      nil)
  (require :java.io.ObjectInputStream "late:;java;io;ObjectInputStream")
  
  (write-java-wrapper "java.io.BufferedInputStream"
		      "late:;java;io;BufferedInputStream.cl"
		      nil)
  (require :java.io.BufferedInputStream "late:;java;io;BufferedInputStream")
  
  (write-java-wrapper "java.io.ByteArrayOutputStream"
		      "late:;java;io;ByteArrayOutputStream.cl"
		      nil)
  (require :java.io.ByteArrayOutputStream 
	   "late:;java;io;ByteArrayOutputStream")
  
  (write-java-wrapper "java.io.ObjectOutputStream"
		      "late:;java;io;ObjectOutputStream.cl"
		      nil)
  (require :java.io.ObjectOutputStream
	   "late:;java;io;ObjectOutputStream")
  
  (write-java-wrapper "java.io.BufferedOutputStream"
		      "late:;java;io;BufferedOutputStream.cl"
		      nil)
  
  (require :java.io.BufferedOutputStream
	   "late:;java;io;BufferedOutputStream")
  
  (require :weka.core.FastVector 
	   "late:;weka;core;FastVector")
  
  (require :weka.core.Attribute 
	   "late:;weka;core;Attribute")
  
  (require :weka.core.Instances 
	   "late:;weka;core;Instances")
  
  (require :weka.core.Instance 
	   "late:;weka;core;Instance")
  
  (require :weka.core.OptionHandler 
	   "late:;weka;core;OptionHandler")
  
  (require :weka.filters.Filter 
	   "late:;weka;filters;Filter")
  
  (require :weka.filters.unsupervised.attribute.StringToWordVector 
	   "late:;weka;filters;unsupervised;attribute;StringToWordVector")
  
  (require :weka.filters.unsupervised.attribute.Discretize 
	   "late:;weka;filters;unsupervised;attribute;Discretize")
  
  (require :weka.core.Utils 
	   "late:;weka;core;Utils")
  
  (require :weka.core.pmml.PMMLFactory 
	   "late:;weka;core;pmml;PMMLFactory")
  
  (require :weka.core.xml.XMLOptions 
	   "late:;weka;core;xml;XMLOptions")
  
  (require :weka.classifiers.xml.XMLClassifier 
	   "late:;weka;classifiers;xml;XMLClassifier")
  
  (require :weka.core.xml.XMLSerialization 
	   "late:;weka;core;xml;XMLSerialization")
  
  (require :weka.classifiers.Classifier 
	   "late:;weka;classifiers;Classifier")
  
  (require :weka.core.xml.KOML 
	   "late:;weka;core;xml;KOML")
  
  (require :weka.classifiers.CostMatrix 
	   "late:;weka;classifiers;CostMatrix")
  
  (require :weka.core.Range 
	   "late:;weka;core;Range")
  
  (require :weka.classifiers.Evaluation 
	   "late:;weka;classifiers;Evaluation")
  
  (require :weka.classifiers.UpdateableClassifier 
	   "late:;weka;classifiers;UpdateableClassifier")
  
  (require :weka.core.Drawable 
	   "late:;weka;core;Drawable")
  
  (require :weka.classifiers.evaluation.ThresholdCurve 
	   "late:;weka;classifiers;evaluation;ThresholdCurve")
  
  )


;;; persistence storage section
;;; This section uses basic utilities from persistence.cl
;;; can store the feature vector (WEKA compatible or not) in other field of 
;;; corpus, document or annotation tables.
;;; this part handles the storage of models (classifiers and instances, I feel
;;; instances should rather be stored here instead of in the other field of 
;;; corpus document and annotation tables, because each dative run may 
;;; generate different feature set. Better to store also the feature 
;;; extraction code, this should be straightforward as Lisp treats code as data
;;; ), cost matrices
(defparameter *weka-db* nil
  "The connection to the Mysql database holding WEKA's persistent store.")

;;; TODO: create user and database for WEKA in Mysql
(defun open-weka-database ()
  "Opens connection to the Mysql database used by WEKA."
  (when (and *weka-db* (not (mysql-connected *weka-db*)))
    (setq *weka-db* nil))
  (unless *weka-db*
    (setq *weka-db* (open-mysql-db "WEKA_DB"))))

;;; duplicate the LATE counterpart
(defmacro weka-transaction (&body body)
  `(prog2 (progn (open-weka-database)
		 (sql "start transaction" :db *weka-db*))
       (progn ,@body)
     (sql "commit" :db *weka-db*)))

;;; TODO: separate these tables out to a learning database.
(defparameter *weka-mysql-create-commands*
    '("CREATE TABLE IF NOT EXISTS classifier (
         id INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY,
         storage_type VARCHAR(255) NOT NULL DEFAULT 'java-bin',
         content LONGBLOB,
         updated_at TIMESTAMP NOT NULL,
         target_type VARCHAR(255) NOT NULL DEFAULT 'document',
         result LONGTEXT,
         result_type VARCHAR(255) NOT NULL DEFAULT 'text',
         threshold LONGTEXT,
         code_version VARCHAR(255),
         cost_matrix LONGTEXT,
         INDEX time_ind (updated_at))"
     
      "CREATE TABLE IF NOT EXISTS model_instances (
         inst_id INTEGER NOT NULL DEFAULT '-1',
         inst_class VARCHAR(255) NOT NULL DEFAULT '',
         inst_type VARCHAR(255) NOT NULL DEFAULT 'train',
         inst_category VARCHAR(255) NOT NULL DEFAULT '',
         result_ann INTEGER,
         model_id INTEGER NOT NULL DEFAULT '-1',
         model_type VARCHAR(255) NOT NULL DEFAULT '',
         PRIMARY KEY (inst_id, inst_class, model_id, model_type),
         INDEX model_ind (model_id, model_type),
         INDEX category_ind (inst_category),
         FOREIGN KEY (inst_id) REFERENCES late.documents(id) 
           ON DELETE CASCADE,
         FOREIGN KEY (inst_id) REFERENCES late.instances(id)
           ON DELETE CASCADE,
         FOREIGN KEY (result_ann) REFERENCES late.annotations(id)
           ON DELETE CASCADE,
         FOREIGN KEY (model_id) REFERENCES classifier(id)
           ON DELETE CASCADE)"
      
      "CREATE TABLE IF NOT EXISTS model_cache (
         model_id INTEGER NOT NULL DEFAULT '-1',
         model_type VARCHAR(255) NOT NULL DEFAULT '',
         train_insts LONGBLOB,
         test_insts LONGBLOB,
         PRIMARY KEY (model_id, model_type),
         FOREIGN KEY (model_id) REFERENCES classifier(id)
           ON DELETE CASCADE)"
      )
  "SQL commands to create the tables for persistent storage of WEKA data.")

(defparameter *weka-mysql-drop-commands*
    '("DROP TABLE IF EXISTS classifier"
      "DROP TABLE IF EXISTS model_instances"
      "DROP TABLE IF EXISTS model_cache")
  "Should match with *weka-mysql-create-commands*")

(defun create-weka-database (&optional (confirm nil))
  "Delete and re-creates an empty WEKA database. The optional argument must ~
   be given and not NIL, as a safety measure."
  (assert confirm
      (confirm)
    "Please confirm that you want to delete all data in the WEKA database.")
  (open-weka-database)
  (dolist (cmd *weka-mysql-drop-commands*)
    (sql cmd :db *weka-db*))
  (dolist (cmd *weka-mysql-create-commands*)
    (sql cmd :db *weka-db*))
  )

(defun close-weka-database ()
  "Commit all transactions and terminate connection to the database used by ~
   WEKA."
  (cond ((null *weka-db*) 
	 (format t "~&WEKA database already closed.~%"))
	
	((mysql-connected *weka-db*)
	 (sql "COMMIT" :db *weka-db*)
	 (disconnect :db *weka-db*)
	 (setf *weka-db* nil))
	
	(t 
	 (format t "~&WEKA database was already disconnected")))
  *weka-db*)

;;; input: cid - classifier id
(defun update-threshold-q (cid thresh-content)
  (format nil "UPDATE classifier SET threshold=~a WHERE id=~a" 
	       (sq thresh-content)
	       (sq cid)))

(defun update-object-q (cid obj-content)
  (format nil "UPDATE classifier SET content=~a WHERE id=~a"
	  (sq obj-content)
	  (sq cid)))

(defun update-result-q (result-type cid result-text)
  (format nil "UPDATE classifier SET result_type=~a, result=~a WHERE id=~a"
	  (sq result-type)
	  (sq result-text)
	  (sq cid)))

;;; should be coupled with mysql-insert-id
;;; input: code-version = repository + version
(defun insert-classifier-q (output-type target-type code-version cost-matrix)
  (format nil "INSERT INTO classifier SET storage_type=~a, target_type=~a, ~
               code_version=~a, cost_matrix=~a"
	  (sq output-type)
	  (sq target-type)
	  (sq code-version)
	  (sq cost-matrix)))



(defun find-file-candidate (where name-pattern)
  (let ((poss (directory (merge-pathnames name-pattern where))))
    (when (> (length poss) 1)
      (warn "~%Multiple matches found for WEKA component ~s" name-pattern))
    (and poss (car poss))))

;;; weka-init print warning message when weka is not 
;;; installed.
(defun weka-init ()

  (when (null *weka-home*)
    (format t "~%Warning: could not find location of WEKA toolkit.~%")
    (return-from weka-init nil)
    )
  (pushnew *weka-home* *classpath-dirs* :test #'equal)

  (when *weka-jar*
    (pushnew *weka-jar* *classpath-jars* :test #'equal))
  (unless (jlinker-query)
    (jl-init)
    )
  (jlinker-slot :jar-file *classpath-jars*)
  (jlinker-slot :classpath *classpath-dirs*)
  ;; (create-weka-database t)
  (open-weka-database)
  )

(def-java-class (atest "test")
    () () () ())

(def-java-constructor new-atest
    (atest))

(def-java-method (atestappend "append")
    (atest "java.util.ArrayList")
  )

(defun dumpalist ()
  (let* ((obj-test (new-atest))
	 (plist1 (java.util.ArrayList:ArrayList))
	 (plist2 (java.util.ArrayList:ArrayList))
	 (alist nil)
	 (res nil))
    (java.util.ArrayList:dr-add plist1 "original")
    (java.util.ArrayList:dr-add plist1 "stuff")
    (java.util.ArrayList:dr-add plist2 "appended")
    (java.util.ArrayList:dr-add plist2 "stuff")
    (format t "plist1: " plist1)
    (dotimes (i (java.util.ArrayList:dr-size plist1))
      (format t "~a " (java.util.ArrayList:dr-get plist1 i))
      )
    (format t "~%")
    (format t "plist2: " plist2)
    (dotimes (i (java.util.ArrayList:dr-size plist2))
      (format t "~a " (java.util.ArrayList:dr-get plist2 i))
      )
    (format t "~%")
    
 
    (atestappend obj-test plist1)
    (setq alist (atestappend obj-test plist2))
    (format t "plist1 has ~a elements~%" (java.util.ArrayList:dr-size  plist1))
    (format t "plist2 has ~a elements~%" (java.util.ArrayList:dr-size  plist2))
    (format t "alist has ~a elements~%" (java.util.ArrayList:dr-size alist))
    (dotimes (i (java.util.ArrayList:dr-size alist))
      (push (java.util.ArrayList:dr-get alist i) res)
      )
    (nreverse res)
    ))

;;; much of this flow from WEKA's readHeader in ArffLoader.java
;;; change it to return a FastVector
(defun make-weka-attr-list (attr-l)
  ;;(break "begin make-weka-attr-list")
  (format t "~&attr-l is ~s~%" attr-l)
  (let* ((attr-num (/ (length attr-l) 2))
	 attr-name attr-type attr
	 (attrs (weka.core.FastVector:FastVector))
	 )
    (dotimes (i attr-num)
      (setf attr-name (elt attr-l (* 2 i)))
      (setf attr-type (getf attr-l attr-name))
      (cond 
       ;; attribute is real, interger or numeric
       ;; string-equal ignore case
       ((and (stringp attr-type)
	     (or (string-equal attr-type "real") 
		 (string-equal attr-type "integer") 
		 (string-equal attr-type "numeric")))
	(setf attr (weka.core.Attribute:Attribute 
		    (format nil "~a" attr-name) i))
	;; (setf (getf attr-l attr-name) attr)
	(weka.core.FastVector:addElement attrs attr)
	)
       
       ;; attribute is string
       ;; "unigram" nil 29 resolves to something string, string, int
       ;; need to cast the nil here to a null reference to FastVector
       ;; use class literal to get a Class object, then use cast?
       ;; why cast seems to return a lisp value instead of a java tran-struct?
       ((and (stringp attr-type) (string-equal attr-type "string"))
	;; will nil be passed into JAVA as null?
	;; this is only a temporary fix here.
	(setf attr (jnew (jconstructor "weka.core.Attribute"
				       "java.lang.String"
				       "weka.core.FastVector"
				       "int")
			 (format nil "~a" attr-name)
			 nil
			 i)
	      )
	;; (setf (getf attr-l attr-name) attr)
	(weka.core.FastVector:addElement attrs attr)
	)
       
       ;; attribute is date
       ((and (listp attr-type) 
	     (stringp (car attr-type)) 
	     (stringp (second attr-type)) 
	     (string-equal (car attr-type) "date"))
	(setf attr (weka.core.Attribute:Attribute 
		    (format nil "~a" attr-name) (second attr-type) i))
	;; (setf (getf attr-l attr-name) attr)
	(weka.core.FastVector:addElement attrs attr)
	)
       
       ;; attribute is relational
       ((and (listp attr-type)
	     (stringp (car attr-type))
	     (string-equal (car attr-type) "relational")
	     (listp (second attr-type)))
	(let* ((relation (weka.core.Instances:Instances 
			  (format nil "~a" attr-name)
			  (make-weka-attr-list (second attr-type)) 0)))
	  (setf attr (weka.core.Attribute:Attribute 
		      (format nil "~a" attr-name) relation i))
	  ;; (setf (getf attr-l attr-name) attr)
	  (weka.core.FastVector:addElement attrs attr)
	  )
	)
       
       ;; attribute is nominal
       ((and (listp attr-type)
	     (stringp (car attr-type))
	     (string-equal (car attr-type) "nominal")
	     (listp (second attr-type)))
	(let* ((attr-vals (weka.core.FastVector:FastVector))
	       )
	  (dolist (val (second attr-type))
	    (weka.core.FastVector:addElement 
	     attr-vals
	     val)
	    )
	  (setf attr (weka.core.Attribute:Attribute 
		      (format nil "~a" attr-name) attr-vals i))
	  ;; (setf (getf attr-l attr-name) attr)
	  (weka.core.FastVector:addElement attrs attr)
	  )
	)
       )
      )
    (format t "~&length of atts is ~a~%" 
	    (weka.core.FastVector:dr-size attrs))
    ;; return the FastVector where each element is of type Attribute
    (values attrs)
    )
  )

(defmethod weka-is-missing-val (x)
  (and (stringp x) (string-equal x "?"))
  )
;;; input: attr-val-l: list where each elt is attr val, corresponding to attrs
;;;        attrs: Fast Vector where each element is an Attribute instance.
;;;        dataset: of type Instances
;;; output format is set after batchFinished() is called
;;; class index already set?
(defmethod make-weka-instance ((attr-val-l list)
			       attrs
			       dataset)
  (assert (jparam-arg-compat "weka.core.FastVector" attrs)
      ()
    "second arg should be compatible to weka.core.FastVector")
  (assert (jparam-arg-compat "weka.core.Instances" dataset)
      ()
    "third arg should be compatible to weka.core.Instances")
  
  (let* ((attr-num (length attr-val-l))
	 (inst (weka.core.Instance:Instance attr-num))
	 )
    (dotimes (i attr-num)
      (if (weka-is-missing-val (elt attr-val-l i))
	  (weka.core.Instance:setMissing 
	   inst
	   (weka.core.FastVector:elementAt attrs i))
	(weka.core.Instance:setValue 
	 inst 
	 (weka.core.FastVector:elementAt attrs i)
	 (elt attr-val-l i)
	 )
	)
      )
    (weka.core.Instance:setDataset inst dataset)
    (values inst)
    )
  )

;;; handles the case where attr-l is an association list whose keys are 
;;; attribute names and whose values are either attribute types or an 
;;; enumeration of nominal values for nominal type attributes
(defmethod make-weka-instance ((attr-val-l list)
			       (attr-l list)
			       dataset)
  (assert (jparam-arg-compat "weka.core.Instances" dataset)
      ()
    "third arg should be compatible to weka.core.Instances")
  (make-weka-instance attr-val-l 
		      (make-weka-attr-list attr-l)
		      dataset)
  )

;;; make WEKA Instances from feature vector list
;;; input: fv-list: feature vector list
;;;        attr-l: attribute list
(defmethod make-weka-instances ((rel-name string)
				(fv-l list)
				attrs)
  (assert (jparam-arg-compat "weka.core.FastVector" attrs)
      ()
    "third arg should be compatible to weka.core.FastVector")
  ;; (break "begin make-weka-instances")
  (let* ((insts (weka.core.Instances:Instances 
		 rel-name
		 attrs
		 0))
	 inst
	 )
    (dotimes (i (length fv-l))
      (setf inst (make-weka-instance (elt fv-l i) attrs insts))  
      (weka.core.Instances:dr-add insts inst)
      )
    ;; (break "end make-weka-instances")
    (values insts)
    )
  )

(defmethod make-weka-instances ((rel-name string)
				(fv-l list)
				(attr-l list))
  (let* ((attrs (make-weka-attr-list attr-l)))
    (make-weka-instances rel-name fv-l attrs)
    )
  )

;;; this filter requires all training instances be read before producing output
;;; as per documentation.
;;; this function reads feature vector from fv-l, constructs instance on the 
;;; fly, feed it into StringToWordVector filter, the setting section follows
;;; from the filterFile method in Filter class, the filter input and output
;;; part follows from sample usage in Filter documentation.
;;; input: fv-l: a list of attribute value list
;;;        attr-l: an association list mapping from attr-name to Attribute
;;;        attrs: a FastVector whose elements are of Attribute type
;;;        option: should be consistent with the weka format, except that one 
;;;                need not to specify input/output filename
;;; handles the case when input is in the format of feature vector list
(defmethod filter-data (filter
			(rel-name string)
			(fv-l list)
			(attr-l list)
			(outfile string)
			(options sequence))
  (assert (jparam-arg-compat "weka.filters.Filter" filter)
      ()
    "first arg should be compatible to weka.filters.Filter")
  (filter-data filter rel-name fv-l 
	       (make-weka-attr-list attr-l) outfile options)
  )

;;; the problem is in the current setting, we don't declare class heirarchy
;;; in Java, as a result, a StringToWordVector won't get matched to filter 
;;; parameter here.
(defmethod filter-data (filter
			(rel-name string)
			(fv-l list)
			attrs
			(outfile string)
			(options sequence))
  (assert (jparam-arg-compat "weka.filters.Filter" filter)
      ()
    "first arg should be compat to weka.filters.Filter")
  (assert (jparam-arg-compat "weka.core.FastVector" attrs)
      ()
    "fourth arg should be compat to weka.core.FastVector")
  
  (filter-insts filter (make-weka-instances rel-name fv-l attrs) 
	       outfile options)
  )

;;; handles the case where input is in Instances format
(defmethod filter-insts (filter
			 old-data
			 (outfile string)
			 (options sequence))
  (assert (jparam-arg-compat "weka.filters.Filter" filter)
      ()
    "first arg should be compat to weka.filters.Filter")
  (assert (jparam-arg-compat "weka.core.Instances" old-data)
      ()
    "second arg should be compat to weka.core.Instances")
  
  ;; (break "begin filter-insts")
  (let* (new-data inst processed class-ind) ; source-code)
    ;; need to manage filter options first, lisp side exception handling?
    ;; Lisp character = Java char
    (setf class-ind (weka-get-option #\c options))
    
    ;; not sure if should add package qualification
    
    ;; can this really the setOptions method in the OptionHandler interface
    ;; and can Optionhandler.setOptions resolve to, say, StringToWordVector.
    ;; setOptions method that actually implements the interface?
    (when (jcall "isInstance" (jclass "weka.core.OptionHandler") filter)
      (format t "~&Setting options in filter-data.~%")
      (weka.core.OptionHandler:setOptions 
       filter options))
    
    ;; temporarily commented out, waiting Franz reponse on passing by 
    ;; reference using jLinker
    ;; (weka.core.Utils:checkForRemainingOptions options)
    

    (cond
     ((or (null class-ind) (equal 0 (length class-ind)))
      (weka.core.Instances:setClassIndex old-data 0)
      )
     
     ((string-equal class-ind "first")
      (weka.core.Instances:setClassIndex old-data 0)
      )
     
     ((string-equal class-ind "last")
      (weka.core.Instances:setClassIndex 
       old-data
       (- (weka.core.Instances:numAttributes old-data) 1))
      )
     
     (t
      (weka.core.Instances:setClassIndex old-data 
					 (- (parse-integer 
					     class-ind :junk-allowed t) 
					    1))))
    
    ;; (break "before actual filtering")
    (weka.filters.Filter:setInputFormat filter old-data)
    
    ;; filter instance in a batch
    (dotimes (i (weka.core.Instances:numInstances old-data))
      (setf inst (weka.core.Instances:dr-instance old-data i))
      (weka.filters.Filter:dr-input filter inst))
    ;; after the loops, dataset constains the dataset instance information
    ;; filter contains the filtered dataset
    
    ;; StringToWordVector calls determineDictionary which in turn calls
    ;; setOutputFormat
    (weka.filters.Filter:batchFinished filter)
    (setf new-data (weka.filters.Filter:getOutputFormat filter))
    (while (setq processed (weka.filters.Filter:dr-output filter))
      (weka.core.Instances:dr-add new-data processed)
      )
    ;; output filtered data to a file for debugging purposes.
    (with-open-file (out outfile :direction :output :if-exists :supersede)
      (format out "~a" (weka.core.Instances:toString new-data)))
    ;; (break "end filter-insts")
    (discard-in-java old-data)
    (discard-in-java filter)
    (values new-data)
    )
  )

;; input: attr-l is a list of either Attribute or its Java counterpart 
;;        suff: user supplied suffix to the rel names for making out file name
;;              no output if suffix is null
(defmethod batch-filter-data (filter
			      (l1-rel-name list)
			      (l2-rel-name list)
			      (l1-fv-l list)
			      (l2-fv-l list)
			      (attr-l list)
			      (options sequence)
			      (prev string)
			      (suff string))
  ;; sanity check
  (assert (jparam-arg-compat "weka.filters.Filter" filter)
      ()
    "filter should be compat to weka.filters.Filter")
  (assert (and (mapcar #'stringp l1-rel-name))
      ()
    "l1-rel-name should be a list of strings correspond to first-old-data")
  (assert (and (mapcar #'stringp l2-rel-name))
      ()
    "l2-rel-name should be a list of strings correspond to second-old-data")
  (assert (and (mapcar #'listp l1-fv-l))
      ()
    "l1-fv-l should be a list of fv-l correspond to first-old-data")
  (assert (and (mapcar #'listp l2-fv-l))
      ()
    "l2-fv-l should be a list of fv-l correspond to second-old-data")
  
  (let* ((first-old-data (mapcar #'(lambda (rel-name fv-l) 
				     (make-weka-instances rel-name 
							  fv-l 
							  attr-l))
				 l1-rel-name
				 l1-fv-l))
	 (second-old-data (mapcar #'(lambda (rel-name fv-l)
				      (make-weka-instances rel-name
							   fv-l
							   attr-l))
				  l2-rel-name
				  l2-fv-l))
	 )
    (batch-filter-insts filter first-old-data second-old-data options prev suff)
    )
  )


;;; method for filtering multiple batches, especially useful to calibrate both
;;; multiple training and testing datasets (e.g. for co-training).
;;; handles the case where data is in feature vector list format

;;; handles the case where data is in Instances format
;;; input: prev: user supplied, specify directory to put output 
;;;        suff: user supplied suffix to the rel names for making out file name
;;;              no output if suffix is null

(defmethod batch-filter-insts (filter
			       (first-old-data list)
			       (second-old-data list)
			       (options sequence)
			       &optional 
			       ;; default to the "late:;" directory
			       (prev "data:;") 
			       (suff nil))
  (assert (jparam-arg-compat "weka.filters.Filter" filter)
      ()
    "first arg should be compat to weka.filters.Filter")
  ;; check first-old-data and second-old-data are lists of Instances
  (assert (and (mapcar #'(lambda (x) 
			   (jparam-arg-compat "weka.core.Instances" x))
		       first-old-data))
      ()
    "first-old-data should be a list of weka.core.Instances")
  (assert (and (mapcar #'(lambda (x)
			   (jparam-arg-compat "weka.core.Instances" x))
		       second-old-data)))
  
  (assert (and (mapcar #'(lambda (x) 
			   (weka.core.Instances:equalHeaders 
			    x (first first-old-data))) 
		       second-old-data)
	       (mapcar #'(lambda (x) 
			   (weka.core.Instances:equalHeaders 
			    x (first second-old-data))) 
		       first-old-data))
      ()
    "Secondary data should have same headers with primary data")
  ;; first-new-data is a list consisting of Instances
  ;; second-new-data is a list consisting of Instances
  ;; first data should be training data
  ;; second data should be test data
  (let* (first-new-data second-new-data inst processed class-ind) ; source-code)
    ;; need to manage filter options first, lisp side exception handling?
    ;; Lisp character = Java char
    (setf class-ind (weka-get-option #\c options))
    
    ;; not sure if should add package qualification
;;;    (if (jcall "isInstance" (jclass "weka.filters.Sourcable") filter)
;;;	(setf source-code (weka-get-option #\z options))) 
    
    ;; can this really the setOptions method in the OptionHandler interface
    ;; and can Optionhandler.setOptions resolve to, say, StringToWordVector.
    ;; setOptions method that actually implements the interface?
    (if (jcall "isInstance" (jclass "weka.core.OptionHandler") filter)
	(weka.core.OptionHandler:setOptions
	 filter options))
    
    ;; temorarily commented out
    ;; (weka.core.Utils:checkForRemainingOptions options)
    
    
    
    (cond
     ((or (null class-ind) (equal 0 (length class-ind)))
      (dolist (old-data first-old-data)
	(weka.core.Instances:setClassIndex old-data 0))
      (dolist (old-data second-old-data)
	(weka.core.Instances:setClassIndex old-data 0))
      )
     
     ((string-equal class-ind "first")
      (dolist (old-data first-old-data)
	(weka.core.Instances:setClassIndex old-data 0))
      (dolist (old-data second-old-data)
	(weka.core.Instances:setClassIndex old-data 0))
      )
     
     ((string-equal class-ind "last")
      (dolist (old-data first-old-data)
	(weka.core.Instances:setClassIndex 
	 old-data
	 (- (weka.core.Instances:numAttributes old-data) 1)))
      (dolist (old-data second-old-data)
	(weka.core.Instances:setClassIndex
	 old-data
	 (- (weka.core.Instances:numAttributes old-data) 1)))
      )
     
     (t
      (dolist (old-data first-old-data)
	(weka.core.Instances:setClassIndex 
	 old-data (- (parse-integer class-ind :junk-allowed t) 1)))
      (dolist (old-data second-old-data)
	(weka.core.Instances:setClassIndex
	 old-data (- (parse-integer class-ind :junk-allowed t) 1)))
      )
     )
    

    ;; for filters that operate only under batch mode, multiple training files
    ;; impose a difficulty, as we need to read all of them into filter 
    ;; temporarily in order to get the output format right. For example, in the
    ;; StringToWordVector filter, words are turned into attributes in 
    ;; determineDictionary which is in turn called in batchFinished. After 
    ;; output all temporary instances (or resetQueue), you may input and 
    ;; output required
    ;; Instances
    
    (weka.filters.Filter:setInputFormat filter (first first-old-data))
    
    ;; filter instance in a batch
    (dolist (old-data first-old-data)
      (dotimes (i (weka.core.Instances:numInstances old-data))
	(setf inst (weka.core.Instances:dr-instance old-data i))
	(weka.filters.Filter:dr-input filter inst)
	)
      )
    ;; after the loops, dataset constains the dataset instance information
    ;; filter contains the filtered dataset
    (weka.filters.Filter:batchFinished filter)
    ;; drain the output queue, inefficient
    (while (setq processed (weka.filters.Filter:dr-output filter))
      )
    
    ;; StringToWordVector calls determineDictionary which in turn calls
    ;; setOutputFormat
    ;; filter the primary dataset
    (dolist (old-data first-old-data)
      (dotimes (i (weka.core.Instances:numInstances old-data))
	(setf inst (weka.core.Instances:dr-instance old-data i))
	(weka.filters.Filter:dr-input filter inst)
	)
      (weka.filters.Filter:batchFinished filter)
      (let* ((new-data (weka.filters.Filter:getOutputFormat filter)))
	(while (setq processed (weka.filters.Filter:dr-output filter))
	  (weka.core.Instances:dr-add new-data processed)
	  )
	;; output filtered data to a file for debugging purposes.
	(when suff
	  (let* ((outfile (concatenate 'string
				   prev
				   (replace-re
				    (weka.core.Instances:relationName old-data)
				    "^(.*?)-.*" "\\1")
				   suff)))
	    (format t "~&output primary filtered file: ~a~%" outfile)
	    (with-open-file (out outfile
			     :direction :output :if-exists :supersede)
	      (format out "~a" (weka.core.Instances:toString new-data)))))
	
	(push new-data first-new-data)
	)

      )
    (setf first-new-data (nreverse first-new-data))    
    
    ;; filter the secondary dataset
    (dolist (old-data second-old-data)
      (dotimes (i (weka.core.Instances:numInstances old-data))
	(setf inst (weka.core.Instances:dr-instance old-data i))
	(weka.filters.Filter:dr-input filter inst)
	)
      (weka.filters.Filter:batchFinished filter)
      (let* ((new-data (weka.filters.Filter:getOutputFormat filter))
	     (old-rel-name-head (replace-re
				 (weka.core.Instances:relationName old-data)
				 "^(.*?)-.*" "\\1")))
	(weka.core.Instances:setRelationName 
	 new-data 
	 (replace-re
	  (weka.core.Instances:relationName new-data)
	  "^(.*?)-" (concatenate 'string old-rel-name-head "-")))
	
	(while (setq processed (weka.filters.Filter:dr-output filter))
	  (weka.core.Instances:dr-add new-data processed)
	  )
	;; output filtered data to a file for debugging purposes.
	(when suff
	  (let* ((outfile (concatenate 'string 
			    prev
			    (replace-re
			     (weka.core.Instances:relationName new-data)
			     "^(.*?)-.*" "\\1")
			    suff)))
	    (format t "~&output secondary filtered file: ~a~%" outfile)
	    (with-open-file (out outfile 
			     :direction :output :if-exists :supersede)
	      (format out "~a" (weka.core.Instances:toString new-data)))))
	
	(push new-data second-new-data)
	)

      )
    (setf second-new-data (nreverse second-new-data))
    

    (discard-in-java first-old-data)
    (discard-in-java second-old-data)
    ;; return multiple values
    (values first-new-data second-new-data)
    )

  )

(defmethod str-to-word-vector ((rel-name string)
			       (fv-l list) 
			       attrs
			       (outfile string)
			       (options sequence))
  (assert (jparam-arg-compat "weka.core.FastVector" attrs)
      ()
    "third arg should be compat to weka.core.FastVector")
  
  ;; initialize the filter
  (let* ((filter (weka.filters.unsupervised.attribute.StringToWordVector:StringToWordVector)))
    (filter-data filter rel-name fv-l attrs outfile options)
    )
  )


(defmethod str-to-word-vector ((rel-name string)
			       (fv-l list) 
			       (attr-l list)
			       (outfile string)
			       (options sequence))
  ;; initialize the filter
  ;; (break "begin str-to-word-vector")
  (let* ((filter (weka.filters.unsupervised.attribute.StringToWordVector:StringToWordVector)))
    (filter-data filter rel-name fv-l attr-l outfile options)
    )
  )

(defmethod str-to-word-vector-insts (old-data
				     (outfile string)
				     (options sequence))
  (assert (jparam-arg-compat "weka.core.Instances" old-data)
      ()
    "first arg should be compat to weka.core.Instances")
  
  ;; initialize the filter
  (let* ((filter (weka.filters.unsupervised.attribute.StringToWordVector:StringToWordVector)))
    (filter-insts filter old-data outfile options)
    )
  )

(defmethod batch-str-to-word-vector-insts ((first-old-data list)
					   (second-old-data list)
					   (options sequence)
					   &optional
					   (prev "data:;")
					   (suff nil))
  ;; initialize the filter
  (let* ((filter (weka.filters.unsupervised.attribute.StringToWordVector:StringToWordVector)))
    (batch-filter-insts filter first-old-data second-old-data 
			options prev suff)
    )
  )

(defmethod batch-str-to-word-vector ((l1-rel-name list)
				     (l2-rel-name list)
				     (l1-fv-l list)
				     (l2-fv-l list)
				     (attr-l list)
				     (options sequence)
				     &optional
				     (prev "data:;")
				     (suff nil))
  ;; initialize the filter
  (let* ((filter (weka.filters.unsupervised.attribute.StringToWordVector:StringToWordVector)))
    (batch-filter-data filter l1-rel-name l2-rel-name l1-fv-l l2-fv-l 
		       attr-l options prev suff)
    )
  )

(defmethod discretize ((rel-name string)
		       (fv-l list) 
		       (attr-l list)
		       (outfile string)
		       (options sequence))
  ;; initialize the filter
  (let* ((filter (weka.filters.unsupervised.attribute.Discretize:Discretize)))
    (filter-data filter rel-name fv-l attr-l outfile options)
    )
  )

(defmethod discretize ((rel-name string)
		       (fv-l list) 
		       attrs
		       (outfile string)
		       (options sequence))
  (assert (jparam-arg-compat "weka.core.FastVector" attrs)
      ()
    "third arg should be compat to weka.core.FastVector")
  ;; initialize the filter
  (let* ((filter (weka.filters.unsupervised.attribute.Discretize:Discretize)))
    (filter-data filter rel-name fv-l attrs outfile options)
    )
  )

(defmethod discretize-insts (old-data
			     (outfile string)
			     (options sequence))
  (assert (jparam-arg-compat "weka.core.Instances" old-data)
      ()
    "first arg should be compat to weka.core.Instances")
  ;; initialize the filter
  (let* ((filter (weka.filters.unsupervised.attribute.Discretize:Discretize)))
    (filter-insts filter old-data outfile options)
    )
  )

(defmethod batch-discretize ((l1-rel-name list)
			     (l2-rel-name list)
			     (l1-fv-l list)
			     (l2-fv-l list)
			     (attr-l list)
			     (options sequence)
			     &optional
			     (prev "data:;")
			     (suff nil))
  (let* ((filter (weka.filters.unsupervised.attribute.Discretize:Discretize)))
    (batch-filter-data filter l1-rel-name l2-rel-name l1-fv-l l2-fv-l
		       attr-l options prev suff)
    )
  )

(defmethod batch-discretize-insts ((first-old-data list)
				   (second-old-data list)
				   (options sequence)
				   &optional
				   (prev "data:;")
				   (suff nil))
  ;; initialize the filter
  (let* ((filter (weka.filters.unsupervised.attribute.Discretize:Discretize)))
    (batch-filter-insts filter first-old-data second-old-data 
			options prev suff)
    )
  )

(defmethod weka-get-flag (flag
			  (options sequence))
  (assert (string= "" (first options))
      ()
    "options must have its first element to be \"\".")
  ;; (format t "~&before weka-get-flag: ~a~%" options)
  (let* ((res (weka.core.Utils:getFlag flag options)))
    (delete (concatenate 'string "-" (format nil "~a" flag)) 
			  options :count 1 :test #'string=)
    ;; (format t "~&after getting ~a, options: ~a~%" flag options)
    res
    )
  )

;;; takes in a String[] format weka options, get the option and remove 
;;; corresponding option string from options. This is needed because jlinker
;;; cannot pass by reference.
;;; Any composite objects whose components are passed between Lisp and
;;; Java are passed as copies and modifications are seen only in the
;;; environment where they are made.  When objects are passed as
;;; references, the references are atomic and can only be passed back to
;;; the calling environment where they are resolved to the original
;;; object. 
(defmethod weka-get-option (option
			    (options sequence))
  (assert (string= "" (first options))
      ()
    "options must have its first element to be \"\".")
  ;; (format t "~&before weka-get-option: ~a~%" options)
  (let* (pos
	 (res (weka.core.Utils:getOption option options)))
    ;; delete -l and its next element, delete-if modifies the sequence
    (when (/= 0 (length res))
      (setf pos (position (concatenate 'string "-" (format nil "~a" option)) 
			  options :test #'string=))
      ;; note that delete cannot destructively delete the first item
      (delete (concatenate 'string "-" (format nil "~a" option)) 
			    options :count 1 :test #'string=)
      (delete res options :count 1 :test #'string= :start pos)
      ;; (format t "~&after getting ~a, options: ~a~%" option options)
      )
    res
    )
  )

(defmethod weka-set-options (option-handler
			     (options sequence))
  (format t "~&options in weka-set-options: ~a~%" options)
  ;; performs the actual option setting
  (weka.core.OptionHandler:setOptions option-handler options)
  ;; empty the options that have been set
  (cond 
   ((jcall "isInstance" 
	   (jclass "weka.classifiers.functions.SMO") 
	   option-handler)
    (weka-get-flag "no-checks" options)
    (weka-get-option #\C options)
    (weka-get-option #\L options)
    (weka-get-option #\P options)
    (weka-get-option #\N options)
    (weka-get-flag #\M options)
    (weka-get-option #\V options)
    (weka-get-option #\W options)
    (weka-get-option #\K options))
   
   ((jcall "isInstance"
	   (jclass "weka.filters.unsupervised.attribute.StringToWordVector")
	   option-handler)
    (weka-get-option #\R options)
    (weka-get-flag #\V options)
    (weka-get-option #\P options)
    (weka-get-option #\W options)
    (weka-get-option "prune-rate" options)
    (weka-get-option #\M options)
    (weka-get-flag #\C options)
    (weka-get-flag #\T options)
    (weka-get-flag #\I options)
    (weka-get-flag #\O options)
    (weka-get-option #\N options)
    (weka-get-flag #\L options)
    (weka-get-flag #\S options)
    (weka-get-option "stemmer" options)
    (weka-get-option "stopwords" options)
    (weka-get-option "tokenizer" options))
   
   ((jcall "isInstance"
	   (jclass "weka.filters.unsupervised.attribute.Discretize")
	   option-handler)
    (weka-get-flag #\D options)
    (weka-get-flag #\F options)
    (weka-get-flag #\O options)
    (weka-get-flag #\V options)
    (weka-get-option #\M options)
    (weka-get-option #\B options)
    (weka-get-option #\R options))
   )
  
  (format t "~&options for ~a are ~a~%" 
	  (jcall "getName" (jcall "getClass" option-handler))
	  (weka.core.OptionHandler:getOptions option-handler)
	  ))

;;; TODO: you can generate all the wrappers for once, but that doesn't mean 
;;; you need to load all of them. Should load them in a lazy way. Or even
;;; generate the wrappers in a lazy way, as specified by the programmer
;;; generate wrappers for org.w3c.dom.Document; Element; Node classes
;;; since the only public method is getPMMLModel in PMMLFactory.java in weka
;;; we convert string to InputStream, however, this should still be IO-free
(defmethod weka-get-pmml-model ((serial-str string))
  
  ;; cannot use isPMML check because it's a private method!
  (let* ((is (java.io.ByteArrayInputStream:ByteArrayInputStream
	      (java.lang.String:getBytes serial-str "UTF-8")))
	 )
    (weka.core.pmml.PMMLFactory:getPMMLModel is)
    )
  )

;;; see Evaluation.java for an example usage.
;;; much of it follows from evaluateModel, the reason that we rewrite it here
;;; is because we short-pass the file-in file-out detour
;;; also we handle the case of input from model stored in database and output
;;; model to database, the model should be serialized?
;;; TODO: distinguish between different return values
(defmethod eval-classifier (train-insts
			    (output-type string)
			    (classifier-name string)
			    (options sequence)
			    (code-version string)
			    (target-type string)
			    &optional
			    (cost-matrix nil)
			    (test-insts nil))
  ;; train-insts should be able to spit instances one by one, should I do it
  ;; as a pipe? Or is it really something that I should do? It's weka's job!
  (assert (or (jcall "isInstance" (jclass "weka.core.Instances") train-insts)
	      (jcall "isInstance" (jclass "java.io.PipedInputStream") 
		     train-insts))
      ()
    "train-insts should be compat to either weka.core.Instances or ~
     java.io.PipedInputStream.")
  
  (assert (or (null cost-matrix) (arrayp cost-matrix))
      ()
    "cost-matrix should either be nil or a lisp array.")
  
  (assert (or (null test-insts)
	      (jcall "isInstance" (jclass "weka.core.Instances") test-insts)
	      (jcall "isInstance" (jclass "java.io.PipedInputStream") 
		     train-insts))
      ()
    "test-insts should either be niln or weka.core.Instances or ~
     PipedInputStream.")
  
  
  ;; if get input from xml (XMLOptions handles cases of file, string etc.)
  (let* ((xml (weka-get-option "xml" options)))
    (if (> (length xml) 0)
	(setf options (weka.core.xml.XMLOptions:toArray
		       (weka.core.xml.XMLOptions:XMLOptions xml))))
    )
  
  (let* ((options-tmp (copy-seq options))
	 ;; for PMML Model in database, you want to serialize a Document 
	 ;; instance
	 ;; model-qtmp "select format, model from table where ..."
	 ;; users should specify database to select from in db_name.tbl_name
	 (model-qtmp (weka-get-option #\l options-tmp))
	 res-tmp
	 load-success model model-type 
	 (classifier (jcall "newInstance" 
			    (jstatic (jmethod "java.lang.Class" "forName"
					      "java.lang.String") 
				     (jclass "java.lang.Class")
				     classifier-name)))
	 no-cv 
	 (class-ind -1) 
	 (folds 10)
	 (seed 1)
	 train-template test-template
	 template 
	 (actual-class-ind -1)
	 (split-perc -1)
	 preserve-order
	 class-stats no-output train-stats print-complex-stats print-margins
	 print-graph source-class print-source print-dist threshold-out?
	 obj-in-q obj-out?
	 threshold-label attr-range-str print-classifications attr-to-output
	 scheme-options-text train-eval test-eval classifier-backup
	 (train-time-start 0)
	 (train-time-elapsed 0)
	 classifier-classifications
	 (res-text (make-array 0 :element-type 'character :adjustable t
			       :fill-pointer 0))
	 (preds-buff (make-array 0 :element-type 'character :adjustable t
				 :fill-pointer 0))
	 (test-time-start 0) 
	 (test-time-elapsed 0)
	 model-istream saved-structure cid)
    
    (sql (insert-classifier-q 
	  output-type target-type code-version cost-matrix) :db *weka-db*)
    (setf cid (mysql-insert-id *weka-db*))
    
    (when (and (> (length model-qtmp) 0)
	       (setf res-tmp (sql model-qtmp :db *weka-db*))
	       (string-equal "xml" (setf model-type (caar res-tmp))))
      ;; unwind-protect or handler-case is the lisp equivalent to 
      ;; java's try-catch 
      (handler-case
	  (progn 
	    (setf model (weka-get-pmml-model (cadar res-tmp)))
	    (when (jparam-arg-compat 
		   "weka.classifiers.pmml.consumer.PMMLClassifier"
		   model)
	      (setf classifier model)
	      (weka-get-option #\l options)
	      (setf load-success t))
	    )
	(error () (setf load-success nil))
	)
      
      (unless load-success
	;; load options from serialized data, erase -l option
	(let* ((xml-serial (weka.classifiers.xml.XMLClassifier:XMLClassifier))
	       (cl (weka.core.xml.XMLSerialization:dr-read xml-serial
		    (weka-get-option #\l options)))
	       (cl-options (weka.classifiers.Classifier:getOptions cl))
	       )
	  ;; merge options
	  (setf options (concatenate 'sequence cl-options options))
	  )
	)
      )
    
  
    (setf no-cv (weka-get-flag "no-cv" options))
    (setf class-ind (weka-get-option #\c options))
    (when (/= 0 (length class-ind))
      (cond 
       ((string-equal class-ind "first")
	(setf class-ind 1)
	)
       
       ((string-equal class-ind "last")
	(setf class-ind -1)
	)
       
       (t
	(setf class-ind (parse-integer class-ind :junk-allowed t))
	)
       )
      )
    
    ;; I changed these from filenames to queries that result in serialized 
    ;; entries from a database
    (setf obj-in-q (weka-get-option #\l options))
    (setf obj-out? (weka-get-flag #\d options))
    (setf folds (parse-integer (weka-get-option #\x options) :junk-allowed t))
    (setf seed (parse-integer (weka-get-option #\s options) :junk-allowed t))
    
    (cond
     ((null train-insts)
      (if (= 0 (length obj-in-q))
	  (error "No training file and no object input data given.")
	)
      (if (null test-insts)
	  (error "No training file and no test data given")
	)
      )

    ((and (/= 0 (length obj-in-q))
	  (or (not (jcall "isInstance" 
			  (jclass "weka.classifiers.UpdateableClassifier") 
			  classifier))
	      (null test-insts)))
     (error "Classifier not incremental, or no test file provided: can't use ~
             both train and model file")
     )
    )    
    ;; there seems to be three different ways of serialization
    ;; - java-bin, Java's proprietary binary format
    ;; - KOML, Koala Object Markup Language
    ;; - PMML, Predictive Modeling Markup Language
    ;; see http://weka.wikispaces.com/XML for their pros and cons
    (when (and (not load-success) (/= 0 (length obj-in-q)))
      (setf res-tmp (sql obj-in-q :db *weka-db*))
      (cond 
       ((and (string-equal "koml" (setf model-type (caar res-tmp)))
	     (weka.core.xml.KOML:isPresent))
	(setf model-istream 
	  (java.io.ByteArrayInputStream:ByteArrayInputStream 
	   (java.lang.String:getBytes (cadar res-tmp) "UTF-8"))))

       (t 
	(setf model-type "java-bin")
	(setf model-istream 
	  (java.io.ByteArrayInputStream:ByteArrayInputStream 
	   (java.lang.String:getBytes (cadar res-tmp) "UTF-8"))))
       )
      )
    
    
    (cond 
     (test-insts
      ;; getStructure
      (setf template (weka.core.Instances:Instances test-insts 0))
      (setf test-template (weka.core.Instances:Instances test-insts 0))
      (cond 
       ((/= -1 class-ind)
	(weka.core.Instances:setClassIndex test-insts (1- class-ind)))
       (t
	(if (or (= -1 (weka.core.Instances:classIndex test-insts))
		class-ind)
	    (weka.core.Instances:setClassIndex 
	     test-insts
	     (1- (weka.core.Instances:numAttributes test-insts))))))
      (setf actual-class-ind (weka.core.Instances:classIndex test-insts)))

     (t
      (setf split-perc (weka-get-option "split-percentage" options))
      (cond 
       ((/= 0 (length split-perc))
	(unless folds 
	  (error "Percentage split can't be used in conjunction with cv."))
	(setf split-perc (parse-integer split-perc :junk-allowed t))
	(assert (and (< 0 split-perc) (< split-perc 100))
	    ()
	  "Percentage split value needs be >0 and <100"))
       (t
	(setf split-perc -1)))

      (setf preserve-order (weka-get-flag "preserve-order" options))
      
      (if (and preserve-order (= -1 split-perc))
	  (error "Percentage split is missing."))
      
      (when (> split-perc 0)
	(let* ((tmp-insts (weka.core.Instances:Instances train-insts))
	       (train-size (* (/ split-perc 100)
			      (weka.core.Instances:numInstances tmp-insts)))
	       (test-size (- (weka.core.Instances:numInstances tmp-insts)
			     train-size)))
	  (unless preserve-order
	    (weka.core.Instances:randomize tmp-insts
					   (java.util.Random:Random seed)))
	  (setf test-insts (weka.core.Instances:Instances tmp-insts 
							  train-size
							  test-size))
	  (setf train-insts (weka.core.Instances:Instances tmp-insts
							   0 
							   train-size))
	  (setf template (weka.core.Instances:Instances test-insts 0))
	  (setf test-template (weka.core.Instances:Instances test-insts 0))
	  
	  (cond 
	   ((/= -1 class-ind)
	    (weka.core.Instances:setClassIndex test-insts (1- class-ind)))
	   (t
	    (if (or (= -1 (weka.core.Instances:classIndex test-insts))
		    class-ind)
		(weka.core.Instances:setClassIndex 
		 test-insts
		 (1- (weka.core.Instances:numAttributes test-insts))))))
	  (setf actual-class-ind (weka.core.Instances:classIndex test-insts))
	  )
	)
      )
     
     
     )
    
    (when train-insts
      (setf template (weka.core.Instances:Instances train-insts 0))
      (setf train-template (weka.core.Instances:Instances train-insts 0))
      
      (cond 
       ((/= -1 class-ind)
	(weka.core.Instances:setClassIndex train-insts (1- class-ind)))
       (t
	(if (or (= -1 (weka.core.Instances:classIndex train-insts))
		class-ind)
	    (weka.core.Instances:setClassIndex 
	     train-insts
	     (1- (weka.core.Instances:numAttributes train-insts))))))
      (setf actual-class-ind (weka.core.Instances:classIndex train-insts))
      (if (and test-insts
	       (not (weka.core.Instances:equalHeaders test-template 
						      train-template)))
	  (error "Train and test files not compatible.")))
    
    (if (null template)
	(error "NO actual dataset provided to use as template"))
    
    ;; assuming the stored cost matrix is in lisp array format
    (let* ((cm-in-q (weka-get-option #\m options)))
      (when (> (length cm-in-q) 0)
	(let* ((cm-res (sql cm-in-q :db *weka-db*))
	       (cost-array (caar cm-res)))
	  (multiple-value-bind (rows cols)
	      (values-list (array-dimensions cost-array))
	    (assert (= rows cols)
		()
	      "Cost Matrix not square.")
	    (setf cost-matrix (weka.classifiers.CostMatrix:CostMatrix rows))
	    (dotimes (i rows)
	      (dotimes (j cols)
		(weka.classifiers.CostMatrix:setCell cost-matrix i j 
						     (aref cost-array i j))))))
	))
    
    (setf class-stats (weka-get-flag #\i options))
    (setf no-output (weka-get-flag #\o options))
    (setf train-stats (not (weka-get-flag #\v options)))
    (setf print-complex-stats (weka-get-flag #\k options))
    (setf print-margins (weka-get-flag #\r options))
    (setf print-graph (weka-get-flag #\g options))
    (setf source-class (weka-get-option #\z options))
    (setf print-source (/= 0 (length source-class)))
    (setf print-dist (weka-get-flag "distribution" options))
    ;; serialize it to database, changed label from threshold-file to 
    ;; threshold-out flag
    (setf threshold-out? (weka-get-flag "threshold-out" options))
    (setf threshold-label (weka-get-option "threshold-label" options))
    ;; TODO: handles exception
    (setf attr-range-str (weka-get-option #\p options))
    
    (when (/= 0 (length attr-range-str))
      (setf print-classifications t)
      (setf no-output t)
      (unless (string-equal "0" attr-range-str)
	(setf attr-to-output (weka.core.Range:Range attr-range-str))))
    
    (if (and (not print-classifications)
	     print-dist)
	(error "Cannot print distribution without '-p' option!"))
    
    (if (and (null train-insts) print-complex-stats)
	(error "Cannot print complexity statistics ('-k') without training ~
                file ('-t')!"))
    
    (cond
     ((/= 0 (length obj-in-q))
      (weka.core.Utils:checkForRemainingOptions options))
     (t
      (when (jcall "isInstance" (jclass "weka.core.OptionHandler") 
		   classifier)
	(dotimes (i (length options))
	  (when (/= 0 (length (elt options i)))
	    (if (null scheme-options-text)
		;; TODO: generate wrapper for StringBuffer Class
		(setf scheme-options-text 
		  (java.lang.StringBuffer:StringBuffer)))
	    (if (position #\Space (elt options i))
		(setf scheme-options-text
		  (java.lang.StringBuffer:dr-append
		   scheme-options-text
		   (concatenate 'string "\"" (elt options i) "\" ")))
	      (setf scheme-options-text
		(java.lang.StringBuffer:dr-append
		 scheme-options-text
		 (concatenate 'string (elt options i) " "))))))
	(weka-set-options classifier options)))
     )
    (weka.core.Utils:checkForRemainingOptions options)
    
    
    ;; setup evaluation objects
    (setf train-eval (weka.classifiers.Evaluation:Evaluation
		      (weka.core.Instances:Instances template 0)
		      cost-matrix))
    (setf test-eval (weka.classifiers.Evaluation:Evaluation
		     (weka.core.Instances:Instances template 0)
		     cost-matrix))
    
    (if (null train-insts)
	(weka.classifiers.Evaluation:useNoPriors test-eval))
    
    ;; TODO complete it after you figure out different serializations
    (cond
     ;; load classifier from database
     ((string-equal model-type "java-bin")
      (let* ((ois (java.io.ObjectInputStream:ObjectInputStream model-istream)))
	(setf classifier (java.io.ObjectInputStream:readObject ois))
	(setf saved-structure (java.io.ObjectInputStream:readObject ois))
	(if (and saved-structure (not (weka.core.Instances:equalHeaders template saved-structure)))
	    (error "training and test sets are not compatible.")
	  )
	(java.io.ObjectInputStream:dr-close ois)
	)
      )
     
     ((string-equal model-type "koml")
      (let* ((xmlis (java.io.BufferedInputStream:BufferedInputStream 
		     model-istream)))
	(setf classifier (weka.core.xml.KOML:dr-read xmlis))
	(java.io.BufferedInputStream:dr-close xmlis))
      )
     
     ((or (null model-type)
	  (= 0 (length model-type))))
     
     (t
      (error "unrecognized model type."))
     )
    
    ;; backup of fully setup classifier for cross-validation
    (setf classifier-backup (weka.classifiers.Classifier:makeCopy
			     classifier))
    
    ;; build the classifier if no object file provided
    (cond 
     ((and (jcall "isInstance" 
		  (jclass "weka.classifiers.UpdateableClassifier")
		  classifier)
	   (or test-insts no-cv)
	   (null cost-matrix)
	   train-insts)
      ;; build classifier incrementally
      (format t "~&building classifier incrementally.~%")
      (weka.classifiers.Evaluation:setPriors train-eval train-template)
      (weka.classifiers.Evaluation:setPriors test-eval train-template)
      ;; by default, the internal-time-units-per-second is 1000, and
      ;; get-internal-real-time would return millis
      (setf train-time-start (get-internal-real-time))
      (if (= 0 (length obj-in-q))
	  (weka.classifiers.Classifier:buildClassifier classifier 
						       train-template))
      
      ;; seems read instance incrementally, the m_structure doesn't contain
      ;; any instance, it only serves as a formatter. Correspondingly, we
      ;; probably should have a routine to spit instance from filters one by 
      ;; one and routines and tables such that each instance can be stored
      ;; and retrieved as a row. TODO: do above, can use PipedInputStream
      ;; and PipedOutputStream
      ;; defer the implementation until I have the real instance generator
      (let* (train-inst)
	(cond 
	 ((jcall "isInstance" (jclass "weka.core.Instances") train-insts)
	  (dotimes (i (weka.core.Instances:numInstances train-insts))
	    (setf train-inst (weka.core.Instances:dr-instance train-insts i))
	    (weka.classifiers.Evaluation:updatePriors train-eval train-inst)
	    (weka.classifiers.Evaluation:updatePriors test-eval train-inst)
	    (weka.classifiers.UpdateableClassifier:updateClassifier 
	     classifier train-inst))
	  (setf train-time-elapsed (- (get-internal-real-time) 
				      train-time-start)))
	 ;; the usage of PipedInputStream may not be better than 
	 ;; buffer-by-buffer copy
	 )
	)
      )
     
     ((= 0 (length obj-in-q))
      ;; build classifier in one go
      (format t "~&building classifier in one go.~%")
      (weka.classifiers.Evaluation:setPriors train-eval train-insts)
      (weka.classifiers.Evaluation:setPriors test-eval train-insts)
      (setf train-time-start (get-internal-real-time))
      (weka.classifiers.Classifier:buildClassifier classifier train-insts)
      (setf train-time-elapsed (- (get-internal-real-time) train-time-start)))
      
     )
    
    ;; backup of fully trained classifier for printing the classifications
    (if print-classifications
	(setf classifier-classifications 
	  (weka.classifiers.Classifier:makeCopy classifier)))
    
    ;; Save the classifier if an object output query is provided
    (when obj-out?
      (let* ((os (java.io.ByteArrayOutputStream:ByteArrayOutputStream)))
	(cond
	 ((string-equal "java-bin" output-type)
	  ;; binary
	  (let* ((obj-os (java.io.ObjectOutputStream:ObjectOutputStream os)))
	    (java.io.ObjectOutputStream:writeObject obj-os classifier)
	    (if template
		(java.io.ObjectOutputStream:writeObject obj-os template))
	    (java.io.ObjectOutputStream:dr-flush obj-os)
	    (java.io.ObjectOutputStream:dr-close obj-os)))
	 
	 ((string-equal "xml" output-type)
	  ;; XML, TODO: to write more model information to the database
	  (let* ((xml-os (java.io.BufferedOutputStream:BufferedOutputStream
			  os))
		 (xml-serial 
		  (weka.classifiers.xml.XMLClassifier:XMLClassifier)))
	    (weka.core.xml.XMLSerialization:dr-write xml-serial 
						  xml-os 
						  classifier)
	    (java.io.BufferedOutputStream:dr-close xml-os)))


	 ((string-equal "koml" output-type)
	  (let* ((koml-os (java.io.BufferedOutputStream:BufferedOutputStream 
			   os)))
	    (weka.core.xml.KOML:dr-write koml-os classifier)
	    (java.io.BufferedOutputStream:dr-close koml-os)))
	 )
	(sql (update-object-q cid (jcall "toString" os)) :db *weka-db*)
	)
      )
    
    (when (and (jcall "isInstance" (jclass "weka.core.Drawable") classifier)
	       print-graph)
      (sql (update-result-q "graph" cid (weka.core.Drawable:graph classifier))
	   :db *weka-db*)
      (return-from eval-classifier (values
				    "graph"
				    (weka.core.Drawable:graph classifier))))
    
    (when (and (jcall "isInstance" 
		      (jclass "weka.classifiers.Sourcable")
		      classifier)
	       print-source)
      (sql (update-result-q "equiv-source" 
			    cid 
			    (weka.classifiers.Evaluation:wekaStaticWrapper
			     classifier source-class))
	   :db *weka-db*)
      
      (return-from eval-classifier
	(values
	 "equiv-source"
	 (weka.classifiers.Evaluation:wekaStaticWrapper
	  classifier source-class))))
    
    ;; output model
    (format t "~&outputing model.~%")
    (unless (or no-output print-margins)
      (if (jcall "isInstance"
		 (jclass "weka.core.OptionHandler")
		 classifier)
	  (if scheme-options-text
	      (format res-text (concatenate 'string 
				 "~%Options: " 
				 (jcall "replaceAll" 
					scheme-options-text
					"\n" 
					"~%")
				 "~%"))))
      (format res-text (concatenate 'string 
			 "~%" 
			 (jcall "replaceAll" 
				(weka.classifiers.Classifier:toString 
				 classifier)
				"\n"
				"~%")
			 "~%")))
    
    (when (and (not print-margins) cost-matrix)
      (format res-text "~%=== Evaluation Cost Matrix ===~%~%")
      (format res-text (jcall "replaceAll" 
			      (weka.classifiers.CostMatrix:toString 
			       cost-matrix)
			      "\n"
			      "~%")))
    
    ;; Output test instance predictions only
    (when print-classifications
      (cond
       ;; no test set -> use train set
       ((and (null test-insts) no-cv)
	(format preds-buff "~%=== Predictions on training data ===~%~%"))
       (t
	(format preds-buff "~%=== Predictions on test data ===~%~%"))
       )
      
      (if test-insts
	(weka.classifiers.Evaluation:printClassifications
	 classifier-classifications
	 (weka.core.Instances:Instances template 0)
	 test-insts
	 (1+ actual-class-ind)
	 attr-to-output
	 print-dist
	 preds-buff))
      )
    
    ;; Compute error estimate from training data
    (format t "~&Compute error estimate from training data.~%")
    (when (and train-stats train-insts)
      (cond
       ((and (jcall "isInstance" 
		    (jclass "weka.classifiers.UpdateableClassifier")
		    classifier)
	     test-insts
	     (null cost-matrix))
	;; TODO: incremental stuff
	(setf test-time-start (get-internal-real-time))
	(dotimes (i (weka.core.Instances:numInstances train-insts))
	  (let* ((train-inst (weka.core.Instances:dr-instance train-insts i)))
	    (weka.classifiers.Evaluation:evaluateModelOnce train-eval
							   classifier
							   train-inst)))
	(setf test-time-elapsed (- (get-internal-real-time) test-time-start)))      
       (t
	(setf test-time-start (get-internal-real-time))
	;; pay attention to actual-class-ind get set
	(weka.classifiers.Evaluation:evaluateModel
	 train-eval classifier train-insts)
	(setf test-time-elapsed (- (get-internal-real-time) test-time-start)))
       )
      
      (format t "~%Time taken to build model: ~s seconds~%"
		  (/ train-time-elapsed 1000.0))
      (cond
       ((> split-perc 0)
	(format t
		"~%Time taken to test model on training split: "))
       (t
	(format t
		"~%Time taken to test model on training data: ")))
      (format t "~s seconds~%" (/ test-time-elapsed 1000.0))
      
      ;; Print the results of the training evaluation
      (cond
       (print-margins
	(sql (update-result-q 
	      "cum-margin-dist" 
	      cid 
	      (weka.classifiers.Evaluation:toCumulativeMarginDistributionString
	       train-eval))
	     :db *weka-db*)
	
	(return-from eval-classifier
	  (values
	   "cum-margin-dist"
	   (weka.classifiers.Evaluation:toCumulativeMarginDistributionString
	   train-eval))))
       
       (t
	(unless print-classifications
	  (format res-text "~%Time taken to build model: ~s seconds~%"
		  (/ train-time-elapsed 1000.0))
	  (cond
	   ((> split-perc 0)
	    (format res-text 
		    "~%Time taken to test model on training split: "))
	   (t
	    (format res-text
		    "~%Time taken to test model on training data: ")))
	  
	  (format res-text "~s seconds~%" (/ test-time-elapsed 1000.0))
	  
	  (cond
	   ((> split-perc 0)
	    (format res-text 
		    (jcall "replaceAll"
			   (weka.classifiers.Evaluation:toSummaryString
			    train-eval
			    "\n\n=== Error on training split ===\n"
			    print-complex-stats)
			   "\n" 
			   "~%")))
	   (t
	    (format res-text
		    (jcall "replaceAll"
			   (weka.classifiers.Evaluation:toSummaryString
			    train-eval
			    "\n\n=== Error on training data ===\n"
			    print-complex-stats)
			   "\n" 
			   "~%"))))
	  
	  (when (weka.core.Attribute:isNominal 
		 (weka.core.Instances:classAttribute template))
	    (if class-stats
		(format 
		 res-text 
		 (concatenate 'string 
		   "~%~%" 
		   (jcall "replaceAll"
			  (weka.classifiers.Evaluation:toClassDetailsString
			   train-eval)
			  "\n" 
			  "~%"))))
	    (unless no-cv
	      (format 
	       res-text 
	       (concatenate 'string
		 "~%~%" 
		 (jcall "replaceAll"
		       (weka.classifiers.Evaluation:toMatrixString
			train-eval)
		       "\n" 
		       "~%")))))))
       )
      )
    
    ;; compute proper error estimates
    (format t "~&compute proper error estimates.~%")
    (cond
     ;; Testing is on the supplied test data
     (test-insts
      (dotimes (i (weka.core.Instances:numInstances test-insts))
	(let* ((test-inst (weka.core.Instances:dr-instance test-insts i)))
	  (weka.classifiers.Evaluation:evaluateModelOnceAndRecordPrediction
	   test-eval classifier test-inst)))
      (cond 
       ((> split-perc 0)
	(unless print-classifications
	  (format 
	   res-text 
	   (concatenate 'string 
	     "~%~%"
	     (jcall "replaceAll"
		    (weka.classifiers.Evaluation:toSummaryString
		     test-eval
		     "=== Error on test split ===\n"
		    print-complex-stats)
		   "\\n" "~%")))))
       
       (t
	(unless print-classifications
	  (format res-text "~%~%~s"
		  (replace-re
		   (weka.classifiers.Evaluation:toSummaryString
		    test-eval
		    "=== Error on test data ===\n"
		    print-complex-stats)
		   "\n" 
		   "~%"))))
       )
      )
     
     (train-insts
      (cond
       ((not no-cv)
	(let* ((srandom (java.util.Random:Random seed)))
	  (setf classifier (weka.classifiers.Classifier:makeCopy 
			    classifier-backup))
	  (cond
	   ((not print-classifications)
	    (weka.classifiers.Evaluation:crossValidateModel
	     test-eval classifier train-insts folds srandom)
	    (if (weka.core.Attribute:isNumeric
		 (weka.core.Instances:classAttribute template))
		(format 
		 res-text 
		 (concatenate 'string
		   "~%~%~%"
		   (jcall "replaceAll"
			  (weka.classifiers.Evaluation:toSummaryString
			   test-eval
			   "=== Cross-validation ===\n"
			   print-complex-stats)
			  "\n" 
			  "~%")))
	      (format 
	       res-text 
	       (concatenate 'string
		 "~%~%~%"
		 (jcall "replaceAll"
			(weka.classifiers.Evaluation:toSummaryString
			 test-eval
			 "=== Stratified Cross-validation ===\n"
			 print-complex-stats)
			"\n"
			"~%")))))
	   (t 
	    (setf preds-buff (make-array 0 :element-type 'character 
					 :adjustable t :fill-pointer 0))
	    (format preds-buff 
		    "~%=== Predictions under cross-validation ===~%~%")
	    ;; pay attention print-dist may not converted to Boolean auto.
	    (weka.classifiers.Evaluation:crossValidateModel 
	     test-eval classifier train-insts folds srandom preds-buff
	     attr-to-output print-dist))
	   ))))))
    
    (when (weka.core.Attribute:isNominal 
	   (weka.core.Instances:classAttribute template))
      (if (and class-stats (not no-cv))
	  (format res-text 
		  (concatenate 'string
		    "~%~%" 
		    (jcall "replaceAll"
			   (weka.classifiers.Evaluation:toClassDetailsString 
			    test-eval)
			   "\n"
			   "~%"))))
      
      (if (and (not no-cv) (not print-classifications))
	  (format res-text 
		  (concatenate 'string
		    "~%~%"
		    (jcall "replaceAll" 
			   (weka.classifiers.Evaluation:toMatrixString 
			    test-eval)
			   "\n"
			   "~%")))))
    
    (if preds-buff
	(format res-text 
		(concatenate 'string 
		  "~%" 
		  (jcall "replaceAll" 
			 preds-buff
			 "\n"
			 "~%"))))
    
    ;; TODO: serialize threshold to database
    (format t "~&serialize threshold to database.~%")
    (when (and threshold-out? 
	       (weka.core.Attribute:isNominal 
		(weka.core.Instances:classAttribute template)))
      (let* ((label-index 0))
	(if (/= 0 (length threshold-label))
	    (setf label-index (weka.core.Attribute:indexOfValue
			       (weka.core.Instances:classAttribute template)
			       threshold-label)))
	(if (= -1 label-index)
	    (error "Class label ~s is unknown!" threshold-label))
	
	(let* ((tc (weka.classifiers.evaluation.ThresholdCurve:ThresholdCurve))
	       (result (weka.core.Instances:toString
			(weka.classifiers.evaluation.ThresholdCurve:getCurve
			 tc
			 (weka.classifiers.Evaluation:predictions test-eval)
			 label-index)))
	       ;; (os (java.io.ByteArrayOutputStream:ByteArrayOutputStream))
	       )
	  (sql (update-threshold-q cid result) :db *weka-db*))
	)
      )
    (sql (update-result-q "text" cid res-text) :db *weka-db*)
    (values "text" res-text)
    
    )
  
