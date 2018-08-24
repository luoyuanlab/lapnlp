;;; -*- Mode: Lisp; Package: wordnet; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/10/2015 rewrite using asdf framework
yluo - 07/05/2010 Creation

Package to interface with WordNet MySQL database
WordNet MySQL database is available at http://wnsql.sourceforge.net/
|#

(defpackage :wordnet
  (:use #+allegro :excl :common-lisp :late :link :dbi.mysql :ff)
  (:export 
   "*debug-wordnet*"
   "*h-wn-senses*"
   "*libwordnet-home*"
   "*wordnet-db*"
   "close-wordnet-database"
   "load-libwordnet"
   "open-wordnet-database"
   "penn-to-wordnet-pos"
   "persist-synset-stats"
   "synsense-ff"
   "synsensedef-ff"
   "synset-db"
   "synset-ff"
   "synsetize"
   "synonyms?"
   "top-hypernym-ff"
   "wordnet-transaction"
   ))
(in-package :wordnet)

(defparameter *debug-wordnet* nil
  "controls whether to output debug information for wordnet.cl")

(defparameter *wordnet-db* nil
  "The connection to the MySQL database holding WordNet's persistent store")

(defparameter *libwordnet-home* nil
  "The home directory of the customized wordnet application")

(defparameter *libwordnet* nil
  "The parameter denoting whether libwordnet.so has been loaded")

(defparameter *h-wn-senses* (make-hash-table :test #'equalp)
  "The parameter stores temperarily syn sense stats.
Key: 'synsense invoked-by pos'
Val: freq")

(defmethod load-libwordnet ()
  (unless *libwordnet*
    (format t "~&Loading libwordnet~%")
    (setq *libwordnet-home*
	  (let ((lwh (get-env "LIBWORDNET_HOME")))
	    (and lwh (get-directory-pathname (translate-logical-pathname lwh)))))
    (if (null *libwordnet-home*)
	(error "Could not find location of libwordnet tookit.")
      (setf *libwordnet* (load (merge-pathnames
				(make-pathname :name "libwordnet"
					       :type "so")
				*libwordnet-home*))))))

(def-foreign-type str-arr (:array (* :char)))

(def-foreign-type Synonyms
  (:struct
   (ptr str-arr)
   (wordnum :int)))

(def-foreign-type str-arr-arr (:array (:array (* :char))))
(def-foreign-type int-arr (:array :int))

(def-foreign-type SynonymSenses 
  (:struct
   (sensenum :int)
   (ptr str-arr-arr)
   (sensesize int-arr)
   ))

(def-foreign-type SynonymSenseDefs 
  (:struct
   (sensenum :int)
   (ptr str-arr)
   ))

(def-foreign-call (synset-ffi "synset")
  ((word (* :char) string) 
   (pos (* :char) string))
  :strings-convert t
  :returning :foreign-address)

(def-foreign-call (synsense-ffi "synsense")
  ((word (* :char) string) 
   (pos (* :char) string))
  :strings-convert t
  :returning :foreign-address)

(def-foreign-call (top-hypernym-ffi "top_hypernym")
  ((word (* :char) string) 
   (pos (* :char) string)
   (senseidx :int integer))
  :strings-convert t
  :returning :foreign-address)

(def-foreign-call (synsensedef-ffi "synsensedef")
  ((word (* :char) string) 
   (pos (* :char) string))
  :strings-convert t
  :returning :foreign-address)

(defun free-synonyms (syns)
  (let* (synarr synsize)
    (setf synarr (fslot-value-typed '(* str-arr) :c
				    (fslot-value-typed 'Synonyms :c syns 'ptr)
				    '*))
    (setf synsize (fslot-value-typed 'Synonyms :c syns 'wordnum))
    (dotimes (i synsize)
      (free-fobject (fslot-value-typed 'str-arr :c synarr i)))
    (free-fobject synarr)
    (free-fobject syns)))

(defun free-synsets (synsenses)
  (let* (synsense-arr sensenum sensesizes sensesize sense)
    (setf synsense-arr (fslot-value-typed '(* str-arr-arr) :c
					  (fslot-value-typed 'SynonymSenses 
							     :c synsenses :ptr)
					  '*)
	  sensenum (fslot-value-typed 'SynonymSenses :c synsenses 'sensenum)
	  sensesizes (fslot-value-typed '(* int-arr) :c
					(fslot-value-typed 'SynonymSenses 
							   :c 
							   synsenses 
							   :sensesize)
					'*))
    
    (dotimes (i sensenum)
      (setf sensesize (fslot-value-typed 'int-arr :c sensesizes i)
	    sense (fslot-value-typed '(* str-arr) :c
				     (fslot-value-typed 'str-arr-arr 
							:c synsense-arr i)
				     '*))
      (dotimes (j sensesize)
	(free-fobject (fslot-value-typed 'str-arr :c sense j)))
      (free-fobject sense))
    (free-fobject sensesizes)
    (free-fobject synsense-arr)
    (free-fobject synsenses)))

(defun free-synsendefs (synsendefs)
  (let* (synsendef-arr sensenum)
    (setf synsendef-arr (fslot-value-typed '(* str-arr) :c
					   (fslot-value-typed 'SynonymSenseDefs 
							      :c synsendefs :ptr)
					   '*)
	  sensenum (fslot-value-typed 'SynonymSenseDefs :c synsendefs 'sensenum)
	  )
    
    (dotimes (i sensenum)
      (free-fobject (fslot-value-typed 'str-arr :c synsendef-arr i)))
    (free-fobject synsendef-arr)
    (free-fobject synsendefs)))

(defun open-wordnet-database ()
  "Opens connection to the MySQL database used by WordNet."
  (when (and *wordnet-db* (not (mysql-connected *wordnet-db*)))
    (setq *wordnet-db* nil))
  (unless *wordnet-db*
    (setq *wordnet-db* (open-mysql-db "WORDNET_DB"))))

;;; duplicate the LATE counterpart
(defmacro wordnet-transaction (&body body)
  `(prog2 (progn (open-wordnet-database)
		 (sql "start transaction" :db *wordnet-db*))
       (progn ,@body)
     (sql "commit" :db *wordnet-db*)))

(defun close-wordnet-database ()
  "Commit all transactions and terminate connection to the database used by ~
   WordNet."
  (cond ((null *wordnet-db*) 
	 (format t "~&WordNet database already closed.~%"))
	
	((mysql-connected *wordnet-db*)
	 (sql "COMMIT" :db *wordnet-db*)
	 (disconnect :db *wordnet-db*)
	 (setf *wordnet-db* nil))
	
	(t 
	 (format t "~&WordNet database was already disconnected")))
  *wordnet-db*)


(defmethod synset-db ((word string)
		       (pos string))
  (assert (member pos (list "n" "v" "a" "r") :test #'equalp)
	  ()
	  "pos must be one of n, v, a, or r.")
  (let* (ans t-start)
    (setf t-start (get-internal-real-time))
    (mapcar #'(lambda (x) (pushnew (car x) ans :test #'equalp))
	    (sql (format nil "SELECT lemma FROM dict WHERE synsetid IN
                              (SELECT synsetid FROM dict WHERE
                                 lemma IN (SELECT lemma FROM morphology
                                             WHERE morph='~a' AND pos='~a')
                                 AND pos='~a')"
			 word pos pos)
		 :db *wordnet-db*))
    (format t "~&synset2 in ~a msecs~%" (- (get-internal-real-time) t-start))
    ans)
  )


;;; maps from Penn treebank part of speech to wordnet part of speech
(defmethod penn-to-wordnet-pos ((pos string))
  (cond
   ((member pos (list "JJ" "JJR" "JJS") :test #'equalp)
    "a")
   ((member pos (list "RB" "RBR" "RBS" "WRB") :test #'equalp)
    "r")
   ((member pos (list "VB" "VBD" "VBG" "VBN" "VBP" "VBZ") :test #'equalp)
    "v")
   ((member pos (list "NN" "NNS" "NNP" "NNPS") :test #'equalp)
    "n")
   (t
    (format t "~&Warning: ~a cannot be mapped to a, r, v or n~%" pos))))

;;; synset extraction depending on foreign function call on wordnet shared lib
(defmethod synset-ff ((word string)
		      (pos string))
  (load-libwordnet)
  (assert (member pos (list "n" "v" "a" "r") :test #'equalp)
	  ()
	  "pos must be one of n, v, a, or r to confirm with wordnet definition.")
  (let* (syns synarr synsize ans t-start synword str-adr)
    (setf t-start (get-internal-real-time))
    (setf syns (synset-ffi word pos))
    (if *debug-wordnet* (format t "~&syns is ~a~%" syns))
    (setf synarr (fslot-value-typed '(* str-arr) :c
				    (fslot-value-typed 'Synonyms :c syns 'ptr)
				    '*))
    (if *debug-wordnet* (format t "~&synarr is ~a~%" synarr))
    (setf synsize (fslot-value-typed 'Synonyms :c syns 'wordnum))
    (if *debug-wordnet* (format t "~&synsize is ~a~%" synsize))
    (dotimes (i synsize)
      (setf str-adr (fslot-value-typed 'str-arr :c synarr i))
      (if *debug-wordnet* (format t "~&str-adr is ~a~%" str-adr))
      (setf synword (native-to-string str-adr))
      (if *debug-wordnet* (format t "~&synword is ~a~%" synword))
      (pushnew synword ans :test #'equalp))
    (free-synonyms syns)
    (if *debug-wordnet* 
	(format t "~&synset in ~a msecs~%" 
		(- (get-internal-real-time) t-start)))
    ;; make sure to return same result for words that are synonyms
    ;; unfortunately, this is not true, transivitiy doesn't hold for synonyms
    ;; this is probably because synonyms really talk about different words
    ;; resolved to individual senses, but down to this level, transitivity 
    ;; should already hold.
    (sort ans #'string-lessp)))

(defmethod synsense-ff ((word string)
			(pos string))
  (load-libwordnet)
  (assert (member pos (list "n" "v" "a" "r") :test #'equalp)
	  ()
	  "pos must be one of n, v, a, or r to confirm with wordnet definition.")
  (let* (synsenses synsense-arr sensenum sense sensesizes sensesize l-sense ans 
		   synword str-adr)

    (setf synsenses (synsense-ffi word pos))

    (setf sensenum (fslot-value-typed 'SynonymSenses :c synsenses 'sensenum))
    (setf synsense-arr (fslot-value-typed '(* str-arr-arr) :c
					  (fslot-value-typed 'SynonymSenses 
							     :c synsenses :ptr)
					  '*))

    (setf sensesizes (fslot-value-typed '(* int-arr) :c
					(fslot-value-typed 'SynonymSenses 
							   :c 
							   synsenses 
							   :sensesize)
					'*))
    
    (when *debug-wordnet* 
      (format t "~&synsenses is ~a~%" synsenses)
      (format t "~&synsense-arr is ~a~%" synsense-arr)
      (format t "~&sensenum is ~a~%" sensenum))
    (dotimes (i sensenum)
      (setf sensesize (fslot-value-typed 'int-arr :c sensesizes i)
	    sense (fslot-value-typed '(* str-arr) :c
				     (fslot-value-typed 'str-arr-arr 
							:c synsense-arr i)
				     '*)
	    l-sense nil)
      
      (dotimes (j sensesize)
	(setf str-adr (fslot-value-typed 'str-arr :c sense j)
	      synword (native-to-string str-adr))
	
	(when *debug-wordnet* 
	  (format t "~&str-adr is ~a~%" str-adr)
	  (format t "~&synword is ~a~%" synword))
	
	(push synword l-sense))
      
      (push (format nil "~{~a~^-~}" (nreverse l-sense)) ans))
    (free-synsets synsenses)
    ;; make sure to return same result for words that are synonyms
    ;; unfortunately, this is not true, transivitiy doesn't hold for synonyms
    ;; this is probably because synonyms really talk about different words
    ;; resolved to individual senses, but down to this level, transitivity 
    ;; should already hold.
    (nreverse ans))
  )

(defmethod top-hypernym-ff ((word string)
			    (pos string)
			    (senseidx integer))
  (load-libwordnet)
  (assert (member pos (list "n" "v" "a" "r") :test #'equalp)
	  ()
	  "pos must be one of n, v, a, or r to conform with wordnet definition.")
  (let* (synsenses synsense-arr sensenum sense sensesizes sensesize l-sense ans 
		   synword str-adr)

    (setf synsenses (top-hypernym-ffi word pos senseidx))

    (setf sensenum (fslot-value-typed 'SynonymSenses :c synsenses 'sensenum))
    (setf synsense-arr (fslot-value-typed '(* str-arr-arr) :c
					  (fslot-value-typed 'SynonymSenses 
							     :c synsenses :ptr)
					  '*))

    (setf sensesizes (fslot-value-typed '(* int-arr) :c
					(fslot-value-typed 'SynonymSenses 
							   :c 
							   synsenses 
							   :sensesize)
					'*))
    
    (when *debug-wordnet* 
      (format t "~&synsenses is ~a~%" synsenses)
      (format t "~&synsense-arr is ~a~%" synsense-arr)
      (format t "~&sensenum is ~a~%" sensenum))
    (dotimes (i sensenum)
      (setf sensesize (fslot-value-typed 'int-arr :c sensesizes i)
	    sense (fslot-value-typed '(* str-arr) :c
				     (fslot-value-typed 'str-arr-arr 
							:c synsense-arr i)
				     '*)
	    l-sense nil)
      
      (dotimes (j sensesize)
	(setf str-adr (fslot-value-typed 'str-arr :c sense j)
	      synword (native-to-string str-adr))
	
	(when *debug-wordnet* 
	  (format t "~&str-adr is ~a~%" str-adr)
	  (format t "~&synword is ~a~%" synword))
	
	(push synword l-sense))
      
      (push (format nil "~{~a~^-~}" (nreverse l-sense)) ans))
    (free-synsets synsenses)
    ;; make sure to return same result for words that are synonyms
    ;; unfortunately, this is not true, transivitiy doesn't hold for synonyms
    ;; this is probably because synonyms really talk about different words
    ;; resolved to individual senses, but down to this level, transitivity 
    ;; should already hold.
    (nreverse ans)))

(defmethod synsensedef-ff ((word string)
			   (pos string))
  (load-libwordnet)
  (assert (member pos (list "n" "v" "a" "r") :test #'equalp)
	  ()
	  "pos must be one of n, v, a, or r to confirm with wordnet definition.")
  (let* (synsendefs synsendef-arr sensenum ans syndef str-adr)
    (setf synsendefs (synsensedef-ffi word pos))
    (setf sensenum (fslot-value-typed 'SynonymSenses :c synsendefs 'sensenum))
    (setf synsendef-arr (fslot-value-typed '(* str-arr) :c
					   (fslot-value-typed 'SynonymSenseDefs 
							      :c synsendefs 
							      :ptr)
					   '*))

    
    (when *debug-wordnet* 
      (format t "~&synsendefs is ~a~%" synsendefs)
      (format t "~&synsendef-arr is ~a~%" synsendef-arr)
      (format t "~&sensenum is ~a~%" sensenum))
    (dotimes (i sensenum)
      (setf str-adr (fslot-value-typed 'str-arr :c synsendef-arr i)
	    syndef (native-to-string str-adr))
      
      (when *debug-wordnet* 
	(format t "~&str-adr is ~a~%" str-adr)
	(format t "~&syndef is ~a~%" syndef))
      (multiple-value-bind (m? whole def)
	  (match-re "^\\(([^;]*?);" syndef)
	(declare (ignore whole))
	(if m?
	    (push def ans)
	  (multiple-value-bind (m2? whole2 def2)
	      (match-re "^\\((.*)\\)$" syndef)
	    (declare (ignore whole2))
	    (if m2?
		(push def2 ans)
	      (error "needs concise def for ~a~%" syndef))))))
    (free-synsendefs synsendefs)
    ;; make sure to return same result for words that are synonyms
    ;; unfortunately, this is not true, transivitiy doesn't hold for synonyms
    ;; this is probably because synonyms really talk about different words
    ;; resolved to individual senses, but down to this level, transitivity 
    ;; should already hold.
    (nreverse ans))
  )


(defmethod synsetize ((ta token-annotation))
  (let* ((pos (penn-to-wordnet-pos (car (annotations-on-spec ta :type (gtype 'gtag-type)))))
	 (synsenses (synsense-ff (string-downcase (content ta)) pos)))
    (dolist (synsense synsenses)
      (incf (gethash (format nil "~{~a~^-~} ~a ~a" 
			     synsense (string-downcase (content ta)) pos) 
		     *h-wn-senses* 0)))))

(defmethod synsetize ((doc document))
  (dolist (ta (annotations-spec doc :type (gtype 'gtoken-type)))
    (synsetize ta)))

(defmethod synsetize ((corp corpus))
  (dolist (doc (documents corp))
    (synsetize doc)))

(defmethod persist-synset-stats (&aux (old-freq nil))
  (maphash #'(lambda (key freq)
	       (multiple-value-bind (m? whole? synsense invoked-by pos)
		   (match-re "(\\S+) (\\S+) (\\S+)" key)
		 (declare (ignore m? whole?))
		 (setf old-freq (wn-synset-freq synsense invoked-by pos))
		 (if old-freq
		     (update-wn-synset synsense 
				       invoked-by
				       (+ (parse-integer old-freq) freq)
				       pos)
		   (save-wn-synset synsense invoked-by freq pos))))
	   *h-wn-senses*)
  (clrhash *h-wn-senses*))

(defmethod synonyms? ((word1 string)
		      (pos1 string)
		      (word2 string)
		      (pos2 string))
  (setf pos1 (penn-to-wordnet-pos pos1)
	pos2 (penn-to-wordnet-pos pos2))
  (assert (and (member pos1 (list "a" "v" "n" "r"))
	       (string-equal pos1 pos2))
	  ()
	  "pos1 and pos2 must agree for synonym testing.")
  (setf word1 (string-downcase word1)
	word2 (string-downcase word2))
  (member word1 (synset-ff word2 pos2) :test #'equalp))
