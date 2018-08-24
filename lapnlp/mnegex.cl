;;; -*- Mode: Lisp; Package: late -*-
#| 
yluo - 08/16/2018 clean and reorganization
yluo - 06/10/2015 rewrite using asdf framework
yluo - 07/10/2009 creation
|#



(eval-when (:compile-toplevel :load-toplevel :execute)  
  (require :regexp2))

(defpackage :mnegex
  (:use :common-lisp #+allegro :excl :late :util)
  (:export "*family*"
	   "*pre-negative*"
	   "*long-pre-negative*"
	   "*long-pre-possible*"
	   "*post-negative*"
	   "*pre-possible*"
	   "*post-possible*"
	   "*pre-weak*"
	   "*post-weak*"
	   "*pre-strong*"
	   "*post-strong*"
	   "*possessives*"
	   "*conjunctions*"
	   "*section-id*"
	   "*h-chem-tags*"
	   "*hr-chem-tags*"
	   "*h-cand-chem-ph*"
	   "*h-pred-chem-ph*"
	   "regex-encode"
	   "import-regex"
	   "import-negex-regex"
	   "predict"
	   ))
(in-package :mnegex)

(defparameter *family* nil)
(defparameter *pre-negative* nil)
(defparameter *long-pre-negative* nil)
(defparameter *long-pre-possible* nil)
(defparameter *post-negative* nil)
(defparameter *pre-possible* nil)
(defparameter *post-possible* nil)
(defparameter *pre-weak* nil)
(defparameter *post-weak* nil)
(defparameter *pre-strong* nil)
(defparameter *post-strong* nil)
(defparameter *possessives* nil)
(defparameter *conjunctions* nil)
(defparameter *section-id* nil)

;; not sure about the naming
(defparameter *h-chem-tags* (make-hash-table :test #'equal))
(setf (gethash "strong" *h-chem-tags*) 4) ;; including "high grade"
(setf (gethash "positive" *h-chem-tags*) 3)
(setf (gethash "weak" *h-chem-tags*) 2)
(setf (gethash "negative" *h-chem-tags*) 1)

(defparameter *hr-chem-tags* (make-hash-table :test #'equal))
(setf (gethash 1 *hr-chem-tags*) "negative")
(setf (gethash 2 *hr-chem-tags*) "weak")
(setf (gethash 3 *hr-chem-tags*) "positive")
(setf (gethash 4 *hr-chem-tags*) "strong")

(defparameter *h-cand-chem-ph* (make-hash-table :test #'equal))
(defparameter *h-pred-chem-ph* (make-hash-table :test #'equal))
(defun regex-encode (regex)
  (replace-re regex "\\(" "\\\\(")
  (replace-re regex "\\)" "\\\\)")
  (replace-re regex "\\[" "\\\\[")
  (replace-re regex "\\]" "\\\\]")
  (replace-re regex "\\{" "\\\\{")
  (replace-re regex "\\}" "\\\\}")
  (replace-re regex "\\<" "\\\\<")
  (replace-re regex "\\>" "\\\\>")
  (replace-re regex "\\-" "\\\\-")
  regex)

;; files is a list, in the following order
;; post negative
;; post possible
;; prenegation
;; prepossible
;; family - note that this is not appliable in pathology reports
;; conjunctions
;; longprepossible
;; longprenegation
;; possessives
(defun import-regex (file)
  (let ((lines (read-lines file))
	(ans nil))
    (dolist (ln lines)
      (unless (match-re "^\\#" ln)
	(setq ans (concatenate 'string ans "|" (string-downcase ln))))
      )
    (regex-encode ans)
    )
  )

(defun import-negex-regex ()
;;; I commented out the codes for inserting negex rules to DB, uncomment when needed
;;;  (sql "drop table if exists negex_regex" :db *late-db*)
;;;  (sql "create table if not exists negex_regex (
;;;        type varchar(30) not null primary key,
;;;        regex longtext,
;;;        index (type))" :db *late-db*)
  (setq *post-negative* (import-regex "./MNegexRules/postnegative"))
  (format t "post-negative's length is ~a~%" (length *post-negative*))
;;;  (sql (format nil 
;;;	       "insert into negex_regex value (~a, ~a)"
;;;	       (sq "post-negative")
;;;	       (sq *post-negative*)) :db *late-db*)
  
  (setq *post-possible* (import-regex "./MNegexRules/postpossible"))
  (format t "post-possible's length is ~a~%" (length *post-possible*))
;;;  (sql (format nil 
;;;	       "insert into negex_regex value (~a, ~a)"
;;;	       (sq "post-possible")
;;;	       (sq *post-possible*)) :db *late-db*)
  
  (setq *pre-negative* (import-regex "./MNegexRules/prenegation"))
  (format t "pre-negative's length is ~a~%" (length *pre-negative*))
;;;  (sql (format nil 
;;;	       "insert into negex_regex value (~a, ~a)"
;;;	       (sq "pre-negative")
;;;	       (sq *pre-negative*)) :db *late-db*)  

  (setq *post-strong* (import-regex "./MNegexRules/poststrong"))
  (format t "post-strong's length is ~a~%" (length *post-strong*))
;;;  (sql (format nil 
;;;	       "insert into negex_regex value (~a, ~a)"
;;;	       (sq "post-strong")
;;;	       (sq *post-possible*)) :db *late-db*)
  
  (setq *pre-strong* (import-regex "./MNegexRules/prestrong"))
  (format t "pre-strong's length is ~a~%" (length *pre-strong*))
;;;  (sql (format nil 
;;;	       "insert into negex_regex value (~a, ~a)"
;;;	       (sq "pre-strong")
;;;	       (sq *pre-negative*)) :db *late-db*)    

  (setq *post-weak* (import-regex "./MNegexRules/postweak"))
  (format t "post-weak's length is ~a~%" (length *post-weak*))
;;;  (sql (format nil 
;;;	       "insert into negex_regex value (~a, ~a)"
;;;	       (sq "post-weak")
;;;	       (sq *post-possible*)) :db *late-db*)
  
  (setq *pre-weak* (import-regex "./MNegexRules/preweak"))
  (format t "pre-weak's length is ~a~%" (length *pre-weak*))
;;;  (sql (format nil 
;;;	       "insert into negex_regex value (~a, ~a)"
;;;	       (sq "pre-weak")
;;;	       (sq *pre-negative*)) :db *late-db*)      
  
  (setq *pre-possible* (import-regex "./MNegexRules/prepossible"))
  (format t "pre-possible's length is ~a~%" (length *pre-possible*))
;;;  (sql (format nil 
;;;	       "insert into negex_regex value (~a, ~a)"
;;;	       (sq "post-possible")
;;;	       (sq *post-possible*)) :db *late-db*)
  
  (setq *family* (import-regex "./MNegexRules/family"))
  (format t "family's length is ~a~%" (length *family*))
;;;  (sql (format nil 
;;;	       "insert into negex_regex value (~a, ~a)"
;;;	       (sq "family")
;;;	       (sq *family*)) :db *late-db*)
  
  (setq *conjunctions* (import-regex "./MNegexRules/conjunctions"))
  (format t "conjunction's length is ~a~%" (length *conjunctions*))
;;;  (sql (format nil 
;;;	       "insert into negex_regex value (~a, ~a)"
;;;	       (sq "conjunctions")
;;;	       (sq *conjunctions*)) :db *late-db*)
  
  (setq *long-pre-possible* (import-regex "./MNegexRules/longprepossible"))
  (format t "long-pre-possible's length is ~a~%" (length *long-pre-possible*))
;;;  (sql (format nil 
;;;	       "insert into negex_regex value (~a, ~a)"
;;;	       (sq "long-pre-possible")
;;;	       (sq *long-pre-possible*)) :db *late-db*)  
  
  (setq *long-pre-negative* (import-regex "./MNegexRules/longprenegation"))
  (format t "long-pre-negative's length is ~a~%" (length *long-pre-negative*))
;;;  (sql (format nil 
;;;	       "insert into negex_regex value (~a, ~a)"
;;;	       (sq "long-pre-negative")
;;;	       (sq *long-pre-negative*)) :db *late-db*)
  
  (setq *possessives* (import-regex "./MNegexRules/possessives"))
  (format t "possessives' length is ~a~%" (length *possessives*))
;;;  (sql (format nil 
;;;	       "insert into negex_regex value (~a, ~a)"
;;;	       (sq "possessives")
;;;	       (sq *possessives*)) :db *late-db*)
  
  )

(defun predict (ref)
  (setq ref (string-downcase ref))
  ;;  (clrhash *h-cand-chem-ph*)
  (clrhash *h-pred-chem-ph*)
  (let ((seen nil)
	(match nil)
	(sg nil))
    (with-hash-table-iterator
     (entry *h-cand-chem-ph*)
     (labels
	 ((hfun-iter
	   (got-one &optional key val)
	   (when got-one
	     ;; (format t "got-one is: ~a~%" got-one)
	     ;; (format t "key is: ~a~%" key)
	     ;; (format t "val is: ~a~%" val)
	     (setq key (regex-encode key))
	     (setq val (regex-encode val))
	     (setq seen nil)
	     (cond
	      ;; possessive means positive
	      ((match-re (concatenate 'string "(\\s|^)("
				      *possessives*
				      ")\\s("
				      key
				      ")") ref)
	       (setf (gethash key *h-pred-chem-ph*) 
		     (gethash "positive" *h-chem-tags*)))
	      ;; look for things in a "no" list
	      ((setq match (match-re (concatenate 'string 
						  "(.*)((?:\\S\\s|^){1,})no\\s"
						  "((?:\\s*\\S*\\s*){0,})("
						  key
						  ")(.*)") ref :return :match))
	       (let* ((pre-no (re-submatch match nil nil 1))
		      (pre-symbol (re-submatch match nil nil 2))
		      (post-no (re-submatch match nil nil 3))
		      (post-chem (re-submatch match nil nil 5)))
		 ;; (format t "pre-no: ~a~%pre-symbol: ~a~%post-no: ~a~%post-chem: ~a~%" pre-no pre-symbol post-no post-chem)		 
		 (cond
		  ((match-re (concatenate 'string 
					  "\\,\\s(a|an|and)\\s" key) ref) 
		   (setq seen nil))
		  
		  ((and (or (match-re "\\," pre-symbol)
			    (match-re "\\," pre-no))
			(not (match-re "(\\s|\\b)or\\s" post-no))
			(match-re "\\," post-no)
			(not (match-re "(\\s|\\b)or\\s" post-chem)))
		   (setq seen nil)
		   )
		  
		  ;; handles the conjunction case
		  ;; just a test, need to change back to *conjunction*
		  ((match-re (concatenate 'string "(\\b|\\s)" *conjunctions* "(\\b|\\s)") 
			     post-no)
		   (setq seen nil))
		  
		  (t 
		   (let ((max-l 0)
			 (tmp-l 0)
			 (words (split-re "\\," post-no)))
		     (dolist (w words)
		       (setq tmp-l (length (split-re "\\s" w)))
		       (if (< max-l tmp-l)
			   (setq max-l tmp-l)))
		     (cond
		      ((<= max-l 4)
		       (setf (gethash key *h-pred-chem-ph*)
			     (gethash "negative" *h-chem-tags*))
		       (setq seen t))
		      (t
		       (setq seen nil)))))))))
	     
	     (cond
	      ((null seen)
	       ;; (format t "seen is null~%")
	       (cond
		;; look for long-pre-negative
		((and (setq match 
			    (match-re (concatenate 'string 
						   ".*(^|\\s)("
						   *long-pre-negative*
						   ")\\s((\\s*\\S*\\s*)*)("
						   key ")") ref :return :match))
		      (setq sg (re-submatch match nil nil 3))
		      ;; (format t "0 sg is ~a~%" sg)
		      (not (match-re (concatenate 'string
						  "(\\b|\\s)("
						  *conjunctions*
						  ")(\\b|\\s)") sg)))
		 (setf (gethash key *h-pred-chem-ph*)
		       (gethash "negative" *h-chem-tags*)))
		
		;; look for pre-negative
		((and (setq match 
			    (match-re (concatenate 'string
						   "(^|\\s)("
						   *pre-negative*
						   ")\\s((\\s*\\S*\\s*){0,4})("
						   key ")") ref :return :match))
		      (setq sg (re-submatch match nil nil 3))
		      ;; (format t "1 sg is ~a~%" sg)
		      (not (match-re (concatenate 'string
						  "(\\b|\\s)("
						  *conjunctions*
						  ")(\\b|\\s)") sg)))
		 (setf (gethash key *h-pred-chem-ph*)
		       (gethash "negative" *h-chem-tags*)))
		
		;; look for post-negative
		((and (setq match 
			    (match-re (concatenate 'string
						   key
						   "((\\s*\\S*\\s*){0,4}?)\\s("
						   *post-negative*
						   ")(\\s|$)")
				      ref :return :match))
		      (setq sg (re-submatch match nil nil 1))
		      ;; (format t "2 sg is ~a~%" sg)
		      (not (match-re (concatenate 'string
						  "(\\b|\\s)("
						  *conjunctions*
						  ")(\\b|\\s)") sg)))
		 (setf (gethash key *h-pred-chem-ph*)
		       (gethash "negative" *h-chem-tags*)))

		;; look for pre-strong
		((and (setq match 
			    (match-re (concatenate 'string
						   "(^|\\s)("
						   *pre-strong*
						   ")\\s((\\s*\\S*\\s*){0,4})("
						   key ")") ref :return :match))
		      (setq sg (re-submatch match nil nil 3))
		      ;; (format t "3 sg is ~a~%" sg)
		      (not (match-re (concatenate 'string
						  "(\\b|\\s)("
						  *conjunctions*
						  ")(\\b|\\s)") sg)))
		 (setf (gethash key *h-pred-chem-ph*)
		       (gethash "strong" *h-chem-tags*)))
		
		;; look for post-strong
		((and (setq match 
			    (match-re (concatenate 'string
						   key
						   "((\\s*\\S*\\s*){0,4}?)\\s("
						   *post-strong*
						   ")(\\s|$)")
				      ref :return :match))
		      (setq sg (re-submatch match nil nil 1))
		      ;; (format t "4 sg is ~a~%" sg)
		      (not (match-re (concatenate 'string
						  "(\\b|\\s)("
						  *conjunctions*
						  ")(\\b|\\s)") sg)))
		 (setf (gethash key *h-pred-chem-ph*)
		       (gethash "strong" *h-chem-tags*)))

		;; look for pre-weak
		((and (setq match 
			    (match-re (concatenate 'string
						   "(^|\\s)("
						   *pre-weak*
						   ")\\s((\\s*\\S*\\s*){0,4})("
						   key ")") ref :return :match))
		      (setq sg (re-submatch match nil nil 3))
		      ;; (format t "5 sg is ~a~%" sg)
		      (not (match-re (concatenate 'string
						  "(\\b|\\s)("
						  *conjunctions*
						  ")(\\b|\\s)") sg)))
		 (setf (gethash key *h-pred-chem-ph*)
		       (gethash "weak" *h-chem-tags*)))
		
		;; look for post-weak
		((and (setq match 
			    (match-re (concatenate 'string
						   key
						   "((\\s*\\S*\\s*){0,4}?)\\s("
						   *post-weak*
						   ")(\\s|$)")
				      ref :return :match))
		      (setq sg (re-submatch match nil nil 1))
		      ;; (format t "6 sg is ~a~%" sg)
		      (not (match-re (concatenate 'string
						  "(\\b|\\s)("
						  *conjunctions*
						  ")(\\b|\\s)") sg)))
		 (setf (gethash key *h-pred-chem-ph*)
		       (gethash "weak" *h-chem-tags*)))
		
		(t 
		 (setf (gethash key *h-pred-chem-ph*)
		       (gethash "positive" *h-chem-tags*))))))
	     (multiple-value-call #'hfun-iter (entry)))))
       (multiple-value-call #'hfun-iter (entry))))))

;; (provide :mnegex)
