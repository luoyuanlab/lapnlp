;;; -*- Mode: LISP; Package: umlsize -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 10/21/2012 prefer mesh-bearing cui's in hnorm->cui, previously work not
fully tracked: load local hash table instead of looking up remote db, to 
save time
yluo - 05/29/2012 str->cui uses lragr to reduce str to base form, generalize 
from w->sp-pos to str->sp-pos, add str->sp-lex
yluo - 05/11/2012 added umls distribution utility, added umls-cover
yluo - 05/02/2012 added socket-error binding to umlssql.
yluo - 03/11/2012 added Specialist Lexicon complement code search function
and predicates
yluo - 07/15/2010 added w-sca->sp-agr
yluo - 06/15/2010 added cui->code->mesh, scui->mesh, mcui->mesh, refer to 
notes on UMLS and MeSH
psz  - 07/15/2009 fixed bugs
psz  -           creation
Utilities that create annotations of a tokenized document using the UMLS.

The UMLS is multi-lingual, and currently may contain data for the
following languages: ENG, BAQ, CZE, DAN, DUT, FIN, FRE, GER, HEB, HUN,
ITA, JPN, NOR, POR, RUS, SPA, and SWE.  For now, we really assume that
all text is in English.  The preferred name of a CUI is given in the
language selected by *umls-pref-lang*, but we do not use alternative
languages to search for CUI's.  This is in part because only for
English are there MRXNW and MRXNS tables.
|#

(defpackage :umlsize
  (:use :common-lisp :late :util :norm :regexp :dbi.mysql)
  (:export 
   "*cui-covered-tokens*"
   "*cui-uncovered-tokens*"
   "*cvf-nlp*"
   "*debug-umls*"
   "*default-max-concept-length*"
   "*h-mesh-heading*"
   "*h-mrconso-mesh*"
   "*h-mrconso-mrxns*"
   "*h-mrconso-wind*"
   "*kp-va*"
   "*kp-va+MeDRA*"
   "*l-cui-ambig*"
   "*l-cui-length*"
   "*l-sp-lex-ambig*"
   "*l-sp-lex-length*"
   "*l-tui-ambig*"
   "*l-tui-length*"
   "*lns*"
   "*mesh-tree-depth*"
   "*profile-umls*"
   "*sp-lex-covered-tokens*"
   "*sp-lex-fn-scas*"
   "*sp-lex-uncovered-tokens*"
   "*stn-depth*"
   "*tui-covered-tokens*"
   "*tui-hash*"
   "*tui-map*"
   "*tui-stn-hash*"
   "*tui-uncovered-tokens*"
   "tuis->sty"
   "*type-map*"
   "*umls*"
   "*umls-has-pname*"
   "*umls-pref-lang*"
   "ann-coverage-cuis"
   "hstr->sp-pos"
   "str->sp-pos"
   "augment-opennlp-postag-dict"
   "close-umls"
   "cname"
   "cnames"
   "create-mrconso-wind"
   "create-pnames"
   "cui->code->mesh"
   "cui->mesh"
   "cui->mesh2"
   "cui->tui"
   "event-stnp"
   "str->cui"
   "dump-mrconso-wind"
   "get-lengths"
   "get-stys"
   "h-infix-search-mrconso-mesh"
   "h-mesh-heading"
   "h-nominalize"
   "hnorm->cui"
   "immunologic-tui"
   "infix-search-mrconso-nlp"
   "intran-partp"
   "intranp"
   "load-mesh-heading"
   "load-mrconso-mesh"
   "load-mrconso-mrxns"
   "load-mrconso-wind"
   "load-sp-lex"
   "mname"
   "noun-norm"
   "non-fn-pos"
   "norm->cui"
   "online-lookup"
   "open-umls"
   "opt-tran-partp"
   "opt-tranp"
   "output-l-dist"
   "pname"
   "populate-tui-stn-hash"
   "rel"
   "s->cui"
   "sample"
   "sample-sents"
   "show-top-umls"
   "sp-lex-cover"
   "stn->sty-rl"
   "subsumed?"
   "tag-inconsistency"
   "tname"
   "tran-partp"
   "tranp"
   "tui->sem"
   "tui->stn"
   "trim-stns"
   "umls-conj-list"
   "umls-cover"
   "umls-get-version"
   "umls-prep-list"
   "umlsize"
   "umlssql"
   "umlssql1"
   "w->cui"
   "w->fn"
   "w->sp-pos"
   "w-sca->sp-agr"
   "w-sca->sp-com"
   "wn->cui"
   "words->umls-fn->ptb"
   "non-if-chemicals"
   "if-chemicals"
   "pname-has-plus?"
   "if-tui?"
   "*if-tuis*"
   "if-tuis"
   "sty-rl->stn"
   ))

(in-package :umlsize)

(defparameter *debug-umls* nil)

(defparameter *mesh-tree-depth* 10
  "The mesh tree depth seen over the current corpus.")

(defparameter *stn-depth* 7
  "The STN depth seen over the current corpus.")

(defparameter *umls* nil
  "The connection to the UMLS database, if any.")

(defparameter *umls-pref-lang* "ENG"
  "The preferred language in which to describe UMLS concepts.")

(defparameter *umls-has-pname* nil
  "Boolean telling whether the instance of the UMLS database we
  connect to contains a table called PNAME. It is an optimization to
  give us a simple way to look up the preferred name of any CUI. The
  pname function shows the equivalent retrieval from MRCONSO.")

(defparameter *profile-umls* t)

(defparameter *norm->cui-time* 0)
(defparameter *cui->mesh-time* 0)
(defparameter *cui->tui-time* 0)
(defparameter *w->sp-pos-time* 0)
(defparameter *str->sp-lex-time* 0)

(defparameter *umlsize-1sth-time* 0)
(defparameter *umlsize-2ndh-time* 0)
(defparameter *umlsize-tr-time* 0)
(defparameter *norm->cui-cnt* 0)
(defparameter *cui->mesh-cnt* 0)
(defparameter *cui->tui-cnt* 0)
(defparameter *w->sp-pos-cnt* 0)
(defparameter *str->sp-lex-cnt* 0)

(defparameter *cui-covered-tokens* 0)
(defparameter *cui-uncovered-tokens* 0)
(defparameter *tui-covered-tokens* 0)
(defparameter *tui-uncovered-tokens* 0)
(defparameter *sp-lex-covered-tokens* 0)
(defparameter *sp-lex-uncovered-tokens* 0)

(defparameter *l-cui-length* nil
  "Association list, Key: CUI length; Val: count")
(defparameter *l-tui-length* nil
  "Key: TUI length; Val: count")
(defparameter *l-sp-lex-length* nil
  "Association list, key: sp-lex length; val: count")

(defparameter *if-tuis* (list "T123" "T129" "T192" "T116" "T126" "T028" "T005"))

(defparameter *l-cui-ambig* nil
  "Key: CUI ambiguity - how many CUIs are there in this span, only consider those CUIs returned by umls-cover; Val: count")
(defparameter *l-tui-ambig* nil
  "Key: CUI ambiguity - how many TUIs are there in this span, only consider those TUIs returned by umls-cover; Val: count")
(defparameter *l-sp-lex-ambig* nil
  "Key: sp-lex ambiguity - how many sp-lex there are in this span, only consider
those sp-lex returned by sp-lex-covewr; val: count")

(defparameter *preferred-sem-types* 
  '()
  "For each stn in this list, all stns in its subtrees are also preferred.
TODO instantiate it.")

(defun output-l-dist (l)
  (setf l (sort l '> :key 'car))
  (dolist (a-cons l)
    (format t "~&~a: ~a~%" (car a-cons) (cdr a-cons))))

(defun umls-get-version ()
  (caar (umlssql "SELECT EXPL FROM MRDOC WHERE DOCKEY~a AND VALUE~a" 
		 (sql-matcher "release") (sql-matcher "umls.release.name"))))

(defun umlsizedp (doc)
  (member (format nil "umlsized-~a" (umls-get-version)) (analyses doc)
	  :test #'equalp))

(defun open-umls (&key (external-format nil))
  (unless (and *umls*
	       (dbi.mysql:mysql-connected *umls*))
    (setq *umls* (open-mysql-db "UMLS_DB" :external-format external-format))
    (setq *umls-has-pname*
	  (member "pname" (dbi.mysql:sql "show tables" :db *umls*)
		  :key #'car
		  :test #'string-equal)))  
  *umls*)

(defun close-umls ()
  (when *umls*
    (format t "~%... closing MySQL connection to UMLS database.")
    (ignore-errors (dbi.mysql:disconnect :db *umls*))
    (setq *umls* nil)))

;;;;;;;;;;;;;;;;;;;; 
;;; A note on looking up things in UMLS.  UMLS provides three
;;; different index tables that permit a user to find CUIs (concept
;;; unique identiiers) from an English text string.  The simplest is
;;; MRXW_ENG, which indexes every word in every string from MRCONSO
;;; and thus tells us which concepts' definitions that word appears
;;; in. Next is MRXNW_ENG, which is similar except that each word is
;;; transformed by the NORM program into its normalized form. Thus,
;;; retrieving "running" from MRXNW_ENG will also find concepts whose
;;; definitions include "ran", because both normalize to "run", among
;;; other terms. Note that stop words are normalized to
;;; nothing. Finally, for phrases, UMLS also normalizes these and
;;; indexes the normalized form. Normalization in this case involves
;;; not only the normalization of each word, but the elimination of
;;; stop-words and putting all the normalized words into alphabetical
;;; order, thus eliminating word order in the phrase.
;;;
;;; Many of the UMLS tables also have a CVF (content view flag) that
;;; identifies subsets of UMLS particularly appropriate to specific
;;; purposes. As of June 2009, only three such flags have been
;;; defined, as given below.
;;;;;;;;;;;;;;;;;;;;

(defparameter *cvf-nlp* 256
  "The CVF flag value corresponding to 'useful for NLP', as determined
  by the developers of MetaMap.")
(defparameter *kp-va* 512
  "The CVF flag value indicating that this concept belongs to the
  Veterans Health Administration and Kaiser Permanente Problem List
  Subset, a controlled terminology for use in the HL7/FDA 'Structured
  Product Label' data. The 512 flag indicates only concepts from those
  vocabularies that are available via UMLS without further
  licensing. See *kp-va+MeDRA*.  Also
  http://www.nlm.nih.gov/research/umls/Snomed/snomed_problem_list.html")
(defparameter *kp-va+MeDRA* 1024
  "A superset of concepts marked by the *kp-va* CVF flag, that
  includes MeDRA concepts. These require additional license terms to
  use.")

(defun umlssql1 (template &rest args)
  "Returns a list of the first columns of the requested rows. This is
  particularly useful when we retrieve just a single column. Note that
  it does not mean we return a single row."
  (mapcar #'car (apply #'umlssql template args)))

(defun umlssql (template &rest args)
  (open-umls)
  (let ((result
	 (catch 'mysql-retry
	   ;; This catch will be thrown to within the error handler for the
	   ;; mysql-protocol-error that is signalled when an apparently
	   ;; connected mysql connection has actually lost its mysql
	   ;; end. The handler tries to re-open the database, whose Lisp
	   ;; status has changed as a result of the error, and then throws
	   ;; :retry in order to retry the call.  If no error occurs, the catch
	   ;; simply receives the answer, 
	   (handler-bind ((dbi.mysql:mysql-protocol-error
			   #'(lambda (condition)
			       (declare (ignore condition))
			       (close-umls)
			       (when (open-umls)
				 (format t "~&Warning: umls re-opened on mysql protocol error~%")
				 (throw 'mysql-retry :retry))))
			  (excl:socket-error
			   #'(lambda (condition)
			       (declare (ignore condition))
			       (close-umls)
			       (when (open-umls)
				 (format t "~&Warning: umls re-opened on socket error~%")
				 (throw 'mysql-retry :retry)))))
	     (dbi.mysql:sql (apply #'format nil template args)
			    :db *umls*)))))
    (if (eq result :retry)
	(apply #'umlssql template args)
      result)))

(defun adj-agr->ptb-pos (agr &aux pos)
  (cond
   ((member agr (list "positive" "positive;periph") :test #'equalp)
    (setf pos "JJ"))
   ((equalp agr "comparative")
    (setf pos "JJR"))
   ((equalp agr "superlative")
    (setf pos "JJS")))
  pos)

(defun adv-agr->ptb-pos (agr &aux pos)
  (cond
   ((member agr (list "positive" "positive;periph") :test #'equalp)
    (setf pos "RB"))
   ((equalp agr "comparative")
    (setf pos "RBR"))
   ((equalp agr "superlative")
    (setf pos "RBS")))
  pos)

(defun verb-aux-agr->ptb-pos (agr &aux pos)
  "Handles conversion for both verbs and auxiliary verbs."
  (cond
   ;; Verb, non-3rd person singular present
   ((member agr (list "pres(fst_sing)" "pres(fst_plur,second,thr_plur)" "pres(fst_plur,second,thr_plur):negative" "pres(fst_sing,fst_plur,second,thr_plur)" "pres(fst_sing,fst_plur,second,thr_plur):negative" "pres(fst_sing,fst_plur,thr_plur,second)") :test #'equalp)
    (setf pos "VBP"))
   ;; Verb, base form
   ((member agr (list "infinitive") :test #'equalp)
    (setf pos "VB"))
   ;; Verb, past participle
   ((member agr (list "past_part") :test #'equalp)
    (setf pos "VBN"))
   ;; Verb, gerund or present participle
   ((member agr (list "pres_part") :test #'equalp)
    (setf pos "VBG"))
   ;; Verb, 3rd person singular present
   ((member agr (list "pres(thr_sing)" "pres(thr_sing):negative") :test #'equalp)
    (setf pos "VBZ"))
   ;; Verb, past tense
   ((member agr (list "past(fst_sing,thr_sing)" "past(fst_sing,thr_sing):negative" "past(fst_plur,second,thr_plur)" "past(fst_plur,second,thr_plur):negative" "past" "past:negative") :test #'equalp)
    (setf pos "VBD")))
  pos)

(defun conj-agr->ptb-pos (str &aux pos)
  "This is manually clasified by reading tags.tagdict from Opennlp and sca_agr.csv from UMLS."
  (cond
   ((member str (list "and/or" "and" "nor" "or" "yet" "therefore") :test #'equalp)
    (setf pos "CC"))
   ((member str (list "according as" "as if" "as though" "in as much as" "inasmuch as" "provided that" "providing that" "what with" "insofar as" "insomuch as" "forasmuch as" "e.g." "after" "albeit" "although" "as" "before" "by" "except" "for" "if" "i.e." "lest" "like" "notwithstanding" "once" "only" "provided" "providing" "since" "suppose" "supposing" "than" "though" "unless" "until" "versus" "when" "whenever" "where" "whereas" "wherein" "wherever" "while" "whilst" "with" "without" "whereafter" "wherefore" "whereof" "whereupon" "v." "vs." "whereat") :test #'equalp)
    (setf pos "IN"))
   ((member str (list "as well as" "rather than" "because" "but" "so" "whether") :test #'equalp)
    (setf pos (list "CC" "IN"))))
  pos)

(defun noun-agr->ptb-pos (agr &aux pos)
  (cond
   ((member agr (list "count(thr_plur)" "count(fst_plur)") :test #'equalp)
    (setf pos "NNS"))
   (t
    (setf pos "NN")))
  pos)

(defun sca-agr->ptb-pos (str sca agr &aux pos)
  "See sca_agr.csv that is obtained by
SELECT DISTINCT sca, agr FROM lragr ORDER BY sca;"
  (cond 
   ((equalp "adj" sca)
    (setf pos (adj-agr->ptb-pos agr)))
   ((equalp "adv" sca)
    (setf pos (adv-agr->ptb-pos agr)))
   ((equalp "aux" sca)
    (setf pos (verb-aux-agr->ptb-pos agr)))
   ;; (equalp "compl" sca), there is only one "that", do nothing
   ((equalp "conj" sca)
    (setf pos (conj-agr->ptb-pos str)))
   ((equalp "det" sca)
    (setf pos "DT"))
   ((equalp "modal" sca)
    (setf pos "MD"))
   ((equalp "noun" sca)
    (setf pos (noun-agr->ptb-pos agr)))
   ((equalp "prep" sca)
    (setf pos "IN"))
   ;; (equalp "pron" sca), no need?
   ((equalp "verb" sca)
    (setf pos (verb-aux-agr->ptb-pos agr))))
  pos)

;; build index on MRXNS_ENG.CUI;
;; CREATE INDEX X_MRXNS_ENG_CUI ON MRXNS_ENG(CUI);
(defmemo norm->cui-old (normalized &key (cvf *cvf-nlp*))
  "Retrieves matching CUIs from a normalized text string or list of strings."
  (format t "~&norm->cui-old~%")
  (let* (res time-start time-elapsed)
    (setf time-start (get-internal-real-time))
    (open-umls)
    (if (null cvf)
	;; Get all CUI's, independent of CVF flag.
	(setf res (umlssql1 "select distinct CUI from MRXNS_ENG ~
                           where NSTR~a order by CUI"
			    (sql-matcher normalized)))
      (setf res (umlssql1
		 "select distinct C.CUI from MRCONSO C ~
                join MRXNS_ENG S on C.CUI=S.CUI ~
                where NSTR~a and C.CVF & ~a order by C.CUI"
		 (sql-matcher normalized) cvf)))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *norm->cui-time* time-elapsed)
    (incf *norm->cui-cnt*)
    res))

(defun cmdstr (&rest args)
  (when args
    (apply #'concatenate 'string args)))

(defparameter *h-mrconso-mrxns* (make-hash-table :test #'equalp))

(defun load-mrconso-mrxns (&key 
			   (in-fn "late:;umls_cache;mrconso_mrxns_inexact"))
  (when (= 0 (hash-table-count *h-mrconso-mrxns*))
    (format t "~&loading mrconso_mrxns_exact~%")
    (let* (ln time-start time-elapsed)
      (setf time-start (get-internal-real-time))
      (with-open-file (in-f in-fn :direction :input :external-format :utf-8)
		      (loop (unless (setf ln (read-line in-f nil nil)) (return))
			    (destructuring-bind (nstr cui cntaui)
				(split-re "\\|" ln)
			      (pushnew (list cui (parse-integer cntaui)) 
				       (gethash nstr *h-mrconso-mrxns*) 
				       :test #'equalp))))
      (setf time-elapsed (- (get-internal-real-time) time-start))
      (format t "~&load mrconso mrxns time: ~f secs~%" (/ time-elapsed 1000)))))

(defparameter *h-mrconso-mesh* (make-hash-table :test #'equalp))
(defparameter *h-mrconso-wind* (make-hash-table :test #'equalp)
  "Word index table for mrconso entries")

(defun dump-mrconso-wind (&key (out-fn "late:;umls_cache;mrconso_wind"))
  (with-open-file (out-f out-fn :direction :output :external-format :utf-8
			 :if-exists :supersede :if-does-not-exist :create)
		  (format out-f "~s" (hash-table-alist *h-mrconso-wind*))))

(defun create-mrconso-wind (&key (in-fn "late:;umls_cache;mrconso_mesh")
				 (out-fn "late:;umls_cache;mrconso_wind"))
  (format t "~&creating mrconso_wind~%")
  (let* (ln time-start time-elapsed wds)
    (setf time-start (get-internal-real-time))
    (with-open-file (in-f in-fn :direction :input :external-format :utf-8)
		    (loop (unless (setf ln (read-line in-f nil nil)) (return))
			  (destructuring-bind (str cui cntaui)
			      (split-re "\\|" ln)
			    (declare (ignorable cui cntaui))
			    (cond 
			     ;; almost all brackets bearing entries are chemical names
			     ((match-re "[()]" str)
			      (setf wds nil))
			     (t
			      (setf wds (split-re "\\s+" str))))
			    (dolist (wd wds)
			      (unless (h-in-stoplist? wd)
				(pushnew str (gethash wd *h-mrconso-wind*) :test #'equalp))))))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (format t "~&size of *h-mrconso-wind*: ~a~%" 
	    (hash-table-count *h-mrconso-wind*))
    (format t "~&creating mrconso_wind time: ~f secs" 
	    (/ time-elapsed 1000)))
  (dump-mrconso-wind :out-fn out-fn))

(defun load-mrconso-wind (&key (in-fn "late:;umls_cache;mrconso_wind"))
  (when (= 0 (hash-table-count *h-mrconso-wind*))
    (format t "~&loading mrconso_wind~%")
    (let* ((time-start (get-internal-real-time))
	   (alist (read-from-string (read-file in-fn)))
	   time-elapsed)
      (setf *h-mrconso-wind* (alist-hash-table alist))
      (setf time-elapsed (- (get-internal-real-time) time-start))
      (format t "~&load mrconso_wind time: ~f secs~%" (/ time-elapsed 1000)))))

(defun load-mrconso-mesh (&key (in-fn "late:;umls_cache;mrconso_mesh"))
  (when (= 0 (hash-table-count *h-mrconso-mesh*))
    (format t "~&loading mrconso_mesh~%")
    (let* (ln time-start time-elapsed)
      (setf time-start (get-internal-real-time))
      (with-open-file (in-f in-fn :direction :input :external-format :utf-8)
		      (loop (unless (setf ln (read-line in-f nil nil)) (return))
			    (destructuring-bind (str cui cntaui)
				(split-re "\\|" ln)
			      (pushnew (list cui (parse-integer cntaui)) 
				       (gethash str *h-mrconso-mesh*) 
				       :test #'equalp))))
      (setf time-elapsed (- (get-internal-real-time) time-start))
      (format t "~&load mrconso_mesh time: ~f secs~%" (/ time-elapsed 1000)))))

(defun h-infix-search-mrconso-mesh (str)
  (let* (idx-strs ans intersect)
    (load-mrconso-wind)
    (load-mrconso-mesh)
    (dolist (wd (split-re "\\s+" str))
      (unless (h-in-stoplist? wd)
	(cond
	 (intersect
	  (setf idx-strs (intersection idx-strs (gethash wd *h-mrconso-wind*) 
				       :test #'equalp)))
	 (t
	  (setf idx-strs (gethash wd *h-mrconso-wind*))))))

    (dolist (idx-str idx-strs)
      (when (search str idx-str :test #'equalp)
	(dolist (cui (gethash idx-str *h-mrconso-mesh*))
	  (pushnew cui ans :test #'equalp))))
    ans))

(defun infix-search-mrconso-nlp (str)
  (let* ((cmd (cmdstr "select cui, count(*) from "
		      "(select * from mrconso where str like '%~a%') as T "
		      "where str regexp '(^|[^a-x])~a([^a-x]|$)' and cvf & ~a "
		      "group by cui")))
    (umlssql cmd str str *cvf-nlp*)))

(defun hnorm->cui-mesh-filter (normalized)
  "Retrieves matching CUIs from a normalized text string or list of strings.
Interestingly I need to add the following indexes to tables:
create index x_mrconso_cvf on mrconso (cvf);
create index x_mrxns_eng_cui on mrxns_eng (cui);"
  (format t "~&hnorm->cui-mesh-filter~%")
  (unless normalized
    (return-from hnorm->cui-mesh-filter nil))
  (load-mrconso-mrxns)
  (let* (res time-start time-elapsed max-cnt)
    (setf time-start (get-internal-real-time))
    ;; copy is to prevent the following from operating in place.
    (setf res (copy-list (gethash normalized *h-mrconso-mrxns*)))
    (setf res (mapcar #'(lambda (a) 
			  (nconc a (cons (if (cui->mesh (car a)) 1 0) nil))) 
		      res))
    (setf res (remove-if #'(lambda (x) (< (third x) 1)) res))
    
    (unless res
      (setf res (copy-list (h-infix-search-mrconso-mesh normalized)))
      (setf res (mapcar #'(lambda (a) 
			    (nconc a (cons (if (cui->mesh (car a)) 1 0) nil))) 
			res))
      (setf res (remove-if #'(lambda (x) (< (third x) 1)) res)))
    
    (unless res
      (setf res (copy-list (h-infix-search-mrconso-mesh normalized)))
      (setf res (mapcar #'(lambda (a) 
			    (nconc a (cons (if (cui->mesh-supp (car a)) 1 0) 
					   nil))) 
			res))
      (setf res (remove-if #'(lambda (x) (< (third x) 1)) res)))
    ;; sort on count
    ;; (format t "~&res: ~a~%" res)
    (setf res (stable-sort res #'> :key #'second))
    
    ;; (format t "norm: ~a; res: ~a~%" normalized res)
    (setf max-cnt (second (car res)))
    (setf res (remove-if #'(lambda (x) (< (second x) max-cnt)) res))
    (setf res (mapcar #'first res))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *norm->cui-time* time-elapsed)
    (incf *norm->cui-cnt*)
    res))


(defun hnorm->cui (normalized)
  "Retrieves matching CUIs from a normalized text string or list of strings.
Interestingly I need to add the following indexes to tables:
create index x_mrconso_cvf on mrconso (cvf);
create index x_mrxns_eng_cui on mrxns_eng (cui);"
  (format t "~&hnorm->cui~%")
  (unless normalized
    (return-from hnorm->cui nil))
  (load-mrconso-mrxns)
  (let* (res time-start time-elapsed max-cnt)
    (setf time-start (get-internal-real-time))
    ;; copy is to prevent the following from operating in place.
    (setf res (copy-list (gethash normalized *h-mrconso-mrxns*)))
    
    ;; sort on count
    ;; (format t "~&res: ~a~%" res)
    (setf res (stable-sort res #'> :key #'second))
    
    ;; (format t "norm: ~a; res: ~a~%" normalized res)
    (setf max-cnt (second (car res)))
    (setf res (remove-if #'(lambda (x) (< (second x) max-cnt)) res))
    (setf res (mapcar #'first res))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *norm->cui-time* time-elapsed)
    (incf *norm->cui-cnt*)
    res))

(defun enable-umls (sen ann-st ann-en)
  (let* ((st (start ann-st))
	 (en (end ann-en))
	 (sen-st (- st (start sen)))
	 (sen-en (- en (start sen)))
	 (str (subseq (content sen) sen-st sen-en))
	 (nstr (medg-norm str))
	 (postags (annotations-spec sen :type (gtype 'gtag-type)
				    :filter #'(lambda (a) 
						(<= st (start a) (end a) en))))
	 (first-postag (car postags))
	 (last-postag (car (last postags))))
    (not (or (null nstr)
	     (match-re "^\\W*$" (content ann-st))
	     (match-re "^\\W*$" (content ann-en))
	     (< (length nstr) 2)
	     (search (content first-postag) *punc* :test #'equalp)
	     (search (content last-postag) *punc* :test #'equalp)
	     ;; TODO: use sp-lex for more part of speech
	     (and (not (match-re "in situ" str :case-fold t))
		  (some #'ptb-prepp postags))
	     
	     (and (not (match-re "in situ" str :case-fold t))
		  (ptb-functionp first-postag))
	     (and (> (length postags) 1) 
		  (ptb-adjp last-postag)
		  (not (match-re "(\\+|-|\\wbright|\\wdim|\\wvariable)$" str)))
	     (ptb-advp last-postag)
	     (ptb-verbp last-postag) 
	     (ptb-functionp last-postag)
	     (match-re "^no(\\s|$)" nstr :case-fold t)
	     (member nstr '("a" "an" "the" "thi" "that" "these" "those" "be") 
		     :test #'equalp)))))

(defmemo cached-norm->cui (normalized)
  "Retrieves matching CUIs from a normalized text string or list of strings.
Interestingly I need to add the following indexes to tables:
create index x_mrconso_cvf on mrconso (cvf);
create index x_mrxns_eng_cui on mrxns_eng (cui);"
  (format t "~&cached-norm->cui~%")
  (unless normalized
    (return-from cached-norm->cui nil))
  
  
  
  (let* (res time-start time-elapsed max-cnt)
    (setf time-start (get-internal-real-time))
    ;; copy is to prevent the following from operating in place.
    (setf res (umlssql "SELECT CUI, CNT FROM mrconso_mrxns_cui WHERE str=~a"
		       (sq normalized)))
    
    ;; sort on count
    ;; (format t "~&res: ~a~%" res)
    (setf res (stable-sort res #'> :key #'second))
    
    ;; (format t "norm: ~a; res: ~a~%" normalized res)
    (setf max-cnt (second (car res)))
    (setf res (remove-if #'(lambda (x) (< (second x) max-cnt)) res))
    (setf res (mapcar #'first res))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *norm->cui-time* time-elapsed)
    (incf *norm->cui-cnt*)
    res))

(defparameter *h-nominalizer* (make-hash-table :test #'equalp))

(defun load-nominalizer (&key (in-fn "late:;umls_cache;nominalizer_medg"))
  (when (= 0 (hash-table-count *h-nominalizer*))
    (let* (ln time-start time-elapsed)
      (setf time-start (get-internal-real-time))
      (with-open-file (in-f in-fn :direction :input :external-format :utf-8)
		      (loop (unless (setf ln (read-line in-f nil nil)) (return))
			    (destructuring-bind (str pos noun)
				(split-re "\\|" ln)
			      (pushnew noun (gethash (cons str pos) *h-nominalizer*) 
				       :test #'equalp))))
      (setf time-elapsed (- (get-internal-real-time) time-start))
      (format t "load nominalizer time: ~f secs" (/ time-elapsed 1000)))))



(defun h-nominalize (str pos)
  "can I get more intelligent?"
  (load-nominalizer)
  ;; (setf strs (split-re "\\s+" str-in))
  (if (match-re "\\s+" str)
      (format t "~&warning: nominalize spaced string |~a|~%" str))
  (when (or (<= (length str) 3) ;; not after nominalization is a gene ...
	    (match-re "^no(\\s|$)" str :case-fold t))
    (return-from h-nominalize (safe-norm str)))

  (when (equalp pos "adj")
    (let* ((cuis (str->cui str)))
      (when cuis
	(return-from h-nominalize (replace-re (pname (car cuis)) "\\s+" "_")))))
  
  (let* ((nominals (copy-list (gethash (cons str pos) *h-nominalizer*)))
	 ostr cuis)
    (setf nominals (stable-sort nominals #'< :key #'length))
    (setf ostr (car nominals))
    ;; if has noun form
    (when ostr
      (setf cuis (str->cui ostr))
      ;; if no cui, use noun form
      (setf ostr (or (pname (car cuis)) ostr))
      (setf ostr (replace-re ostr "\\s+" "_")))
    ;; if no noun form, use original str
    (or ostr (replace-re (safe-norm str) "\\s+" "_"))))



(defmemo norm->cui (normalized &key (cvf *cvf-nlp*))
  "Retrieves matching CUIs from a normalized text string or list of strings.
Interestingly I need to add the following indexes to tables:
create index x_mrconso_cvf on mrconso (cvf);
create index x_mrxns_eng_cui on mrxns_eng (cui);"
  ;; (format t "~&norm->cui ~a~%" normalized)
  (let* (res time-start time-elapsed ; max-cnt
	     ;; command for exact matching string with cvf flag
	     (cmd-exact-cvf (cmdstr "select C.CUI, count(distinct AUI) as cntaui " 
				    "from MRCONSO C join MRXNS_ENG S " 
				    "on C.CUI=S.CUI and C.STR=S.NSTR "
				    "where NSTR~a and C.CVF & 256 " 
				    "group by C.CUI order by cntaui DESC"))
	     ;; command for exact matching string
	     (cmd-exact (cmdstr "select C.CUI, count(distinct AUI) as cntaui " 
				"from MRCONSO C join MRXNS_ENG S " 
				"on C.CUI=S.CUI and C.STR=S.NSTR "
				"where NSTR~a " 
				"group by C.CUI order by cntaui DESC"))
	     ;; command for inexact matching with cvf flag
	     (cmd-cvf (cmdstr "select C.CUI, count(distinct AUI) as cntaui " 
			      "from MRCONSO C join MRXNS_ENG S " 
			      "on C.CUI=S.CUI "
			      "where NSTR~a and C.CVF & 256 " 
			      "group by C.CUI order by cntaui DESC"))
	     ;; command for inexact matching
	     (cmd (cmdstr "select C.CUI, count(distinct AUI) as cntaui " 
			  "from MRCONSO C join MRXNS_ENG S " 
			  "on C.CUI=S.CUI "
			  "where NSTR~a " 
			  "group by C.CUI order by cntaui DESC")))
    (setf time-start (get-internal-real-time))
    (open-umls)
    (if (null cvf)
	;; Get all CUI's, independent of CVF flag.
	(setf res (or (umlssql cmd-exact (sql-matcher normalized))
		      (umlssql cmd (sql-matcher normalized))))
      (setf res (or (umlssql cmd-exact-cvf (sql-matcher normalized) cvf)
		    (umlssql cmd-cvf (sql-matcher normalized) cvf))))
    ;; (setf max-cnt (second (first res)))
    ;; (setf res (remove-if #'(lambda (x) (< (second x) max-cnt)) res))
    (setf res (mapcar #'car res))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *norm->cui-time* time-elapsed)
    (incf *norm->cui-cnt*)
    res))

(defmemo lragr-base (str)
  (car (umlssql1 "SELECT DISTINCT BAS FROM lragr WHERE STR=~a" (sq str))))

(defmemo str->cui (str &key (cvf nil))
  "Another config: cvf = *cvf-nlp*
In order to use this function, you need to augment lragr as follow:
alter table MRCONSO add RSTR TEXT;
update MRCONSO set rstr = replace(str, '-', ''); 
update MRCONSO set rstr = replace(rstr, '.', '');
alter table MRCONSO add index MRCONSO_RSTR (RSTR(255));"
  
  (let*  ((old-str str)
	  ans max-cnt)
    (open-umls)
    (setf str (replace-re str "\\s+" " "))
    (setf str (replace-re str "[.-]" "")) ;; \\-
    (setf str (replace-re str "(-|\\+|/|bright|dim|variable)+$" ""))
    ;; (lragr-base str)
    (setf ans (umlssql "SELECT CUI, COUNT(*) FROM MRCONSO WHERE RSTR~a ~@[and CVF & ~a~] GROUP BY CUI ORDER BY COUNT(*) DESC"
		       (sql-matcher str) cvf))
    (setf max-cnt (second (first ans)))
    (setf ans (remove-if #'(lambda (x) (< (second x) max-cnt)) ans))
    (setf ans (mapcar #'car ans))
    (when (and (not ans)
	       (match-re "(?i)^[a-z()\\-\\s]+$" old-str))
      (setf ans (norm->cui (norm:norm old-str))))
    
    (cond
     ;; lambda
     ((match-re "(?i)^ig[a-z]?[\\-\\s]+lambda$" old-str)
      (setf ans '("C0021037")))
     ((equalp "lambda" old-str)
      (setf ans '("C0021037")))
     ;; kappa
     ((match-re "(?i)^ig[a-z]?[\\-\\s]+kappa$" old-str)
      (setf ans '("C0021036")))
     ((equalp "kappa" old-str)
      (setf ans '("C0021036")))
     ((equalp "fdc" old-str)
      (setf ans '("C0242245"))))
    ans))

(defun load-immunologic-factor-list (fn)
  "Load the immunologic factor list from file."
  (let* ((h-ip (make-hash-table :test #'equalp))
	 ln)
    (with-open-file (f fn :direction :input)
		    (loop (unless (setf ln (read-line f nil nil)) (return))
			  (setf ln (replace-re ln "(^\\s+|\\s+$)" ""))
			  (setf ln (safe-norm ln))
			  (setf (gethash ln h-ip) (norm->cui ln))))
    
    h-ip))

(defun imf-match (s1 s2)
  (cond 
   ((< (edit-dist s1 s2) 3)
    t)
   (t
    nil)))

(defun trivial-str (str)
  (or (match-re "^(and|,|\\.|;|:)" str :case-fold t)
      (match-re "(and|,|\\.|;|:)$" str :case-fold t)
      (match-re "(^[(.,);]|[(.,);]$)" str)))

(defmemo immunologic-tui (str cuis)
  (when (trivial-str str)
    (return-from immunologic-tui nil))
  (let* ((h-imf (load-immunologic-factor-list "late:;umls_cache;immunologic_factor"))
	 (l-imf (hash-keys h-imf))
	 (tuis (cui->tui cuis)))
    (setf str (safe-norm str))
    (cond
     ((member str l-imf :test #'equalp)
      '("T129"))
     ((match-re "^CD\\d+[a-z]*[^\\sa-z]*$" str :case-fold t)
      '("T129"))
     ((intersection tuis '("T192" "T116" "T124") :test #'equalp)
      '("T129"))
     (t
      nil))))

(defun regex-escape (str)
  "I stop this, just seem to always get into regex syntactic errors"
  (setf str (replace-re str "([()\\[\\]+?*|.])" "\\\\\\\\\\1")))

(defparameter *h-sp-lex* (make-hash-table :test #'equalp))

(defun load-sp-lex-full (&key (in-fn "late:;umls_cache;sp_lex"))
  (when (= 0 (hash-table-count *h-sp-lex*))
    (let* (ln time-start time-elapsed rstr-hash)
      (setf time-start (get-internal-real-time))
      (with-open-file
       (in-f in-fn :direction :input :external-format :utf-8)
       (loop (unless (setf ln (read-line in-f nil nil)) (return))
	     (destructuring-bind (bas rstr sca)
		 (split-re "\\|" ln)
	       (setf rstr-hash (gethash rstr *h-sp-lex*))
	       (unless rstr-hash
		 (setf rstr-hash (make-hash-table :test #'equalp))
		 (setf (gethash rstr *h-sp-lex*) rstr-hash))
	       (pushnew bas (gethash sca rstr-hash) :test #'equalp))))
      (setf time-elapsed (- (get-internal-real-time) time-start))
      (format t "load full sp lex time: ~f secs" (/ time-elapsed 1000)))))


(defun load-sp-lex (&key (in-fn "late:;umls_cache;sp_lex"))
  (when (= 0 (hash-table-count *h-sp-lex*))
    (let* (ln time-start time-elapsed)
      (setf time-start (get-internal-real-time))
      (with-open-file
       (in-f in-fn :direction :input :external-format :utf-8)
       (loop (unless (setf ln (read-line in-f nil nil)) (return))
	     (destructuring-bind (bas rstr sca)
		 (split-re "\\|" ln)
	       ;; i don't care multiple ones at the moment
	       (pushnew bas (gethash (cons rstr sca) *h-sp-lex*) :test #'equalp))))
      (setf time-elapsed (- (get-internal-real-time) time-start))
      (format t "load sp lex time: ~f secs" (/ time-elapsed 1000)))))

(defun hstr->sp-lex (str sca)
  (load-sp-lex)
  (let* (sp-lexes time-start time-elapsed)
    (setf time-start (get-internal-real-time))
    (setf str (replace-re str "\\s+" " "))
    (setf str (replace-re str "-" " "))
    
    (setf sp-lexes (gethash (cons str sca) *h-sp-lex*))
    (if (> (length sp-lexes) 1)
	(format t "~&Warning: multiple sp-lex base for ~a~%" str))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *str->sp-lex-time* time-elapsed)
    (incf *str->sp-lex-cnt*)	
    sp-lexes))

(defmemo str->sp-lex (str &key (sca nil))
  "In order to use this function, you need to augment lragr as follow:
alter table lragr add RSTR varchar(255);
update lragr set rstr = replace(str, '-', ' '); 
alter table lragr add index LRAGR_RSTR (RSTR);"
  (let* ((cmd (cmdstr "SELECT DISTINCT BAS FROM lragr "
		      "WHERE RSTR=~a ~@[AND SCA='~a'~]"))
	 sp-lexes time-start time-elapsed)
    (setf time-start (get-internal-real-time))
    (setf str (replace-re str "\\s+" " "))
    (setf str (replace-re str "-" " "))
    (setf sp-lexes (umlssql1 cmd (sq str) sca))
    (if (> (length sp-lexes) 1)
	(format t "~&Warning: multiple sp-lex base for ~a~%" str))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *str->sp-lex-time* time-elapsed)
    (incf *str->sp-lex-cnt*)	
    sp-lexes))


(defmemo str->sp-lex2 (str &key (sca nil))
  (let* ((head-str (car (split-re "[\\s-]+" str)))
	 (rstr (replace-re (regex-escape str) "[ -]+" "[ -]+"))
	 (cmd (cmdstr "SELECT DISTINCT BAS FROM (SELECT * FROM lragr "
		      "WHERE STR LIKE '~a%' ~@[AND SCA='~a'~]) AS T "
		      "WHERE STR REGEXP '^~a$'"))
	 sp-lexes time-start time-elapsed)
    (setf time-start (get-internal-real-time))
    (open-umls)	
    (setf sp-lexes (umlssql1 cmd head-str sca rstr))
    (if (> (length sp-lexes) 1)
	(format t "~&Warning: multiple sp-lex base for ~a~%" str))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *str->sp-lex-time* time-elapsed)
    (incf *str->sp-lex-cnt*)
    sp-lexes))

(defmemo norm->cui2 (normalized &key (cvf *cvf-nlp*))
  "Retrieves matching CUIs from a normalized text string or list of strings."
  (format t "~&norm->cui2~%")
  (open-umls)
  (cond 
   ((listp normalized)
    (remove-duplicates (mapcan #'(lambda (x) (norm->cui x :cvf cvf)) 
			       normalized) :test #'equalp))
   ((stringp normalized)
    (umlssql1
     "select distinct C.CUI from MRCONSO C ~
      join MRXNS_ENG S on C.CUI=S.CUI ~
      where NSTR=~a~@[ and C.CVF & ~a~] order by C.CUI"
     (sq normalized) cvf))
   (t
    (error "Input ~s should either be a list or string" normalized))))

(defun s->cui (string &key (cvf *cvf-nlp*))
  "Retrieves matching CUIs from a text string. The input is run
  through the UMLS normalizer, which is the basis on which text
  appearing in MRCONSO definitions is indexed by UMLS."
  (norm->cui (norm string) :cvf cvf))

(defun w->cui (word &key (cvf *cvf-nlp*))
  "Retrieves matching CUIs from a single-word text string, using
  MRXW. Note that this gets all concepts whose names include the word,
  and thus usually generates a long list.  s->cui will generally fetch
  a much smaller number of concepts because all the non-noise words in
  the string must occur in the normalized concept name."
  (umlssql1
   "select distinct C.CUI from MRCONSO C join MRXW_ENG W on C.CUI=W.CUI where W.WD=~a~@[ and C.CVF & ~a~]"
   (sq word) cvf))

(defun wn->cui (word &key (cvf *cvf-nlp*))
  "Retrieves matching CUIs from a single-word text string after
  normalizing it, using MRXNS. Normalization can yield multiple words.
  Note that for multi-word phrases, you want to be using s->cui. There
  may be some indexing oddities: for example, wn->cui retrieves 768
  concepts for 'infarction', but w->cui gets 767, 'bilateral occipital
  infarctions', presumably because of the plural."
  (umlssql1
   "select distinct C.CUI from MRCONSO C join MRXNW_ENG W on C.CUI=W.CUI where W.NWD~a~@[ and C.CVF & ~a~]"
   (sql-matcher (norm word)) cvf))

#|
Most CUIs have zero or one associated MeSH term, but the frequency of
multiple MeSH terms goes down by about powers of 2:
mysql> select x.c,count(*) from (select cui,count(atv) c from MRSAT where sab='MSH' and atn='MN' group by cui) x group by x.c;
+----+----------+
| c  | count(*) |
+----+----------+
|  1 |    12544 | 
|  2 |     6609 | 
|  3 |     3291 | 
|  4 |     1471 | 
|  5 |      652 | 
|  6 |      343 | 
|  7 |      157 | 
|  8 |       74 | 
|  9 |       55 | 
| 10 |       39 | 
| 11 |       11 | 
| 12 |       10 | 
| 13 |        5 | 
| 14 |        4 | 
| 17 |        1 | 
| 19 |        1 | 
+----+----------+
Conversely, every MeSH atv has only a single CUI.
|#
;;; should we use UI's in MSH to join MRSAT and MRCONSO?
(defmemo cui->mesh (cui)
  "Returns a list of MeSH hierarchic codes for the CUI or list of CUIs given.
yluo: I have added supplementary concepts, 10/22/2012, this gives broader 
coverage than cui->mesh2, see C2933271"
  (let* ((cmd (cmdstr "select distinct atv from MRSAT "
		      "where sab='MSH' and atn='MN' and cui~a order by atv"))
	 res time-start time-elapsed)
    (setf time-start (get-internal-real-time))
    (setf res (umlssql1 cmd (sql-matcher cui)))
    (setf res (remove-if-not #'h-mesh-heading res))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *cui->mesh-time* time-elapsed)
    (incf *cui->mesh-cnt*)
    res))

(defmemo cui->mesh-supp (cui)
  "Returns a list of MeSH hierarchic codes for the CUI or list of CUIs given.
yluo: I have added supplementary concepts, 10/22/2012, this gives broader 
coverage than cui->mesh2, see C2933271"
  (let* ((cmd (cmdstr "select distinct atv from MRSAT "
		      "where sab='MSH' and atn='MN' and cui~a order by atv"))
	 ;; was using rela='mapped_from', but for 
	 (cmd2 (cmdstr "select cui2 from mrrel "
		       "where cui1~a and sab='MSH'"))
	 res time-start time-elapsed cui2)
    (setf time-start (get-internal-real-time))
    ;; search as supplementary concepts
    (setf cui2 (umlssql1 cmd2 (sql-matcher cui)))
    (setf res (umlssql1 cmd (sql-matcher cui2)))
    (setf res (remove-if-not #'h-mesh-heading res))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *cui->mesh-time* time-elapsed)
    (incf *cui->mesh-cnt*)
    res))

(defmemo scui->mesh (cui)
  "Returns a list of MeSH hierarchic codes for the single CUI supplied."
  (let* (code)
    ;; assume a CUI has only one CODE in MSH, should be true by design
    (setf code (car (umlssql1 "SELECT CODE FROM MRCONSO 
                                 WHERE CUI=~a AND SAB='MSH' LIMIT 1" 
			      (sq cui))))
    (umlssql1
     "SELECT DISTINCT ATV FROM MRSAT 
        WHERE SAB='MSH' AND ATN='MN' AND CODE=~a
        ORDER BY ATV"
     (sq code))))

(defmemo mcui->mesh (cuis)
  "Returns a list of MeSH Tree Numbers for a list of CUIs"
  (let* (codes)
    (setf codes (mapcar #'(lambda (cui) 
			    (car 
			     (umlssql1
			      "SELECT CODE FROM MRCONSO
                                 WHERE CUI=~a AND SAB='MSH' LIMIT 1"
			      (sq cui))))
			cuis))
    (umlssql1
     "SELECT DISTINCT ATV FROM MRSAT
        WHERE SAB='MSH' AND ATN='MN' AND CODE~a
        ORDER BY ATV"
     (sql-matcher codes))
    ))

(defmethod cui->code->mesh ((cui string))
  (scui->mesh cui))

(defmethod cui->code->mesh ((cuis list))
  (mcui->mesh cuis))

(defmemo cui->mesh2 (cui-or-list-of-cuis)
  "An alternative to cui->mesh sugguested by Yuan Luo.  Instead of
  going directly from the CUI to a MeSH term, it does so indirectly
  through the CODE, which seems to give broader coverage.  I'm not
  convinced it's semantically correct, however.  Note: to make this
  efficient, one must index MRSAT on CODE."
  (umlssql1
   "select distinct atv from MRSAT s join (select code from MRCONSO where cui~a and sab='MSH') c on s.code=c.code
where s.sab='MSH' and s.atn='MN'"
   (sql-matcher cui-or-list-of-cuis)))

(defun mname (mesh)
  (umlssql1
   "select distinct str from MRCONSO c join MRSAT s on c.cui = s.cui where s.sab='MSH' and s.atn='MN' and s.atv~a and c.TS='P' and c.STT='PF' and c.ISPREF='Y' and c.LAT='~a'"
   (sql-matcher mesh)
   *umls-pref-lang*))

(defmemo cui->tui (cui)
  "Returns a list of UMLS Semantic Network TUIs for the concept(s)
  cui. Note that MRSTY is not normalized, and the TUI, STN and STY
  each convey identical information."
  (let* (res time-start time-elapsed)
    (setf time-start (get-internal-real-time))
    (setf res (and cui
		   (umlssql1
		    "select distinct tui from MRSTY where cui~a"
		    (sql-matcher cui))))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *cui->tui-time* time-elapsed)
    (incf *cui->tui-cnt*)
    res))



(defun create-pnames ()
  "Creates a pname table in the UMLS DB to make it more efficient to
  retrieve the preferred name of a concept."
  (open-umls)
  (unless (member "pname" (umlssql1 "show tables") :test #'equalp)
    (umlssql "create table pname select CUI,STR from MRCONSO where TS='P' and STT='PF' and ISPREF='Y' and LAT=~a"
	     (sq *umls-pref-lang*))
    (umlssql "create unique index pname_cui on pname(CUI)")
    (setq *umls-has-pname* t)))

(defun pname (item)
  "Retrieves the preferred name(s) of a CUI or list of CUIs. The
  result is a string if only one item is given, or a list of strings
  if multiple ones are given."
  (open-umls)
  (let ((res (cname item)))
    (cond 
     ((null res) nil)
     ((null (cdr res)) (cadar res))
     (t (mapcar #'cadr res)))))

(defun cname (item)
  "Retrieves the preferred name of a CUI or list of CUIs, and returns
  a list of the CUI and name for each."
  (umlssql (if *umls-has-pname*
	       "select CUI,STR from PNAME where CUI~a"
	     "select CUI,STR from MRCONSO where CUI~a and TS='P' and STT='PF' and ISPREF='Y' and LAT='~a'")
	   (sql-matcher item)
	   *umls-pref-lang*))

(defun cnames (item &key (cvf *cvf-nlp*))
  "Retrieves all the names of a CUI."
  (open-umls)
  (dbi.mysql:sql
   (format
    nil
    "select distinct CUI,STR from MRCONSO where CUI~a~@[ and CVF & ~a~]"
    (sql-matcher item) cvf)
   :db *umls*))

(defun tname (tui-or-list-of-tuis)
  "Returns the name of a TUI or names of a list of TUIs."
  (umlssql1 "select sty_rl from SRDEF where ui~a"
	    (sql-matcher
	     (if (listp tui-or-list-of-tuis)
		 tui-or-list-of-tuis
	       (list tui-or-list-of-tuis)))))


(defun rel (cui1 cui2)
  (umlssql "select * from MRREL where CUI1=~a and CUI2=~a"
	   (sq cui1) (sq cui2)))

(defmemo w->fn (word)
  "Looks in the Specialist Lexicon to see if the word has a syntactic
  category indicating a function word (closed class of words. These
  are prep, pron, conj, det, aux, modal and compl. Returns the list of
  lists of (EUI SCA) or nil."
  (umlssql "select distinct eui,sca from lragr where str=~a and sca in ('prep', 'pron', 'conj', 'det', 'aux', 'modal', 'compl')"
	   (sq word)))

(defmemo words->umls-fn->ptb (str)
  ;; select str, count(*) from lragr where str like '% %' and sca in ('prep', 'conj', 'det', 'modal') group by str; give sme single count
  (let* ((sca (car (umlssql1 "select distinct sca from lragr where str=~a and sca in ('prep', 'conj', 'det', 'modal')"
			     (sq str)))))
    (cond 
     ((equalp "prep" sca)
      "IN")
     ((equalp "model" sca)
      "MD")
     ((equalp "conj" sca)
      "CC")
     ((equalp "det" sca)
      "DT")
     ((equalp "rule out" str)
      "VB")
     (t
      nil))))

(defmemo umls-prep-list ()
  (umlssql1 "SELECT str FROM lragr WHERE sca='prep'"))

(defmemo umls-conj-list ()
  (umlssql1 "SELECT str FROM lragr WHERE sca='conj'"))

(defmemo w->sp-pos (word)
  "Looks up the parts of speech of word in the Specialist Lexicon."
  (let* (res time-start time-elapsed)
    (setf time-start (get-internal-real-time))
    (open-umls)
    (setf res (umlssql1 "select distinct sca from lragr where str=~a" (sq word)))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *w->sp-pos-time* time-elapsed)
    (incf *w->sp-pos-cnt*)
    res))

(defun hstr->sp-pos (str)
  "Looks up the parts of speech of str in the Specialist Lexicon."
  (let* (res time-start time-elapsed)
    (setf time-start (get-internal-real-time))
    (open-umls)
    (setf str (replace-re str "\\s+" " "))
    (setf str (replace-re str "-" " "))

    (setf res (and (gethash str *h-sp-lex*)
		   (hash-keys (gethash str *h-sp-lex*))))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *w->sp-pos-time* time-elapsed)
    (incf *w->sp-pos-cnt*)
    res))

(defmemo str->sp-pos (str)
  "Looks up the parts of speech of str in the Specialist Lexicon."
  (let* (res time-start time-elapsed)
    (setf time-start (get-internal-real-time))
    (open-umls)
    (setf str (replace-re str "\\s+" " "))
    (setf str (replace-re str "-" " "))

    (setf res (umlssql1 "select distinct sca from lragr where rstr=~a" 
			(sq str)))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *w->sp-pos-time* time-elapsed)
    (incf *w->sp-pos-cnt*)
    res))

(defmemo w-sca->sp-agr (word sca)
  "Looks up the agreement/inflection code given parts of speech of word in the
   Specialist Lexicon."
  (open-umls)
  (umlssql1 "SELECT DISTINCT agr FROM lragr WHERE str=~a AND sca=~a"
	    (sq word) (sq sca)))

(defmemo w-sca->sp-com (word sca)
  "Looks up the complement code given parts of speech of word in the Specialist
Lexicon"
  (umlssql1 "SELECT DISTINCT com FROM lrcmp WHERE bas=~a AND sca=~a"
	    (sq word) (sq sca)))

(defun opt-tranp (word &aux coms)
  "Predicate whether the word is optionally transitive and cannot form two word 
verb"
  (setf coms (w-sca->sp-com word "verb"))
  (and (some #'(lambda (c) (match-re "^intran" c)) coms)
       (some #'(lambda (c) (match-re "^tran=" c)) coms)
       ;; I've verified that part never starts in the beginning of the com
       (notany #'(lambda (c) (match-re ";part\\(" c)) coms)))

(defun opt-tran-partp (word &aux coms)
  "Predicate whether the word is optionally transitive and may form two-word 
verbs."
  (setf coms (w-sca->sp-com word "verb"))
  (and (some #'(lambda (c) (and (match-re "^intran" c) (match-re ";part\\(" c)))
	     coms)
       (some #'(lambda (c) (and (match-re "^tran=" c) (match-re ";part\\(" c)))
	     coms)))

(defun tranp (word &aux coms)
  "Is it a transitive word?"
  (setf coms (w-sca->sp-com word "verb"))
  (and (notany #'(lambda (c) (match-re "^intran" c)) coms)
       (some #'(lambda (c) (match-re "^tran=" c)) coms)
       ;; I've verified that part never starts in the beginning of the com
       (notany #'(lambda (c) (match-re ";part\\(" c)) coms)))

(defun tran-partp (word &aux coms)
  "Is it a transitive word that may form a two-word verb?"
  (setf coms (w-sca->sp-com word "verb"))
  (and (notany #'(lambda (c) (match-re "^intran" c)) coms)
       (some #'(lambda (c) (and (match-re "^tran=" c) (match-re ";part\\(" c)))
	     coms)))

(defun intranp (word &aux coms)
  "Is it an intransitive word?"
  (setf coms (w-sca->sp-com word "verb"))
  (and (some #'(lambda (c) (match-re "^intran" c)) coms)
       (notany #'(lambda (c) (match-re "^tran=" c)) coms)
       ;; I've verified that part never starts in the beginning of the com
       (notany #'(lambda (c) (match-re ";part\\(" c)) coms)))

(defun intran-partp (word &aux coms)
  "Is it an intransitive word that may form a two-word verb?"
  (setf coms (w-sca->sp-com word "verb"))
  (and (notany #'(lambda (c) (match-re "^tran" c)) coms)
       (some #'(lambda (c) (and (match-re "^intran=" c)
				(match-re ";part\\(" c))) coms)))

(defparameter *sp-lex-fn-scas*
  '("prep" "pron" "conj" "det" "aux" "modal" "compl"))

(defun non-fn-pos (pos-list)
  (set-difference pos-list *sp-lex-fn-scas* :test #'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility to analyze lengths of MRCONSO strings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *lns* (make-hash-table))

(defun get-lengths (&aux row)
  (open-umls)
  (clrhash *lns*)
  (dbi.mysql:sql-start "select str from MRCONSO" :db *umls*)
  (loop
   (setq row (dbi.mysql:get-next-row :db *umls*))
   (when (null row) (return t))
   (let ((n (+ (count #\Space (car row))
	       (count #\- (car row))
	       (count #\, (car row)))))
     (incf (gethash n *lns* 0))))
  (let* ((kv nil))
    (maphash #'(lambda (k v) (push (cons k v) kv)) *lns*)
    (sort kv #'< :key #'car)))

#|
Number of times k spaces, commas or hyphens occurs in str in MRCONSO (2009aa):
((0 . 480257) (1 . 1016507) (2 . 927997) (3 . 891350) (4 . 751884)
 (5 . 576623) (6 . 436155) (7 . 332133) (8 . 252300) (9 . 183789)
 (10 . 136696) (11 . 98279) (12 . 73572) (13 . 52655) (14 . 40264)
 (15 . 34244) (16 . 30873) (17 . 31423) (18 . 33047) (19 . 29544) (20 . 27800)
 (21 . 23478) (22 . 15354) (23 . 10729) (24 . 7023) (25 . 4538) (26 . 3184)
 (27 . 2106) (28 . 1622) (29 . 1341) (30 . 1062) (31 . 947) (32 . 850)
 (33 . 607) (34 . 548) (35 . 395) (36 . 286) (37 . 314) (38 . 279) (39 . 269)
 (40 . 301) (41 . 244) (42 . 217) (43 . 172) (44 . 192) (45 . 159) (46 . 135)
 (47 . 175) (48 . 128) (49 . 135) (50 . 80) (51 . 80) (52 . 88) (53 . 85)
 (54 . 57) (55 . 68) (56 . 55) (57 . 54) (58 . 71) (59 . 61) (60 . 110)
 (61 . 63) (62 . 92) (63 . 105) (64 . 121) (65 . 90) (66 . 70) (67 . 53)
 (68 . 56) (69 . 58) (70 . 48) (71 . 70) (72 . 51) (73 . 30) (74 . 46)
 (75 . 46) (76 . 63) (77 . 51) (78 . 45) (79 . 49) (80 . 32) (81 . 60)
 (82 . 35) (83 . 34) (84 . 36) (85 . 40) (86 . 24) (87 . 37) (88 . 23)
 (89 . 17) (90 . 20) (91 . 28) (92 . 44) (93 . 23) (94 . 20) (95 . 9)
 (96 . 19) (97 . 18) (98 . 18) (99 . 8) (100 . 27) (101 . 12) (102 . 15)
 (103 . 4) (104 . 5) (105 . 8) (106 . 7) (107 . 9) (108 . 14) (109 . 20)
 (110 . 7) (111 . 14) (112 . 18) (113 . 22) (114 . 7) (115 . 13) (116 . 6)
 (117 . 15) (118 . 4) (119 . 2) (120 . 5) (121 . 6) (122 . 9) (123 . 4)
 (124 . 3) (125 . 1) (127 . 5) (128 . 4) (129 . 6) (130 . 4) (132 . 5)
 (133 . 3) (135 . 1) (136 . 2) (138 . 3) (139 . 4) (140 . 2) (141 . 3)
 (142 . 1) (143 . 2) (145 . 3) (146 . 9) (148 . 8) (149 . 1) (151 . 4)
 (152 . 5) (153 . 8) (155 . 3) (156 . 13) (157 . 4) (158 . 6) (159 . 4)
 (160 . 1) (163 . 1) (164 . 1) (165 . 1) (166 . 1) (169 . 4) (171 . 4)
 (174 . 1) (176 . 1) (178 . 1) (181 . 1) (183 . 1) (184 . 5) (185 . 2)
 (186 . 3) (268 . 1) (271 . 4))
					;;; If we count just spaces:
((0 . 638139) (1 . 1180549) (2 . 1060718) (3 . 840646) (4 . 678111)
 (5 . 532350) (6 . 396398) (7 . 296671) (8 . 216979) (9 . 154285)
 (10 . 104765) (11 . 73055) (12 . 52585) (13 . 40065) (14 . 29816)
 (15 . 28304) (16 . 32033) (17 . 29664) (18 . 28940) (19 . 30262) (20 . 24846)
 (21 . 16009) (22 . 9961) (23 . 5895) (24 . 3497) (25 . 2795) (26 . 1607)
 (27 . 860) (28 . 736) (29 . 546) (30 . 408) (31 . 413) (32 . 424) (33 . 316)
 (34 . 193) (35 . 142) (36 . 174) (37 . 140) (38 . 256) (39 . 178) (40 . 111)
 (41 . 90) (42 . 83) (43 . 99) (44 . 113) (45 . 65) (46 . 109) (47 . 120)
 (48 . 88) (49 . 64) (50 . 59) (51 . 42) (52 . 50) (53 . 64) (54 . 84)
 (55 . 51) (56 . 44) (57 . 80) (58 . 60) (59 . 30) (60 . 38) (61 . 62)
 (62 . 56) (63 . 70) (64 . 128) (65 . 64) (66 . 30) (67 . 47) (68 . 63)
 (69 . 51) (70 . 44) (71 . 52) (72 . 52) (73 . 31) (74 . 40) (75 . 67)
 (76 . 51) (77 . 60) (78 . 43) (79 . 27) (80 . 43) (81 . 49) (82 . 29)
 (83 . 27) (84 . 19) (85 . 24) (86 . 41) (87 . 16) (88 . 14) (89 . 15)
 (90 . 35) (91 . 39) (92 . 30) (93 . 12) (94 . 23) (95 . 11) (96 . 13)
 (97 . 13) (98 . 23) (99 . 19) (100 . 14) (101 . 12) (102 . 6) (103 . 4)
 (104 . 4) (105 . 4) (106 . 9) (107 . 5) (108 . 17) (109 . 22) (110 . 24)
 (111 . 12) (112 . 11) (113 . 10) (114 . 5) (115 . 12) (116 . 4) (117 . 2)
 (118 . 1) (119 . 4) (120 . 6) (121 . 10) (122 . 12) (123 . 6) (124 . 2)
 (125 . 10) (126 . 2) (127 . 5) (128 . 2) (129 . 3) (130 . 4) (131 . 3)
 (134 . 5) (135 . 5) (138 . 5) (139 . 10) (140 . 6) (141 . 2) (145 . 2)
 (146 . 5) (147 . 5) (154 . 4) (155 . 4) (156 . 7) (157 . 1) (158 . 2)
 (159 . 1) (161 . 1) (162 . 1) (163 . 2) (168 . 3) (169 . 5) (170 . 2)
 (178 . 1) (180 . 2) (181 . 2) (182 . 3) (201 . 1) (237 . 4))

The above shows that of the of the approximately 5M distinct
strings that give (alternative) names to the 2.1M distinct concepts in
the UMLS, the most common names consist of two or three tokens. The
fall-off is not very steep, as even 24-token strings occur over 10,000
times.
|#

(defparameter *type-map*
  '((_disease "Disease or Syndrome" "Fungus" "Injury or Poisoning"
	      "Anatomical Abnormality" "Congenital Abnormality"
	      "Mental or Behavioral Dysfunction" "Acquired Abnormality" 
	      "Hazardous or Poisonous Substance" "Neoplastic Process"
	      "Pathologic Function")
    (_medication "Pharmacologic Substance" "Organic Chemical"
		 "Antibiotic" "Clinical Drug" "Biomedical or Dental Material")
    (_procedure "Therapeutic or Preventive Procedure" "Medical Device")
    (_procparam "Laboratory Procedure" "Diagnostic Procedure")
    (_bodyloc "Body Location or Region"
	      "Body Part, Organ, or Organ Component" "Body System")
    (_symptom "Sign or Symptom" )
    (_finding "Finding" "Clinical Attribute"
	      "Cell or Molecular Dysfunction" "Virus")
    (_event-time "Temporal Concept" )
    (_modifier "Qualitative Concept" "Functional Concept"
	       "Spatial Concept" "Bacterium" "Quantitative Concept")
    (_caregivers "Professional or Occupational Group")
    (care-unit "Health Care Related Organization"
	       "Health Care Activity" "Patient or Disabled Group")
    (family "Family Group")
    (_bodyparam "Biologically Active Substance"
		"Organ or Tissue Function" "Laboratory or Test Result"
		"Organism Function" "Organism Attribute" "Cell")
    (nil "Geographic Area" "Intellectual Product") ;; ignore these types
    ))

(defparameter *tui-map*
  '((_disease "T047" "T004" "T037" "T190" "T019" "T048" "T020"
	      "T131" "T191" "T046")
    (_medication "T121" "T109" "T195" "T200" "T122")
    (_procedure "T061" "T074")
    (_procparam "T059" "T060")
    (_bodyloc "T029" "T023" "T022")
    (_symptom "T184")
    (_finding "T033" "T201" "T049" "T005")
    (_event-time "T079")
    (_modifier "T080" "T169" "T082" "T007" "T081")
    (_caregivers "T097")
    (care-unit "T093" "T058" "T101")
    (family "T099")
    (_bodyparam "T123" "T042" "T034" "T040" "T032" "T025")
    (nil "T083" "T170")))

(defparameter *tui-hash*
  (let ((h (make-hash-table :test #'equalp)))
    (dolist (l *tui-map*)
      (let ((type (car l)))
	(dolist (s (cdr l))
	  (setf (gethash s h) type))))
    h))

(defparameter *tui-stn-hash* (make-hash-table :test #'equalp))

(defun populate-tui-stn-hash (confirm?)
  (when confirm?
    (dolist (res (umlssql "SELECT DISTINCT STN_RTN, UI FROM SRDEF"))
      (let* ((stn (car res))
	     (tui (cadr res)))
	(setf (gethash tui *tui-stn-hash*) stn)))))

(defun tui->sem (tui)
  "Returns a list of the semantic categories, as defined by
  *tui-hash*, for the TUI or TUIs given as argument."
  (let ((ans nil)
	(tuis (if (consp tui) tui (list tui))))
    (dolist (x tuis)
      (let ((s (gethash x *tui-hash* nil)))
	(when s (pushnew s ans :test #'equal))))
    ans))

(defun tui->stn (tui)
  "Return STN corresponding to TUI."
  (cond 
   ((null tui)
    nil)
   ((typep tui 'tui-annotation)
    (let* ((stui (format nil "~a" (data tui))))
      (or (gethash stui *tui-stn-hash*)
	  (setf (gethash stui *tui-stn-hash*) 
		(car (umlssql1 "SELECT STN_RTN FROM SRDEF WHERE UI~a" 
			       (sql-matcher stui)))))))
   ((stringp tui)
    (or (gethash tui *tui-stn-hash*)
	(setf (gethash tui *tui-stn-hash*) 
	      (car (umlssql1 "SELECT STN_RTN FROM SRDEF WHERE UI~a" 
			     (sql-matcher tui))))))
   (t
    (error "expecting nil or tui or string in tui->stn while got ~a" tui))
   ))

(defmemo stn->sty-rl (stn)
  "Converts semantic tree number to semantic type or relation name"
  (car (umlssql1 "SELECT STY_RL FROM SRDEF WHERE STN_RTN=~a" (sq stn))))

(defmemo sty-rl->stn (sty-rl)
  "Converts semantic tree number to semantic type or relation name"
  (car (umlssql1 "SELECT STN_RTN FROM SRDEF WHERE STY_RL=~a" (sq sty-rl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Analyze sentence for occurrence of UMLS concepts.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *default-max-concept-length* 10
  "Maximum number of tokens to consider in a sentence when looking up
  whether a succession of tokens might name a UMLS concept. Note the
  result above, which shows the distribution of the number of tokens
  in UMLS concept names.")

(defmethod umlsize ((corp corpus)
		    &key (n-tokens *default-max-concept-length*)
		    (flush t))
  (let ((i 0))
    (dolist (d (documents corp))
      (format t "~%~7d: ~a" (incf i) d)
      (umlsize (document d) :n-tokens n-tokens :flush flush))))

(defmethod umlsize ((doc document)
		    &key (n-tokens *default-max-concept-length*)
		    (flush t))
  (let* (sents
	 time-start time-elapsed)
    (setf time-start (get-internal-real-time))
    (setf sents (annotations doc :type 'sentence-annotation))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (when *profile-umls*
      (format t "~&ulmsize sents read time: ~f secs~%" (/ time-elapsed 1000)))
    (setf *norm->cui-time* 0
	  *cui->mesh-time* 0
	  *cui->tui-time* 0
	  *w->sp-pos-time* 0
	  *str->sp-lex-time* 0
	  *umlsize-1sth-time* 0
	  *umlsize-2ndh-time* 0
	  *umlsize-tr-time* 0
	  *norm->cui-cnt* 0
	  *cui->mesh-cnt* 0
	  *str->sp-lex-cnt* 0
	  *cui->tui-cnt* 0
	  *w->sp-pos-cnt* 0)
    (cond ((not (analyzedp doc :ganalysis 'gsentencize))
	   (format t "~%Warning: Cannot umlsize document that has not been sentencized: ~a"
		   doc))
	  (t (when flush
	       ;; flush the annotations on the document wholesale, not per
	       ;; sentence
	       (setf time-start (get-internal-real-time))
	       (flush-annotations-spec doc :type 'umls-annotation)
	       (setf time-elapsed (- (get-internal-real-time) time-start))
	       (when *profile-umls* 
		 (format t "~&ulmsize flush time: ~f secs~%" 
			 (/ time-elapsed 1000))))
	     (setf time-start (get-internal-real-time))

	     (dolist (s sents)
	       (umlsize s :n-tokens n-tokens :flush nil))
	     
	     (setf time-elapsed (- (get-internal-real-time) time-start))
	     (add-analysis doc :ganalysis 'gumlsize)))
    (when *profile-umls*
      (format t "~&stats: norm->cui time: ~f secs, count ~d~%~
cui->mesh time: ~f secs, count ~d~%~
cui->tui time: ~f secs, count ~d~%~
w->sp-pos time: ~f secs, count ~d~%~
str->sp-lex time: ~f secs, count ~d~%~
umlsize 1st half time ~f secs, 2nd half time ~f secs~%~
umls sents time: ~f secs, token read time: ~f secs~%"
	      (/ *norm->cui-time* 1000) *norm->cui-cnt*
	      (/ *cui->mesh-time* 1000) *cui->mesh-cnt*
	      (/ *cui->tui-time* 1000) *cui->tui-cnt*
	      (/ *w->sp-pos-time* 1000) *w->sp-pos-cnt*
	      (/ *str->sp-lex-time* 1000) *str->sp-lex-cnt*
	      (/ *umlsize-1sth-time* 1000) (/ *umlsize-2ndh-time* 1000)
	      (/ time-elapsed 1000) (/ *umlsize-tr-time* 1000)))
    ))

(defun noun-norm (tka i j)
  "Takes in token array, do not output norm if it is only one adj or verb token"
  (unless (and (= i j)
	       (null (annotations-spec (aref tka i) :type (gtype 'gtag-type)
				       :filter #'ptb-nounp)))
    (let* ((st (aref tka i))
	   (en (aref tka j))
	   (doc (document (aref tka i)))
	   (text (subseq (content doc) (start st) (end en))))
      (medg-norm text))))

(defmethod umlsize ((sent sentence-annotation)
		    &key (n-tokens *default-max-concept-length*)
		    (flush t))
  "Annotate parts of a sentence by their possible UMLS
  interpretations."
  ;; We have (so far) two ways to tokenize a sentence: (1) the Link
  ;; Parser does so as a step in parsing, (2) the OpenNLP toolkit has
  ;; a tokenizer.  These yield slightly different results, but the
  ;; difference should be small to the UMLS lookup process.
  (let* (tokens
	 time-start time-elapsed)
    (setf time-start (get-internal-real-time))
    (setf tokens (annotations-spec sent :type (gtype 'gtoken-type)))
    (setf time-elapsed (- (get-internal-real-time) time-start))
    (incf *umlsize-tr-time* time-elapsed)
    ;;(assert tokens ()
    ;;  "Cannot umlsize sentence that has not been parsed or tokenized: ~a"
    ;;  sent)
    (cond ((null tokens)
	   (format t "~&Warning: Empty token sentence: ~a" sent))
	  (t
	   (when flush
	     (delete-annotations
	      (annotations-within-spec sent :type 'umls-annotation)))
	   (let* ((doc (document sent))
		  (n (length tokens))
		  (tka (make-array (list n) :initial-contents tokens))
		  (interpretations		;as CUIs
		   (make-array (list n n) :initial-element nil))
		  (m-sp-lexes 
		   (make-array (list n n) :initial-element nil))
		  (m-sp-poses
		   (make-array (list n n) :initial-element nil)))
	     ;; We will be filling in the n-tokens diagonal and superdiagonal
	     ;; elements of the interpretations array with lists of UMLS CUIs
	     ;; retrieved for subsequences of tokens from the sentence. The
	     ;; (i, j) element of this matrix shows the CUIs retrieved for a
	     ;; normalized form of the text spanning from the start of the i'th
	     ;; to the end of the j'th token. We will call such a subsequence
	     ;; of tokens a span.
	     ;; Because lp's tokenizer includes tokens for punctuation, we will
	     ;; often retrieve the same list of CUIs for different spans.  In
	     ;; this case, we want to assign them only to the shortest span for
	     ;; which they appear. Therefore, we explore the set of spans from
	     ;; shortest to longest, and for each subspan encompassed by a
	     ;; longer span, we omit those CUIs for the longer span that
	     ;; already occur in the subspan.
	     ;; We use Lisp's set-difference to do these computations, which
	     ;; could be made more efficient by keeping the CUIs in sorted
	     ;; order. We also use an n x n array for interpretations, although
	     ;; only the diagonal and some superdiagonals are populated.  We
	     ;; could save space by a more sophisticated data structure design.
	     ;; We also eliminate CUI interpretations of single tokens that
	     ;; fall into the function-word grammatical categories, because the
	     ;; CUI we look up for these is often misleading. For example, "is"
	     ;; normalizes to "be", which has the CUI for Beryllium associated
	     ;; with it.
	     ;; 
	     (setf time-start (get-internal-real-time))
	     (dotimes (len n-tokens)
	       (do ((i 0 (1+ i))
		    (j 0))
		   ()
		 (when (>= (setq j (+ i len)) n) (return t))
		 (let* ((st (aref tka i))
			(en (aref tka j))
			(text (subseq (content doc) (start st) (end en)))
			(cuis (and (enable-umls sent st en) 
				   (str->cui text)))
			(sp-lexes (and (enable-umls sent st en)
				       (hstr->sp-lex text "noun")))
			(sp-poses (and (enable-umls sent st en)
				       (hstr->sp-pos text)))
			)
		   (when cuis
		     ;; Note that it's important to store these even if we
		     ;; flush them later for words being too short; otherwise,
		     ;; supersets of this span might get those CUIs.
		     (setf (aref interpretations i j) cuis))
		   (when sp-lexes
		     (setf (aref m-sp-lexes i j) sp-lexes))
		   (when sp-poses
		     (setf (aref m-sp-poses i j) sp-poses)))))
	     (setf time-elapsed (- (get-internal-real-time) time-start))
	     (incf *umlsize-1sth-time* time-elapsed)
	     (setf time-start (get-internal-real-time))
	     (dotimes (i n)
	       (let* ((sp-pos (aref m-sp-poses i i)))
		 
		 (when
		     ;; Now get rid of CUIs for single tokens that are function
		     ;; words of the language (i.e., closed categories)
		     ;; Note that many of the function words in Specialist also
		     ;; have other syntactic categories.  E.g., "was" can be
		     ;; either an aux (expected) or a noun (puzzling). We
		     ;; suspect that the latter kinds of cases are almost never
		     ;; useful, so we treat any word any of whose syntactic
		     ;; categories is a function type as such.  There are 428
		     ;; such words, as of 2009aa.
		     (intersection sp-pos *sp-lex-fn-scas* :test #'equal)
		   (setf (aref interpretations i i) nil))
		 (do ((j i (1+ j)))
		     ((or (>= j (+ i n-tokens)) (>= j n)))
		   (let* ((cuis (aref interpretations i j))
			  (sp-lexes (aref m-sp-lexes i j))
			  (sp-poses (aref m-sp-poses i j))
			  (tuis (or  (cui->tui cuis)))
			  meshs
			  (sem (tui->sem tuis)))
		     (dolist (sp-lex sp-lexes)
		       (add-annotation doc
				       (make-instance 'sp-lex-annotation
						      :document doc :data sp-lex
						      :start (start (aref tka i))
						      :end (end (aref tka j)))))
		     (dolist (cui cuis)
		       (add-annotation doc
				       (make-instance 'cui-annotation
						      :document doc :data cui
						      :start (start (aref tka i))
						      :end (end (aref tka j)))))
		     (dolist (tui tuis)
		       (add-annotation doc
				       (make-instance 'tui-annotation
						      :document doc :data tui
						      :start (start (aref tka i))
						      :end (end (aref tka j)))))
		     (dolist (mesh meshs)
		       (add-annotation doc
				       (make-instance 'mesh-annotation
						      :document doc :data mesh
						      :start (start (aref tka i))
						      :end (end (aref tka j)))))
		     
		     (dolist (sm sem)
		       (add-annotation doc
				       (make-instance 'sem-annotation
						      :document doc :data sm
						      :start (start (aref tka i))
						      :end (end (aref tka j)))))
		     (dolist (sp-pos sp-poses)
		       (add-annotation doc
				       (make-instance 'sp-pos-annotation
						      :document doc :data sp-pos
						      :start (start (aref tka i))
						      :end (end (aref tka j)))))))))
	     (setf time-elapsed (- (get-internal-real-time) time-start))
	     (incf *umlsize-2ndh-time* time-elapsed))))))
#|
mysql> select sca,count(*) c from lragr group by sca;
+-------+--------+
| sca   | c      |
+-------+--------+
| noun  | 804429 | 
| adj   |  93321 | 
| verb  |  85404 | 
| adv   |  13002 | 
| prep  |    168 | 
| pron  |     90 | 
| conj  |     69 | 
| det   |     44 | 
| aux   |     31 | 
| modal |     25 | 
| compl |      1 | 
+-------+--------+
11 rows in set (0.54 sec)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Noted problems:
;;;
;;; 1. The matching process via normalization and MRXNS_ENG
;;;over-generates grossly. For example, because (norm "as") includes
;;;"a", MRXNS_ENG will map the normalized version of the abbreviation
;;;"AS" ("a") to the concept "Aortic Stenosis", for which AS is a
;;;reasonable abbreviation. In addition, (norm "to a") maps to "a"
;;;because normalization drops prepositions, thus "to a" also maps to
;;;Aortic Stenosis! This yields total nonsense, however.  The latter
;;;problem is partly solved by eliminating interpretations of text
;;;that 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun show-top-umls (&key (type 'cui-annotation) (n 50))
  (open-late-database)
  (open-umls)
  (print-table
   (dbi.mysql:sql
    (format nil
	    "select x.data, p.str, x.c ~
from (select data,count(*) c from annotations ~
where type=~a ~
group by data order by c desc limit ~d) x ~
join 2009aa.pname p on x.data=p.cui"
	    (sq type) n)
    :db *late-db*)))

(defmethod sample-sents (corpus data type
				&key (n 50))
  "Samples sentences in the corpus (specified by id, name, or object)
   that contain an annotation of type whose data is or is contained in
   data. N specifies the maximum number of samples. For example,
   (sample 'mimic\" 'C0001732 'cui-annotation)
   would find (50) sentences that contain a cui-annotation with that
   CUI."
  (declare (ignore corpus))
  (open-umls)
  (open-late-database)
  (let ((anns (latesql
	       "select substring(d.content,b.start+1,b.end-b.start),a.data,a.start-b.start,a.end-b.start from (annotations a join annotations b on a.document_id=b.document_id and a.start >= b.start and a.end <= b.end and a.type~a and a.data~a and b.type='sentence-annotation') join documents d on a.document_id=d.id limit ~d"
	       (sql-matcher type)
	       (sql-matcher data)
	       n)))
    anns))

(defmethod sample ((corp corpus) data &key (type 'cui-annotation) (n 50))
  "Samples example sentences in which each of the types of annotations
  appears. Data gives a single or list of values. Type specifies the
  annotation type, and n the max number of examples."
  (open-umls)
  (let ((anns (latesql "select a.document_id,a.id,a.data ~
from annotations a join corpora_documents cd ~
on a.document_id=cd.document_id ~
where cd.corpus_id=~d and type~a and data~a limit ~d"
		       (id corp)
		       (sql-matcher type)
		       (sql-matcher data)
		       n)))
    (mapcan #'(lambda (da)
		(let* ((d (document (car da)))
		       (a (annotation d (cadr da)))
		       (ss (and a (annotations-spanning
				   a :type 'sentence-annotation))))
		  (mapcar #'(lambda (s)
			      (list (caddr da)
				    (and s (content s))
				    (start a) (end a)
				    (data a)
				    (if (eq type 'cui-annotation)
					(pname (data a))
				      nil)))
			  ss)))
	    anns)))

(defmethod sample ((corpus-name string) data
		   &key (type 'cui-annotation) (n 50))
  (sample (corpus corpus-name) data
	  :type type
	  :n n))

(defmemo mesh-heading (mesh-num)
  (let* ((codecmd (cmdstr "SELECT DISTINCT CODE FROM MRSAT "
			  "WHERE ATN='MN' AND SAB='MSH' AND ATV=~a"))
	 (codes (umlssql codecmd (sq mesh-num)))
	 (code (caar codes))
	 (mncmd (cmdstr "SELECT DISTINCT STR FROM MRCONSO "
			"WHERE CODE=~a AND SAB='MSH' AND TTY='MH'"))
	 (mpns (umlssql mncmd (sq code))))
    (when (> (length codes) 1)
      (format t "~&warning: ~a has more than one codes: ~a~%" mesh-num codes))
    (when (> (length mpns) 1)
      (format t "~&warning: ~a has more than one terms: ~a~%" mesh-num mpns))
    ;; (format t "~&mpns: ~a~%" mpns)
    (caar mpns)))

(defparameter *h-mesh-heading* (make-hash-table :test #'equalp))

(defun load-mesh-heading (&key 
			  (in-fn "late:;umls_cache;mesh_heading"))
  (when (= 0 (hash-table-count *h-mesh-heading*))
    (format t "~&loading mesh_heading~%")
    (let* (ln time-start time-elapsed)
      (setf time-start (get-internal-real-time))
      (with-open-file (in-f in-fn :direction :input :external-format :utf-8)
		      (loop (unless (setf ln (read-line in-f nil nil)) (return))
			    (destructuring-bind (mn mh)
				(split-re "\\|" ln)
			      (when (gethash mn *h-mesh-heading*)
				(format t "~&warning: multiple headings (~a, ~a) for mesh number ~a~%" (gethash mn *h-mesh-heading*) mh mn))
			      (setf (gethash mn *h-mesh-heading*) mh))))
      (setf time-elapsed (- (get-internal-real-time) time-start))
      (format t "~&load mesh_heading time: ~f secs~%" (/ time-elapsed 1000)))))

(defmethod h-mesh-heading ((mesh-num string) &aux ans)
  (load-mesh-heading)
  (setf ans (gethash mesh-num *h-mesh-heading*))
  (unless ans
    (format t "~&no heading for mesh: ~a~%" mesh-num))
  ans)

(defmethod h-mesh-heading ((mesh mesh-annotation))
  (h-mesh-heading (data mesh)))

(defun augment-opennlp-postag-dict (out-fn in-fn
					   &aux 
					   (h-dict (make-hash-table :test #'equal))
					   ln (ln-cnt 0) tags)
  "Augment Opennlp POS-tag dictionary with UMLS Specialist Lexicon.
Input
======
in-fn: points to old dictionary
out-fn: points to new dictionary
Note
======
The hash table h-dict is tested on equal because we want it to be 
case-sensitive. The keys of h-dict are token strings while the values of 
h-dict are list of possible tags.
"
;;; sample usage:
;;;  (umlsize:augment-opennlp-postag-dict "<opennlp home>/models/tags.umls.tagdict" "<opennlp home>/models/tags.tagdict")

  ;; read in old dict and populate the hash
  (open-umls)
  (with-open-file
   (in-f in-fn :direction :input)
   (loop (unless (setf ln (read-line in-f nil nil)) (return))
	 (incf ln-cnt)
	 (cond
	  ((search "<entry" ln)
	   (multiple-value-bind (m? whole tag-str)
	       (match-re "<entry +tags=\"(.*)\"" ln)
	     (declare (ignorable whole))
	     (cond
	      (m?
	       (setf tags (split-re " +" tag-str)))
	      (t
	       (format t "~&;Warning: line ~a format err in ~a.~%" 
		       ln-cnt in-fn)))))
	  ((search "</entry>" ln)
	   (setf tags nil))
	  ((search "<token>" ln)
	   (multiple-value-bind (m? whole token)
	       (match-re "<token>(.*)</token>" ln)
	     (declare (ignorable whole))
	     (cond
	      (m?
	       ;; is it a problem if i later unset tags? doesn't seem so,
	       (setf (gethash token h-dict) tags))
	      (t
	       (format t "~&;Warning: line ~a format err in ~a.~%" 
		       ln-cnt in-fn))))))))
  
  ;; iterate through every row in the lragr table
  (with-db-rows
   ((str sca agr) :db *umls* :table "lragr")
   (when (or (member sca (list "prep" "det" "conj") :test #'equalp) 
	     (not (match-re " " str)))
     (let* ((pos (sca-agr->ptb-pos str sca agr)))
       (setf str (replace-re str " +" "_"))
       (when (and (member sca (list "prep" "det" "conj" "modal")) 
		  (not (gethash str h-dict)))
	 (format t "~&add closed category word ~a: sca: ~a, agr: ~a pos: ~a~%"
		 str sca agr pos))
       (cond
	((listp pos)
	 (dolist (pos-str pos)
	   (unless (member pos-str (gethash str h-dict) :test #'equalp)
	     (format t "~&adding ~a/~a~%" str pos-str))
	   (pushnew pos-str (gethash str h-dict) :test #'equalp)))
	((stringp pos)
	 (unless (member pos (gethash str h-dict) :test #'equalp)
	   (format t "~&adding ~a/~a~%" str pos))
	 (pushnew pos (gethash str h-dict) :test #'equalp))
	(t
	 (format "~&unhandled word ~a sca: ~a agr: ~a~%" str sca agr))))))
  
  (with-open-file
   (out-f out-fn :direction :output :if-exists :supersede
	  :if-does-not-exist :create)
   (format out-f "~&<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
   (format out-f "~&<dictionary>~%")
   (maphash #'(lambda (k v)
		;; pay attention to xml escaped characters
		(setf k (replace-re k "&amp;" "&"))
		(setf k (replace-re k "&" "&amp;"))
		(setf k (replace-re k "\"" "&quot;"))
		(setf k (replace-re k "\'" "&apos;"))
		(setf k (replace-re k "<" "&lt;"))
		(setf k (replace-re k ">" "&gt;"))
		(format out-f "~&<entry tags=\"~{~a~^ ~}\">~%" v)
		(format out-f "~&<token>~a</token>~%</entry>~%" k))
	    h-dict)
   (format out-f "</dictionary>~%")))

(defmemo online-lookup (str)
  "Given a string, we look up its possible POS tags from UMLS Specialist 
Lexicon.
A few things are taken care of here:
- Convert from SP-POS tags to PTB-POS tags
- Accept possibly multiple word string
Input
======
str - a string to be looked up
Output
======
A set of tags delimited by |
"
  (let* (tags tag)
    ;; get rid of excessive whitespaces/newlines.
    (setf str (replace-re str "\\s+" " "))
    (dolist (sca (w->sp-pos str))
      (dolist (agr (w-sca->sp-agr str sca))
	(setf tag (sca-agr->ptb-pos str sca agr))
	(unless tag
	  (format t "~&Warning: no tag for ~a, ~a, ~a~%" str sca agr))
	(pushnew tag tags :test #'equalp)))
    (format nil "~{~a~^|~}" tags)))

(defparameter *ph-umls-ooi-cnt* 0)
(defparameter *sent-cnt* 0)

(defun has-noun? (umls-ann)
  (annotations-spec umls-ann 
		    :type (gtype 'gtag-type)
		    :filter #'(lambda (a) 
				(member (data a) *ptb-noun-list* 
					:test 'string=))))

(defun has-prep? (umls-ann)
  (annotations-spec umls-ann :type (gtype 'gtag-type) :filter #'late::ptb-prepp))

(defun has-conj? (umls-ann)
  (annotations-spec umls-ann 
		    :type (gtype 'gtag-type)
		    :filter #'(lambda (a)
				(member (data a) '(CC) :test #'string=))))

(defun has-punc? (umls-ann)
  (some #'(lambda (ta) (search (content ta) *punc*))
	(annotations-spec umls-ann :type (gtype 'gtoken-type))))

(defun non-trivial? (umls-ann) 
  (> (length-wo-punc umls-ann) 1))

(defun subsumed? (a1 a2)
  (member (allen a1 a2) '(:d :s :f)))

(defun overlapped? (a1 a2)
  (member (allen a1 a2) '(:o :oi)))

(defun low-overlap-cost? (umls-ann umls-anns)
  (cond 
   ((or (has-prep? umls-ann) (has-punc? umls-ann) (has-conj? umls-ann))
    (notany #'(lambda (a) (overlapped? umls-ann a)) umls-anns))
   (t
    t)))


(defun NP? (ann)
  (and (typep ann 'phrase-annotation) (equalp (data ann) "NP")))

(defun ann-coverage-cuis (ann)
  "find the cui annotations in a parse node that are not subsumed by others"
  (let* ((cuis (annotations-spec ann :type 'cui-annotation))
	 ans)
    (setf cuis (remove-if #'pname-has-plus? cuis))
    (setf cuis (remove-if #'late::has-overlap-range-adj? cuis))
    ;; (setf cuis (remove-if-not #'ann-end-nounp cuis))
    (setf cuis (stable-sort cuis #'ann-w-tok-len-greaterp))
    (dolist (cui cuis)
      (when (notany #'(lambda (a) (or (subsumed? cui a) (overlapped? cui a))) 
		    ans)
	(push cui ans)))
    ans))

(defun pname-has-plus? (cui)
  (search "+" (pname (data cui))))

(defmethod sp-lex-cover ((sen sentence-annotation)
			 &aux has-overlap? h-umls-ambig)
  ;; key: umls position; val: count
  (setf h-umls-ambig (make-hash-table :test 'equalp))
  
  (let* ((umls-anns (annotations-spec sen :type 'sp-lex-annotation))
	 ans umls-pos len-cons umls-len)
    (setf umls-anns (stable-sort umls-anns #'> :key #'length-wo-punc))
    (dolist (umls-ann umls-anns)
      (when (and (notany #'(lambda (a) (subsumed? umls-ann a)) ans)
		 (non-trivial? umls-ann)
		 (low-overlap-cost? umls-ann umls-anns))
	
	(when (some #'(lambda (a) 
			(cond 
			 ((overlapped? umls-ann a)
			  ;; (format t "~&~a and ~a " umls-ann a)
			  t)
			 (t
			  nil))) 
		    ans)
	  ;; (format t "overlap~%")
	  (setf has-overlap? t))
	
	(push umls-ann ans)
	(setf umls-pos (format nil "~a-~a" (start umls-ann) (end umls-ann)))
	(incf (gethash umls-pos h-umls-ambig 0))
	(setf umls-len (length-wo-punc umls-ann))
	;; update umls length distribution
	(setf len-cons (assoc umls-len *l-sp-lex-length*))
	(if len-cons
	    (incf (cdr len-cons))
	  (setf *l-sp-lex-length* (acons umls-len 1 *l-sp-lex-length*)))))
    
    ;; update umls amabiguity distribution
    (maphash #'(lambda (pos cnt)
		 (declare (ignorable pos))
		 (let* (ambig-cons)
		   (setf ambig-cons (assoc cnt *l-sp-lex-ambig*))
		   (if ambig-cons
		       (incf (cdr ambig-cons))
		     (setf *l-sp-lex-ambig* (acons cnt 1 *l-sp-lex-ambig*)))))
	     h-umls-ambig)
    
    ;; update umls coverage stats
    (dolist (ta (annotations-spec sen :type (gtype 'gtoken-type)))
      (cond 
       ((intersection (annotations-spanning ta :type 'sp-lex-annotation) ans)
	(incf *sp-lex-covered-tokens*))
       (t
	(incf *sp-lex-uncovered-tokens*))))
    
    ;; sort ans in occurrence order
    (setf ans (stable-sort ans #'annotation-lessp))
    (when (and *debug-umls* has-overlap?) 
      (format t "~&cover in sen ~a ~%~a~%~{~a~%~}~%<<<<<<~%>>>>>>~%~%" 
	      (id sen) (content sen) ans))
    ans))

(defmethod sp-lex-cover ((doc document))
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (when (and ;; (not (late::maskedp sen)) 
	   (annotations-spec sen :type (gtype 'gparse-type)))
      (sp-lex-cover sen)
      (incf *sent-cnt*))))

(defmethod sp-lex-cover ((corp corpus))
  (dolist (docid (documents corp))
    (let* ((doc (document docid)))
      ;; (format t "~&umls-covering ~a~%" (name doc))
      (sp-lex-cover doc))))

(defmethod umls-cover ((sen sentence-annotation)
		       (umls-type t)
		       &aux has-overlap? ph-umls-ooi? h-umls-ambig)
  ;; key: umls position; val: count
  (setf h-umls-ambig (make-hash-table :test 'equalp))
  
  (let* ((umls-anns (annotations-spec sen :type umls-type))
	 ans umls-pos len-cons umls-len)
    (setf umls-anns (stable-sort umls-anns #'> :key #'length-wo-punc))
    (dolist (umls-ann umls-anns)
      (when (and (notany #'(lambda (a) (subsumed? umls-ann a)) ans)
		 (non-trivial? umls-ann)
		 (has-noun? umls-ann)
		 (low-overlap-cost? umls-ann umls-anns))
	
	(when (some #'(lambda (a) 
			(cond 
			 ((overlapped? umls-ann a)
			  ;; (format t "~&~a and ~a " umls-ann a)
			  t)
			 (t
			  nil))) 
		    ans)
	  ;; (format t "overlap~%")
	  (setf has-overlap? t))
	(when (or (annotations-spec umls-ann :type (gtype 'gphrase-type) 
				    :relation ':o
				    :filter 'NP?)
		  (annotations-spec umls-ann :type (gtype 'gphrase-type)
				    :relation ':oi
				    :filter 'NP?))
	  (setf ph-umls-ooi? t))
	(push umls-ann ans)
	(setf umls-pos (format nil "~a-~a" (start umls-ann) (end umls-ann)))
	(incf (gethash umls-pos h-umls-ambig 0))
	(setf umls-len (length-wo-punc umls-ann))
	;; update umls length distribution
	(cond 
	 ((eq umls-type 'cui-annotation)
	  (setf len-cons (assoc umls-len *l-cui-length*))
	  (if len-cons
	      (incf (cdr len-cons))
	    (setf *l-cui-length* (acons umls-len 1 *l-cui-length*))))
	 
	 ((eq umls-type 'tui-annotation)
	  (setf len-cons (assoc umls-len *l-tui-length*))
	  (if len-cons
	      (incf (cdr len-cons))
	    (setf *l-tui-length* (acons umls-len 1 *l-tui-length*))))
	 (t
	  (format t "~&Warning: unrecognized umls-type!~%")))))
    
    ;; update umls amabiguity distribution
    (maphash #'(lambda (pos cnt)
		 (declare (ignorable pos))
		 (let* (ambig-cons)
		   (cond 
		    ((eq umls-type 'cui-annotation)
		     (setf ambig-cons (assoc cnt *l-cui-ambig*))
		     (if ambig-cons
			 (incf (cdr ambig-cons))
		       (setf *l-cui-ambig* (acons cnt 1 *l-cui-ambig*))))
		    
		    ((eq umls-type 'tui-annotation)
		     (setf ambig-cons (assoc cnt *l-tui-ambig*))
		     (if ambig-cons
			 (incf (cdr ambig-cons))
		       (setf *l-tui-ambig* (acons cnt 1 *l-tui-ambig*))))
		    (t
		     (format t "~&Warning: unrecognized umls-type!~%")))))
	     h-umls-ambig)
    
    ;; update umls coverage stats
    (dolist (ta (annotations-spec sen :type (gtype 'gtoken-type)))
      (cond 
       ((intersection (annotations-spanning ta :type umls-type)
		      ans)
	(cond ((eq umls-type 'cui-annotation)
	       (incf *cui-covered-tokens*))
	      ((eq umls-type 'tui-annotation)
	       (incf *tui-covered-tokens*))
	      (t
	       (format t "~&Warning: unrecognized umls-type!~%"))))
       (t
	(cond ((eq umls-type 'cui-annotation)
	       (incf *cui-uncovered-tokens*))
	      ((eq umls-type 'tui-annotation)
	       (incf *tui-uncovered-tokens*))
	      (t
	       (format t "~&Warning: unrecognized umls-type!~%"))))))
    
    ;; sort ans in occurrence order
    (setf ans (stable-sort ans #'annotation-lessp))
    (when (and *debug-umls* has-overlap?) 
      (format t "~&cover in sen ~a ~%~a~%~{~a~%~}~%<<<<<<~%>>>>>>~%~%" 
	      (id sen) (content sen) ans))
    (when ph-umls-ooi?
      (incf *ph-umls-ooi-cnt*))
    ans))

(defmethod umls-cover ((doc document)
		       (umls-type t))
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (when (and ;; (not (late::maskedp sen)) 
	   (annotations-spec sen :type (gtype 'gparse-type)))
      (umls-cover sen umls-type)
      (incf *sent-cnt*))))

(defmethod umls-cover ((corp corpus)
		       (umls-type t))
  (dolist (docid (documents corp))
    (let* ((doc (document docid)))
      ;; (format t "~&umls-covering ~a~%" (name doc))
      (umls-cover doc umls-type)))
  (format t "~&ph-umls-ooi'ed sent: ~a; total anal'ed sent: ~a~%"
	  *ph-umls-ooi-cnt* *sent-cnt*))

(defmemo get-stys ()
  "Get a list of UMLS semantic types"
  (let* ((stys (umlssql1 "select STY_RL from SRDEF where RT='STY'")))
    stys))



(defun foo ()
  (member (intern (allenr 1 3 2 4)) '(o oi)))

(defun non-if-chemicals (corpn &aux h-chem)
  "Find chemicals that are not immunologic factors"
  (setf h-chem (make-hash-table :test #'equalp))
  (dolist (docid (documents (corpus corpn)))
    (let* ((doc (document docid))
	   stn sty-rl)
      (dolist (tui (annotations doc :type 'tui-annotation))
	(setf stn (tui->stn (data tui)))
	(setf sty-rl (stn->sty-rl stn))
	(when (and (not (equalp "A1.4.1.1.3.5" stn))
		   (search "A1.4.1" stn))
	  (incf (gethash (cons (content tui) sty-rl) h-chem 0))))))
  
  (dolist (chem-cnt (hash-table-val-desc-alist h-chem))
    (format t "~&~a: ~a~%" (car chem-cnt) (cdr chem-cnt))))

(defun if-chemicals (corpn &aux h-chem)
  "Find chemicals that are not immunologic factors"
  (setf h-chem (make-hash-table :test #'equalp))
  (dolist (docid (documents (corpus corpn)))
    (let* ((doc (document docid)))
      (dolist (tui (annotations doc :type 'tui-annotation))
	(when (equalp "T129" (data tui))
	  (when (equalp "PhD" (content tui))
	    (format t "~&~a in ~a~%" tui (name (document tui))))
	  (incf (gethash (content tui) h-chem 0))))))
  
  (dolist (chem-cnt (hash-table-val-desc-alist h-chem))
    (format t "~&~a: ~a~%" (car chem-cnt) (cdr chem-cnt))))


(defun chem-coloc-graph (corpn &aux h-chem)
  "If two chem co-occurs in one sentence, create an edge between them"
  (setf h-chem (make-hash-table :test #'equalp))
  (dolist (docid (documents (corpus corpn)))
    (let* ((doc (document docid))
	   stn sty-rl)
      (dolist (tui (annotations doc :type 'tui-annotation))
	(setf stn (tui->stn (data tui)))
	(setf sty-rl (stn->sty-rl stn))
	(when (and (not (equalp "A1.4.1.1.3.5" stn))
		   (search "A1.4.1" stn))
	  (incf (gethash (cons (content tui) sty-rl) h-chem 0))))))
  
  (dolist (chem-cnt (hash-table-val-desc-alist h-chem))
    (format t "~&~a: ~a~%" (car chem-cnt) (cdr chem-cnt))))


(defun trim-stns (stns &aux event-max-depth entity-max-depth max-depth ans tmp)
  "Trimmed stns to desired depth, also remove duplicates afterwards.
Note:
We treat event and entity desired depth differently, however, one might either
want a more uniform or a more flexible way."
  (setf event-max-depth (gkey 'gconcept-graph 'max-event-stn-depth)
	entity-max-depth (gkey 'gconcept-graph 'max-entity-stn-depth))
  (setf event-max-depth (parse-integer event-max-depth)
	entity-max-depth (parse-integer entity-max-depth))
  (dolist (stn stns)
    (let* ((levs (split-re "\\." stn))
	   new-stn)
      (cond 
       ((equalp "A1.4.1.1.3.5" stn)
	(setf max-depth (length levs)))
       ;; entity
       ((match-re "^A" stn)
	(setf max-depth (min (length levs) entity-max-depth)))
       ;; event
       ((match-re "^B" stn)
	(setf max-depth (min (length levs) event-max-depth))))
      
      (setf new-stn (format nil "~{~a~^.~}" (subseq levs 0 max-depth)))
      (pushnew new-stn tmp :test #'equalp)))
  (setf tmp (stable-sort tmp #'string-greaterp))
  (dolist (stn tmp)
    (unless (some #'(lambda (a) (search stn a)) ans)
      (push stn ans)))
  ans)

(defun event-stnp (stn)
  (match-re "^B" stn))


(defun tuis->sty (tuis)
  "Assume those tuis have same spans"
  (let* ((stns (trim-stns (mapcar #'tui->stn tuis)))
	 (stns (remove-if #'event-stnp stns))
	 (toks (annotations-spec (car tuis) :type (gtype 'gtoken-type))))
    (cond 
     (stns
      (format nil "~{~a~^-~}" 
	      (mapcar #'(lambda (a) 
			  (replace-re (stn->sty-rl a) " +" "_"))
		      stns)))
     (t
      (string-downcase (content (car (nreverse toks))))))))


(defmethod tag-inconsistency ((sen sentence-annotation)
			      &aux 
			      (buf (make-array 0 :element-type 'character 
					       :adjustable t
					       :fill-pointer 0))
			      out?)
  (format buf "In sentence ~a, ~a:~%" (id sen) (content sen))
  (do ((tas (annotations-spec sen :type (gtype 'gtoken-type)) (cdr tas))
       (tags 
	(annotations-spec sen :type (gtype 'gtag-type)) 
	(cdr tags)))
      ((null tas))
    (let* ((ta (car tas))
	   (tag (car tags))
	   (tag-str (format nil "~a" (data tag)))
	   (ta-str (content ta))
	   (l-sp-pos (w->sp-pos (content ta))))
      (when l-sp-pos
	(cond
	 ((member tag-str *ptb-noun-list* :test #'string=)
	  (unless (member "noun" l-sp-pos :test #'string=)
	    (setf out? t)
	    (format buf "~&~a: ~a | ~a~%" ta-str tag-str l-sp-pos)))
	 
	 ((member tag-str *ptb-verb-list* :test #'string=)
	  (unless (intersection (list "aux" "verb") l-sp-pos :test #'string=)
	    (setf out? t)
	    (format buf "~&~a: ~a | ~a~%" ta-str tag-str l-sp-pos)))
	 
	 ((member tag-str *ptb-adj-list* :test #'string=)
	  (unless (member "adj" l-sp-pos :test #'string=)
	    (setf out? t)
	    (format buf "~&~a: ~a | ~a~%" ta-str tag-str l-sp-pos)))
	 
	 ((member tag-str *ptb-adv-list* :test #'string=)
	  (unless (member "adv" l-sp-pos :test #'string=)
	    (setf out? t)
	    (format buf "~&~a: ~a | ~a~%" ta-str tag-str l-sp-pos)))))))
  (when out?
    (format t "~&~a~%" buf)))

(defmethod tag-inconsistency ((doc document))
  (dolist (sen (annotations doc :type 'sentence-annotation))
    (tag-inconsistency sen)))


(defun if-tui? (a)
  (member (data a) *if-tuis* :test #'equalp))

(defun if-tuis (sen)
  (annotations sen :type 'tui-annotation :filter #'if-tui?))
