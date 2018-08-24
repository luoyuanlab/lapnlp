;;; -*- Mode: Lisp; Package: late; -*-
#|

yluo - 08/16/2018 clean and reorganization
yluo - 06/08/2015 rewrite using asdf framework
yluo - 06/02/2012 post-processing, not break on abbreviations Drs. etc.
yluo - 12/17/2011 delete exclude-sections related.
yluo - 11/23/2009 added exclude-sections option to sentencize
yluo - 11/19/2008 creation 
|#

(defpackage :late
  (:use :common-lisp :excl :opennlp :util)
  (:export
   "blank-char?"
   "section-lessp"
   "sent-dist"
   "sentence-annotations"
   "sentencize"
   "sentencize-record"
   "sentencize-within-sections"
   "trim-blanks"
   "cc-sent"
   "id->sen"
   "load-sen-hash"
   "all-sen-list"
   "sen-secs"
   "sen-len-dist"
   "sent-templ"
   ))
(in-package :late)

(defparameter *sap-sentencize* (list "narrative") ; "enumerate"
  "Sentencizable section processors.")

(defparameter *abbr-pat-nb* "\\b(Drs\\.|Mr\\.|Dr\\.|Mrs\\.|Ms\\.)$"
  "abbreviation pattern not breakable")

(defmethod sentencize ((corp corpus)
		       &key (in-sections? t) 
		       (line-splits? nil))
  "Adds sentence break annotations to all the documents in a
  corpus. If in-sections? is true, and if there are section annotations
  for this document, then we find sentences within sections; otherwise
  in the entire text. If line-splits? is true, we assume that every
  linefeed indicates the end of a line. This is the case for documents
  from word processors that represent each paragraph as a single line,
  or for corpora that have already been split into sentences by lines." 
  (dolist (d (documents corp))
    (sentencize (document d) :in-sections? in-sections?
		:line-splits? line-splits?)))

(defmethod sentencize ((doc document)
		       &key (in-sections? t)
		       (line-splits? nil))
  "Adds annotations to a document indicating the likely sentence
  breaks therein. If in-sections? is non-NIL and there are section
  annotations for this document, then we instead call
  sentencize-within-sections. If line-splits? is true, then we do not
  allow sentences to span over a line break." 

  (if (and in-sections? (annotations doc :type 'section-annotation))
      (sentencize-within-sections doc :line-splits? line-splits?)
    (let ((text (content doc)))
      (sentencize-record doc 0 (length text) nil :line-splits? line-splits?)))
  (add-analysis doc :ganalysis 'gsentencize))

(defun blank-char? (ch)
  (position ch *trimchars*))

(defmethod trim-blanks ((string string) 
			&optional (start 0) (end (length string)))
  (let* ((left-non-blank (position-if-not #'blank-char?
					  string
					  :start start
					  :end end))
	 (right-non-blank (position-if-not #'blank-char?
					   string
					   :from-end t
					   :start start
					   :end end)))
    (when (and (integerp left-non-blank) (integerp right-non-blank))
      (cons left-non-blank (1+ right-non-blank)))))

(defun sentencize-rec-no-interrupt (doc start end hierarchy
					&key (line-splits? nil))
  "Note: OpenNLP 1.5.2 returns Span[], while OPenNLP 1.4.3 returns breaks."
  ;; (format t "~&sentencize-record start ~a; end ~a~%" start end)
  (let* ((text (content doc))
	 ss spans)
    
    (cond
     ;; if we don't use OpenNLP
     ((not (equalp 'sentencize-opennlp (ganalysis 'gsentencize)))
      (setf spans (list (cons 0 (- end start)))))
     ;; if we use OpenNLP, check version
     ((equalp "1.4.3" *opennlp-version*)
      (let* ((sentbreaks (coerce (sentdetect (subseq text start end)) 'list))
	     (span-begin (concatenate 'list (list 0) sentbreaks))
	     (span-end (concatenate 'list sentbreaks (list (- end start)))))
	(setf spans (mapcar #'(lambda (x y) (cons x y)) span-begin span-end))
	(setf spans (mapcar #'(lambda (span) 
				(trim-blanks text (car span) (cdr span))) 
			    spans))))
     
     ((equalp "1.5.2" *opennlp-version*)
      (setf spans (sentdetect (subseq text start end))))
     
     (t
      (error "Unsupported OpenNLP version ~a" *opennlp-version*)))
    
    ;; offsetted by the doc start.
    (setf spans (mapcar #'(lambda (x) 
			    (cons (+ (car x) start) (+ (cdr x) start)))
			spans))
    
    ;; post-process to exclude abbr (Drs. etc.) as sentence breakers 
    (let* (new-spans)
      (do* ((spans spans (cdr spans))
	    (span (car spans) (car spans)))
	  ((null spans))
	(let* ((s (car span))
	       (e (cdr span))
	       (sen-str (subseq text (car span) (cdr span))))
	  (loop (unless (match-re *abbr-pat-nb* sen-str) (return))
		(setf spans (cdr spans))
		(setf span (car spans))
		(setf sen-str (subseq text (car span) (cdr span)))
		(setf e (cdr span)))
	  (push (cons s e) new-spans)))
      (setf spans (nreverse new-spans)))
    
    
    
    (dolist (span spans)
      (let* ((s (car span))
	     (e (cdr span))
	     m)
	(when (and s e)
	  (setq ss s)
	  (when line-splits?
	    ;; If line-splits? is true, we split up a detected sentence
	    ;; into separate sentences consisting of its component lines
	    ;; (while (setq ee (position #\linefeed text :start ss :end e))
	    (while (setf m (match-re line-splits? text :start ss :end e 
				     :return :match))
	      (let* ((mstart (car (re-submatch m nil nil 0 :type :index)))
		     (mend (cdr (re-submatch m nil nil 0 :type :index)))
		     (inner-span (trim-blanks text ss mstart))
		     (sss (car inner-span))
		     (eee (cdr inner-span)))
		(when (and sss eee)
		  (multiple-value-bind (lstm? lst)
		      (match-re "^\\d\\.\\s*" (subseq text sss eee))
		    (when lstm?
		      (incf sss (length lst))))
		  (let ((ann (make-instance 'sentence-annotation
					    :document doc
					    :start sss
					    :end eee)))
		    (add-annotation doc ann)
		    (when hierarchy
		      (add-annotation hierarchy ann))))
		(setq ss mend))))
	  
	  (when (and ss e (< ss e))
	    (let* ((inner-span (trim-blanks text ss e))
		   (sss (car inner-span))
		   (eee (cdr inner-span)))
	      (when (and sss eee)
		(multiple-value-bind (lstm? lst)
		    (match-re "^\\d\\.\\s*" (subseq text sss eee))
		  (when lstm?
		    (incf sss (length lst))))
		(let ((ann (make-instance 'sentence-annotation
					  :document doc
					  :start sss
					  :end eee)))
		  (add-annotation doc ann)
		  (when hierarchy
		    (add-annotation hierarchy ann)))))))))))

(defmethod sentencize-record (doc start end hierarchy 
				  &key (line-splits? nil))
  "Changed to first exclude quantitative and separator expression."
  (let* ((quants (annotations doc :type (gtype 'gquant-type)))
	 (fmarks (annotations doc :type (gtype 'gformat-mark-type)))
	 (nosens (union quants fmarks :test #'annotation-equal))
	 (nosens (sort nosens #'annotation-lessp))
	 astart aend)
    (do* ((nosens nosens (cdr nosens))
	  
	  (nosen (car nosens) (car nosens)))
	((or (null nosens)
	     ;; we have searched pass end of rec, no need to search further
	     (< end (start nosen))))
      (cond
       ((< (end nosen) start))
       
       ;; rec inside nosen, nothing to do.
       ((<= (start nosen) start end (end nosen))
	(return-from sentencize-record nil))
       
       ;; nosen contains rec start, start from end of nosen
       ((<= (start nosen) start (end nosen))
	(setf astart (end nosen)))
       
       ;; nosen is inside rec, process until start of nosen, then renew at
       ;; end of nosen
       ((<= start (start nosen) (end nosen) end)
	(setf astart (or astart start)
	      aend (start nosen))
	(sentencize-rec-no-interrupt doc astart aend hierarchy 
				     :line-splits? line-splits?)
	(setf astart (end nosen)))
       
       ;; nosen contains rec end, process until start of nosen, finish
       ((<= (start nosen) end (end nosen))
	(setf astart (or astart start)
	      aend (start nosen))
	(sentencize-rec-no-interrupt doc astart aend hierarchy 
				     :line-splits? line-splits?)
	(return-from sentencize-record nil))
       
       (t
	(error "~&Unrecognized relation ~a and [~a-~a]~%" nosen start end))))
    
    (setf astart (or astart start)
	  aend end)
    (sentencize-rec-no-interrupt doc astart aend hierarchy 
				 :line-splits? line-splits?)))


(defun sentencize? (sec)
  "Determines whether this section is to be sentencized."
  (and (not (typep sec 'section-head-annotation))
       (processor sec)
       (stringp (processor sec))
       (intersection *sap-sentencize* (split-re "\\|" (processor sec))
		     :test #'equalp)
       (match-re "[A-Za-z]" (content sec))
       (not (equalp "clinical_data" (data sec))) ;; 08/19
       ;; (not (annotations-spanning sec :type 'section-annotation
       ;;							  :filter #'useless-sec?)) 02/07
       ))

(defun sentencize-section (sa &key (line-splits? nil))
  (when (sentencize? sa)
    (let* ((subsas (sort (remove-if-not #'sectionp (h-down sa)) #'section-lessp))
	   (l-pos (mapcan #'(lambda (a) (list (start a) (end a))) subsas))
	   (l-pos (concatenate 'list (list (start sa)) l-pos (list (end sa))))
	   (doc (document sa)))
      (do ((l-pos l-pos (cddr l-pos)))
	  ((null l-pos))
	(let* ((s (car l-pos))
	       (e (cadr l-pos)))
	  (sentencize-record doc s e sa :line-splits? line-splits?)))
      (dolist (subsa subsas)
	(sentencize-section subsa :line-splits? line-splits?)))))


(defmethod sentencize-within-sections ((doc document) 
				       &key
				       (line-splits? nil))
  (let* ((doc-anns (annotations doc :type 'document-annotation))
	 (doc-ann (and doc-anns (car doc-anns))))
    ;; (format t "~&~{~a~%~}" (annotations doc :type 'section-annotation))
    (sentencize-section doc-ann :line-splits? line-splits?)))

(defun section-lessp (x y)
  "Defines a canonical order on section names. Each is a list of subsection
hierarchies, in top-to-bottom order (opposite of how they are stored in
actual annotations).  Shorter hierarchies (higher up) precede ones further down,
and at one level they are ordered by start location and then length. I changed 
\(> (end x) (end y)) to (< (end x) (end y))"
  (let ((xs (start x))
	(ys (start y)))
    (or (< (depth x) (depth y))
	(< xs ys)
	(and (= xs ys) (< (end x) (end y))))))


(defmethod sentence-annotations ((doc document))
  (annotations doc :type 'sentence-annotation))

(defun sen-norm (str)
  (setf str (replace-re str "[()\\[\\]{},.]" ""))
  (setf str (replace-re str "\\d+%" ""))
  (setf str (replace-re str "\\s+" " "))
  (setf str (replace-re str "(^\\s+|\\s+$)" "")))

(defun sent-dist (corpn cut &aux h-sents doc sen-str)
  (setf h-sents (make-hash-table :test #'equalp))
  (dolist (docid (documents (corpus corpn)))
    (setf doc (document docid))
    (dolist (sen (annotations doc :type 'sentence-annotation))
      (when (annotations sen :type 'parse-annotation)
	(setf sen-str (sen-norm (content sen)))
	(incf (gethash sen-str h-sents 0)))))

  (let* ((l-sen-cnt (hash-table-val-desc-alist h-sents)))
    (format t "~&length of h-sents: ~a~%" (length l-sen-cnt))
    
    (dolist (sen-cnt l-sen-cnt)
      (when (> (cdr sen-cnt) cut)
	(format t "~&~a: ~a~%" (cdr sen-cnt) (car sen-cnt)))))
  h-sents)

(defun sent-singleton (corpn cut &aux h-sents doc sen-str)
  (setf h-sents (make-hash-table :test #'equalp))
  (dolist (docid (documents (corpus corpn)))
    (setf doc (document docid))
    (dolist (sen (annotations doc :type 'sentence-annotation))
      (when (= 1 (length (annotations sen :type (gtype 'ghier-parse-node-type))))
	(setf sen-str (sen-norm (content sen)))
	(incf (gethash sen-str h-sents 0)))))

  (let* ((l-sen-cnt (hash-table-val-desc-alist h-sents)))
    (format t "~&length of h-sents: ~a~%" (length l-sen-cnt))
    
    (dolist (sen-cnt l-sen-cnt)
      (when (> (cdr sen-cnt) cut)
	(format t "~&~a: ~a~%" (cdr sen-cnt) (car sen-cnt)))))
  h-sents)

(defparameter *sen-templ-cnt* 66)

(defmemo sent-templ (corpn)
  (let* ((h-sents (make-hash-table :test #'equalp))
	 (h-stmp (make-hash-table :test #'equalp))
	 doc sen-str)
    (dolist (docid (documents (corpus corpn)))
      (setf doc (document docid))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(when (annotations sen :type 'parse-annotation)
	  (setf sen-str (sen-norm (content sen)))
	  (push (id sen) (gethash sen-str h-sents)))))
    
    (maphash #'(lambda (s l)
		 (declare (ignorable s))
		 (when (>= (length l) *sen-templ-cnt*)
		   (dolist (id l)
		     (setf (gethash id h-stmp) 1))))
	     h-sents)
    h-stmp))

(defun cc-sent (corpn &aux h-sen)
  (setf h-sen (make-hash-table :test #'equalp))
  (dolist (docid (documents (corpus corpn)))
    (let* ((doc (document docid)))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(when (annotations-spec sen :type (gtype 'gtoken-type)
				:filter #'(lambda (a) 
					    (member (content a)
						    '("and" "or" "and/or")
						    :test #'equalp)))
	  (incf (gethash (content sen) h-sen 0))))))
  (dolist (sen-cnt (hash-table-val-desc-alist h-sen))
    (format t "~&~a: ~a~%" (cdr sen-cnt) (car sen-cnt))))

(defmemo id->sen (sid did)
  (find-annotation sid (id->doc did)))


(defun load-sen-hash (corpn)
  (let* ((corp (corpus corpn))
	 doc sid k
	 (hsen (make-hash-table :test #'equalp)))
    (dolist (docid (documents corp))
      (setf doc (document docid))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(setf sid (id sen))
	(setf k (format nil "~a_~a" docid sid))
	(setf (gethash k hsen) sen)))
    hsen))


(defun addendum-sent (corpn addendum &aux sents)
  (dolist (docid (documents (corpus corpn)))
    (let* ((doc (document docid))
	   sn)
      (dolist (sa (annotations doc :type 'section-annotation))
	(setf sn (content sa))
	(setf sn (replace-re sn "(^\\s+|\\s+$)" ""))
	(setf sn (replace-re sn "\\s+" " "))
	(when (equalp (data sa) addendum)
	  (dolist (sen (annotations sa :type 'sentence-annotation))
	    (when (annotations-spec sen :type (gtype 'gtoken-type))
	      (push sen sents)))))))
  sents)


(defun all-sen-list (corpn &aux ans)
  (dolist (docid (documents (corpus corpn)))
    (let* ((doc (document docid))
	   tas)
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(setf tas (annotations-spec sen :type (gtype 'gtoken-type)))
	(when (match-re "[A-Za-z]" (format nil "~{~a ~}" (mapcar #'content tas)))
	  (push (format nil "~a_~a" docid (id sen)) ans)))))
  ans)


(defun sen-secs (sens &aux doc sen)
  (let* ((h-sec (make-hash-table :test #'equalp)))
    (dolist (doc-sen sens)
      (destructuring-bind (doc-id sen-id)
	  (split-re "_" doc-sen)
	(setf doc (document (parse-integer doc-id)))
	(setf sen (find-annotation (parse-integer sen-id) doc))
	(let* ((sas (annotations-spanning sen :type 'section-annotation)))
	  (incf (gethash (data (car (last sas))) h-sec 0)))))
    (dolist (kv (hash-table-alist h-sec))
      (format t "~&~a: ~a~%" (car kv) (cdr kv)))))

(defun sen-len-dist (corpn fnout)
  (let* ((h-slen (make-hash-table :test #'equalp))
	 (fout (open fnout :direction :output :if-exists :supersede
		     :if-does-not-exist :create))
	 doc pns)
    (dolist (docid (documents (corpus corpn)))
      (setf doc (document docid))
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(setf pns (annotations-spec sen :type (gtype 'gparse-node-type)))
	(when (> (length pns) 20)
	  (format fout "~&~a_~a: ~a~%" docid (id sen) 
		  (replace-re (content sen) "\\s+" " ")))
	(incf (gethash (length pns) h-slen 0))))
    (close fout)
    (dolist (kv (hash-table-key-ascd-alist h-slen))
      (format t "~&~a: ~a~%" (car kv) (cdr kv)))))

