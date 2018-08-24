;;; -*- Mode: Lisp; Package: User -*-
#||
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 11/01/2012 creation
||#

(defpackage :WHO
  (:use :late :excl :excl.osi :common-lisp :util :norm :umlsize)
  (:export
   "import-WHO-corpora"
   "immunophenotype-sectionp"
   "immunophenotype-marker"
   "immunophenotype-chemical"
   "immunophenotype-cui"
   "load-immunologic-factor-list"
   "abbreviations"
   ))

(in-package :WHO)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :osi))

(defparameter *debug-WHO-exp* nil)

(defun import-WHO-corpora ()
  (unless (corpus "WHO_HemeOnc")
    (format t "~%importing corpus WHO Heme/Onc~%")
    (import-corpus "data:;WHO_HemeOnc;*" 
		   :name "WHO_HemeOnc" 
		   :spec "late:;WHO-HemeOnc.xml")))

(defun immunophenotype-sectionp (sa)
  (and (not (typep sa 'section-head-annotation))
       (equalp "Immunophenotype" (data sa))))

(defun immunophenotype-marker (&aux h-marker)
  (setf h-marker (make-hash-table :test #'equalp))
  (dolist (docid (documents (corpus "WHO_HemeOnc")))
    (let* ((doc (document docid)))
      (dolist (sec (annotations doc :type 'section-annotation
				:filter #'immunophenotype-sectionp))
	(dolist (tok (annotations-spec sec :type (gtype 'gtoken-type)))
	  (when (and (not (match-re "[a-z]" (content tok)))
		     (match-re "[A-Z]" (content tok)))
	    (incf (gethash (content tok) h-marker 0)))))))
  (dolist (marker-freq (hash-table-val-desc-alist h-marker))
    (format t "~&~a: ~a~%" (car marker-freq) (cdr marker-freq))))

(defun immunophenotype-chemical (&aux h-marker)
  (setf h-marker (make-hash-table :test #'equalp))
  (dolist (docid (documents (corpus "WHO_HemeOnc")))
    (let* ((doc (document docid)))
      (dolist (sec (annotations doc :type 'section-annotation
				:filter #'immunophenotype-sectionp))
	(dolist (tui (annotations sec :type 'tui-annotation))
	  (let* ((str-tui (data tui))
		 (stn (tui->stn str-tui)))
	    (when (search "A1.4.1" stn)
	      ;; (format t "~&~a~%" tui)
	      (incf (gethash (content tui) h-marker 0))))))))
  (dolist (marker-freq (hash-table-val-desc-alist h-marker))
    (format t "~&~a: ~a~%" (car marker-freq) (cdr marker-freq))))

(defun abbreviations (&aux h-abbr)
  (setf h-abbr (make-hash-table :test #'equalp))
  (dolist (docid (documents (corpus "WHO_HemeOnc")))
    (let* ((doc (document docid))
	   pns gov-pn)
      (dolist (sen (annotations doc :type 'sentence-annotation))
	(setf pns (annotations sen :type 'parse-node-stanford-hier-tagged))
	(dolist (pn pns)
	  (dolist (ne (sp::dep-deps pn))
	    (when (and (equalp (sp::d-rel ne) "appos") 
		       (match-re "^[A-Z\\-/]+$" (content pn)))
	      (setf gov-pn (find-annotation (sp::d-govid ne) doc))
	      (setf (gethash (content pn) h-abbr) (content gov-pn))))))))
  
  (dolist (abbr (hash-table-alist h-abbr))
    (format t "~&~a: ~a~%" (car abbr) (cdr abbr))))

(defun immunophenotype-cui (fn)
  "Retrieves the CUIs for selected immunophenotype"
  (let* ((h-ip (make-hash-table :test #'equalp))
	 ln)
    (with-open-file (f fn :direction :input)
		    (loop (unless (setf ln (read-line f nil nil)) (return))
			  (setf ln (replace-re ln "(^\\s+|\\s+$)" ""))
			  (setf (gethash ln h-ip) (str->cui ln))))
    
    (dolist (ip-cui (hash-table-alist h-ip))
      (format t "~&~a: ~a~%" (car ip-cui) (cdr ip-cui)))))

