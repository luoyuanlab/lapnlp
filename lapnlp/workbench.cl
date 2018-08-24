;;; -*- Mode: Lisp; Package: stanford-parser; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/10/2016 creation 

Example on loading and using the LATE system
|#

(defpackage :late
  (:use :common-lisp :util #+allegro :excl)
  (:export 
   "late-init"
   "late-close"))

(in-package :late)

;; is it right to put it here?
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew #\_ *additional-logical-pathname-name-chars*))

(defun load-flow-spec (flow-spec)
  (format t "~&Loading flow spec~%")
  (require flow-spec)
  (cond
   ((equalp (ganalysis 'gsentencize) 'sentencize-opennlp)
    (format t "~&Use opennlp sentencizer~%"))
   ((equalp (ganalysis 'gsentencize) 'line-breaker)
    (format t "~&Use line breaker sentencizer~%"))
   ((null (ganalysis 'gsentencize))
    (format t "~&No gsentencize spec, this step will not be executed.~%"))
   (t 
    (error "invalid gsentencize specification")))

  (cond
   ((equalp (ganalysis 'gcross-ref) 'cross-ref)
    (set-gtype 'gcross-ref-type 'cross-ref-annotation)
    (format t "~&Use cross reference~%"))
   ((null (ganalysis 'gcross-ref))
    (format t "~&No gcross-ref spec, this step will not be executed.~%"))
   (t 
    (error "invalid gcross-ref specification")))

  (cond
   ((equalp (ganalysis 'gformat-mark) 'format-mark)
    (set-gtype 'gformat-mark-type 'format-mark-annotation)
    (format t "~&Use format mark~%"))
   ((null (ganalysis 'gformat-mark))
    (format t "~&No gformat-mark spec, this step will not be executed.~%"))
   (t 
    (error "invalid gformat-mark specification")))

  (cond
   ((equalp (ganalysis 'gquantize) 'quantize)
    (set-gtype 'gquant-type 'quantize-annotation)
    (format t "~&Use quantize~%"))
   ((null (ganalysis 'gquantize))
    (format t "~&No gquantize spec, this step will not be executed.~%"))
   (t 
    (error "invalid gquantize specification")))

  ;; init tokenization part
  ;; g stands for global, pay special attention to ensure consistency among the
  ;; below four parameters.
  (cond
   ((equalp (ganalysis 'gtokenize) 'tokenize-link)
    (set-gtype 'gtoken-type 'lp-token)
    (format t "~&Use link tokenizer~%"))
   ((equalp (ganalysis 'gtokenize) 'tokenize-stanford)
    (set-gtype 'gtoken-type 'sp-token)
    (format t "~&Use stanford tokenizer~%"))
   ((equalp (ganalysis 'gtokenize) 'tokenize-opennlp)
    (set-gtype 'gtoken-type 'opennlp-token)
    (format t "~&Use opennlp tokenizer~%"))
   ((null (ganalysis 'gtokenize))
    (format t "~&No gtokenize spec, this step will not be executed.~%"))
   (t
    (error "invalid gtokenize specification")))

  ;; init the tagize
  (cond
   ((equalp (ganalysis 'gtagize) 'tagize-stanford)
    (set-gtype 'gtag-type 'pos-tag-stanford)
    (format t "~&Use stanford tagger~%"))
   ((equalp (ganalysis 'gtagize) 'tagize-stanford-constrained)
    (set-gtype 'gtag-type 'pos-tag-stanford)
    (format t "~&Use constrianed stanford tagger%"))
   ((equalp (ganalysis 'gtagize) 'tagize-opennlp)
    (set-gtype 'gtag-type 'pos-tag-opennlp)
    (format t "~&Use opennlp tagger~%"))
   ((null (ganalysis 'gtagize))
    (format t "~&No gtagize spec, this step will not be executed.~%"))
   (t
    (error "invalid gtagize specification")))

  ;; init the chunkize
  (cond
   ((equalp (ganalysis 'gchunkize) 'chunkize-opennlp)
    (set-gtype 'gchunk-tag-type 'chunk-tag-opennlp)
    (set-gtype 'gphrase-type 'phrase-opennlp)
    (format t "~&Use opennlp chunker~%"))
   ((null (ganalysis 'gchunkize))
    (format t "~&No gchunkize spec, this step will not be executed.~%"))
   (t
    (error "invalid gchunkize specification")))

  ;; init parse
  (cond
   ((equalp (ganalysis 'gparse) 'parse-link-biased)
    (set-gtype 'gparse-type 'parse-annotation-link-biased)
    (set-gtype 'gparse-node-type 'parse-node-link-biased))
   ((equalp (ganalysis 'gparse) 'parse-stanford-tagged)
    (set-gtype 'gparse-type 'parse-annotation-stanford-tagged)
    (set-gtype 'gparse-node-type 'parse-node-stanford-tagged))
   ((equalp (ganalysis 'gparse) 'parse-stanford-tokenized)
    (set-gtype 'gparse-type 'parse-annotation-stanford-tokenized)
    (set-gtype 'gparse-node-type 'parse-node-stanford-tokenized))
   ((equalp (ganalysis 'gparse) 'parse-stanford-tokenized-constrained)
    (set-gtype 'gparse-type 'parse-annotation-stanford-tokenized-constrained)
    (set-gtype 'gparse-node-type 'parse-node-stanford-tokenized-constrained))
   ((null (ganalysis 'gparse))
    (format t "~&No gparse spec, this step will not be executed.~%"))
   (t
    (error "invalid gparse specification")))

  ;; init hierarchical parse
  (cond
   ((equalp (ganalysis 'ghier-parse) 'parse-link-hier)
    (set-gtype 'ghier-parse-type 'parse-annotation-link-hier)
    (set-gtype 'ghier-parse-node-type 'parse-node-link-hier))
   ((equalp (ganalysis 'ghier-parse) 'parse-stanford-hier-tagged)
    (set-gtype 'ghier-parse-type 'parse-annotation-stanford-hier-tagged)
    (set-gtype 'ghier-parse-node-type 'parse-node-stanford-hier-tagged))
   ((equalp (ganalysis 'ghier-parse) 'parse-stanford-hier-tokenized)
    (set-gtype 'ghier-parse-type 'parse-annotation-stanford-hier-tokenized)
    (set-gtype 'ghier-parse-node-type 'parse-node-stanford-hier-tokenized))
   ((null (ganalysis 'ghier-parse))
    (format t "~&No ghier-parse spec, this step will not be executed.~%"))
   (t
    (error "invalid ghier-parse specification"))))

(defun load-ml-spec (ml-spec)
  (format t "~&Loading machine learning spec file~%")
  (require ml-spec)
  (setf *ml-features* (getf *ml-spec* 'ml-features))
  (setf *cg-conf* (getf *ml-spec* 'cg)))



;; You load jlinker earlier, you run the risk of not loading jar files
;; jl-init in java-util.cl, is there any 
(defun late-init (&key 
		  (flow-spec "late:;flow-spec")
		  (ml-spec "late:;ml-spec")
		  (fn-pcfg "late:;lymph-config.cl")
		  (gazette? nil))
  ;; Read LATE preferences file(s)
  (read-prefs)
  ;; for peristent storage of documents, annotations, etc.
  ;;  (create-late-database t)
  (load-flow-spec flow-spec)
  (load-ml-spec ml-spec)
  (load-pconfig fn-pcfg)
  (open-late-database)
  ;; For retrieval of gazette data
  (when gazette?
    (open-gazette-database))
  ;; (open-wordnet-database)
  ;; for feature-extract; changed!
  ;; (populate-dict)
  ;; for opennlp

  (op:opennlp-env-init)
  (sp:stanford-parser-env-init)
  (bl:biolemmatizer-env-init)
  (mm:metamap-env-init)
  (op::opennlp-init)
  ;; for weka
  ;; (when *weka-home*
  ;;  (weka-init))
  ;; for link and link-parse
  (open-lp)

  ;; to establish Java connection; this must occur after various jar
  ;; and classpath variables are set by other initialization code!!!
;;;  (unless (jlinker-query)
;;;    (jl-init)	
;;;    )
;;;  (jlinker-slot :jar-file *classpath-jars*)
;;;  (jlinker-slot :classpath *classpath-dirs*)  
  ;;
  "Late system initialization complete."
  )

(defun late-close ()
  "Close various components of the LATE system cleanly."
  (jl-end)				;disconnect jlinker
  (close-umls)				;close UMLS database
  (close-late-database)			;close persistent database
  (close-gazette-database)		;close db connection to gazette
  t)
