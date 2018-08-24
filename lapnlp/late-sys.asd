;;; -*- Mode: Lisp; Package: User; -*-
#|
yluo - 08/16/2018 pass
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation

To load LATE using asdf, do 
(asdf:load-system :util-sys), then 
(asdf:load-system :late-sys), then 
(use-package :late) and 
(late-init)
|#

(defpackage :late-sys
  (:use :cl :asdf))
(in-package :late-sys)
"Load the LATE system and perform needed initializations for it."

;;; The system definition for Late:
(defsystem :late-sys 
  :description "The system definition for LATE (Lisp Architecture of Text 
                  Engineering)"
  :version "0.5"
  :author "Peter Szolovits <psz@mit.edu>, Yuan Luo <yuan.luo@northwestern.edu>"
  :licence "Public Domain"
  :serial t
  :default-component-class cl-source-file.cl
  :components ((:module :mysql)
	       (:module :datetime)
	       (:module :regexp2)
	       (:module :jlinker)
	       (:module :foreign)
	       (:module :util-sys)
	       (:file "config")
	       (:file "late")
	       (:file "link-interface")
	       (:file "link")
	       (:file "annotations")
	       (:file "java-connect")
	       (:file "java-wrap-gen")
	       (:file "list-gen")
	       (:file "stanford-parser-env")
	       (:file "persistence")
	       (:file "import-gazettes")
	       (:file "gazette")
	       (:file "opennlp-env")
	       (:file "porter-stemmer")
	       (:file "biolemmatizer-env")
	       ;; "weka-env"
	       (:file "wordnet")
	       (:file "libsvm")
	       (:file "norm")
	       (:file "merge-inst")
	       (:file "findstruct")
	       (:file "cross-ref")
	       (:file "quantize")
	       (:file "format-mark")
	       (:file "sentencize")
	       (:file "tokenize")
	       (:file "karyotype")
	       (:file "tagize")
	       (:file "umlsize")
	       (:file "coded-text")
	       (:file "ne-list")
	       (:file "range-adj")
	       (:file "link-utils")
	       (:file "chunkize")
	       (:file "mask")
	       (:file "metamap-env")
	       (:file "link-parse")
	       (:file "parse")
	       (:file "concept-graph-reader")
	       (:file "concept-graph")
	       (:file "concept-graph-writer")
	       (:file "concept-graph-stats")
	       (:file "concept-graph-path")
	       (:file "mnegex")
	       (:file "number-recognize")
	       (:file "import")
	       (:file "mimic")
	       (:file "import-deid")
	       (:file "item-content-pair")
	       (:file "feature-extract")
	       (:file "boostexter-gen")
	       (:file "feature-vector")
	       (:file "document")
	       (:file "corpus")
	       (:file "workbench")))





