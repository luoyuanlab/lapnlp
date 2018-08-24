;;; -*- Mode: Lisp; Package: User; -*-
#|
yluo - 08/16/2018 pass
yluo - 06/26/2015 rewrite using asdf framework
psz  -            creation


To start a LATE session, assuming all dependencies are set up
correctly, as described in config.cl, one simply needs to do the
following things at top-level in ACL:
To load LATE using asdf, do 
(asdf:load-system :util-sys), then 
(asdf:load-system :late-sys), then 
(use-package :late) 
and (late-init)

|#

(defpackage :util-sys
  (:use :cl :asdf))
(in-package :util-sys)
"Load the utility system and perform needed initializations for it.
 Define :util-sys to include the subset of utilities in util:; that
 we need."
(defsystem :util-sys
  :description "The utility system definition for LATE"
  :version "0.5"
  :author "Peter Szolovits <psz@mit.edu>, Yuan Luo <yuan.luo@northwestern.edu>"
  :licence "Public Domain"
  :serial t
  :default-component-class cl-source-file.cl
  ;; :depends-on (:mysql :datetime :regexp2)
  :components ((:module :mysql)
	       (:module :datetime)
	       (:module :regexp2)
	       (:file "for-dir")
	       (:file "osutil")
	       (:file "count")
	       (:file "profiling")
	       (:file "string")
	       (:file "hash-utils")
	       (:file "set-util")
	       (:file "matrix-utils")
	       (:file "rand-util")
	       (:file "defmemo")
	       (:file "read-xml")
	       (:file "read-file")
	       (:file "write-file")
	       (:file "read-csv")
	       (:file "interval-tree")
	       (:file "print-table")
	       (:file "partition")))








