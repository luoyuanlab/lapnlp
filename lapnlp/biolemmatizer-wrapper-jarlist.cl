;;; -*- Mode: Lisp; Package: stanford-parser; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 10/20/2012 creation 
Note: the Biolemmatizer wrapper jar should be in the same directory as the 
Biolemmatizer jar.
|#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :blwrapper.LemmatizerMEDG "<lemmatizer home directory>")
)
