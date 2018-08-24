;;; -*- Mode: Lisp; Package: stanford-parser; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/10/2015 rewrite using asdf framework
yluo - 03/30/2012 creation 

Note: the Stanford Parser wrapper jar should be in the same directory as the 
Stanford Parser jar.
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :spwrapper.ParserMEDG "./spwrapper/ParserMEDG")
)
