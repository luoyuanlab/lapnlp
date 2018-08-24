;;; -*- Mode: Lisp; Package: stanford-parser; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/10/2015 rewrite using asdf framework
yluo - 03/30/2012 creation 

Note: the Opennlp wrapper jar should be in the same directory as the 
Opennlp jar.
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :opwrapper.OpennlpMEDG "/PHShome/yl960/Code/Lisp/late/opwrapper/OpennlpMEDG")

)
