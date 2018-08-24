;;; -*- Mode: Lisp; Package: stanford-parser; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 10/20/2012 creation 
Note: I think it's in general a good idea to qualify symbols not in the main
package rather than use-package unless you are sure there won't be name 
conflict.
|#

(defpackage :biolemmatizer
  (:nicknames :bl)
  (:use #+allegro :excl :common-lisp :javatools.jlinker :jc :umlsize))

;;; in-package has effect at both compile time and load time
(in-package :biolemmatizer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :biolemmatizer-wrapper-jarlist "late:;biolemmatizer-wrapper-jarlist"))

(defun load-lemmatizer ()
  "Defer lemmatizer loading to when it's needed."
  (unless *biolemmatizer*
    (setf *biolemmatizer* (blwrapper.LemmatizerMEDG:LemmatizerMEDG))))


(defun get-lemma (str pos)
  (load-lemmatizer)
  (blwrapper.LemmatizerMEDG:getLemma *biolemmatizer* str pos))
