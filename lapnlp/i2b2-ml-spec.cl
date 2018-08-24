;;; -*- Mode: Lisp; Package: User; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
|#

(defpackage :late)
(in-package :late)

(setf 
 *ml-spec*
 (list 'ml-features  
       (list 'unigram t
	     'bigram t
	     'trigram t
	     'use-stoplist t
	     'cg nil
	     'ic nil
	     'icp nil
	     'karyotype nil)))
