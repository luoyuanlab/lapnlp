;;; -*- Mode: Lisp; Package: late -*-
#| 
yluo - 08/16/2018 clean and reorganization
yluo - 06/10/2015 rewrite using asdf framework
yluo - 06/01/2013 creation 
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
	     'cg t
	     'metamap-con t
	     'fmetamap-con t
	     'ic nil
	     'icp nil
	     'karyotype nil)
       
       'cg
       (list 'nontrivial t
	     'closure nil
	     'suppressed t)))
