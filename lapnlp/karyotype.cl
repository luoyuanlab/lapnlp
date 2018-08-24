;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 01/03/2013 creation

Utility for capturing karyotype expression in documents, such as
t(14;18)
|#

(defpackage :late
  (:use :common-lisp :util)
  (:export
   "has-karyotype"))

(in-package :late)
(defun has-karyotype (ann &aux str doc secs)
  (setf doc (document ann))
  (setf str (content ann))
  (setf secs (annotations-spanning ann :type 'section-annotation))
;;;  (format t "~&secs are: ~{~a ~}~%" (mapcar #'data secs))
;;;  (format t "~&testing karyotype for ann ~a~%" ann)
  (when (or (match-re "^CG" (name doc))
	    (intersection (mapcar #'data secs)
			  '("CYTOGENETICS_Addendum" "MOLECULAR_GENETIC_STUDIES_Addendum") 
			  :test #'equalp))
;;;	(format t "~&str is ~a~%" str)
    (cond
     ((match-re "^(add|cen|del|der|dia|dic|dmin|dup|e|fis|fra|g|h|hsr|idem|ider|idic|inc|ins|inv|lep|mat|med|min|mos|oom|pac|pat|pcc|pcd|prx|psu|pvz|qdp|r|rcp|rea|rec|rob|s|sce|sct|spm|t|tan|tas|tel|ter|tr|trc|trp|upd|v|xma|zyg)\\(" str)
      t)
     ((match-re "\\b(trisomy|monosomy|deletion|addition|translocation|insertion|duplication|inversion|triplication)\\b" str)
      t)
     (t
      nil))))
