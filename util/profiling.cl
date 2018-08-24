;;; -*- Mode:Lisp; Package: util; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 11/29/2010 creation
|#
(defpackage :util
  (:use :common-lisp :regexp)
  (:export "time-profiling"))

(in-package :util)

(defmacro time-profiling (&body body)
  `(progn (let* (time-start time-elapsed)
	    (setf time-start (get-internal-real-time))
	    (progn ,@body)
	    (setf time-elapsed (- (get-internal-real-time) time-start))
	    (format t "~&It took ~f secs~%" (/ time-elapsed 1000)))))

