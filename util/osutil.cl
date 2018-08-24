;;; -*- Mode:Lisp; Package: util; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 12/01/2010 creation
|#
(defpackage :util
  (:use :common-lisp :regexp :excl.osi)
  (:export "cmd-verbose"))

(in-package :util)

(defun cmd-verbose (cmd &key (dir nil))
  (multiple-value-bind (stdout stderr exit)
      (command-output cmd :directory dir)
    (assert (= exit 0)
	()
      "~&Error: ~a~%Output: ~a~%" stderr stdout)
    (format t "~&stdout: ~a~%stderr: ~a~%" stdout stderr)))

