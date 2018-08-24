;;; -*- Mode: Lisp; Package: util -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 02/20/2009 creation
|#

(defpackage :util
  (:use :common-lisp #+allegro :excl)
  (:export "open-new"))

(in-package :util)
(defun open-new (fn)
  (open fn :direction :output :if-exists :supersede :if-does-not-exist :create))
