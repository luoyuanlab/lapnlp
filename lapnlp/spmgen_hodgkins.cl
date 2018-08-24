#! <ACL home>/mlisp -#D
;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/12/2016 creation
Example on how to use generate-train-test-spmat-db
|#

(load "~/.clinit.cl")
;;(load "late:;sys.fasl")
(asdf:load-system :util-sys)
(asdf:load-system :late-sys)

(in-package :user)
(use-package '(:late :excl :excl.osi :common-lisp :concept-graph))
(require :osi)


(late-init)
(generate-train-test-spmat-db "hodgkins_positive_train"
			      "hodgkins_negative_train"
			      "hodgkins_train"
			      "hodgkins_positive_test"
			      "hodgkins_negative_test"
			      "hodgkins_test")
(late-close)
(format t "~&Finished~%")
