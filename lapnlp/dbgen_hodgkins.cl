;; -*- Mode: Lisp; Package: late -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
|#
(asdf:load-system :util-sys)
(asdf:load-system :late-sys)

(in-package :user)
(use-package '(:late :excl :excl.osi :common-lisp :concept-graph))
(require :osi)


(late-init)
(generate-train-test-db "hodgkins_positive_train"
			"hodgkins_negative_train"
			"hodgkins_train"
			"hodgkins_positive_test"
			"hodgkins_negative_test"
			"hodgkins_test")
(late-close)
(format t "~&Finished~%")
