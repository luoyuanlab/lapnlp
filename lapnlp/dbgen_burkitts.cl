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
(generate-train-test-db "burkitts_positive_train"
			"burkitts_negative_train"
			"burkitts_train"
			"burkitts_positive_test"
			"burkitts_negative_test"
			"burkitts_test")
(late-close)
(format t "~&Finished~%")
