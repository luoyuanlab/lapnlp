;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/12/2015 rewrite using asdf framework
yluo - 09/05/2013 creation

This is template to generate parallel scripts on LSF high performance computing clusters, to be used by the corresponding function in corpus.cl. When it encounters [exe] and [setting] line, it will auto-replace them with corresponding directives.
|#

[exe]
(load "~/.clinit.cl")
;;(load "late:;sys.fasl")
(asdf:load-system :util-sys)
(asdf:load-system :late-sys)

(in-package :user)
(use-package '(:late :excl :excl.osi :common-lisp :concept-graph))
(require :osi)
(late-init)

[job]

(late-close)
(format t "~&Finished~%")
