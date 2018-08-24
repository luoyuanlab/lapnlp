;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/12/2015 rewrite using asdf framework
yluo - 09/05/2013 creation

This is template to generate parallel scripts on LSF high performance computing clusters, to be used by the psg-pn-matrix function in corpus.cl. When it encounters [exe] and [setting] line, it will auto-replace them with corresponding directives.
|#

[exe]
(load "~/.clinit.cl")
;;(load "late:;sys.fasl")
(asdf:load-system :util-sys)
(asdf:load-system :late-sys)

(in-package :user)
(use-package '(:late :excl :excl.osi :common-lisp :concept-graph))
(require :osi)
[setting]

(defvar fnsig (format nil "~a~a.sg" mat-ldir idx))
(defvar fnlg (format nil "~algmgh_train_hier.txt" sg-ldir))
(defvar fnldac (format nil "~a~a.mat" mat-ldir idx))
(defvar fnvsg (format nil "~a~a.vsg" mat-ldir idx))
(defvar fnvocab "late:;pnode.vocab")
(late-init)
(subgraph-pnode-matrix fnsig fnlg fnvsg fnldac fnvocab)
(late-close)
(format t "~&Finished~%")
