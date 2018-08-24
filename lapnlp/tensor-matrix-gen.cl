#! <ACL home>/mlisp -#D
;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/12/2015 rewrite using asdf framework
yluo - 09/05/2013 creation

Example on how to generate lymphoma data train tensors. Test tensors are similar
|#

(load "~/.clinit.cl")
;;(load "late:;sys.fasl")
(asdf:load-system :util-sys)
(asdf:load-system :late-sys)

(in-package :user)
(use-package '(:late :excl :excl.osi :common-lisp :mnegex :cg 
		     :wordnet))
(require :osi)
(late-init)

(cg::h-doc-subgraph-pnode-train-tensor "lymphoma_train" "late:;matlab_data;lymphoma_train_tensor" "late:;matlab_data;lymphoma_train_tensor_wd" "late:;matlab_data;lymphoma_train_pn_list" "late:;matlab_data;lymphoma_train_doc_list" "late:;matlab_data;lymphoma_train_sg_list" "late:;matlab_data;lymphoma_train_wd_list")

(cg::h-subgraph-pnode-matrix "lymphoma_train" "late:;matlab_data;lymphoma_train_sg_pn_matrix" "late:;matlab_data;lymphoma_train_pn_list" "late:;matlab_data;lymphoma_train_sg_list")

;; To output document subgraph sparse matrix for train
(cg::h-doc-subgraph-matrix "lymphoma_train" "late:;matlab_data;lymphoma_train_doc_sg_matrix" "late:;matlab_data;lymphoma_train_doc_list" "late:;matlab_data;lymphoma_train_sg_list")

;; To output subgraph graph sparse matrix
(cg::h-sg-lg-matrix "lymphoma_train" "late:;matlab_data;lymphoma_train_sg_lg_matrix" "late:;matlab_data;lymphoma_train_lg_list" "late:;matlab_data;lymphoma_train_sg_list")

;; To print subgraph str:

(require "concept-graph-exp")
(print-sg "matlab_data/lymphoma_train_sg_list" "matlab_data/lymphoma_train_sg_str_list")

;; Generate groundtruth using lymphoma label as topics (need to follow tensor output)
(output-subgraph-topic-groundtruth '("burkitts_positive_train" "dlbcl_positive_train" "follicular_positive_train" "hodgkins_positive_train") "late:;matlab_data;lymphoma_train_label_subgraph_freq" "late:;matlab_data;lymphoma_train_label_subgraph_spmat" "late:;matlab_data;lymphoma_train_sg_list")


;; Generate document topic groundtruth using lymphoma label:
(output-doc-topic-groundtruth '("burkitts_positive_train" "dlbcl_positive_train" "follicular_positive_train" "hodgkins_positive_train")  "late:;matlab_data;lymphoma_train_label_doc_spmat" "late:;matlab_data;lymphoma_train_doc_list")


;; To output document pnode (covered by subgraph) sparse matrix
(cg:h-doc-pn-matrix "lymphoma_train" "late:;matlab_data;lymphoma_train_doc_pn_matrix" "late:;matlab_data;lymphoma_train_doc_list" "late:;matlab_data;lymphoma_train_pn_list")


;; To output document word (covered by subgraph) sparse matrix
(cg:h-doc-wd-matrix "lymphoma_train" "late:;matlab_data;lymphoma_train_doc_wd_matrix" "late:;matlab_data;lymphoma_train_doc_list" "late:;matlab_data;lymphoma_train_wd_list")

;; To generate all tensor and matrix
(setf bptr (mrns "burkitts_positive_train"))
(setf bntr (mrns "burkitts_negative_train"))
(setf tr (union bptr bntr :test #'equalp))
(cg::h-pt-sg-wd-tensor-matrix tr "pt_sg_wd/train.tensor" "pt_sg_wd/pt_sg_train.spmat" "pt_sg_wd/pt_wd_train.spmat" "pt_sg_wd/sg_wd_train.spmat" "pt_sg_wd/lymphoma_train_mrn_list" "pt_sg_wd/sg_train.tid" "pt_sg_wd/wd_train.tid" "pt_sg_wd/pt_gt_train.spmat")



(setf bptr (mrns "burkitts_positive_train"))
(setf bntr (mrns "burkitts_negative_train"))
(setf bpte (mrns "burkitts_positive_test"))
(setf bnte (mrns "burkitts_negative_test"))
(setf tr (union bptr bntr :test #'equalp))
(setf te (union bpte bnte :test #'equalp))
(setf mrns (union tr te :test #'equalp))
(cg::h-pt-sg-wd-tensor-matrix-context mrns "pt_sg_wd/train.tensor" "pt_sg_wd/train.doctensor" "pt_sg_wd/pt_sg_train.spmat" "pt_sg_wd/pt_wd_train.spmat" "pt_sg_wd/sg_wd_train.spmat" "pt_sg_wd/lymphoma_mrn_list" "pt_sg_wd/sg_train.tid" "pt_sg_wd/wd_train.tid" "pt_sg_wd/doc_train.tid" "pt_sg_wd/pt_gt_train.spmat")
(cg::subnames->sgstr "pt_sg_wd/sg_train.tid" "pt_sg_wd/sg_train.str")



