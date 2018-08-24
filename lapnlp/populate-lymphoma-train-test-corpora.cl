;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/12/2016 creation
Example on how to use populate-instance-set-corpus
|#
;;; Burkitts
(populate-instance-set-corpus "burkitts_negative_test" "burkitts_negative_test" "data:;groundtruth;burkitts_negative_test" :set-desc "derived from burkitts_negative_test" :set-grp-rule "by MRN with no window" :inst-type "mrn")

(populate-instance-set-corpus "burkitts_negative_train" "burkitts_negative_train" "data:;groundtruth;burkitts_negative_train" :set-desc "derived from burkitts_negative_train" :set-grp-rule "by MRN with no window" :inst-type "mrn")


(populate-instance-set-corpus "burkitts_positive_test" "burkitts_positive_test" "data:;groundtruth;burkitts_positive_test" :set-desc "derived from burkitts_positive_test" :set-grp-rule "by MRN with no window" :inst-type "mrn")

(populate-instance-set-corpus "burkitts_positive_train" "burkitts_positive_train" "data:;groundtruth;burkitts_positive_train" :set-desc "derived from burkitts_positive_train" :set-grp-rule "by MRN with no window" :inst-type "mrn")

;;; DLBCL
(populate-instance-set-corpus "dlbcl_negative_test" "dlbcl_negative_test" "data:;groundtruth;dlbcl_negative_test" :set-desc "derived from dlbcl_negative_test" :set-grp-rule "by MRN with no window" :inst-type "mrn")

(populate-instance-set-corpus "dlbcl_negative_train" "dlbcl_negative_train" "data:;groundtruth;dlbcl_negative_train" :set-desc "derived from dlbcl_negative_train" :set-grp-rule "by MRN with no window" :inst-type "mrn")


(populate-instance-set-corpus "dlbcl_positive_test" "dlbcl_positive_test" "data:;groundtruth;dlbcl_positive_test" :set-desc "derived from dlbcl_positive_test" :set-grp-rule "by MRN with no window" :inst-type "mrn")

(populate-instance-set-corpus "dlbcl_positive_train" "dlbcl_positive_train" "data:;groundtruth;dlbcl_positive_train" :set-desc "derived from dlbcl_positive_train" :set-grp-rule "by MRN with no window" :inst-type "mrn")

;;; Follicular
(populate-instance-set-corpus "follicular_negative_test" "follicular_negative_test" "data:;groundtruth;follicular_negative_test" :set-desc "derived from follicular_negative_test" :set-grp-rule "by MRN with no window" :inst-type "mrn")

(populate-instance-set-corpus "follicular_negative_train" "follicular_negative_train" "data:;groundtruth;follicular_negative_train" :set-desc "derived from follicular_negative_train" :set-grp-rule "by MRN with no window" :inst-type "mrn")


(populate-instance-set-corpus "follicular_positive_test" "follicular_positive_test" "data:;groundtruth;follicular_positive_test" :set-desc "derived from follicular_positive_test" :set-grp-rule "by MRN with no window" :inst-type "mrn")

(populate-instance-set-corpus "follicular_positive_train" "follicular_positive_train" "data:;groundtruth;follicular_positive_train" :set-desc "derived from follicular_positive_train" :set-grp-rule "by MRN with no window" :inst-type "mrn")


;;; Hodgkin
(populate-instance-set-corpus "hodgkins_negative_test" "hodgkins_negative_test" "data:;groundtruth;hodgkins_negative_test" :set-desc "derived from hodgkins_negative_test" :set-grp-rule "by MRN with no window" :inst-type "mrn")

(populate-instance-set-corpus "hodgkins_negative_train" "hodgkins_negative_train" "data:;groundtruth;hodgkins_negative_train" :set-desc "derived from hodgkins_negative_train" :set-grp-rule "by MRN with no window" :inst-type "mrn")


(populate-instance-set-corpus "hodgkins_positive_test" "hodgkins_positive_test" "data:;groundtruth;hodgkins_positive_test" :set-desc "derived from hodgkins_positive_test" :set-grp-rule "by MRN with no window" :inst-type "mrn")

(populate-instance-set-corpus "hodgkins_positive_train" "hodgkins_positive_train" "data:;groundtruth;hodgkins_positive_train" :set-desc "derived from hodgkins_positive_train" :set-grp-rule "by MRN with no window" :inst-type "mrn")
