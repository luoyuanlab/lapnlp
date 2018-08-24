;;; -*- Mode: Lisp; Package: late; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
|#
(defpackage :late)
(in-package :late)


(setf 
 *flow-spec*
 (list 'gsentencize
       (list 'analysis 'sentencize-opennlp
	     'ver "1.5.2"
	     'setting nil
	     'split "\\n *\\n")

       'gcross-ref
       (list 'analysis 'cross-ref
	     'ver "1.0"
	     'setting "mgh-path")

       'gformat-mark
       (list 'analysis 'format-mark
	     'ver "1.0"
	     'setting "mgh-path")

       'gquantize
       (list 'analysis 'quantize
	     'ver "1.0"
	     'setting nil)

       ;; 'gsentence-expand
       ;; (list 'analysis 'sentence-rewrite
       ;; 		 'ver nil
       ;; 		 'setting nil)

       'gcoded-text
       (list 'analysis 'coded-text
	     'ver "1.0"
	     'setting "pmdb")

       'gif-list
       (list 'analysis 'if-list
	     'ver "1.0"
	     'setting "default")

       'gif-adj
       (list 'analysis 'if-adj
	     'ver "1.0"
	     'setting "default")
       
       'grange-adj
       (list 'analysis 'range-adj
	     'ver "1.0"
	     'setting "default")

       ;; tokenize-stanford: sp-token; tokenize-opennlp: opennlp-token
       'gtokenize
       (list 'analysis 'tokenize-link
	     'ver "4.7.4"
	     'setting "customized")

       'gnum-recognize
       (list 'analysis 'num-recognize
	     'ver "1.0"
	     'setting nil)

       ;; tagize-stanford: pos-tag-stanford; 
       ;; tagize-stanford-constrained: pos-tag-stanford
       'gtagize
       (list 'analysis 'tagize-opennlp
	     'ver "1.5.2"
	     'setting "umls-tag")

       'gchunkize
       (list 'analysis 'chunkize-opennlp
	     'ver "1.5.2"
	     'setting "umls-tag")

       'gparse
       (list 'analysis 'parse-stanford-tagged
	     'ver "2012-03-09"
	     'setting "umls-tag"
	     'allcap2normal t)

       'gumlsize
       (list 'analysis 'umlsize
	     'ver "2011AB"
	     'setting "str->cui")

       ;; 'gmetamap
       ;; (list 'analysis 'metamap
       ;; 		 'ver "2012"
       ;; 		 'setting "all-wsd-neg")

       'gmask
       (list 'analysis 'mask
	     'ver "1.0"
	     'setting "tui-mask")
       
       'gic-pair
       (list 'analysis 'ic-paired
	     'ver "1.0"
	     'setting "num, enum")

       'gimmuchem
       (list 'analysis 'immuchem
	     'ver "1.0"
	     'setting "default")

       'gconcept-graph
       (list 'analysis 'concept-graph
	     'ver "1.0"
	     'setting "plain, parse-stanford-tagged"
	     'graph-type "plain_graph"
	     'outdir nil
	     'umls 'cui
	     'max-entity-stn-depth 3
	     'max-event-stn-depth 4
	     'min-cg-size 1
	     'noun-node "mesh")

       'ghierarchize
       (list 'analysis 'hierarchize-syn-sem
	     'ver "1.0"
	     'setting "umls-and-NP")
       
       'ghier-parse
       (list 'analysis 'parse-stanford-hier-tagged
	     'ver "2012-03-09"
	     'setting "umls-tag, umls-and-NP-pnode")

       'ghier-concept-graph
       (list 'analysis 'hier-concept-graph
	     'ver "1.0"
	     'setting "plain, parse-stanford-hier-tagged"
	     'graph-type "plain_graph"
	     'outdir nil
	     'umls 'cui
	     'max-entity-stn-depth 3
	     'max-event-stn-depth 4
	     'min-cg-size 1)))
