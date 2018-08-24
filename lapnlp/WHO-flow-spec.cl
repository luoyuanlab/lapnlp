;;; -*- Mode: Lisp; Package: User -*-
#||
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
yluo - 11/01/2012 creation
||#
(defpackage :late)
(in-package :late)


(setf 
 *flow-spec*
 (list 'gsentencize
       (list 'analysis 'sentencize-opennlp
	     'ver "1.5.2"
	     'setting nil
	     'split "\n *\n")

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
