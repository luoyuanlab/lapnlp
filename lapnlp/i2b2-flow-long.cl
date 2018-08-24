;;; -*- Mode: Lisp; Package: User; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/26/2015 rewrite using asdf framework
|#
'(gsentencize 
  '(exec line-breaker
	ver nil 
	setting nil
	sentence-split "\n")
  
  gcross-ref
  '(exec cross-ref
	ver 1.0
	setting i2b2
	ann-type cross-ref-annotation)

  gformat-mark
  '(exec format-mark
	ver 1.0
	setting i2b2
	ann-type format-mark-annotation)

  gquantize
  '(exec quantize
	ver 1.0
	setting nil
	ann-type quantize-annotation)

  gtokenize
  '(exec tokenize-link
	ver 4.7.4 ; need to check link parser
	setting customized)
  
  gnum-recognize
  '(exec num-recognize
	ver 1.0
	setting nil) 
  
  
  gtagize
  '(exec tagize-opennlp
	ver 1.5.2
	setting umls-tag)
  
  gchunkize
  '(exec chunkize-opennlp
	ver 1.5.2
	setting umls-tag)
  
  gparse
  '(exec parse-stanford-tagged
	ver 2012-02-03
	setting umls-tag
	allcap2normal yes)

  gumlsize
  '(exec umlsize
	ver 2011AB
	setting str->cui)


  gmask
  '(exec mask
	ver 1.0
	setting tui-mask)
  
  gic-pair
  '(exec ic-paired
	ver 1.0
	setting "num, enum") ; maybe list?
  
  gimmuchem
  '(exec immuchem
	ver 1.0
	setting default)
  
  gconcept-graph
  '(exec concept-graph
	ver 1.0
	setting "plain, parse-stanford-tagged"
	event-type plain_graph				; or factor_graph
	outdir nil
	umls tui-annotation
	max-entity-stn-depth 3
	max-event-stn-depth 4
	min-size 1 ; was using 3
	noun-node mesh)
  
  ghierarchize
  '(exec hierarchize-syn-sem
	ver 1.0
	setting umls-and-NP)


  ghier-parse
  '(exec parse-stanford-hier-tagged
	ver 2012-03-09
	setting "umls-tag, umls-and-NP-pnode")


  ghier-concept-graph
  '(exec hier-concept-graph
	ver: 1.0
	setting "plain, parse-stanford-hier-tagged"
	hier-concept-graph-type plain_graph ; or factor_graph
	outdir nil
	umls tui-annotation
	max-entity-stn-depth 3
	max-event-stn-depth 4
	min-size 1)) ; was using 3
