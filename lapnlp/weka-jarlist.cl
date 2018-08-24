;;; -*- Mode: Lisp; Package: stanford-parser; -*-
#|
yluo - 08/16/2018 clean and reorganization
yluo - 06/10/2015 rewrite using asdf framework
yluo - 03/30/2012 creation 

Note: the WEKA wrapper jar should be in the same directory as the 
WEKA jar.
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :weka.associations.AbstractAssociator "/home/barnard/Code/Lisp/late/weka/associations/AbstractAssociator")

  (require :weka.associations.Apriori "/home/barnard/Code/Lisp/late/weka/associations/Apriori")

  (require :weka.associations.AprioriItemSet "/home/barnard/Code/Lisp/late/weka/associations/AprioriItemSet")

  (require :weka.associations.Associator "/home/barnard/Code/Lisp/late/weka/associations/Associator")

  (require :weka.associations.AssociatorEvaluation "/home/barnard/Code/Lisp/late/weka/associations/AssociatorEvaluation")

  (require :weka.associations.CARuleMiner "/home/barnard/Code/Lisp/late/weka/associations/CARuleMiner")

  (require :weka.associations.CaRuleGeneration "/home/barnard/Code/Lisp/late/weka/associations/CaRuleGeneration")

  (require :weka.associations.CheckAssociator "/home/barnard/Code/Lisp/late/weka/associations/CheckAssociator")

  (require :weka.associations.FilteredAssociator "/home/barnard/Code/Lisp/late/weka/associations/FilteredAssociator")

  (require :weka.associations.GeneralizedSequentialPatterns "/home/barnard/Code/Lisp/late/weka/associations/GeneralizedSequentialPatterns")

  (require :weka.associations.HotSpot "/home/barnard/Code/Lisp/late/weka/associations/HotSpot")

  (require :weka.associations.ItemSet "/home/barnard/Code/Lisp/late/weka/associations/ItemSet")

  (require :weka.associations.LabeledItemSet "/home/barnard/Code/Lisp/late/weka/associations/LabeledItemSet")

  (require :weka.associations.PredictiveApriori "/home/barnard/Code/Lisp/late/weka/associations/PredictiveApriori")

  (require :weka.associations.PriorEstimation "/home/barnard/Code/Lisp/late/weka/associations/PriorEstimation")

  (require :weka.associations.RuleGeneration "/home/barnard/Code/Lisp/late/weka/associations/RuleGeneration")

  (require :weka.associations.RuleItem "/home/barnard/Code/Lisp/late/weka/associations/RuleItem")

  (require :weka.associations.SingleAssociatorEnhancer "/home/barnard/Code/Lisp/late/weka/associations/SingleAssociatorEnhancer")

  (require :weka.associations.Tertius "/home/barnard/Code/Lisp/late/weka/associations/Tertius")

  (require :weka.associations.gsp.Element "/home/barnard/Code/Lisp/late/weka/associations/gsp/Element")

  (require :weka.associations.gsp.Sequence "/home/barnard/Code/Lisp/late/weka/associations/gsp/Sequence")

  (require :weka.associations.tertius.AttributeValueLiteral "/home/barnard/Code/Lisp/late/weka/associations/tertius/AttributeValueLiteral")

  (require :weka.associations.tertius.Body "/home/barnard/Code/Lisp/late/weka/associations/tertius/Body")

  (require :weka.associations.tertius.Head "/home/barnard/Code/Lisp/late/weka/associations/tertius/Head")

  (require :weka.associations.tertius.IndividualInstance "/home/barnard/Code/Lisp/late/weka/associations/tertius/IndividualInstance")

  (require :weka.associations.tertius.IndividualInstances "/home/barnard/Code/Lisp/late/weka/associations/tertius/IndividualInstances")

  (require :weka.associations.tertius.IndividualLiteral "/home/barnard/Code/Lisp/late/weka/associations/tertius/IndividualLiteral")

  (require :weka.associations.tertius.Literal "/home/barnard/Code/Lisp/late/weka/associations/tertius/Literal")

  (require :weka.associations.tertius.LiteralSet "/home/barnard/Code/Lisp/late/weka/associations/tertius/LiteralSet")

  (require :weka.associations.tertius.Predicate "/home/barnard/Code/Lisp/late/weka/associations/tertius/Predicate")

  (require :weka.associations.tertius.Rule "/home/barnard/Code/Lisp/late/weka/associations/tertius/Rule")

  (require :weka.associations.tertius.SimpleLinkedList "/home/barnard/Code/Lisp/late/weka/associations/tertius/SimpleLinkedList")

  (require :weka.attributeSelection.ASEvaluation "/home/barnard/Code/Lisp/late/weka/attributeSelection/ASEvaluation")

  (require :weka.attributeSelection.ASSearch "/home/barnard/Code/Lisp/late/weka/attributeSelection/ASSearch")

  (require :weka.attributeSelection.AttributeEvaluator "/home/barnard/Code/Lisp/late/weka/attributeSelection/AttributeEvaluator")

  (require :weka.attributeSelection.AttributeSelection "/home/barnard/Code/Lisp/late/weka/attributeSelection/AttributeSelection")

  (require :weka.attributeSelection.AttributeSetEvaluator "/home/barnard/Code/Lisp/late/weka/attributeSelection/AttributeSetEvaluator")

  (require :weka.attributeSelection.AttributeTransformer "/home/barnard/Code/Lisp/late/weka/attributeSelection/AttributeTransformer")

  (require :weka.attributeSelection.BestFirst "/home/barnard/Code/Lisp/late/weka/attributeSelection/BestFirst")

  (require :weka.attributeSelection.CfsSubsetEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/CfsSubsetEval")

  (require :weka.attributeSelection.CheckAttributeSelection "/home/barnard/Code/Lisp/late/weka/attributeSelection/CheckAttributeSelection")

  (require :weka.attributeSelection.ChiSquaredAttributeEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/ChiSquaredAttributeEval")

  (require :weka.attributeSelection.ClassifierSubsetEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/ClassifierSubsetEval")

  (require :weka.attributeSelection.ConsistencySubsetEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/ConsistencySubsetEval")

  (require :weka.attributeSelection.CostSensitiveASEvaluation "/home/barnard/Code/Lisp/late/weka/attributeSelection/CostSensitiveASEvaluation")

  (require :weka.attributeSelection.CostSensitiveAttributeEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/CostSensitiveAttributeEval")

  (require :weka.attributeSelection.CostSensitiveSubsetEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/CostSensitiveSubsetEval")

  (require :weka.attributeSelection.ErrorBasedMeritEvaluator "/home/barnard/Code/Lisp/late/weka/attributeSelection/ErrorBasedMeritEvaluator")

  (require :weka.attributeSelection.ExhaustiveSearch "/home/barnard/Code/Lisp/late/weka/attributeSelection/ExhaustiveSearch")

  (require :weka.attributeSelection.FCBFSearch "/home/barnard/Code/Lisp/late/weka/attributeSelection/FCBFSearch")

  (require :weka.attributeSelection.FilteredAttributeEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/FilteredAttributeEval")

  (require :weka.attributeSelection.FilteredSubsetEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/FilteredSubsetEval")

  (require :weka.attributeSelection.GainRatioAttributeEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/GainRatioAttributeEval")

  (require :weka.attributeSelection.GeneticSearch "/home/barnard/Code/Lisp/late/weka/attributeSelection/GeneticSearch")

  (require :weka.attributeSelection.GreedyStepwise "/home/barnard/Code/Lisp/late/weka/attributeSelection/GreedyStepwise")

  (require :weka.attributeSelection.HoldOutSubsetEvaluator "/home/barnard/Code/Lisp/late/weka/attributeSelection/HoldOutSubsetEvaluator")

  (require :weka.attributeSelection.InfoGainAttributeEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/InfoGainAttributeEval")

  (require :weka.attributeSelection.LFSMethods "/home/barnard/Code/Lisp/late/weka/attributeSelection/LFSMethods")

  (require :weka.attributeSelection.LatentSemanticAnalysis "/home/barnard/Code/Lisp/late/weka/attributeSelection/LatentSemanticAnalysis")

  (require :weka.attributeSelection.LinearForwardSelection "/home/barnard/Code/Lisp/late/weka/attributeSelection/LinearForwardSelection")

  (require :weka.attributeSelection.OneRAttributeEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/OneRAttributeEval")

  (require :weka.attributeSelection.PrincipalComponents "/home/barnard/Code/Lisp/late/weka/attributeSelection/PrincipalComponents")

  (require :weka.attributeSelection.RaceSearch "/home/barnard/Code/Lisp/late/weka/attributeSelection/RaceSearch")

  (require :weka.attributeSelection.RandomSearch "/home/barnard/Code/Lisp/late/weka/attributeSelection/RandomSearch")

  (require :weka.attributeSelection.RankSearch "/home/barnard/Code/Lisp/late/weka/attributeSelection/RankSearch")

  (require :weka.attributeSelection.RankedOutputSearch "/home/barnard/Code/Lisp/late/weka/attributeSelection/RankedOutputSearch")

  (require :weka.attributeSelection.Ranker "/home/barnard/Code/Lisp/late/weka/attributeSelection/Ranker")

  (require :weka.attributeSelection.ReliefFAttributeEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/ReliefFAttributeEval")

  (require :weka.attributeSelection.SVMAttributeEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/SVMAttributeEval")

  (require :weka.attributeSelection.ScatterSearchV1 "/home/barnard/Code/Lisp/late/weka/attributeSelection/ScatterSearchV1")

  (require :weka.attributeSelection.StartSetHandler "/home/barnard/Code/Lisp/late/weka/attributeSelection/StartSetHandler")

  (require :weka.attributeSelection.SubsetEvaluator "/home/barnard/Code/Lisp/late/weka/attributeSelection/SubsetEvaluator")

  (require :weka.attributeSelection.SubsetSizeForwardSelection "/home/barnard/Code/Lisp/late/weka/attributeSelection/SubsetSizeForwardSelection")

  (require :weka.attributeSelection.SymmetricalUncertAttributeEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/SymmetricalUncertAttributeEval")

  (require :weka.attributeSelection.SymmetricalUncertAttributeSetEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/SymmetricalUncertAttributeSetEval")

  (require :weka.attributeSelection.UnsupervisedAttributeEvaluator "/home/barnard/Code/Lisp/late/weka/attributeSelection/UnsupervisedAttributeEvaluator")

  (require :weka.attributeSelection.UnsupervisedSubsetEvaluator "/home/barnard/Code/Lisp/late/weka/attributeSelection/UnsupervisedSubsetEvaluator")

  (require :weka.attributeSelection.WrapperSubsetEval "/home/barnard/Code/Lisp/late/weka/attributeSelection/WrapperSubsetEval")

  (require :weka.classifiers.BVDecompose "/home/barnard/Code/Lisp/late/weka/classifiers/BVDecompose")

  (require :weka.classifiers.BVDecomposeSegCVSub "/home/barnard/Code/Lisp/late/weka/classifiers/BVDecomposeSegCVSub")

  (require :weka.classifiers.CheckClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/CheckClassifier")

  (require :weka.classifiers.CheckSource "/home/barnard/Code/Lisp/late/weka/classifiers/CheckSource")

  (require :weka.classifiers.Classifier "/home/barnard/Code/Lisp/late/weka/classifiers/Classifier")

  (require :weka.classifiers.CostMatrix "/home/barnard/Code/Lisp/late/weka/classifiers/CostMatrix")

  (require :weka.classifiers.EnsembleLibrary "/home/barnard/Code/Lisp/late/weka/classifiers/EnsembleLibrary")

  (require :weka.classifiers.EnsembleLibraryModel "/home/barnard/Code/Lisp/late/weka/classifiers/EnsembleLibraryModel")

  (require :weka.classifiers.EnsembleLibraryModelComparator "/home/barnard/Code/Lisp/late/weka/classifiers/EnsembleLibraryModelComparator")

  (require :weka.classifiers.Evaluation "/home/barnard/Code/Lisp/late/weka/classifiers/Evaluation")

  (require :weka.classifiers.IntervalEstimator "/home/barnard/Code/Lisp/late/weka/classifiers/IntervalEstimator")

  (require :weka.classifiers.IteratedSingleClassifierEnhancer "/home/barnard/Code/Lisp/late/weka/classifiers/IteratedSingleClassifierEnhancer")

  (require :weka.classifiers.IterativeClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/IterativeClassifier")

  (require :weka.classifiers.JythonClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/JythonClassifier")

  (require :weka.classifiers.MultipleClassifiersCombiner "/home/barnard/Code/Lisp/late/weka/classifiers/MultipleClassifiersCombiner")

  (require :weka.classifiers.RandomizableClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/RandomizableClassifier")

  (require :weka.classifiers.RandomizableIteratedSingleClassifierEnhancer "/home/barnard/Code/Lisp/late/weka/classifiers/RandomizableIteratedSingleClassifierEnhancer")

  (require :weka.classifiers.RandomizableMultipleClassifiersCombiner "/home/barnard/Code/Lisp/late/weka/classifiers/RandomizableMultipleClassifiersCombiner")

  (require :weka.classifiers.RandomizableSingleClassifierEnhancer "/home/barnard/Code/Lisp/late/weka/classifiers/RandomizableSingleClassifierEnhancer")

  (require :weka.classifiers.SingleClassifierEnhancer "/home/barnard/Code/Lisp/late/weka/classifiers/SingleClassifierEnhancer")

  (require :weka.classifiers.Sourcable "/home/barnard/Code/Lisp/late/weka/classifiers/Sourcable")

  (require :weka.classifiers.UpdateableClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/UpdateableClassifier")

  (require :weka.classifiers.bayes.AODE "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/AODE")

  (require :weka.classifiers.bayes.AODEsr "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/AODEsr")

  (require :weka.classifiers.bayes.BayesNet "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/BayesNet")

  (require :weka.classifiers.bayes.BayesianLogisticRegression "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/BayesianLogisticRegression")

  (require :weka.classifiers.bayes.ComplementNaiveBayes "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/ComplementNaiveBayes")

  (require :weka.classifiers.bayes.DMNBtext "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/DMNBtext")

  (require :weka.classifiers.bayes.HNB "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/HNB")

  (require :weka.classifiers.bayes.NaiveBayes "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/NaiveBayes")

  (require :weka.classifiers.bayes.NaiveBayesMultinomial "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/NaiveBayesMultinomial")

  (require :weka.classifiers.bayes.NaiveBayesMultinomialUpdateable "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/NaiveBayesMultinomialUpdateable")

  (require :weka.classifiers.bayes.NaiveBayesSimple "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/NaiveBayesSimple")

  (require :weka.classifiers.bayes.NaiveBayesUpdateable "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/NaiveBayesUpdateable")

  (require :weka.classifiers.bayes.WAODE "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/WAODE")

  (require :weka.classifiers.bayes.blr.GaussianPriorImpl "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/blr/GaussianPriorImpl")

  (require :weka.classifiers.bayes.blr.LaplacePriorImpl "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/blr/LaplacePriorImpl")

  (require :weka.classifiers.bayes.blr.Prior "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/blr/Prior")

  (require :weka.classifiers.bayes.net.ADNode "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/ADNode")

  (require :weka.classifiers.bayes.net.BIFReader "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/BIFReader")

  (require :weka.classifiers.bayes.net.BayesNetGenerator "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/BayesNetGenerator")

  (require :weka.classifiers.bayes.net.EditableBayesNet "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/EditableBayesNet")

  (require :weka.classifiers.bayes.net.GUI "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/GUI")

  (require :weka.classifiers.bayes.net.MarginCalculator "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/MarginCalculator")

  (require :weka.classifiers.bayes.net.ParentSet "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/ParentSet")

  (require :weka.classifiers.bayes.net.VaryNode "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/VaryNode")

  (require :weka.classifiers.bayes.net.estimate.BMAEstimator "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/estimate/BMAEstimator")

  (require :weka.classifiers.bayes.net.estimate.BayesNetEstimator "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/estimate/BayesNetEstimator")

  (require :weka.classifiers.bayes.net.estimate.DiscreteEstimatorBayes "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/estimate/DiscreteEstimatorBayes")

  (require :weka.classifiers.bayes.net.estimate.DiscreteEstimatorFullBayes "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/estimate/DiscreteEstimatorFullBayes")

  (require :weka.classifiers.bayes.net.estimate.MultiNomialBMAEstimator "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/estimate/MultiNomialBMAEstimator")

  (require :weka.classifiers.bayes.net.estimate.SimpleEstimator "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/estimate/SimpleEstimator")

  (require :weka.classifiers.bayes.net.search.SearchAlgorithm "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/SearchAlgorithm")

  (require :weka.classifiers.bayes.net.search.ci.CISearchAlgorithm "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/ci/CISearchAlgorithm")

  (require :weka.classifiers.bayes.net.search.ci.ICSSearchAlgorithm "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/ci/ICSSearchAlgorithm")

  (require :weka.classifiers.bayes.net.search.fixed.FromFile "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/fixed/FromFile")

  (require :weka.classifiers.bayes.net.search.fixed.NaiveBayes "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/fixed/NaiveBayes")

  (require :weka.classifiers.bayes.net.search.global.GeneticSearch "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/global/GeneticSearch")

  (require :weka.classifiers.bayes.net.search.global.GlobalScoreSearchAlgorithm "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/global/GlobalScoreSearchAlgorithm")

  (require :weka.classifiers.bayes.net.search.global.HillClimber "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/global/HillClimber")

  (require :weka.classifiers.bayes.net.search.global.K2 "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/global/K2")

  (require :weka.classifiers.bayes.net.search.global.RepeatedHillClimber "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/global/RepeatedHillClimber")

  (require :weka.classifiers.bayes.net.search.global.SimulatedAnnealing "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/global/SimulatedAnnealing")

  (require :weka.classifiers.bayes.net.search.global.TAN "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/global/TAN")

  (require :weka.classifiers.bayes.net.search.global.TabuSearch "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/global/TabuSearch")

  (require :weka.classifiers.bayes.net.search.local.GeneticSearch "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/local/GeneticSearch")

  (require :weka.classifiers.bayes.net.search.local.HillClimber "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/local/HillClimber")

  (require :weka.classifiers.bayes.net.search.local.K2 "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/local/K2")

  (require :weka.classifiers.bayes.net.search.local.LAGDHillClimber "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/local/LAGDHillClimber")

  (require :weka.classifiers.bayes.net.search.local.LocalScoreSearchAlgorithm "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/local/LocalScoreSearchAlgorithm")

  (require :weka.classifiers.bayes.net.search.local.RepeatedHillClimber "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/local/RepeatedHillClimber")

  (require :weka.classifiers.bayes.net.search.local.Scoreable "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/local/Scoreable")

  (require :weka.classifiers.bayes.net.search.local.SimulatedAnnealing "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/local/SimulatedAnnealing")

  (require :weka.classifiers.bayes.net.search.local.TAN "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/local/TAN")

  (require :weka.classifiers.bayes.net.search.local.TabuSearch "/home/barnard/Code/Lisp/late/weka/classifiers/bayes/net/search/local/TabuSearch")

  (require :weka.classifiers.evaluation.ConfusionMatrix "/home/barnard/Code/Lisp/late/weka/classifiers/evaluation/ConfusionMatrix")

  (require :weka.classifiers.evaluation.CostCurve "/home/barnard/Code/Lisp/late/weka/classifiers/evaluation/CostCurve")

  (require :weka.classifiers.evaluation.EvaluationUtils "/home/barnard/Code/Lisp/late/weka/classifiers/evaluation/EvaluationUtils")

  (require :weka.classifiers.evaluation.MarginCurve "/home/barnard/Code/Lisp/late/weka/classifiers/evaluation/MarginCurve")

  (require :weka.classifiers.evaluation.NominalPrediction "/home/barnard/Code/Lisp/late/weka/classifiers/evaluation/NominalPrediction")

  (require :weka.classifiers.evaluation.NumericPrediction "/home/barnard/Code/Lisp/late/weka/classifiers/evaluation/NumericPrediction")

  (require :weka.classifiers.evaluation.Prediction "/home/barnard/Code/Lisp/late/weka/classifiers/evaluation/Prediction")

  (require :weka.classifiers.evaluation.ThresholdCurve "/home/barnard/Code/Lisp/late/weka/classifiers/evaluation/ThresholdCurve")

  (require :weka.classifiers.evaluation.TwoClassStats "/home/barnard/Code/Lisp/late/weka/classifiers/evaluation/TwoClassStats")

  (require :weka.classifiers.functions.GaussianProcesses "/home/barnard/Code/Lisp/late/weka/classifiers/functions/GaussianProcesses")

  (require :weka.classifiers.functions.IsotonicRegression "/home/barnard/Code/Lisp/late/weka/classifiers/functions/IsotonicRegression")

  (require :weka.classifiers.functions.LeastMedSq "/home/barnard/Code/Lisp/late/weka/classifiers/functions/LeastMedSq")

  (require :weka.classifiers.functions.LibLINEAR "/home/barnard/Code/Lisp/late/weka/classifiers/functions/LibLINEAR")

  (require :weka.classifiers.functions.LibSVM "/home/barnard/Code/Lisp/late/weka/classifiers/functions/LibSVM")

  (require :weka.classifiers.functions.LinearRegression "/home/barnard/Code/Lisp/late/weka/classifiers/functions/LinearRegression")

  (require :weka.classifiers.functions.Logistic "/home/barnard/Code/Lisp/late/weka/classifiers/functions/Logistic")

  (require :weka.classifiers.functions.MultilayerPerceptron "/home/barnard/Code/Lisp/late/weka/classifiers/functions/MultilayerPerceptron")

  (require :weka.classifiers.functions.PLSClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/functions/PLSClassifier")

  (require :weka.classifiers.functions.PaceRegression "/home/barnard/Code/Lisp/late/weka/classifiers/functions/PaceRegression")

  (require :weka.classifiers.functions.RBFNetwork "/home/barnard/Code/Lisp/late/weka/classifiers/functions/RBFNetwork")

  (require :weka.classifiers.functions.SMO "/home/barnard/Code/Lisp/late/weka/classifiers/functions/SMO")

  (require :weka.classifiers.functions.SMOreg "/home/barnard/Code/Lisp/late/weka/classifiers/functions/SMOreg")

  (require :weka.classifiers.functions.SVMreg "/home/barnard/Code/Lisp/late/weka/classifiers/functions/SVMreg")

  (require :weka.classifiers.functions.SimpleLinearRegression "/home/barnard/Code/Lisp/late/weka/classifiers/functions/SimpleLinearRegression")

  (require :weka.classifiers.functions.SimpleLogistic "/home/barnard/Code/Lisp/late/weka/classifiers/functions/SimpleLogistic")

  (require :weka.classifiers.functions.VotedPerceptron "/home/barnard/Code/Lisp/late/weka/classifiers/functions/VotedPerceptron")

  (require :weka.classifiers.functions.Winnow "/home/barnard/Code/Lisp/late/weka/classifiers/functions/Winnow")

  (require :weka.classifiers.functions.neural.LinearUnit "/home/barnard/Code/Lisp/late/weka/classifiers/functions/neural/LinearUnit")

  (require :weka.classifiers.functions.neural.NeuralConnection "/home/barnard/Code/Lisp/late/weka/classifiers/functions/neural/NeuralConnection")

  (require :weka.classifiers.functions.neural.NeuralMethod "/home/barnard/Code/Lisp/late/weka/classifiers/functions/neural/NeuralMethod")

  (require :weka.classifiers.functions.neural.NeuralNode "/home/barnard/Code/Lisp/late/weka/classifiers/functions/neural/NeuralNode")

  (require :weka.classifiers.functions.neural.SigmoidUnit "/home/barnard/Code/Lisp/late/weka/classifiers/functions/neural/SigmoidUnit")

  (require :weka.classifiers.functions.pace.ChisqMixture "/home/barnard/Code/Lisp/late/weka/classifiers/functions/pace/ChisqMixture")

  (require :weka.classifiers.functions.pace.DiscreteFunction "/home/barnard/Code/Lisp/late/weka/classifiers/functions/pace/DiscreteFunction")

  (require :weka.classifiers.functions.pace.MixtureDistribution "/home/barnard/Code/Lisp/late/weka/classifiers/functions/pace/MixtureDistribution")

  (require :weka.classifiers.functions.pace.NormalMixture "/home/barnard/Code/Lisp/late/weka/classifiers/functions/pace/NormalMixture")

  (require :weka.classifiers.functions.pace.PaceMatrix "/home/barnard/Code/Lisp/late/weka/classifiers/functions/pace/PaceMatrix")

  (require :weka.classifiers.functions.supportVector.CachedKernel "/home/barnard/Code/Lisp/late/weka/classifiers/functions/supportVector/CachedKernel")

  (require :weka.classifiers.functions.supportVector.CheckKernel "/home/barnard/Code/Lisp/late/weka/classifiers/functions/supportVector/CheckKernel")

  (require :weka.classifiers.functions.supportVector.Kernel "/home/barnard/Code/Lisp/late/weka/classifiers/functions/supportVector/Kernel")

  (require :weka.classifiers.functions.supportVector.KernelEvaluation "/home/barnard/Code/Lisp/late/weka/classifiers/functions/supportVector/KernelEvaluation")

  (require :weka.classifiers.functions.supportVector.NormalizedPolyKernel "/home/barnard/Code/Lisp/late/weka/classifiers/functions/supportVector/NormalizedPolyKernel")

  (require :weka.classifiers.functions.supportVector.PolyKernel "/home/barnard/Code/Lisp/late/weka/classifiers/functions/supportVector/PolyKernel")

  (require :weka.classifiers.functions.supportVector.PrecomputedKernelMatrixKernel "/home/barnard/Code/Lisp/late/weka/classifiers/functions/supportVector/PrecomputedKernelMatrixKernel")

  (require :weka.classifiers.functions.supportVector.Puk "/home/barnard/Code/Lisp/late/weka/classifiers/functions/supportVector/Puk")

  (require :weka.classifiers.functions.supportVector.RBFKernel "/home/barnard/Code/Lisp/late/weka/classifiers/functions/supportVector/RBFKernel")

  (require :weka.classifiers.functions.supportVector.RegOptimizer "/home/barnard/Code/Lisp/late/weka/classifiers/functions/supportVector/RegOptimizer")

  (require :weka.classifiers.functions.supportVector.RegSMO "/home/barnard/Code/Lisp/late/weka/classifiers/functions/supportVector/RegSMO")

  (require :weka.classifiers.functions.supportVector.RegSMOImproved "/home/barnard/Code/Lisp/late/weka/classifiers/functions/supportVector/RegSMOImproved")

  (require :weka.classifiers.functions.supportVector.SMOset "/home/barnard/Code/Lisp/late/weka/classifiers/functions/supportVector/SMOset")

  (require :weka.classifiers.functions.supportVector.StringKernel "/home/barnard/Code/Lisp/late/weka/classifiers/functions/supportVector/StringKernel")

  (require :weka.classifiers.lazy.IB1 "/home/barnard/Code/Lisp/late/weka/classifiers/lazy/IB1")

  (require :weka.classifiers.lazy.IBk "/home/barnard/Code/Lisp/late/weka/classifiers/lazy/IBk")

  (require :weka.classifiers.lazy.KStar "/home/barnard/Code/Lisp/late/weka/classifiers/lazy/KStar")

  (require :weka.classifiers.lazy.LBR "/home/barnard/Code/Lisp/late/weka/classifiers/lazy/LBR")

  (require :weka.classifiers.lazy.LWL "/home/barnard/Code/Lisp/late/weka/classifiers/lazy/LWL")

  (require :weka.classifiers.lazy.kstar.KStarCache "/home/barnard/Code/Lisp/late/weka/classifiers/lazy/kstar/KStarCache")

  (require :weka.classifiers.lazy.kstar.KStarConstants "/home/barnard/Code/Lisp/late/weka/classifiers/lazy/kstar/KStarConstants")

  (require :weka.classifiers.lazy.kstar.KStarNominalAttribute "/home/barnard/Code/Lisp/late/weka/classifiers/lazy/kstar/KStarNominalAttribute")

  (require :weka.classifiers.lazy.kstar.KStarNumericAttribute "/home/barnard/Code/Lisp/late/weka/classifiers/lazy/kstar/KStarNumericAttribute")

  (require :weka.classifiers.lazy.kstar.KStarWrapper "/home/barnard/Code/Lisp/late/weka/classifiers/lazy/kstar/KStarWrapper")

  (require :weka.classifiers.meta.AdaBoostM1 "/home/barnard/Code/Lisp/late/weka/classifiers/meta/AdaBoostM1")

  (require :weka.classifiers.meta.AdditiveRegression "/home/barnard/Code/Lisp/late/weka/classifiers/meta/AdditiveRegression")

  (require :weka.classifiers.meta.AttributeSelectedClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/meta/AttributeSelectedClassifier")

  (require :weka.classifiers.meta.Bagging "/home/barnard/Code/Lisp/late/weka/classifiers/meta/Bagging")

  (require :weka.classifiers.meta.CVParameterSelection "/home/barnard/Code/Lisp/late/weka/classifiers/meta/CVParameterSelection")

  (require :weka.classifiers.meta.ClassificationViaClustering "/home/barnard/Code/Lisp/late/weka/classifiers/meta/ClassificationViaClustering")

  (require :weka.classifiers.meta.ClassificationViaRegression "/home/barnard/Code/Lisp/late/weka/classifiers/meta/ClassificationViaRegression")

  (require :weka.classifiers.meta.CostSensitiveClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/meta/CostSensitiveClassifier")

  (require :weka.classifiers.meta.Dagging "/home/barnard/Code/Lisp/late/weka/classifiers/meta/Dagging")

  (require :weka.classifiers.meta.Decorate "/home/barnard/Code/Lisp/late/weka/classifiers/meta/Decorate")

  (require :weka.classifiers.meta.END "/home/barnard/Code/Lisp/late/weka/classifiers/meta/END")

  (require :weka.classifiers.meta.EnsembleSelection "/home/barnard/Code/Lisp/late/weka/classifiers/meta/EnsembleSelection")

  (require :weka.classifiers.meta.FilteredClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/meta/FilteredClassifier")

  (require :weka.classifiers.meta.Grading "/home/barnard/Code/Lisp/late/weka/classifiers/meta/Grading")

  (require :weka.classifiers.meta.GridSearch "/home/barnard/Code/Lisp/late/weka/classifiers/meta/GridSearch")

  (require :weka.classifiers.meta.LogitBoost "/home/barnard/Code/Lisp/late/weka/classifiers/meta/LogitBoost")

  (require :weka.classifiers.meta.MetaCost "/home/barnard/Code/Lisp/late/weka/classifiers/meta/MetaCost")

  (require :weka.classifiers.meta.MultiBoostAB "/home/barnard/Code/Lisp/late/weka/classifiers/meta/MultiBoostAB")

  (require :weka.classifiers.meta.MultiClassClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/meta/MultiClassClassifier")

  (require :weka.classifiers.meta.MultiScheme "/home/barnard/Code/Lisp/late/weka/classifiers/meta/MultiScheme")

  (require :weka.classifiers.meta.OrdinalClassClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/meta/OrdinalClassClassifier")

  (require :weka.classifiers.meta.RacedIncrementalLogitBoost "/home/barnard/Code/Lisp/late/weka/classifiers/meta/RacedIncrementalLogitBoost")

  (require :weka.classifiers.meta.RandomCommittee "/home/barnard/Code/Lisp/late/weka/classifiers/meta/RandomCommittee")

  (require :weka.classifiers.meta.RandomSubSpace "/home/barnard/Code/Lisp/late/weka/classifiers/meta/RandomSubSpace")

  (require :weka.classifiers.meta.RegressionByDiscretization "/home/barnard/Code/Lisp/late/weka/classifiers/meta/RegressionByDiscretization")

  (require :weka.classifiers.meta.RotationForest "/home/barnard/Code/Lisp/late/weka/classifiers/meta/RotationForest")

  (require :weka.classifiers.meta.Stacking "/home/barnard/Code/Lisp/late/weka/classifiers/meta/Stacking")

  (require :weka.classifiers.meta.StackingC "/home/barnard/Code/Lisp/late/weka/classifiers/meta/StackingC")

  (require :weka.classifiers.meta.ThresholdSelector "/home/barnard/Code/Lisp/late/weka/classifiers/meta/ThresholdSelector")

  (require :weka.classifiers.meta.Vote "/home/barnard/Code/Lisp/late/weka/classifiers/meta/Vote")

  (require :weka.classifiers.meta.ensembleSelection.EnsembleMetricHelper "/home/barnard/Code/Lisp/late/weka/classifiers/meta/ensembleSelection/EnsembleMetricHelper")

  (require :weka.classifiers.meta.ensembleSelection.EnsembleModelMismatchException "/home/barnard/Code/Lisp/late/weka/classifiers/meta/ensembleSelection/EnsembleModelMismatchException")

  (require :weka.classifiers.meta.ensembleSelection.EnsembleSelectionLibrary "/home/barnard/Code/Lisp/late/weka/classifiers/meta/ensembleSelection/EnsembleSelectionLibrary")

  (require :weka.classifiers.meta.ensembleSelection.EnsembleSelectionLibraryModel "/home/barnard/Code/Lisp/late/weka/classifiers/meta/ensembleSelection/EnsembleSelectionLibraryModel")

  (require :weka.classifiers.meta.ensembleSelection.ModelBag "/home/barnard/Code/Lisp/late/weka/classifiers/meta/ensembleSelection/ModelBag")

  (require :weka.classifiers.meta.nestedDichotomies.ClassBalancedND "/home/barnard/Code/Lisp/late/weka/classifiers/meta/nestedDichotomies/ClassBalancedND")

  (require :weka.classifiers.meta.nestedDichotomies.DataNearBalancedND "/home/barnard/Code/Lisp/late/weka/classifiers/meta/nestedDichotomies/DataNearBalancedND")

  (require :weka.classifiers.meta.nestedDichotomies.ND "/home/barnard/Code/Lisp/late/weka/classifiers/meta/nestedDichotomies/ND")

  (require :weka.classifiers.mi.CitationKNN "/home/barnard/Code/Lisp/late/weka/classifiers/mi/CitationKNN")

  (require :weka.classifiers.mi.MDD "/home/barnard/Code/Lisp/late/weka/classifiers/mi/MDD")

  (require :weka.classifiers.mi.MIBoost "/home/barnard/Code/Lisp/late/weka/classifiers/mi/MIBoost")

  (require :weka.classifiers.mi.MIDD "/home/barnard/Code/Lisp/late/weka/classifiers/mi/MIDD")

  (require :weka.classifiers.mi.MIEMDD "/home/barnard/Code/Lisp/late/weka/classifiers/mi/MIEMDD")

  (require :weka.classifiers.mi.MILR "/home/barnard/Code/Lisp/late/weka/classifiers/mi/MILR")

  (require :weka.classifiers.mi.MINND "/home/barnard/Code/Lisp/late/weka/classifiers/mi/MINND")

  (require :weka.classifiers.mi.MIOptimalBall "/home/barnard/Code/Lisp/late/weka/classifiers/mi/MIOptimalBall")

  (require :weka.classifiers.mi.MISMO "/home/barnard/Code/Lisp/late/weka/classifiers/mi/MISMO")

  (require :weka.classifiers.mi.MISVM "/home/barnard/Code/Lisp/late/weka/classifiers/mi/MISVM")

  (require :weka.classifiers.mi.MIWrapper "/home/barnard/Code/Lisp/late/weka/classifiers/mi/MIWrapper")

  (require :weka.classifiers.mi.SimpleMI "/home/barnard/Code/Lisp/late/weka/classifiers/mi/SimpleMI")

  (require :weka.classifiers.mi.TLD "/home/barnard/Code/Lisp/late/weka/classifiers/mi/TLD")

  (require :weka.classifiers.mi.TLDSimple "/home/barnard/Code/Lisp/late/weka/classifiers/mi/TLDSimple")

  (require :weka.classifiers.mi.TLDSimple_Optm "/home/barnard/Code/Lisp/late/weka/classifiers/mi/TLDSimple_Optm")

  (require :weka.classifiers.mi.TLD_Optm "/home/barnard/Code/Lisp/late/weka/classifiers/mi/TLD_Optm")

  (require :weka.classifiers.mi.supportVector.MIPolyKernel "/home/barnard/Code/Lisp/late/weka/classifiers/mi/supportVector/MIPolyKernel")

  (require :weka.classifiers.mi.supportVector.MIRBFKernel "/home/barnard/Code/Lisp/late/weka/classifiers/mi/supportVector/MIRBFKernel")

  (require :weka.classifiers.misc.FLR "/home/barnard/Code/Lisp/late/weka/classifiers/misc/FLR")

  (require :weka.classifiers.misc.HyperPipes "/home/barnard/Code/Lisp/late/weka/classifiers/misc/HyperPipes")

  (require :weka.classifiers.misc.MinMaxExtension "/home/barnard/Code/Lisp/late/weka/classifiers/misc/MinMaxExtension")

  (require :weka.classifiers.misc.OLM "/home/barnard/Code/Lisp/late/weka/classifiers/misc/OLM")

  (require :weka.classifiers.misc.OSDL "/home/barnard/Code/Lisp/late/weka/classifiers/misc/OSDL")

  (require :weka.classifiers.misc.SerializedClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/misc/SerializedClassifier")

  (require :weka.classifiers.misc.VFI "/home/barnard/Code/Lisp/late/weka/classifiers/misc/VFI")

  (require :weka.classifiers.misc.monotone.AbsoluteLossFunction "/home/barnard/Code/Lisp/late/weka/classifiers/misc/monotone/AbsoluteLossFunction")

  (require :weka.classifiers.misc.monotone.BitMatrix "/home/barnard/Code/Lisp/late/weka/classifiers/misc/monotone/BitMatrix")

  (require :weka.classifiers.misc.monotone.BooleanBitMatrix "/home/barnard/Code/Lisp/late/weka/classifiers/misc/monotone/BooleanBitMatrix")

  (require :weka.classifiers.misc.monotone.Coordinates "/home/barnard/Code/Lisp/late/weka/classifiers/misc/monotone/Coordinates")

  (require :weka.classifiers.misc.monotone.CumulativeDiscreteDistribution "/home/barnard/Code/Lisp/late/weka/classifiers/misc/monotone/CumulativeDiscreteDistribution")

  (require :weka.classifiers.misc.monotone.DiscreteDistribution "/home/barnard/Code/Lisp/late/weka/classifiers/misc/monotone/DiscreteDistribution")

  (require :weka.classifiers.misc.monotone.DistributionUtils "/home/barnard/Code/Lisp/late/weka/classifiers/misc/monotone/DistributionUtils")

  (require :weka.classifiers.misc.monotone.EnumerationIterator "/home/barnard/Code/Lisp/late/weka/classifiers/misc/monotone/EnumerationIterator")

  (require :weka.classifiers.misc.monotone.InstancesComparator "/home/barnard/Code/Lisp/late/weka/classifiers/misc/monotone/InstancesComparator")

  (require :weka.classifiers.misc.monotone.InstancesUtil "/home/barnard/Code/Lisp/late/weka/classifiers/misc/monotone/InstancesUtil")

  (require :weka.classifiers.misc.monotone.MultiDimensionalSort "/home/barnard/Code/Lisp/late/weka/classifiers/misc/monotone/MultiDimensionalSort")

  (require :weka.classifiers.misc.monotone.NominalLossFunction "/home/barnard/Code/Lisp/late/weka/classifiers/misc/monotone/NominalLossFunction")

  (require :weka.classifiers.misc.monotone.OSDLCore "/home/barnard/Code/Lisp/late/weka/classifiers/misc/monotone/OSDLCore")

  (require :weka.classifiers.misc.monotone.ZeroOneLossFunction "/home/barnard/Code/Lisp/late/weka/classifiers/misc/monotone/ZeroOneLossFunction")

  (require :weka.classifiers.pmml.consumer.GeneralRegression "/home/barnard/Code/Lisp/late/weka/classifiers/pmml/consumer/GeneralRegression")

  (require :weka.classifiers.pmml.consumer.NeuralNetwork "/home/barnard/Code/Lisp/late/weka/classifiers/pmml/consumer/NeuralNetwork")

  (require :weka.classifiers.pmml.consumer.PMMLClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/pmml/consumer/PMMLClassifier")

  (require :weka.classifiers.pmml.consumer.Regression "/home/barnard/Code/Lisp/late/weka/classifiers/pmml/consumer/Regression")

  (require :weka.classifiers.rules.ConjunctiveRule "/home/barnard/Code/Lisp/late/weka/classifiers/rules/ConjunctiveRule")

  (require :weka.classifiers.rules.DTNB "/home/barnard/Code/Lisp/late/weka/classifiers/rules/DTNB")

  (require :weka.classifiers.rules.DecisionTable "/home/barnard/Code/Lisp/late/weka/classifiers/rules/DecisionTable")

  (require :weka.classifiers.rules.DecisionTableHashKey "/home/barnard/Code/Lisp/late/weka/classifiers/rules/DecisionTableHashKey")

  (require :weka.classifiers.rules.JRip "/home/barnard/Code/Lisp/late/weka/classifiers/rules/JRip")

  (require :weka.classifiers.rules.M5Rules "/home/barnard/Code/Lisp/late/weka/classifiers/rules/M5Rules")

  (require :weka.classifiers.rules.NNge "/home/barnard/Code/Lisp/late/weka/classifiers/rules/NNge")

  (require :weka.classifiers.rules.OneR "/home/barnard/Code/Lisp/late/weka/classifiers/rules/OneR")

  (require :weka.classifiers.rules.PART "/home/barnard/Code/Lisp/late/weka/classifiers/rules/PART")

  (require :weka.classifiers.rules.Prism "/home/barnard/Code/Lisp/late/weka/classifiers/rules/Prism")

  (require :weka.classifiers.rules.Ridor "/home/barnard/Code/Lisp/late/weka/classifiers/rules/Ridor")

  (require :weka.classifiers.rules.Rule "/home/barnard/Code/Lisp/late/weka/classifiers/rules/Rule")

  (require :weka.classifiers.rules.RuleStats "/home/barnard/Code/Lisp/late/weka/classifiers/rules/RuleStats")

  (require :weka.classifiers.rules.ZeroR "/home/barnard/Code/Lisp/late/weka/classifiers/rules/ZeroR")

  (require :weka.classifiers.rules.part.C45PruneableDecList "/home/barnard/Code/Lisp/late/weka/classifiers/rules/part/C45PruneableDecList")

  (require :weka.classifiers.rules.part.ClassifierDecList "/home/barnard/Code/Lisp/late/weka/classifiers/rules/part/ClassifierDecList")

  (require :weka.classifiers.rules.part.MakeDecList "/home/barnard/Code/Lisp/late/weka/classifiers/rules/part/MakeDecList")

  (require :weka.classifiers.rules.part.PruneableDecList "/home/barnard/Code/Lisp/late/weka/classifiers/rules/part/PruneableDecList")

  (require :weka.classifiers.trees.ADTree "/home/barnard/Code/Lisp/late/weka/classifiers/trees/ADTree")

  (require :weka.classifiers.trees.BFTree "/home/barnard/Code/Lisp/late/weka/classifiers/trees/BFTree")

  (require :weka.classifiers.trees.DecisionStump "/home/barnard/Code/Lisp/late/weka/classifiers/trees/DecisionStump")

  (require :weka.classifiers.trees.FT "/home/barnard/Code/Lisp/late/weka/classifiers/trees/FT")

  (require :weka.classifiers.trees.Id3 "/home/barnard/Code/Lisp/late/weka/classifiers/trees/Id3")

  (require :weka.classifiers.trees.J48 "/home/barnard/Code/Lisp/late/weka/classifiers/trees/J48")

  (require :weka.classifiers.trees.J48graft "/home/barnard/Code/Lisp/late/weka/classifiers/trees/J48graft")

  (require :weka.classifiers.trees.LADTree "/home/barnard/Code/Lisp/late/weka/classifiers/trees/LADTree")

  (require :weka.classifiers.trees.LMT "/home/barnard/Code/Lisp/late/weka/classifiers/trees/LMT")

  (require :weka.classifiers.trees.M5P "/home/barnard/Code/Lisp/late/weka/classifiers/trees/M5P")

  (require :weka.classifiers.trees.NBTree "/home/barnard/Code/Lisp/late/weka/classifiers/trees/NBTree")

  (require :weka.classifiers.trees.REPTree "/home/barnard/Code/Lisp/late/weka/classifiers/trees/REPTree")

  (require :weka.classifiers.trees.RandomForest "/home/barnard/Code/Lisp/late/weka/classifiers/trees/RandomForest")

  (require :weka.classifiers.trees.RandomTree "/home/barnard/Code/Lisp/late/weka/classifiers/trees/RandomTree")

  (require :weka.classifiers.trees.SimpleCart "/home/barnard/Code/Lisp/late/weka/classifiers/trees/SimpleCart")

  (require :weka.classifiers.trees.UserClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/trees/UserClassifier")

  (require :weka.classifiers.trees.adtree.PredictionNode "/home/barnard/Code/Lisp/late/weka/classifiers/trees/adtree/PredictionNode")

  (require :weka.classifiers.trees.adtree.ReferenceInstances "/home/barnard/Code/Lisp/late/weka/classifiers/trees/adtree/ReferenceInstances")

  (require :weka.classifiers.trees.adtree.Splitter "/home/barnard/Code/Lisp/late/weka/classifiers/trees/adtree/Splitter")

  (require :weka.classifiers.trees.adtree.TwoWayNominalSplit "/home/barnard/Code/Lisp/late/weka/classifiers/trees/adtree/TwoWayNominalSplit")

  (require :weka.classifiers.trees.adtree.TwoWayNumericSplit "/home/barnard/Code/Lisp/late/weka/classifiers/trees/adtree/TwoWayNumericSplit")

  (require :weka.classifiers.trees.ft.FTInnerNode "/home/barnard/Code/Lisp/late/weka/classifiers/trees/ft/FTInnerNode")

  (require :weka.classifiers.trees.ft.FTLeavesNode "/home/barnard/Code/Lisp/late/weka/classifiers/trees/ft/FTLeavesNode")

  (require :weka.classifiers.trees.ft.FTNode "/home/barnard/Code/Lisp/late/weka/classifiers/trees/ft/FTNode")

  (require :weka.classifiers.trees.ft.FTtree "/home/barnard/Code/Lisp/late/weka/classifiers/trees/ft/FTtree")

  (require :weka.classifiers.trees.j48.BinC45ModelSelection "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/BinC45ModelSelection")

  (require :weka.classifiers.trees.j48.BinC45Split "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/BinC45Split")

  (require :weka.classifiers.trees.j48.C45ModelSelection "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/C45ModelSelection")

  (require :weka.classifiers.trees.j48.C45PruneableClassifierTree "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/C45PruneableClassifierTree")

  (require :weka.classifiers.trees.j48.C45PruneableClassifierTreeG "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/C45PruneableClassifierTreeG")

  (require :weka.classifiers.trees.j48.C45Split "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/C45Split")

  (require :weka.classifiers.trees.j48.ClassifierSplitModel "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/ClassifierSplitModel")

  (require :weka.classifiers.trees.j48.ClassifierTree "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/ClassifierTree")

  (require :weka.classifiers.trees.j48.Distribution "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/Distribution")

  (require :weka.classifiers.trees.j48.EntropyBasedSplitCrit "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/EntropyBasedSplitCrit")

  (require :weka.classifiers.trees.j48.EntropySplitCrit "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/EntropySplitCrit")

  (require :weka.classifiers.trees.j48.GainRatioSplitCrit "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/GainRatioSplitCrit")

  (require :weka.classifiers.trees.j48.GraftSplit "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/GraftSplit")

  (require :weka.classifiers.trees.j48.InfoGainSplitCrit "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/InfoGainSplitCrit")

  (require :weka.classifiers.trees.j48.ModelSelection "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/ModelSelection")

  (require :weka.classifiers.trees.j48.NBTreeClassifierTree "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/NBTreeClassifierTree")

  (require :weka.classifiers.trees.j48.NBTreeModelSelection "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/NBTreeModelSelection")

  (require :weka.classifiers.trees.j48.NBTreeNoSplit "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/NBTreeNoSplit")

  (require :weka.classifiers.trees.j48.NBTreeSplit "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/NBTreeSplit")

  (require :weka.classifiers.trees.j48.NoSplit "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/NoSplit")

  (require :weka.classifiers.trees.j48.PruneableClassifierTree "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/PruneableClassifierTree")

  (require :weka.classifiers.trees.j48.SplitCriterion "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/SplitCriterion")

  (require :weka.classifiers.trees.j48.Stats "/home/barnard/Code/Lisp/late/weka/classifiers/trees/j48/Stats")

  (require :weka.classifiers.trees.lmt.CompareNode "/home/barnard/Code/Lisp/late/weka/classifiers/trees/lmt/CompareNode")

  (require :weka.classifiers.trees.lmt.LMTNode "/home/barnard/Code/Lisp/late/weka/classifiers/trees/lmt/LMTNode")

  (require :weka.classifiers.trees.lmt.LogisticBase "/home/barnard/Code/Lisp/late/weka/classifiers/trees/lmt/LogisticBase")

  (require :weka.classifiers.trees.lmt.ResidualModelSelection "/home/barnard/Code/Lisp/late/weka/classifiers/trees/lmt/ResidualModelSelection")

  (require :weka.classifiers.trees.lmt.ResidualSplit "/home/barnard/Code/Lisp/late/weka/classifiers/trees/lmt/ResidualSplit")

  (require :weka.classifiers.trees.m5.CorrelationSplitInfo "/home/barnard/Code/Lisp/late/weka/classifiers/trees/m5/CorrelationSplitInfo")

  (require :weka.classifiers.trees.m5.Impurity "/home/barnard/Code/Lisp/late/weka/classifiers/trees/m5/Impurity")

  (require :weka.classifiers.trees.m5.M5Base "/home/barnard/Code/Lisp/late/weka/classifiers/trees/m5/M5Base")

  (require :weka.classifiers.trees.m5.PreConstructedLinearModel "/home/barnard/Code/Lisp/late/weka/classifiers/trees/m5/PreConstructedLinearModel")

  (require :weka.classifiers.trees.m5.Rule "/home/barnard/Code/Lisp/late/weka/classifiers/trees/m5/Rule")

  (require :weka.classifiers.trees.m5.RuleNode "/home/barnard/Code/Lisp/late/weka/classifiers/trees/m5/RuleNode")

  (require :weka.classifiers.trees.m5.SplitEvaluate "/home/barnard/Code/Lisp/late/weka/classifiers/trees/m5/SplitEvaluate")

  (require :weka.classifiers.trees.m5.Values "/home/barnard/Code/Lisp/late/weka/classifiers/trees/m5/Values")

  (require :weka.classifiers.trees.m5.YongSplitInfo "/home/barnard/Code/Lisp/late/weka/classifiers/trees/m5/YongSplitInfo")

  (require :weka.classifiers.xml.XMLClassifier "/home/barnard/Code/Lisp/late/weka/classifiers/xml/XMLClassifier")

  (require :weka.clusterers.AbstractClusterer "/home/barnard/Code/Lisp/late/weka/clusterers/AbstractClusterer")

  (require :weka.clusterers.AbstractDensityBasedClusterer "/home/barnard/Code/Lisp/late/weka/clusterers/AbstractDensityBasedClusterer")

  (require :weka.clusterers.CLOPE "/home/barnard/Code/Lisp/late/weka/clusterers/CLOPE")

  (require :weka.clusterers.CheckClusterer "/home/barnard/Code/Lisp/late/weka/clusterers/CheckClusterer")

  (require :weka.clusterers.ClusterEvaluation "/home/barnard/Code/Lisp/late/weka/clusterers/ClusterEvaluation")

  (require :weka.clusterers.Clusterer "/home/barnard/Code/Lisp/late/weka/clusterers/Clusterer")

  (require :weka.clusterers.Cobweb "/home/barnard/Code/Lisp/late/weka/clusterers/Cobweb")

  (require :weka.clusterers.DBScan "/home/barnard/Code/Lisp/late/weka/clusterers/DBScan")

  (require :weka.clusterers.DensityBasedClusterer "/home/barnard/Code/Lisp/late/weka/clusterers/DensityBasedClusterer")

  (require :weka.clusterers.EM "/home/barnard/Code/Lisp/late/weka/clusterers/EM")

  (require :weka.clusterers.FarthestFirst "/home/barnard/Code/Lisp/late/weka/clusterers/FarthestFirst")

  (require :weka.clusterers.FilteredClusterer "/home/barnard/Code/Lisp/late/weka/clusterers/FilteredClusterer")

  (require :weka.clusterers.MakeDensityBasedClusterer "/home/barnard/Code/Lisp/late/weka/clusterers/MakeDensityBasedClusterer")

  (require :weka.clusterers.NumberOfClustersRequestable "/home/barnard/Code/Lisp/late/weka/clusterers/NumberOfClustersRequestable")

  (require :weka.clusterers.OPTICS "/home/barnard/Code/Lisp/late/weka/clusterers/OPTICS")

  (require :weka.clusterers.RandomizableClusterer "/home/barnard/Code/Lisp/late/weka/clusterers/RandomizableClusterer")

  (require :weka.clusterers.RandomizableDensityBasedClusterer "/home/barnard/Code/Lisp/late/weka/clusterers/RandomizableDensityBasedClusterer")

  (require :weka.clusterers.RandomizableSingleClustererEnhancer "/home/barnard/Code/Lisp/late/weka/clusterers/RandomizableSingleClustererEnhancer")

  (require :weka.clusterers.SimpleKMeans "/home/barnard/Code/Lisp/late/weka/clusterers/SimpleKMeans")

  (require :weka.clusterers.SingleClustererEnhancer "/home/barnard/Code/Lisp/late/weka/clusterers/SingleClustererEnhancer")

  (require :weka.clusterers.UpdateableClusterer "/home/barnard/Code/Lisp/late/weka/clusterers/UpdateableClusterer")

  (require :weka.clusterers.XMeans "/home/barnard/Code/Lisp/late/weka/clusterers/XMeans")

  (require :weka.clusterers.forOPTICSAndDBScan.DataObjects.DataObject "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/DataObjects/DataObject")

  (require :weka.clusterers.forOPTICSAndDBScan.DataObjects.EuclidianDataObject "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/DataObjects/EuclidianDataObject")

  (require :weka.clusterers.forOPTICSAndDBScan.DataObjects.ManhattanDataObject "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/DataObjects/ManhattanDataObject")

  (require :weka.clusterers.forOPTICSAndDBScan.Databases.Database "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/Databases/Database")

  (require :weka.clusterers.forOPTICSAndDBScan.Databases.SequentialDatabase "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/Databases/SequentialDatabase")

  (require :weka.clusterers.forOPTICSAndDBScan.OPTICS_GUI.GraphPanel "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/OPTICS_GUI/GraphPanel")

  (require :weka.clusterers.forOPTICSAndDBScan.OPTICS_GUI.OPTICS_Visualizer "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/OPTICS_GUI/OPTICS_Visualizer")

  (require :weka.clusterers.forOPTICSAndDBScan.OPTICS_GUI.ResultVectorTableModel "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/OPTICS_GUI/ResultVectorTableModel")

  (require :weka.clusterers.forOPTICSAndDBScan.OPTICS_GUI.SERFileFilter "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/OPTICS_GUI/SERFileFilter")

  (require :weka.clusterers.forOPTICSAndDBScan.OPTICS_GUI.SERObject "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/OPTICS_GUI/SERObject")

  (require :weka.clusterers.forOPTICSAndDBScan.Utils.EpsilonRange_ListElement "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/Utils/EpsilonRange_ListElement")

  (require :weka.clusterers.forOPTICSAndDBScan.Utils.PriorityQueue "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/Utils/PriorityQueue")

  (require :weka.clusterers.forOPTICSAndDBScan.Utils.PriorityQueueElement "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/Utils/PriorityQueueElement")

  (require :weka.clusterers.forOPTICSAndDBScan.Utils.UpdateQueue "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/Utils/UpdateQueue")

  (require :weka.clusterers.forOPTICSAndDBScan.Utils.UpdateQueueElement "/home/barnard/Code/Lisp/late/weka/clusterers/forOPTICSAndDBScan/Utils/UpdateQueueElement")

  (require :weka.clusterers.sIB "/home/barnard/Code/Lisp/late/weka/clusterers/sIB")

  (require :weka.core.AbstractStringDistanceFunction "/home/barnard/Code/Lisp/late/weka/core/AbstractStringDistanceFunction")

  (require :weka.core.AdditionalMeasureProducer "/home/barnard/Code/Lisp/late/weka/core/AdditionalMeasureProducer")

  (require :weka.core.AlgVector "/home/barnard/Code/Lisp/late/weka/core/AlgVector")

  (require :weka.core.AllJavadoc "/home/barnard/Code/Lisp/late/weka/core/AllJavadoc")

  (require :weka.core.Attribute "/home/barnard/Code/Lisp/late/weka/core/Attribute")

  (require :weka.core.AttributeExpression "/home/barnard/Code/Lisp/late/weka/core/AttributeExpression")

  (require :weka.core.AttributeLocator "/home/barnard/Code/Lisp/late/weka/core/AttributeLocator")

  (require :weka.core.AttributeStats "/home/barnard/Code/Lisp/late/weka/core/AttributeStats")

  (require :weka.core.BinarySparseInstance "/home/barnard/Code/Lisp/late/weka/core/BinarySparseInstance")

  (require :weka.core.Capabilities "/home/barnard/Code/Lisp/late/weka/core/Capabilities")

  (require :weka.core.CapabilitiesHandler "/home/barnard/Code/Lisp/late/weka/core/CapabilitiesHandler")

  (require :weka.core.ChebyshevDistance "/home/barnard/Code/Lisp/late/weka/core/ChebyshevDistance")

  (require :weka.core.Check "/home/barnard/Code/Lisp/late/weka/core/Check")

  (require :weka.core.CheckGOE "/home/barnard/Code/Lisp/late/weka/core/CheckGOE")

  (require :weka.core.CheckOptionHandler "/home/barnard/Code/Lisp/late/weka/core/CheckOptionHandler")

  (require :weka.core.CheckScheme "/home/barnard/Code/Lisp/late/weka/core/CheckScheme")

  (require :weka.core.ClassDiscovery "/home/barnard/Code/Lisp/late/weka/core/ClassDiscovery")

  (require :weka.core.ClassloaderUtil "/home/barnard/Code/Lisp/late/weka/core/ClassloaderUtil")

  (require :weka.core.ContingencyTables "/home/barnard/Code/Lisp/late/weka/core/ContingencyTables")

  (require :weka.core.Copyable "/home/barnard/Code/Lisp/late/weka/core/Copyable")

  (require :weka.core.Copyright "/home/barnard/Code/Lisp/late/weka/core/Copyright")

  (require :weka.core.Debug "/home/barnard/Code/Lisp/late/weka/core/Debug")

  (require :weka.core.DistanceFunction "/home/barnard/Code/Lisp/late/weka/core/DistanceFunction")

  (require :weka.core.Drawable "/home/barnard/Code/Lisp/late/weka/core/Drawable")

  (require :weka.core.EditDistance "/home/barnard/Code/Lisp/late/weka/core/EditDistance")

  (require :weka.core.Environment "/home/barnard/Code/Lisp/late/weka/core/Environment")

  (require :weka.core.EuclideanDistance "/home/barnard/Code/Lisp/late/weka/core/EuclideanDistance")

  (require :weka.core.FastVector "/home/barnard/Code/Lisp/late/weka/core/FastVector")

  (require :weka.core.FindWithCapabilities "/home/barnard/Code/Lisp/late/weka/core/FindWithCapabilities")

  (require :weka.core.GlobalInfoJavadoc "/home/barnard/Code/Lisp/late/weka/core/GlobalInfoJavadoc")

  (require :weka.core.Instance "/home/barnard/Code/Lisp/late/weka/core/Instance")

  (require :weka.core.InstanceComparator "/home/barnard/Code/Lisp/late/weka/core/InstanceComparator")

  (require :weka.core.Instances "/home/barnard/Code/Lisp/late/weka/core/Instances")

  (require :weka.core.Javadoc "/home/barnard/Code/Lisp/late/weka/core/Javadoc")

  (require :weka.core.Jython "/home/barnard/Code/Lisp/late/weka/core/Jython")

  (require :weka.core.JythonObject "/home/barnard/Code/Lisp/late/weka/core/JythonObject")

  (require :weka.core.JythonSerializableObject "/home/barnard/Code/Lisp/late/weka/core/JythonSerializableObject")

  (require :weka.core.ListOptions "/home/barnard/Code/Lisp/late/weka/core/ListOptions")

  (require :weka.core.ManhattanDistance "/home/barnard/Code/Lisp/late/weka/core/ManhattanDistance")

  (require :weka.core.Matchable "/home/barnard/Code/Lisp/late/weka/core/Matchable")

  (require :weka.core.MathematicalExpression "/home/barnard/Code/Lisp/late/weka/core/MathematicalExpression")

  (require :weka.core.Matrix "/home/barnard/Code/Lisp/late/weka/core/Matrix")

  (require :weka.core.Memory "/home/barnard/Code/Lisp/late/weka/core/Memory")

  (require :weka.core.MultiInstanceCapabilitiesHandler "/home/barnard/Code/Lisp/late/weka/core/MultiInstanceCapabilitiesHandler")

  (require :weka.core.NoSupportForMissingValuesException "/home/barnard/Code/Lisp/late/weka/core/NoSupportForMissingValuesException")

  (require :weka.core.NormalizableDistance "/home/barnard/Code/Lisp/late/weka/core/NormalizableDistance")

  (require :weka.core.Optimization "/home/barnard/Code/Lisp/late/weka/core/Optimization")

  (require :weka.core.Option "/home/barnard/Code/Lisp/late/weka/core/Option")

  (require :weka.core.OptionHandler "/home/barnard/Code/Lisp/late/weka/core/OptionHandler")

  (require :weka.core.OptionHandlerJavadoc "/home/barnard/Code/Lisp/late/weka/core/OptionHandlerJavadoc")

  (require :weka.core.PropertyPath "/home/barnard/Code/Lisp/late/weka/core/PropertyPath")

  (require :weka.core.ProtectedProperties "/home/barnard/Code/Lisp/late/weka/core/ProtectedProperties")

  (require :weka.core.Queue "/home/barnard/Code/Lisp/late/weka/core/Queue")

  (require :weka.core.RandomVariates "/home/barnard/Code/Lisp/late/weka/core/RandomVariates")

  (require :weka.core.Randomizable "/home/barnard/Code/Lisp/late/weka/core/Randomizable")

  (require :weka.core.Range "/home/barnard/Code/Lisp/late/weka/core/Range")

  (require :weka.core.RelationalLocator "/home/barnard/Code/Lisp/late/weka/core/RelationalLocator")

  (require :weka.core.RevisionHandler "/home/barnard/Code/Lisp/late/weka/core/RevisionHandler")

  (require :weka.core.RevisionUtils "/home/barnard/Code/Lisp/late/weka/core/RevisionUtils")

  (require :weka.core.SelectedTag "/home/barnard/Code/Lisp/late/weka/core/SelectedTag")

  (require :weka.core.SerializationHelper "/home/barnard/Code/Lisp/late/weka/core/SerializationHelper")

  (require :weka.core.SerializedObject "/home/barnard/Code/Lisp/late/weka/core/SerializedObject")

  (require :weka.core.SingleIndex "/home/barnard/Code/Lisp/late/weka/core/SingleIndex")

  (require :weka.core.SparseInstance "/home/barnard/Code/Lisp/late/weka/core/SparseInstance")

  (require :weka.core.SpecialFunctions "/home/barnard/Code/Lisp/late/weka/core/SpecialFunctions")

  (require :weka.core.Statistics "/home/barnard/Code/Lisp/late/weka/core/Statistics")

  (require :weka.core.Stopwords "/home/barnard/Code/Lisp/late/weka/core/Stopwords")

  (require :weka.core.StringLocator "/home/barnard/Code/Lisp/late/weka/core/StringLocator")

  (require :weka.core.Summarizable "/home/barnard/Code/Lisp/late/weka/core/Summarizable")

  (require :weka.core.SystemInfo "/home/barnard/Code/Lisp/late/weka/core/SystemInfo")

  (require :weka.core.Tag "/home/barnard/Code/Lisp/late/weka/core/Tag")

  (require :weka.core.TechnicalInformation "/home/barnard/Code/Lisp/late/weka/core/TechnicalInformation")

  (require :weka.core.TechnicalInformationHandler "/home/barnard/Code/Lisp/late/weka/core/TechnicalInformationHandler")

  (require :weka.core.TechnicalInformationHandlerJavadoc "/home/barnard/Code/Lisp/late/weka/core/TechnicalInformationHandlerJavadoc")

  (require :weka.core.Tee "/home/barnard/Code/Lisp/late/weka/core/Tee")

  (require :weka.core.TestInstances "/home/barnard/Code/Lisp/late/weka/core/TestInstances")

  (require :weka.core.Trie "/home/barnard/Code/Lisp/late/weka/core/Trie")

  (require :weka.core.UnassignedClassException "/home/barnard/Code/Lisp/late/weka/core/UnassignedClassException")

  (require :weka.core.UnassignedDatasetException "/home/barnard/Code/Lisp/late/weka/core/UnassignedDatasetException")

  (require :weka.core.Undoable "/home/barnard/Code/Lisp/late/weka/core/Undoable")

  (require :weka.core.UnsupportedAttributeTypeException "/home/barnard/Code/Lisp/late/weka/core/UnsupportedAttributeTypeException")

  (require :weka.core.UnsupportedClassTypeException "/home/barnard/Code/Lisp/late/weka/core/UnsupportedClassTypeException")

  (require :weka.core.Utils "/home/barnard/Code/Lisp/late/weka/core/Utils")

  (require :weka.core.Version "/home/barnard/Code/Lisp/late/weka/core/Version")

  (require :weka.core.WeightedInstancesHandler "/home/barnard/Code/Lisp/late/weka/core/WeightedInstancesHandler")

  (require :weka.core.WekaException "/home/barnard/Code/Lisp/late/weka/core/WekaException")

  (require :weka.core.converters.AbstractFileLoader "/home/barnard/Code/Lisp/late/weka/core/converters/AbstractFileLoader")

  (require :weka.core.converters.AbstractFileSaver "/home/barnard/Code/Lisp/late/weka/core/converters/AbstractFileSaver")

  (require :weka.core.converters.AbstractLoader "/home/barnard/Code/Lisp/late/weka/core/converters/AbstractLoader")

  (require :weka.core.converters.AbstractSaver "/home/barnard/Code/Lisp/late/weka/core/converters/AbstractSaver")

  (require :weka.core.converters.ArffLoader "/home/barnard/Code/Lisp/late/weka/core/converters/ArffLoader")

  (require :weka.core.converters.ArffSaver "/home/barnard/Code/Lisp/late/weka/core/converters/ArffSaver")

  (require :weka.core.converters.BatchConverter "/home/barnard/Code/Lisp/late/weka/core/converters/BatchConverter")

  (require :weka.core.converters.C45Loader "/home/barnard/Code/Lisp/late/weka/core/converters/C45Loader")

  (require :weka.core.converters.C45Saver "/home/barnard/Code/Lisp/late/weka/core/converters/C45Saver")

  (require :weka.core.converters.CSVLoader "/home/barnard/Code/Lisp/late/weka/core/converters/CSVLoader")

  (require :weka.core.converters.CSVSaver "/home/barnard/Code/Lisp/late/weka/core/converters/CSVSaver")

  (require :weka.core.converters.ConverterUtils "/home/barnard/Code/Lisp/late/weka/core/converters/ConverterUtils")

  (require :weka.core.converters.DatabaseConnection "/home/barnard/Code/Lisp/late/weka/core/converters/DatabaseConnection")

  (require :weka.core.converters.DatabaseConverter "/home/barnard/Code/Lisp/late/weka/core/converters/DatabaseConverter")

  (require :weka.core.converters.DatabaseLoader "/home/barnard/Code/Lisp/late/weka/core/converters/DatabaseLoader")

  (require :weka.core.converters.DatabaseSaver "/home/barnard/Code/Lisp/late/weka/core/converters/DatabaseSaver")

  (require :weka.core.converters.FileSourcedConverter "/home/barnard/Code/Lisp/late/weka/core/converters/FileSourcedConverter")

  (require :weka.core.converters.IncrementalConverter "/home/barnard/Code/Lisp/late/weka/core/converters/IncrementalConverter")

  (require :weka.core.converters.LibSVMLoader "/home/barnard/Code/Lisp/late/weka/core/converters/LibSVMLoader")

  (require :weka.core.converters.LibSVMSaver "/home/barnard/Code/Lisp/late/weka/core/converters/LibSVMSaver")

  (require :weka.core.converters.Loader "/home/barnard/Code/Lisp/late/weka/core/converters/Loader")

  (require :weka.core.converters.SVMLightLoader "/home/barnard/Code/Lisp/late/weka/core/converters/SVMLightLoader")

  (require :weka.core.converters.SVMLightSaver "/home/barnard/Code/Lisp/late/weka/core/converters/SVMLightSaver")

  (require :weka.core.converters.Saver "/home/barnard/Code/Lisp/late/weka/core/converters/Saver")

  (require :weka.core.converters.SerializedInstancesLoader "/home/barnard/Code/Lisp/late/weka/core/converters/SerializedInstancesLoader")

  (require :weka.core.converters.SerializedInstancesSaver "/home/barnard/Code/Lisp/late/weka/core/converters/SerializedInstancesSaver")

  (require :weka.core.converters.TextDirectoryLoader "/home/barnard/Code/Lisp/late/weka/core/converters/TextDirectoryLoader")

  (require :weka.core.converters.URLSourcedLoader "/home/barnard/Code/Lisp/late/weka/core/converters/URLSourcedLoader")

  (require :weka.core.converters.XRFFLoader "/home/barnard/Code/Lisp/late/weka/core/converters/XRFFLoader")

  (require :weka.core.converters.XRFFSaver "/home/barnard/Code/Lisp/late/weka/core/converters/XRFFSaver")

  (require :weka.core.logging.ConsoleLogger "/home/barnard/Code/Lisp/late/weka/core/logging/ConsoleLogger")

  (require :weka.core.logging.FileLogger "/home/barnard/Code/Lisp/late/weka/core/logging/FileLogger")

  (require :weka.core.logging.Logger "/home/barnard/Code/Lisp/late/weka/core/logging/Logger")

  (require :weka.core.logging.OutputLogger "/home/barnard/Code/Lisp/late/weka/core/logging/OutputLogger")

  (require :weka.core.mathematicalexpression.Parser "/home/barnard/Code/Lisp/late/weka/core/mathematicalexpression/Parser")

  (require :weka.core.mathematicalexpression.Scanner "/home/barnard/Code/Lisp/late/weka/core/mathematicalexpression/Scanner")

  (require :weka.core.mathematicalexpression.sym "/home/barnard/Code/Lisp/late/weka/core/mathematicalexpression/sym")

  (require :weka.core.matrix.CholeskyDecomposition "/home/barnard/Code/Lisp/late/weka/core/matrix/CholeskyDecomposition")

  (require :weka.core.matrix.DoubleVector "/home/barnard/Code/Lisp/late/weka/core/matrix/DoubleVector")

  (require :weka.core.matrix.EigenvalueDecomposition "/home/barnard/Code/Lisp/late/weka/core/matrix/EigenvalueDecomposition")

  (require :weka.core.matrix.ExponentialFormat "/home/barnard/Code/Lisp/late/weka/core/matrix/ExponentialFormat")

  (require :weka.core.matrix.FlexibleDecimalFormat "/home/barnard/Code/Lisp/late/weka/core/matrix/FlexibleDecimalFormat")

  (require :weka.core.matrix.FloatingPointFormat "/home/barnard/Code/Lisp/late/weka/core/matrix/FloatingPointFormat")

  (require :weka.core.matrix.IntVector "/home/barnard/Code/Lisp/late/weka/core/matrix/IntVector")

  (require :weka.core.matrix.LUDecomposition "/home/barnard/Code/Lisp/late/weka/core/matrix/LUDecomposition")

  (require :weka.core.matrix.LinearRegression "/home/barnard/Code/Lisp/late/weka/core/matrix/LinearRegression")

  (require :weka.core.matrix.Maths "/home/barnard/Code/Lisp/late/weka/core/matrix/Maths")

  (require :weka.core.matrix.Matrix "/home/barnard/Code/Lisp/late/weka/core/matrix/Matrix")

  (require :weka.core.matrix.QRDecomposition "/home/barnard/Code/Lisp/late/weka/core/matrix/QRDecomposition")

  (require :weka.core.matrix.SingularValueDecomposition "/home/barnard/Code/Lisp/late/weka/core/matrix/SingularValueDecomposition")

  (require :weka.core.neighboursearch.BallTree "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/BallTree")

  (require :weka.core.neighboursearch.CoverTree "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/CoverTree")

  (require :weka.core.neighboursearch.KDTree "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/KDTree")

  (require :weka.core.neighboursearch.LinearNNSearch "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/LinearNNSearch")

  (require :weka.core.neighboursearch.NearestNeighbourSearch "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/NearestNeighbourSearch")

  (require :weka.core.neighboursearch.PerformanceStats "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/PerformanceStats")

  (require :weka.core.neighboursearch.TreePerformanceStats "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/TreePerformanceStats")

  (require :weka.core.neighboursearch.balltrees.BallNode "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/balltrees/BallNode")

  (require :weka.core.neighboursearch.balltrees.BallSplitter "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/balltrees/BallSplitter")

  (require :weka.core.neighboursearch.balltrees.BallTreeConstructor "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/balltrees/BallTreeConstructor")

  (require :weka.core.neighboursearch.balltrees.BottomUpConstructor "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/balltrees/BottomUpConstructor")

  (require :weka.core.neighboursearch.balltrees.MedianDistanceFromArbitraryPoint "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/balltrees/MedianDistanceFromArbitraryPoint")

  (require :weka.core.neighboursearch.balltrees.MedianOfWidestDimension "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/balltrees/MedianOfWidestDimension")

  (require :weka.core.neighboursearch.balltrees.MiddleOutConstructor "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/balltrees/MiddleOutConstructor")

  (require :weka.core.neighboursearch.balltrees.PointsClosestToFurthestChildren "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/balltrees/PointsClosestToFurthestChildren")

  (require :weka.core.neighboursearch.balltrees.TopDownConstructor "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/balltrees/TopDownConstructor")

  (require :weka.core.neighboursearch.covertrees.Stack "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/covertrees/Stack")

  (require :weka.core.neighboursearch.kdtrees.KDTreeNode "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/kdtrees/KDTreeNode")

  (require :weka.core.neighboursearch.kdtrees.KDTreeNodeSplitter "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/kdtrees/KDTreeNodeSplitter")

  (require :weka.core.neighboursearch.kdtrees.KMeansInpiredMethod "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/kdtrees/KMeansInpiredMethod")

  (require :weka.core.neighboursearch.kdtrees.MedianOfWidestDimension "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/kdtrees/MedianOfWidestDimension")

  (require :weka.core.neighboursearch.kdtrees.MidPointOfWidestDimension "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/kdtrees/MidPointOfWidestDimension")

  (require :weka.core.neighboursearch.kdtrees.SlidingMidPointOfWidestSide "/home/barnard/Code/Lisp/late/weka/core/neighboursearch/kdtrees/SlidingMidPointOfWidestSide")

  (require :weka.core.parser.JFlex.Action "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/Action")

  (require :weka.core.parser.JFlex.CharClassException "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/CharClassException")

  (require :weka.core.parser.JFlex.CharClassInterval "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/CharClassInterval")

  (require :weka.core.parser.JFlex.CharClasses "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/CharClasses")

  (require :weka.core.parser.JFlex.CharSet "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/CharSet")

  (require :weka.core.parser.JFlex.CharSetEnumerator "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/CharSetEnumerator")

  (require :weka.core.parser.JFlex.CountEmitter "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/CountEmitter")

  (require :weka.core.parser.JFlex.DFA "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/DFA")

  (require :weka.core.parser.JFlex.EOFActions "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/EOFActions")

  (require :weka.core.parser.JFlex.Emitter "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/Emitter")

  (require :weka.core.parser.JFlex.ErrorMessages "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/ErrorMessages")

  (require :weka.core.parser.JFlex.GeneratorException "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/GeneratorException")

  (require :weka.core.parser.JFlex.HiLowEmitter "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/HiLowEmitter")

  (require :weka.core.parser.JFlex.IntCharSet "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/IntCharSet")

  (require :weka.core.parser.JFlex.IntPair "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/IntPair")

  (require :weka.core.parser.JFlex.Interval "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/Interval")

  (require :weka.core.parser.JFlex.LexParse "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/LexParse")

  (require :weka.core.parser.JFlex.LexScan "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/LexScan")

  (require :weka.core.parser.JFlex.LexicalStates "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/LexicalStates")

  (require :weka.core.parser.JFlex.MacroException "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/MacroException")

  (require :weka.core.parser.JFlex.Macros "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/Macros")

  (require :weka.core.parser.JFlex.Main "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/Main")

  (require :weka.core.parser.JFlex.NFA "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/NFA")

  (require :weka.core.parser.JFlex.Options "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/Options")

  (require :weka.core.parser.JFlex.Out "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/Out")

  (require :weka.core.parser.JFlex.PackEmitter "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/PackEmitter")

  (require :weka.core.parser.JFlex.RegExp "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/RegExp")

  (require :weka.core.parser.JFlex.RegExp1 "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/RegExp1")

  (require :weka.core.parser.JFlex.RegExp2 "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/RegExp2")

  (require :weka.core.parser.JFlex.RegExps "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/RegExps")

  (require :weka.core.parser.JFlex.ScannerException "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/ScannerException")

  (require :weka.core.parser.JFlex.SemCheck "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/SemCheck")

  (require :weka.core.parser.JFlex.SilentExit "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/SilentExit")

  (require :weka.core.parser.JFlex.Skeleton "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/Skeleton")

  (require :weka.core.parser.JFlex.StatePairList "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/StatePairList")

  (require :weka.core.parser.JFlex.StateSet "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/StateSet")

  (require :weka.core.parser.JFlex.StateSetEnumerator "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/StateSetEnumerator")

  (require :weka.core.parser.JFlex.StdOutWriter "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/StdOutWriter")

  (require :weka.core.parser.JFlex.Timer "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/Timer")

  (require :weka.core.parser.JFlex.sym "/home/barnard/Code/Lisp/late/weka/core/parser/JFlex/sym")

  (require :weka.core.parser.java_cup.ErrorManager "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/ErrorManager")

  (require :weka.core.parser.java_cup.Lexer "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/Lexer")

  (require :weka.core.parser.java_cup.Main "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/Main")

  (require :weka.core.parser.java_cup.action_part "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/action_part")

  (require :weka.core.parser.java_cup.action_production "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/action_production")

  (require :weka.core.parser.java_cup.assoc "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/assoc")

  (require :weka.core.parser.java_cup.emit "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/emit")

  (require :weka.core.parser.java_cup.internal_error "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/internal_error")

  (require :weka.core.parser.java_cup.lalr_item "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/lalr_item")

  (require :weka.core.parser.java_cup.lalr_item_set "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/lalr_item_set")

  (require :weka.core.parser.java_cup.lalr_state "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/lalr_state")

  (require :weka.core.parser.java_cup.lalr_transition "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/lalr_transition")

  (require :weka.core.parser.java_cup.lr_item_core "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/lr_item_core")

  (require :weka.core.parser.java_cup.non_terminal "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/non_terminal")

  (require :weka.core.parser.java_cup.nonassoc_action "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/nonassoc_action")

  (require :weka.core.parser.java_cup.parse_action "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/parse_action")

  (require :weka.core.parser.java_cup.parse_action_row "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/parse_action_row")

  (require :weka.core.parser.java_cup.parse_action_table "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/parse_action_table")

  (require :weka.core.parser.java_cup.parse_reduce_row "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/parse_reduce_row")

  (require :weka.core.parser.java_cup.parse_reduce_table "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/parse_reduce_table")

  (require :weka.core.parser.java_cup.parser "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/parser")

  (require :weka.core.parser.java_cup.production "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/production")

  (require :weka.core.parser.java_cup.production_part "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/production_part")

  (require :weka.core.parser.java_cup.reduce_action "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/reduce_action")

  (require :weka.core.parser.java_cup.runtime.ComplexSymbolFactory "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/runtime/ComplexSymbolFactory")

  (require :weka.core.parser.java_cup.runtime.DefaultSymbolFactory "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/runtime/DefaultSymbolFactory")

  (require :weka.core.parser.java_cup.runtime.Scanner "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/runtime/Scanner")

  (require :weka.core.parser.java_cup.runtime.Symbol "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/runtime/Symbol")

  (require :weka.core.parser.java_cup.runtime.SymbolFactory "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/runtime/SymbolFactory")

  (require :weka.core.parser.java_cup.runtime.lr_parser "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/runtime/lr_parser")

  (require :weka.core.parser.java_cup.runtime.virtual_parse_stack "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/runtime/virtual_parse_stack")

  (require :weka.core.parser.java_cup.shift_action "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/shift_action")

  (require :weka.core.parser.java_cup.sym "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/sym")

  (require :weka.core.parser.java_cup.symbol "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/symbol")

  (require :weka.core.parser.java_cup.symbol_part "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/symbol_part")

  (require :weka.core.parser.java_cup.symbol_set "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/symbol_set")

  (require :weka.core.parser.java_cup.terminal "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/terminal")

  (require :weka.core.parser.java_cup.terminal_set "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/terminal_set")

  (require :weka.core.parser.java_cup.version "/home/barnard/Code/Lisp/late/weka/core/parser/java_cup/version")

  (require :weka.core.pmml.Apply "/home/barnard/Code/Lisp/late/weka/core/pmml/Apply")

  (require :weka.core.pmml.BuiltInArithmetic "/home/barnard/Code/Lisp/late/weka/core/pmml/BuiltInArithmetic")

  (require :weka.core.pmml.BuiltInMath "/home/barnard/Code/Lisp/late/weka/core/pmml/BuiltInMath")

  (require :weka.core.pmml.BuiltInString "/home/barnard/Code/Lisp/late/weka/core/pmml/BuiltInString")

  (require :weka.core.pmml.Constant "/home/barnard/Code/Lisp/late/weka/core/pmml/Constant")

  (require :weka.core.pmml.DefineFunction "/home/barnard/Code/Lisp/late/weka/core/pmml/DefineFunction")

  (require :weka.core.pmml.DerivedFieldMetaInfo "/home/barnard/Code/Lisp/late/weka/core/pmml/DerivedFieldMetaInfo")

  (require :weka.core.pmml.Discretize "/home/barnard/Code/Lisp/late/weka/core/pmml/Discretize")

  (require :weka.core.pmml.Expression "/home/barnard/Code/Lisp/late/weka/core/pmml/Expression")

  (require :weka.core.pmml.FieldMetaInfo "/home/barnard/Code/Lisp/late/weka/core/pmml/FieldMetaInfo")

  (require :weka.core.pmml.FieldRef "/home/barnard/Code/Lisp/late/weka/core/pmml/FieldRef")

  (require :weka.core.pmml.Function "/home/barnard/Code/Lisp/late/weka/core/pmml/Function")

  (require :weka.core.pmml.MappingInfo "/home/barnard/Code/Lisp/late/weka/core/pmml/MappingInfo")

  (require :weka.core.pmml.MiningFieldMetaInfo "/home/barnard/Code/Lisp/late/weka/core/pmml/MiningFieldMetaInfo")

  (require :weka.core.pmml.MiningSchema "/home/barnard/Code/Lisp/late/weka/core/pmml/MiningSchema")

  (require :weka.core.pmml.NormContinuous "/home/barnard/Code/Lisp/late/weka/core/pmml/NormContinuous")

  (require :weka.core.pmml.NormDiscrete "/home/barnard/Code/Lisp/late/weka/core/pmml/NormDiscrete")

  (require :weka.core.pmml.PMMLFactory "/home/barnard/Code/Lisp/late/weka/core/pmml/PMMLFactory")

  (require :weka.core.pmml.PMMLModel "/home/barnard/Code/Lisp/late/weka/core/pmml/PMMLModel")

  (require :weka.core.pmml.PMMLUtils "/home/barnard/Code/Lisp/late/weka/core/pmml/PMMLUtils")

  (require :weka.core.pmml.TargetMetaInfo "/home/barnard/Code/Lisp/late/weka/core/pmml/TargetMetaInfo")

  (require :weka.core.pmml.TransformationDictionary "/home/barnard/Code/Lisp/late/weka/core/pmml/TransformationDictionary")

  (require :weka.core.stemmers.IteratedLovinsStemmer "/home/barnard/Code/Lisp/late/weka/core/stemmers/IteratedLovinsStemmer")

  (require :weka.core.stemmers.LovinsStemmer "/home/barnard/Code/Lisp/late/weka/core/stemmers/LovinsStemmer")

  (require :weka.core.stemmers.NullStemmer "/home/barnard/Code/Lisp/late/weka/core/stemmers/NullStemmer")

  (require :weka.core.stemmers.SnowballStemmer "/home/barnard/Code/Lisp/late/weka/core/stemmers/SnowballStemmer")

  (require :weka.core.stemmers.Stemmer "/home/barnard/Code/Lisp/late/weka/core/stemmers/Stemmer")

  (require :weka.core.stemmers.Stemming "/home/barnard/Code/Lisp/late/weka/core/stemmers/Stemming")

  (require :weka.core.tokenizers.AlphabeticTokenizer "/home/barnard/Code/Lisp/late/weka/core/tokenizers/AlphabeticTokenizer")

  (require :weka.core.tokenizers.CharacterDelimitedTokenizer "/home/barnard/Code/Lisp/late/weka/core/tokenizers/CharacterDelimitedTokenizer")

  (require :weka.core.tokenizers.NGramTokenizer "/home/barnard/Code/Lisp/late/weka/core/tokenizers/NGramTokenizer")

  (require :weka.core.tokenizers.Tokenizer "/home/barnard/Code/Lisp/late/weka/core/tokenizers/Tokenizer")

  (require :weka.core.tokenizers.WordTokenizer "/home/barnard/Code/Lisp/late/weka/core/tokenizers/WordTokenizer")

  (require :weka.core.xml.KOML "/home/barnard/Code/Lisp/late/weka/core/xml/KOML")

  (require :weka.core.xml.MethodHandler "/home/barnard/Code/Lisp/late/weka/core/xml/MethodHandler")

  (require :weka.core.xml.PropertyHandler "/home/barnard/Code/Lisp/late/weka/core/xml/PropertyHandler")

  (require :weka.core.xml.SerialUIDChanger "/home/barnard/Code/Lisp/late/weka/core/xml/SerialUIDChanger")

  (require :weka.core.xml.XMLBasicSerialization "/home/barnard/Code/Lisp/late/weka/core/xml/XMLBasicSerialization")

  (require :weka.core.xml.XMLDocument "/home/barnard/Code/Lisp/late/weka/core/xml/XMLDocument")

  (require :weka.core.xml.XMLInstances "/home/barnard/Code/Lisp/late/weka/core/xml/XMLInstances")

  (require :weka.core.xml.XMLOptions "/home/barnard/Code/Lisp/late/weka/core/xml/XMLOptions")

  (require :weka.core.xml.XMLSerialization "/home/barnard/Code/Lisp/late/weka/core/xml/XMLSerialization")

  (require :weka.core.xml.XMLSerializationMethodHandler "/home/barnard/Code/Lisp/late/weka/core/xml/XMLSerializationMethodHandler")

  (require :weka.core.xml.XStream "/home/barnard/Code/Lisp/late/weka/core/xml/XStream")

  (require :weka.datagenerators.ClassificationGenerator "/home/barnard/Code/Lisp/late/weka/datagenerators/ClassificationGenerator")

  (require :weka.datagenerators.ClusterDefinition "/home/barnard/Code/Lisp/late/weka/datagenerators/ClusterDefinition")

  (require :weka.datagenerators.ClusterGenerator "/home/barnard/Code/Lisp/late/weka/datagenerators/ClusterGenerator")

  (require :weka.datagenerators.DataGenerator "/home/barnard/Code/Lisp/late/weka/datagenerators/DataGenerator")

  (require :weka.datagenerators.RegressionGenerator "/home/barnard/Code/Lisp/late/weka/datagenerators/RegressionGenerator")

  (require :weka.datagenerators.Test "/home/barnard/Code/Lisp/late/weka/datagenerators/Test")

  (require :weka.datagenerators.classifiers.classification.Agrawal "/home/barnard/Code/Lisp/late/weka/datagenerators/classifiers/classification/Agrawal")

  (require :weka.datagenerators.classifiers.classification.BayesNet "/home/barnard/Code/Lisp/late/weka/datagenerators/classifiers/classification/BayesNet")

  (require :weka.datagenerators.classifiers.classification.LED24 "/home/barnard/Code/Lisp/late/weka/datagenerators/classifiers/classification/LED24")

  (require :weka.datagenerators.classifiers.classification.RDG1 "/home/barnard/Code/Lisp/late/weka/datagenerators/classifiers/classification/RDG1")

  (require :weka.datagenerators.classifiers.classification.RandomRBF "/home/barnard/Code/Lisp/late/weka/datagenerators/classifiers/classification/RandomRBF")

  (require :weka.datagenerators.classifiers.regression.Expression "/home/barnard/Code/Lisp/late/weka/datagenerators/classifiers/regression/Expression")

  (require :weka.datagenerators.classifiers.regression.MexicanHat "/home/barnard/Code/Lisp/late/weka/datagenerators/classifiers/regression/MexicanHat")

  (require :weka.datagenerators.clusterers.BIRCHCluster "/home/barnard/Code/Lisp/late/weka/datagenerators/clusterers/BIRCHCluster")

  (require :weka.datagenerators.clusterers.SubspaceCluster "/home/barnard/Code/Lisp/late/weka/datagenerators/clusterers/SubspaceCluster")

  (require :weka.datagenerators.clusterers.SubspaceClusterDefinition "/home/barnard/Code/Lisp/late/weka/datagenerators/clusterers/SubspaceClusterDefinition")

  (require :weka.estimators.CheckEstimator "/home/barnard/Code/Lisp/late/weka/estimators/CheckEstimator")

  (require :weka.estimators.ConditionalEstimator "/home/barnard/Code/Lisp/late/weka/estimators/ConditionalEstimator")

  (require :weka.estimators.DDConditionalEstimator "/home/barnard/Code/Lisp/late/weka/estimators/DDConditionalEstimator")

  (require :weka.estimators.DKConditionalEstimator "/home/barnard/Code/Lisp/late/weka/estimators/DKConditionalEstimator")

  (require :weka.estimators.DNConditionalEstimator "/home/barnard/Code/Lisp/late/weka/estimators/DNConditionalEstimator")

  (require :weka.estimators.DiscreteEstimator "/home/barnard/Code/Lisp/late/weka/estimators/DiscreteEstimator")

  (require :weka.estimators.Estimator "/home/barnard/Code/Lisp/late/weka/estimators/Estimator")

  (require :weka.estimators.EstimatorUtils "/home/barnard/Code/Lisp/late/weka/estimators/EstimatorUtils")

  (require :weka.estimators.IncrementalEstimator "/home/barnard/Code/Lisp/late/weka/estimators/IncrementalEstimator")

  (require :weka.estimators.KDConditionalEstimator "/home/barnard/Code/Lisp/late/weka/estimators/KDConditionalEstimator")

  (require :weka.estimators.KKConditionalEstimator "/home/barnard/Code/Lisp/late/weka/estimators/KKConditionalEstimator")

  (require :weka.estimators.KernelEstimator "/home/barnard/Code/Lisp/late/weka/estimators/KernelEstimator")

  (require :weka.estimators.MahalanobisEstimator "/home/barnard/Code/Lisp/late/weka/estimators/MahalanobisEstimator")

  (require :weka.estimators.NDConditionalEstimator "/home/barnard/Code/Lisp/late/weka/estimators/NDConditionalEstimator")

  (require :weka.estimators.NNConditionalEstimator "/home/barnard/Code/Lisp/late/weka/estimators/NNConditionalEstimator")

  (require :weka.estimators.NormalEstimator "/home/barnard/Code/Lisp/late/weka/estimators/NormalEstimator")

  (require :weka.estimators.PoissonEstimator "/home/barnard/Code/Lisp/late/weka/estimators/PoissonEstimator")

  (require :weka.experiment.AveragingResultProducer "/home/barnard/Code/Lisp/late/weka/experiment/AveragingResultProducer")

  (require :weka.experiment.CSVResultListener "/home/barnard/Code/Lisp/late/weka/experiment/CSVResultListener")

  (require :weka.experiment.ClassifierSplitEvaluator "/home/barnard/Code/Lisp/late/weka/experiment/ClassifierSplitEvaluator")

  (require :weka.experiment.Compute "/home/barnard/Code/Lisp/late/weka/experiment/Compute")

  (require :weka.experiment.CostSensitiveClassifierSplitEvaluator "/home/barnard/Code/Lisp/late/weka/experiment/CostSensitiveClassifierSplitEvaluator")

  (require :weka.experiment.CrossValidationResultProducer "/home/barnard/Code/Lisp/late/weka/experiment/CrossValidationResultProducer")

  (require :weka.experiment.DatabaseResultListener "/home/barnard/Code/Lisp/late/weka/experiment/DatabaseResultListener")

  (require :weka.experiment.DatabaseResultProducer "/home/barnard/Code/Lisp/late/weka/experiment/DatabaseResultProducer")

  (require :weka.experiment.DatabaseUtils "/home/barnard/Code/Lisp/late/weka/experiment/DatabaseUtils")

  (require :weka.experiment.DensityBasedClustererSplitEvaluator "/home/barnard/Code/Lisp/late/weka/experiment/DensityBasedClustererSplitEvaluator")

  (require :weka.experiment.Experiment "/home/barnard/Code/Lisp/late/weka/experiment/Experiment")

  (require :weka.experiment.InstanceQuery "/home/barnard/Code/Lisp/late/weka/experiment/InstanceQuery")

  (require :weka.experiment.InstancesResultListener "/home/barnard/Code/Lisp/late/weka/experiment/InstancesResultListener")

  (require :weka.experiment.LearningRateResultProducer "/home/barnard/Code/Lisp/late/weka/experiment/LearningRateResultProducer")

  (require :weka.experiment.OutputZipper "/home/barnard/Code/Lisp/late/weka/experiment/OutputZipper")

  (require :weka.experiment.PairedCorrectedTTester "/home/barnard/Code/Lisp/late/weka/experiment/PairedCorrectedTTester")

  (require :weka.experiment.PairedStats "/home/barnard/Code/Lisp/late/weka/experiment/PairedStats")

  (require :weka.experiment.PairedStatsCorrected "/home/barnard/Code/Lisp/late/weka/experiment/PairedStatsCorrected")

  (require :weka.experiment.PairedTTester "/home/barnard/Code/Lisp/late/weka/experiment/PairedTTester")

  (require :weka.experiment.PropertyNode "/home/barnard/Code/Lisp/late/weka/experiment/PropertyNode")

  (require :weka.experiment.RandomSplitResultProducer "/home/barnard/Code/Lisp/late/weka/experiment/RandomSplitResultProducer")

  (require :weka.experiment.RegressionSplitEvaluator "/home/barnard/Code/Lisp/late/weka/experiment/RegressionSplitEvaluator")

  (require :weka.experiment.RemoteEngine "/home/barnard/Code/Lisp/late/weka/experiment/RemoteEngine")

  (require :weka.experiment.RemoteEngine_Stub "/home/barnard/Code/Lisp/late/weka/experiment/RemoteEngine_Stub")

  (require :weka.experiment.RemoteExperiment "/home/barnard/Code/Lisp/late/weka/experiment/RemoteExperiment")

  (require :weka.experiment.RemoteExperimentEvent "/home/barnard/Code/Lisp/late/weka/experiment/RemoteExperimentEvent")

  (require :weka.experiment.RemoteExperimentListener "/home/barnard/Code/Lisp/late/weka/experiment/RemoteExperimentListener")

  (require :weka.experiment.RemoteExperimentSubTask "/home/barnard/Code/Lisp/late/weka/experiment/RemoteExperimentSubTask")

  (require :weka.experiment.ResultListener "/home/barnard/Code/Lisp/late/weka/experiment/ResultListener")

  (require :weka.experiment.ResultMatrix "/home/barnard/Code/Lisp/late/weka/experiment/ResultMatrix")

  (require :weka.experiment.ResultMatrixCSV "/home/barnard/Code/Lisp/late/weka/experiment/ResultMatrixCSV")

  (require :weka.experiment.ResultMatrixGnuPlot "/home/barnard/Code/Lisp/late/weka/experiment/ResultMatrixGnuPlot")

  (require :weka.experiment.ResultMatrixHTML "/home/barnard/Code/Lisp/late/weka/experiment/ResultMatrixHTML")

  (require :weka.experiment.ResultMatrixLatex "/home/barnard/Code/Lisp/late/weka/experiment/ResultMatrixLatex")

  (require :weka.experiment.ResultMatrixPlainText "/home/barnard/Code/Lisp/late/weka/experiment/ResultMatrixPlainText")

  (require :weka.experiment.ResultMatrixSignificance "/home/barnard/Code/Lisp/late/weka/experiment/ResultMatrixSignificance")

  (require :weka.experiment.ResultProducer "/home/barnard/Code/Lisp/late/weka/experiment/ResultProducer")

  (require :weka.experiment.SplitEvaluator "/home/barnard/Code/Lisp/late/weka/experiment/SplitEvaluator")

  (require :weka.experiment.Stats "/home/barnard/Code/Lisp/late/weka/experiment/Stats")

  (require :weka.experiment.Task "/home/barnard/Code/Lisp/late/weka/experiment/Task")

  (require :weka.experiment.TaskStatusInfo "/home/barnard/Code/Lisp/late/weka/experiment/TaskStatusInfo")

  (require :weka.experiment.Tester "/home/barnard/Code/Lisp/late/weka/experiment/Tester")

  (require :weka.experiment.xml.XMLExperiment "/home/barnard/Code/Lisp/late/weka/experiment/xml/XMLExperiment")

  (require :weka.filters.AllFilter "/home/barnard/Code/Lisp/late/weka/filters/AllFilter")

  (require :weka.filters.CheckSource "/home/barnard/Code/Lisp/late/weka/filters/CheckSource")

  (require :weka.filters.Filter "/home/barnard/Code/Lisp/late/weka/filters/Filter")

  (require :weka.filters.MultiFilter "/home/barnard/Code/Lisp/late/weka/filters/MultiFilter")

  (require :weka.filters.SimpleBatchFilter "/home/barnard/Code/Lisp/late/weka/filters/SimpleBatchFilter")

  (require :weka.filters.SimpleFilter "/home/barnard/Code/Lisp/late/weka/filters/SimpleFilter")

  (require :weka.filters.SimpleStreamFilter "/home/barnard/Code/Lisp/late/weka/filters/SimpleStreamFilter")

  (require :weka.filters.Sourcable "/home/barnard/Code/Lisp/late/weka/filters/Sourcable")

  (require :weka.filters.StreamableFilter "/home/barnard/Code/Lisp/late/weka/filters/StreamableFilter")

  (require :weka.filters.SupervisedFilter "/home/barnard/Code/Lisp/late/weka/filters/SupervisedFilter")

  (require :weka.filters.UnsupervisedFilter "/home/barnard/Code/Lisp/late/weka/filters/UnsupervisedFilter")

  (require :weka.filters.supervised.attribute.AddClassification "/home/barnard/Code/Lisp/late/weka/filters/supervised/attribute/AddClassification")

  (require :weka.filters.supervised.attribute.AttributeSelection "/home/barnard/Code/Lisp/late/weka/filters/supervised/attribute/AttributeSelection")

  (require :weka.filters.supervised.attribute.ClassOrder "/home/barnard/Code/Lisp/late/weka/filters/supervised/attribute/ClassOrder")

  (require :weka.filters.supervised.attribute.Discretize "/home/barnard/Code/Lisp/late/weka/filters/supervised/attribute/Discretize")

  (require :weka.filters.supervised.attribute.NominalToBinary "/home/barnard/Code/Lisp/late/weka/filters/supervised/attribute/NominalToBinary")

  (require :weka.filters.supervised.attribute.PLSFilter "/home/barnard/Code/Lisp/late/weka/filters/supervised/attribute/PLSFilter")

  (require :weka.filters.supervised.instance.Resample "/home/barnard/Code/Lisp/late/weka/filters/supervised/instance/Resample")

  (require :weka.filters.supervised.instance.SMOTE "/home/barnard/Code/Lisp/late/weka/filters/supervised/instance/SMOTE")

  (require :weka.filters.supervised.instance.SpreadSubsample "/home/barnard/Code/Lisp/late/weka/filters/supervised/instance/SpreadSubsample")

  (require :weka.filters.supervised.instance.StratifiedRemoveFolds "/home/barnard/Code/Lisp/late/weka/filters/supervised/instance/StratifiedRemoveFolds")

  (require :weka.filters.unsupervised.attribute.AbstractTimeSeries "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/AbstractTimeSeries")

  (require :weka.filters.unsupervised.attribute.Add "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/Add")

  (require :weka.filters.unsupervised.attribute.AddCluster "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/AddCluster")

  (require :weka.filters.unsupervised.attribute.AddExpression "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/AddExpression")

  (require :weka.filters.unsupervised.attribute.AddID "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/AddID")

  (require :weka.filters.unsupervised.attribute.AddNoise "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/AddNoise")

  (require :weka.filters.unsupervised.attribute.AddValues "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/AddValues")

  (require :weka.filters.unsupervised.attribute.Center "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/Center")

  (require :weka.filters.unsupervised.attribute.ChangeDateFormat "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/ChangeDateFormat")

  (require :weka.filters.unsupervised.attribute.ClassAssigner "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/ClassAssigner")

  (require :weka.filters.unsupervised.attribute.ClusterMembership "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/ClusterMembership")

  (require :weka.filters.unsupervised.attribute.Copy "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/Copy")

  (require :weka.filters.unsupervised.attribute.Discretize "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/Discretize")

  (require :weka.filters.unsupervised.attribute.FirstOrder "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/FirstOrder")

  (require :weka.filters.unsupervised.attribute.InterquartileRange "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/InterquartileRange")

  (require :weka.filters.unsupervised.attribute.KernelFilter "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/KernelFilter")

  (require :weka.filters.unsupervised.attribute.MakeIndicator "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/MakeIndicator")

  (require :weka.filters.unsupervised.attribute.MathExpression "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/MathExpression")

  (require :weka.filters.unsupervised.attribute.MergeTwoValues "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/MergeTwoValues")

  (require :weka.filters.unsupervised.attribute.MultiInstanceToPropositional "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/MultiInstanceToPropositional")

  (require :weka.filters.unsupervised.attribute.NominalToBinary "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/NominalToBinary")

  (require :weka.filters.unsupervised.attribute.NominalToString "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/NominalToString")

  (require :weka.filters.unsupervised.attribute.Normalize "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/Normalize")

  (require :weka.filters.unsupervised.attribute.NumericCleaner "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/NumericCleaner")

  (require :weka.filters.unsupervised.attribute.NumericToBinary "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/NumericToBinary")

  (require :weka.filters.unsupervised.attribute.NumericToNominal "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/NumericToNominal")

  (require :weka.filters.unsupervised.attribute.NumericTransform "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/NumericTransform")

  (require :weka.filters.unsupervised.attribute.Obfuscate "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/Obfuscate")

  (require :weka.filters.unsupervised.attribute.PKIDiscretize "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/PKIDiscretize")

  (require :weka.filters.unsupervised.attribute.PartitionedMultiFilter "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/PartitionedMultiFilter")

  (require :weka.filters.unsupervised.attribute.PotentialClassIgnorer "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/PotentialClassIgnorer")

  (require :weka.filters.unsupervised.attribute.PrincipalComponents "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/PrincipalComponents")

  (require :weka.filters.unsupervised.attribute.PropositionalToMultiInstance "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/PropositionalToMultiInstance")

  (require :weka.filters.unsupervised.attribute.RELAGGS "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/RELAGGS")

  (require :weka.filters.unsupervised.attribute.RandomProjection "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/RandomProjection")

  (require :weka.filters.unsupervised.attribute.RandomSubset "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/RandomSubset")

  (require :weka.filters.unsupervised.attribute.Remove "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/Remove")

  (require :weka.filters.unsupervised.attribute.RemoveType "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/RemoveType")

  (require :weka.filters.unsupervised.attribute.RemoveUseless "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/RemoveUseless")

  (require :weka.filters.unsupervised.attribute.Reorder "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/Reorder")

  (require :weka.filters.unsupervised.attribute.ReplaceMissingValues "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/ReplaceMissingValues")

  (require :weka.filters.unsupervised.attribute.Standardize "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/Standardize")

  (require :weka.filters.unsupervised.attribute.StringToNominal "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/StringToNominal")

  (require :weka.filters.unsupervised.attribute.StringToWordVector "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/StringToWordVector")

  (require :weka.filters.unsupervised.attribute.SwapValues "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/SwapValues")

  (require :weka.filters.unsupervised.attribute.TimeSeriesDelta "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/TimeSeriesDelta")

  (require :weka.filters.unsupervised.attribute.TimeSeriesTranslate "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/TimeSeriesTranslate")

  (require :weka.filters.unsupervised.attribute.Wavelet "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/attribute/Wavelet")

  (require :weka.filters.unsupervised.instance.NonSparseToSparse "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/NonSparseToSparse")

  (require :weka.filters.unsupervised.instance.Normalize "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/Normalize")

  (require :weka.filters.unsupervised.instance.Randomize "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/Randomize")

  (require :weka.filters.unsupervised.instance.RemoveFolds "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/RemoveFolds")

  (require :weka.filters.unsupervised.instance.RemoveFrequentValues "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/RemoveFrequentValues")

  (require :weka.filters.unsupervised.instance.RemoveMisclassified "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/RemoveMisclassified")

  (require :weka.filters.unsupervised.instance.RemovePercentage "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/RemovePercentage")

  (require :weka.filters.unsupervised.instance.RemoveRange "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/RemoveRange")

  (require :weka.filters.unsupervised.instance.RemoveWithValues "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/RemoveWithValues")

  (require :weka.filters.unsupervised.instance.Resample "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/Resample")

  (require :weka.filters.unsupervised.instance.ReservoirSample "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/ReservoirSample")

  (require :weka.filters.unsupervised.instance.SparseToNonSparse "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/SparseToNonSparse")

  (require :weka.filters.unsupervised.instance.SubsetByExpression "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/SubsetByExpression")

  (require :weka.filters.unsupervised.instance.subsetbyexpression.Parser "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/subsetbyexpression/Parser")

  (require :weka.filters.unsupervised.instance.subsetbyexpression.Scanner "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/subsetbyexpression/Scanner")

  (require :weka.filters.unsupervised.instance.subsetbyexpression.sym "/home/barnard/Code/Lisp/late/weka/filters/unsupervised/instance/subsetbyexpression/sym")

  (require :weka.gui.AttributeListPanel "/home/barnard/Code/Lisp/late/weka/gui/AttributeListPanel")

  (require :weka.gui.AttributeSelectionPanel "/home/barnard/Code/Lisp/late/weka/gui/AttributeSelectionPanel")

  (require :weka.gui.AttributeSummaryPanel "/home/barnard/Code/Lisp/late/weka/gui/AttributeSummaryPanel")

  (require :weka.gui.AttributeVisualizationPanel "/home/barnard/Code/Lisp/late/weka/gui/AttributeVisualizationPanel")

  (require :weka.gui.BrowserHelper "/home/barnard/Code/Lisp/late/weka/gui/BrowserHelper")

  (require :weka.gui.CheckBoxList "/home/barnard/Code/Lisp/late/weka/gui/CheckBoxList")

  (require :weka.gui.ComponentHelper "/home/barnard/Code/Lisp/late/weka/gui/ComponentHelper")

  (require :weka.gui.ConverterFileChooser "/home/barnard/Code/Lisp/late/weka/gui/ConverterFileChooser")

  (require :weka.gui.CostMatrixEditor "/home/barnard/Code/Lisp/late/weka/gui/CostMatrixEditor")

  (require :weka.gui.CustomPanelSupplier "/home/barnard/Code/Lisp/late/weka/gui/CustomPanelSupplier")

  (require :weka.gui.DatabaseConnectionDialog "/home/barnard/Code/Lisp/late/weka/gui/DatabaseConnectionDialog")

  (require :weka.gui.EnsembleLibraryEditor "/home/barnard/Code/Lisp/late/weka/gui/EnsembleLibraryEditor")

  (require :weka.gui.EnsembleSelectionLibraryEditor "/home/barnard/Code/Lisp/late/weka/gui/EnsembleSelectionLibraryEditor")

  (require :weka.gui.ExtensionFileFilter "/home/barnard/Code/Lisp/late/weka/gui/ExtensionFileFilter")

  (require :weka.gui.FileEditor "/home/barnard/Code/Lisp/late/weka/gui/FileEditor")

  (require :weka.gui.GUIChooser "/home/barnard/Code/Lisp/late/weka/gui/GUIChooser")

  (require :weka.gui.GenericArrayEditor "/home/barnard/Code/Lisp/late/weka/gui/GenericArrayEditor")

  (require :weka.gui.GenericObjectEditor "/home/barnard/Code/Lisp/late/weka/gui/GenericObjectEditor")

  (require :weka.gui.GenericPropertiesCreator "/home/barnard/Code/Lisp/late/weka/gui/GenericPropertiesCreator")

  (require :weka.gui.HierarchyPropertyParser "/home/barnard/Code/Lisp/late/weka/gui/HierarchyPropertyParser")

  (require :weka.gui.InstancesSummaryPanel "/home/barnard/Code/Lisp/late/weka/gui/InstancesSummaryPanel")

  (require :weka.gui.JListHelper "/home/barnard/Code/Lisp/late/weka/gui/JListHelper")

  (require :weka.gui.JTableHelper "/home/barnard/Code/Lisp/late/weka/gui/JTableHelper")

  (require :weka.gui.ListSelectorDialog "/home/barnard/Code/Lisp/late/weka/gui/ListSelectorDialog")

  (require :weka.gui.Loader "/home/barnard/Code/Lisp/late/weka/gui/Loader")

  (require :weka.gui.LogPanel "/home/barnard/Code/Lisp/late/weka/gui/LogPanel")

  (require :weka.gui.LogWindow "/home/barnard/Code/Lisp/late/weka/gui/LogWindow")

  (require :weka.gui.Logger "/home/barnard/Code/Lisp/late/weka/gui/Logger")

  (require :weka.gui.LookAndFeel "/home/barnard/Code/Lisp/late/weka/gui/LookAndFeel")

  (require :weka.gui.Main "/home/barnard/Code/Lisp/late/weka/gui/Main")

  (require :weka.gui.MainMenuExtension "/home/barnard/Code/Lisp/late/weka/gui/MainMenuExtension")

  (require :weka.gui.MemoryUsagePanel "/home/barnard/Code/Lisp/late/weka/gui/MemoryUsagePanel")

  (require :weka.gui.PropertyDialog "/home/barnard/Code/Lisp/late/weka/gui/PropertyDialog")

  (require :weka.gui.PropertyPanel "/home/barnard/Code/Lisp/late/weka/gui/PropertyPanel")

  (require :weka.gui.PropertySelectorDialog "/home/barnard/Code/Lisp/late/weka/gui/PropertySelectorDialog")

  (require :weka.gui.PropertySheetPanel "/home/barnard/Code/Lisp/late/weka/gui/PropertySheetPanel")

  (require :weka.gui.PropertyText "/home/barnard/Code/Lisp/late/weka/gui/PropertyText")

  (require :weka.gui.PropertyValueSelector "/home/barnard/Code/Lisp/late/weka/gui/PropertyValueSelector")

  (require :weka.gui.ResultHistoryPanel "/home/barnard/Code/Lisp/late/weka/gui/ResultHistoryPanel")

  (require :weka.gui.SaveBuffer "/home/barnard/Code/Lisp/late/weka/gui/SaveBuffer")

  (require :weka.gui.SelectedTagEditor "/home/barnard/Code/Lisp/late/weka/gui/SelectedTagEditor")

  (require :weka.gui.SetInstancesPanel "/home/barnard/Code/Lisp/late/weka/gui/SetInstancesPanel")

  (require :weka.gui.SimpleCLI "/home/barnard/Code/Lisp/late/weka/gui/SimpleCLI")

  (require :weka.gui.SimpleCLIPanel "/home/barnard/Code/Lisp/late/weka/gui/SimpleCLIPanel")

  (require :weka.gui.SimpleDateFormatEditor "/home/barnard/Code/Lisp/late/weka/gui/SimpleDateFormatEditor")

  (require :weka.gui.SortedTableModel "/home/barnard/Code/Lisp/late/weka/gui/SortedTableModel")

  (require :weka.gui.SplashWindow "/home/barnard/Code/Lisp/late/weka/gui/SplashWindow")

  (require :weka.gui.SysErrLog "/home/barnard/Code/Lisp/late/weka/gui/SysErrLog")

  (require :weka.gui.TaskLogger "/home/barnard/Code/Lisp/late/weka/gui/TaskLogger")

  (require :weka.gui.ViewerDialog "/home/barnard/Code/Lisp/late/weka/gui/ViewerDialog")

  (require :weka.gui.WekaTaskMonitor "/home/barnard/Code/Lisp/late/weka/gui/WekaTaskMonitor")

  (require :weka.gui.arffviewer.ArffPanel "/home/barnard/Code/Lisp/late/weka/gui/arffviewer/ArffPanel")

  (require :weka.gui.arffviewer.ArffSortedTableModel "/home/barnard/Code/Lisp/late/weka/gui/arffviewer/ArffSortedTableModel")

  (require :weka.gui.arffviewer.ArffTable "/home/barnard/Code/Lisp/late/weka/gui/arffviewer/ArffTable")

  (require :weka.gui.arffviewer.ArffTableCellRenderer "/home/barnard/Code/Lisp/late/weka/gui/arffviewer/ArffTableCellRenderer")

  (require :weka.gui.arffviewer.ArffTableModel "/home/barnard/Code/Lisp/late/weka/gui/arffviewer/ArffTableModel")

  (require :weka.gui.arffviewer.ArffViewer "/home/barnard/Code/Lisp/late/weka/gui/arffviewer/ArffViewer")

  (require :weka.gui.arffviewer.ArffViewerMainPanel "/home/barnard/Code/Lisp/late/weka/gui/arffviewer/ArffViewerMainPanel")

  (require :weka.gui.beans.AbstractDataSink "/home/barnard/Code/Lisp/late/weka/gui/beans/AbstractDataSink")

  (require :weka.gui.beans.AbstractDataSinkBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/AbstractDataSinkBeanInfo")

  (require :weka.gui.beans.AbstractDataSource "/home/barnard/Code/Lisp/late/weka/gui/beans/AbstractDataSource")

  (require :weka.gui.beans.AbstractDataSourceBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/AbstractDataSourceBeanInfo")

  (require :weka.gui.beans.AbstractEvaluator "/home/barnard/Code/Lisp/late/weka/gui/beans/AbstractEvaluator")

  (require :weka.gui.beans.AbstractTestSetProducer "/home/barnard/Code/Lisp/late/weka/gui/beans/AbstractTestSetProducer")

  (require :weka.gui.beans.AbstractTestSetProducerBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/AbstractTestSetProducerBeanInfo")

  (require :weka.gui.beans.AbstractTrainAndTestSetProducer "/home/barnard/Code/Lisp/late/weka/gui/beans/AbstractTrainAndTestSetProducer")

  (require :weka.gui.beans.AbstractTrainAndTestSetProducerBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/AbstractTrainAndTestSetProducerBeanInfo")

  (require :weka.gui.beans.AbstractTrainingSetProducer "/home/barnard/Code/Lisp/late/weka/gui/beans/AbstractTrainingSetProducer")

  (require :weka.gui.beans.AbstractTrainingSetProducerBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/AbstractTrainingSetProducerBeanInfo")

  (require :weka.gui.beans.Associator "/home/barnard/Code/Lisp/late/weka/gui/beans/Associator")

  (require :weka.gui.beans.AssociatorBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/AssociatorBeanInfo")

  (require :weka.gui.beans.AssociatorCustomizer "/home/barnard/Code/Lisp/late/weka/gui/beans/AssociatorCustomizer")

  (require :weka.gui.beans.AttributeSummarizer "/home/barnard/Code/Lisp/late/weka/gui/beans/AttributeSummarizer")

  (require :weka.gui.beans.AttributeSummarizerBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/AttributeSummarizerBeanInfo")

  (require :weka.gui.beans.BatchClassifierEvent "/home/barnard/Code/Lisp/late/weka/gui/beans/BatchClassifierEvent")

  (require :weka.gui.beans.BatchClassifierListener "/home/barnard/Code/Lisp/late/weka/gui/beans/BatchClassifierListener")

  (require :weka.gui.beans.BatchClustererEvent "/home/barnard/Code/Lisp/late/weka/gui/beans/BatchClustererEvent")

  (require :weka.gui.beans.BatchClustererListener "/home/barnard/Code/Lisp/late/weka/gui/beans/BatchClustererListener")

  (require :weka.gui.beans.BeanCommon "/home/barnard/Code/Lisp/late/weka/gui/beans/BeanCommon")

  (require :weka.gui.beans.BeanConnection "/home/barnard/Code/Lisp/late/weka/gui/beans/BeanConnection")

  (require :weka.gui.beans.BeanInstance "/home/barnard/Code/Lisp/late/weka/gui/beans/BeanInstance")

  (require :weka.gui.beans.BeanVisual "/home/barnard/Code/Lisp/late/weka/gui/beans/BeanVisual")

  (require :weka.gui.beans.ChartEvent "/home/barnard/Code/Lisp/late/weka/gui/beans/ChartEvent")

  (require :weka.gui.beans.ChartListener "/home/barnard/Code/Lisp/late/weka/gui/beans/ChartListener")

  (require :weka.gui.beans.ClassAssigner "/home/barnard/Code/Lisp/late/weka/gui/beans/ClassAssigner")

  (require :weka.gui.beans.ClassAssignerBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/ClassAssignerBeanInfo")

  (require :weka.gui.beans.ClassAssignerCustomizer "/home/barnard/Code/Lisp/late/weka/gui/beans/ClassAssignerCustomizer")

  (require :weka.gui.beans.ClassValuePicker "/home/barnard/Code/Lisp/late/weka/gui/beans/ClassValuePicker")

  (require :weka.gui.beans.ClassValuePickerBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/ClassValuePickerBeanInfo")

  (require :weka.gui.beans.ClassValuePickerCustomizer "/home/barnard/Code/Lisp/late/weka/gui/beans/ClassValuePickerCustomizer")

  (require :weka.gui.beans.Classifier "/home/barnard/Code/Lisp/late/weka/gui/beans/Classifier")

  (require :weka.gui.beans.ClassifierBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/ClassifierBeanInfo")

  (require :weka.gui.beans.ClassifierCustomizer "/home/barnard/Code/Lisp/late/weka/gui/beans/ClassifierCustomizer")

  (require :weka.gui.beans.ClassifierPerformanceEvaluator "/home/barnard/Code/Lisp/late/weka/gui/beans/ClassifierPerformanceEvaluator")

  (require :weka.gui.beans.ClassifierPerformanceEvaluatorBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/ClassifierPerformanceEvaluatorBeanInfo")

  (require :weka.gui.beans.Clusterer "/home/barnard/Code/Lisp/late/weka/gui/beans/Clusterer")

  (require :weka.gui.beans.ClustererBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/ClustererBeanInfo")

  (require :weka.gui.beans.ClustererCustomizer "/home/barnard/Code/Lisp/late/weka/gui/beans/ClustererCustomizer")

  (require :weka.gui.beans.ClustererPerformanceEvaluator "/home/barnard/Code/Lisp/late/weka/gui/beans/ClustererPerformanceEvaluator")

  (require :weka.gui.beans.ClustererPerformanceEvaluatorBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/ClustererPerformanceEvaluatorBeanInfo")

  (require :weka.gui.beans.ConnectionNotificationConsumer "/home/barnard/Code/Lisp/late/weka/gui/beans/ConnectionNotificationConsumer")

  (require :weka.gui.beans.CrossValidationFoldMaker "/home/barnard/Code/Lisp/late/weka/gui/beans/CrossValidationFoldMaker")

  (require :weka.gui.beans.CrossValidationFoldMakerBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/CrossValidationFoldMakerBeanInfo")

  (require :weka.gui.beans.CrossValidationFoldMakerCustomizer "/home/barnard/Code/Lisp/late/weka/gui/beans/CrossValidationFoldMakerCustomizer")

  (require :weka.gui.beans.CustomizerCloseRequester "/home/barnard/Code/Lisp/late/weka/gui/beans/CustomizerCloseRequester")

  (require :weka.gui.beans.CustomizerClosingListener "/home/barnard/Code/Lisp/late/weka/gui/beans/CustomizerClosingListener")

  (require :weka.gui.beans.DataFormatListener "/home/barnard/Code/Lisp/late/weka/gui/beans/DataFormatListener")

  (require :weka.gui.beans.DataSetEvent "/home/barnard/Code/Lisp/late/weka/gui/beans/DataSetEvent")

  (require :weka.gui.beans.DataSink "/home/barnard/Code/Lisp/late/weka/gui/beans/DataSink")

  (require :weka.gui.beans.DataSource "/home/barnard/Code/Lisp/late/weka/gui/beans/DataSource")

  (require :weka.gui.beans.DataSourceListener "/home/barnard/Code/Lisp/late/weka/gui/beans/DataSourceListener")

  (require :weka.gui.beans.DataVisualizer "/home/barnard/Code/Lisp/late/weka/gui/beans/DataVisualizer")

  (require :weka.gui.beans.DataVisualizerBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/DataVisualizerBeanInfo")

  (require :weka.gui.beans.EventConstraints "/home/barnard/Code/Lisp/late/weka/gui/beans/EventConstraints")

  (require :weka.gui.beans.Filter "/home/barnard/Code/Lisp/late/weka/gui/beans/Filter")

  (require :weka.gui.beans.FilterBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/FilterBeanInfo")

  (require :weka.gui.beans.FilterCustomizer "/home/barnard/Code/Lisp/late/weka/gui/beans/FilterCustomizer")

  (require :weka.gui.beans.FlowRunner "/home/barnard/Code/Lisp/late/weka/gui/beans/FlowRunner")

  (require :weka.gui.beans.GraphEvent "/home/barnard/Code/Lisp/late/weka/gui/beans/GraphEvent")

  (require :weka.gui.beans.GraphListener "/home/barnard/Code/Lisp/late/weka/gui/beans/GraphListener")

  (require :weka.gui.beans.GraphViewer "/home/barnard/Code/Lisp/late/weka/gui/beans/GraphViewer")

  (require :weka.gui.beans.GraphViewerBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/GraphViewerBeanInfo")

  (require :weka.gui.beans.IncrementalClassifierEvaluator "/home/barnard/Code/Lisp/late/weka/gui/beans/IncrementalClassifierEvaluator")

  (require :weka.gui.beans.IncrementalClassifierEvaluatorBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/IncrementalClassifierEvaluatorBeanInfo")

  (require :weka.gui.beans.IncrementalClassifierEvaluatorCustomizer "/home/barnard/Code/Lisp/late/weka/gui/beans/IncrementalClassifierEvaluatorCustomizer")

  (require :weka.gui.beans.IncrementalClassifierEvent "/home/barnard/Code/Lisp/late/weka/gui/beans/IncrementalClassifierEvent")

  (require :weka.gui.beans.IncrementalClassifierListener "/home/barnard/Code/Lisp/late/weka/gui/beans/IncrementalClassifierListener")

  (require :weka.gui.beans.InstanceEvent "/home/barnard/Code/Lisp/late/weka/gui/beans/InstanceEvent")

  (require :weka.gui.beans.InstanceListener "/home/barnard/Code/Lisp/late/weka/gui/beans/InstanceListener")

  (require :weka.gui.beans.InstanceStreamToBatchMaker "/home/barnard/Code/Lisp/late/weka/gui/beans/InstanceStreamToBatchMaker")

  (require :weka.gui.beans.InstanceStreamToBatchMakerBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/InstanceStreamToBatchMakerBeanInfo")

  (require :weka.gui.beans.KnowledgeFlow "/home/barnard/Code/Lisp/late/weka/gui/beans/KnowledgeFlow")

  (require :weka.gui.beans.KnowledgeFlowApp "/home/barnard/Code/Lisp/late/weka/gui/beans/KnowledgeFlowApp")

  (require :weka.gui.beans.Loader "/home/barnard/Code/Lisp/late/weka/gui/beans/Loader")

  (require :weka.gui.beans.LoaderBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/LoaderBeanInfo")

  (require :weka.gui.beans.LoaderCustomizer "/home/barnard/Code/Lisp/late/weka/gui/beans/LoaderCustomizer")

  (require :weka.gui.beans.LogPanel "/home/barnard/Code/Lisp/late/weka/gui/beans/LogPanel")

  (require :weka.gui.beans.LogWriter "/home/barnard/Code/Lisp/late/weka/gui/beans/LogWriter")

  (require :weka.gui.beans.MetaBean "/home/barnard/Code/Lisp/late/weka/gui/beans/MetaBean")

  (require :weka.gui.beans.ModelPerformanceChart "/home/barnard/Code/Lisp/late/weka/gui/beans/ModelPerformanceChart")

  (require :weka.gui.beans.ModelPerformanceChartBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/ModelPerformanceChartBeanInfo")

  (require :weka.gui.beans.PredictionAppender "/home/barnard/Code/Lisp/late/weka/gui/beans/PredictionAppender")

  (require :weka.gui.beans.PredictionAppenderBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/PredictionAppenderBeanInfo")

  (require :weka.gui.beans.PredictionAppenderCustomizer "/home/barnard/Code/Lisp/late/weka/gui/beans/PredictionAppenderCustomizer")

  (require :weka.gui.beans.Saver "/home/barnard/Code/Lisp/late/weka/gui/beans/Saver")

  (require :weka.gui.beans.SaverBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/SaverBeanInfo")

  (require :weka.gui.beans.SaverCustomizer "/home/barnard/Code/Lisp/late/weka/gui/beans/SaverCustomizer")

  (require :weka.gui.beans.ScatterPlotMatrix "/home/barnard/Code/Lisp/late/weka/gui/beans/ScatterPlotMatrix")

  (require :weka.gui.beans.ScatterPlotMatrixBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/ScatterPlotMatrixBeanInfo")

  (require :weka.gui.beans.SerializedModelSaver "/home/barnard/Code/Lisp/late/weka/gui/beans/SerializedModelSaver")

  (require :weka.gui.beans.SerializedModelSaverBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/SerializedModelSaverBeanInfo")

  (require :weka.gui.beans.SerializedModelSaverCustomizer "/home/barnard/Code/Lisp/late/weka/gui/beans/SerializedModelSaverCustomizer")

  (require :weka.gui.beans.StartUpListener "/home/barnard/Code/Lisp/late/weka/gui/beans/StartUpListener")

  (require :weka.gui.beans.Startable "/home/barnard/Code/Lisp/late/weka/gui/beans/Startable")

  (require :weka.gui.beans.StripChart "/home/barnard/Code/Lisp/late/weka/gui/beans/StripChart")

  (require :weka.gui.beans.StripChartBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/StripChartBeanInfo")

  (require :weka.gui.beans.StripChartCustomizer "/home/barnard/Code/Lisp/late/weka/gui/beans/StripChartCustomizer")

  (require :weka.gui.beans.TestSetEvent "/home/barnard/Code/Lisp/late/weka/gui/beans/TestSetEvent")

  (require :weka.gui.beans.TestSetListener "/home/barnard/Code/Lisp/late/weka/gui/beans/TestSetListener")

  (require :weka.gui.beans.TestSetMaker "/home/barnard/Code/Lisp/late/weka/gui/beans/TestSetMaker")

  (require :weka.gui.beans.TestSetMakerBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/TestSetMakerBeanInfo")

  (require :weka.gui.beans.TestSetProducer "/home/barnard/Code/Lisp/late/weka/gui/beans/TestSetProducer")

  (require :weka.gui.beans.TextEvent "/home/barnard/Code/Lisp/late/weka/gui/beans/TextEvent")

  (require :weka.gui.beans.TextListener "/home/barnard/Code/Lisp/late/weka/gui/beans/TextListener")

  (require :weka.gui.beans.TextViewer "/home/barnard/Code/Lisp/late/weka/gui/beans/TextViewer")

  (require :weka.gui.beans.TextViewerBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/TextViewerBeanInfo")

  (require :weka.gui.beans.ThresholdDataEvent "/home/barnard/Code/Lisp/late/weka/gui/beans/ThresholdDataEvent")

  (require :weka.gui.beans.ThresholdDataListener "/home/barnard/Code/Lisp/late/weka/gui/beans/ThresholdDataListener")

  (require :weka.gui.beans.TrainTestSplitMaker "/home/barnard/Code/Lisp/late/weka/gui/beans/TrainTestSplitMaker")

  (require :weka.gui.beans.TrainTestSplitMakerBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/TrainTestSplitMakerBeanInfo")

  (require :weka.gui.beans.TrainTestSplitMakerCustomizer "/home/barnard/Code/Lisp/late/weka/gui/beans/TrainTestSplitMakerCustomizer")

  (require :weka.gui.beans.TrainingSetEvent "/home/barnard/Code/Lisp/late/weka/gui/beans/TrainingSetEvent")

  (require :weka.gui.beans.TrainingSetListener "/home/barnard/Code/Lisp/late/weka/gui/beans/TrainingSetListener")

  (require :weka.gui.beans.TrainingSetMaker "/home/barnard/Code/Lisp/late/weka/gui/beans/TrainingSetMaker")

  (require :weka.gui.beans.TrainingSetMakerBeanInfo "/home/barnard/Code/Lisp/late/weka/gui/beans/TrainingSetMakerBeanInfo")

  (require :weka.gui.beans.TrainingSetProducer "/home/barnard/Code/Lisp/late/weka/gui/beans/TrainingSetProducer")

  (require :weka.gui.beans.UserRequestAcceptor "/home/barnard/Code/Lisp/late/weka/gui/beans/UserRequestAcceptor")

  (require :weka.gui.beans.Visible "/home/barnard/Code/Lisp/late/weka/gui/beans/Visible")

  (require :weka.gui.beans.VisualizableErrorEvent "/home/barnard/Code/Lisp/late/weka/gui/beans/VisualizableErrorEvent")

  (require :weka.gui.beans.VisualizableErrorListener "/home/barnard/Code/Lisp/late/weka/gui/beans/VisualizableErrorListener")

  (require :weka.gui.beans.WekaWrapper "/home/barnard/Code/Lisp/late/weka/gui/beans/WekaWrapper")

  (require :weka.gui.beans.xml.XMLBeans "/home/barnard/Code/Lisp/late/weka/gui/beans/xml/XMLBeans")

  (require :weka.gui.boundaryvisualizer.BoundaryPanel "/home/barnard/Code/Lisp/late/weka/gui/boundaryvisualizer/BoundaryPanel")

  (require :weka.gui.boundaryvisualizer.BoundaryPanelDistributed "/home/barnard/Code/Lisp/late/weka/gui/boundaryvisualizer/BoundaryPanelDistributed")

  (require :weka.gui.boundaryvisualizer.BoundaryVisualizer "/home/barnard/Code/Lisp/late/weka/gui/boundaryvisualizer/BoundaryVisualizer")

  (require :weka.gui.boundaryvisualizer.DataGenerator "/home/barnard/Code/Lisp/late/weka/gui/boundaryvisualizer/DataGenerator")

  (require :weka.gui.boundaryvisualizer.KDDataGenerator "/home/barnard/Code/Lisp/late/weka/gui/boundaryvisualizer/KDDataGenerator")

  (require :weka.gui.boundaryvisualizer.RemoteBoundaryVisualizerSubTask "/home/barnard/Code/Lisp/late/weka/gui/boundaryvisualizer/RemoteBoundaryVisualizerSubTask")

  (require :weka.gui.boundaryvisualizer.RemoteResult "/home/barnard/Code/Lisp/late/weka/gui/boundaryvisualizer/RemoteResult")

  (require :weka.gui.ensembleLibraryEditor.AddModelsPanel "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/AddModelsPanel")

  (require :weka.gui.ensembleLibraryEditor.DefaultModelsPanel "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/DefaultModelsPanel")

  (require :weka.gui.ensembleLibraryEditor.LibrarySerialization "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/LibrarySerialization")

  (require :weka.gui.ensembleLibraryEditor.ListModelsPanel "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/ListModelsPanel")

  (require :weka.gui.ensembleLibraryEditor.LoadModelsPanel "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/LoadModelsPanel")

  (require :weka.gui.ensembleLibraryEditor.ModelList "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/ModelList")

  (require :weka.gui.ensembleLibraryEditor.tree.CheckBoxNode "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/tree/CheckBoxNode")

  (require :weka.gui.ensembleLibraryEditor.tree.CheckBoxNodeEditor "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/tree/CheckBoxNodeEditor")

  (require :weka.gui.ensembleLibraryEditor.tree.DefaultNode "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/tree/DefaultNode")

  (require :weka.gui.ensembleLibraryEditor.tree.GenericObjectNode "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/tree/GenericObjectNode")

  (require :weka.gui.ensembleLibraryEditor.tree.GenericObjectNodeEditor "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/tree/GenericObjectNodeEditor")

  (require :weka.gui.ensembleLibraryEditor.tree.InvalidInputException "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/tree/InvalidInputException")

  (require :weka.gui.ensembleLibraryEditor.tree.ModelTreeNodeEditor "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/tree/ModelTreeNodeEditor")

  (require :weka.gui.ensembleLibraryEditor.tree.ModelTreeNodeRenderer "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/tree/ModelTreeNodeRenderer")

  (require :weka.gui.ensembleLibraryEditor.tree.NumberClassNotFoundException "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/tree/NumberClassNotFoundException")

  (require :weka.gui.ensembleLibraryEditor.tree.NumberNode "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/tree/NumberNode")

  (require :weka.gui.ensembleLibraryEditor.tree.NumberNodeEditor "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/tree/NumberNodeEditor")

  (require :weka.gui.ensembleLibraryEditor.tree.PropertyNode "/home/barnard/Code/Lisp/late/weka/gui/ensembleLibraryEditor/tree/PropertyNode")

  (require :weka.gui.experiment.AlgorithmListPanel "/home/barnard/Code/Lisp/late/weka/gui/experiment/AlgorithmListPanel")

  (require :weka.gui.experiment.DatasetListPanel "/home/barnard/Code/Lisp/late/weka/gui/experiment/DatasetListPanel")

  (require :weka.gui.experiment.DistributeExperimentPanel "/home/barnard/Code/Lisp/late/weka/gui/experiment/DistributeExperimentPanel")

  (require :weka.gui.experiment.Experimenter "/home/barnard/Code/Lisp/late/weka/gui/experiment/Experimenter")

  (require :weka.gui.experiment.ExperimenterDefaults "/home/barnard/Code/Lisp/late/weka/gui/experiment/ExperimenterDefaults")

  (require :weka.gui.experiment.GeneratorPropertyIteratorPanel "/home/barnard/Code/Lisp/late/weka/gui/experiment/GeneratorPropertyIteratorPanel")

  (require :weka.gui.experiment.HostListPanel "/home/barnard/Code/Lisp/late/weka/gui/experiment/HostListPanel")

  (require :weka.gui.experiment.OutputFormatDialog "/home/barnard/Code/Lisp/late/weka/gui/experiment/OutputFormatDialog")

  (require :weka.gui.experiment.ResultsPanel "/home/barnard/Code/Lisp/late/weka/gui/experiment/ResultsPanel")

  (require :weka.gui.experiment.RunNumberPanel "/home/barnard/Code/Lisp/late/weka/gui/experiment/RunNumberPanel")

  (require :weka.gui.experiment.RunPanel "/home/barnard/Code/Lisp/late/weka/gui/experiment/RunPanel")

  (require :weka.gui.experiment.SetupModePanel "/home/barnard/Code/Lisp/late/weka/gui/experiment/SetupModePanel")

  (require :weka.gui.experiment.SetupPanel "/home/barnard/Code/Lisp/late/weka/gui/experiment/SetupPanel")

  (require :weka.gui.experiment.SimpleSetupPanel "/home/barnard/Code/Lisp/late/weka/gui/experiment/SimpleSetupPanel")

  (require :weka.gui.explorer.AssociationsPanel "/home/barnard/Code/Lisp/late/weka/gui/explorer/AssociationsPanel")

  (require :weka.gui.explorer.AttributeSelectionPanel "/home/barnard/Code/Lisp/late/weka/gui/explorer/AttributeSelectionPanel")

  (require :weka.gui.explorer.ClassifierPanel "/home/barnard/Code/Lisp/late/weka/gui/explorer/ClassifierPanel")

  (require :weka.gui.explorer.ClustererPanel "/home/barnard/Code/Lisp/late/weka/gui/explorer/ClustererPanel")

  (require :weka.gui.explorer.DataGeneratorPanel "/home/barnard/Code/Lisp/late/weka/gui/explorer/DataGeneratorPanel")

  (require :weka.gui.explorer.Explorer "/home/barnard/Code/Lisp/late/weka/gui/explorer/Explorer")

  (require :weka.gui.explorer.ExplorerDefaults "/home/barnard/Code/Lisp/late/weka/gui/explorer/ExplorerDefaults")

  (require :weka.gui.explorer.PreprocessPanel "/home/barnard/Code/Lisp/late/weka/gui/explorer/PreprocessPanel")

  (require :weka.gui.explorer.VisualizePanel "/home/barnard/Code/Lisp/late/weka/gui/explorer/VisualizePanel")

  (require :weka.gui.graphvisualizer.BIFFormatException "/home/barnard/Code/Lisp/late/weka/gui/graphvisualizer/BIFFormatException")

  (require :weka.gui.graphvisualizer.BIFParser "/home/barnard/Code/Lisp/late/weka/gui/graphvisualizer/BIFParser")

  (require :weka.gui.graphvisualizer.DotParser "/home/barnard/Code/Lisp/late/weka/gui/graphvisualizer/DotParser")

  (require :weka.gui.graphvisualizer.GraphConstants "/home/barnard/Code/Lisp/late/weka/gui/graphvisualizer/GraphConstants")

  (require :weka.gui.graphvisualizer.GraphEdge "/home/barnard/Code/Lisp/late/weka/gui/graphvisualizer/GraphEdge")

  (require :weka.gui.graphvisualizer.GraphNode "/home/barnard/Code/Lisp/late/weka/gui/graphvisualizer/GraphNode")

  (require :weka.gui.graphvisualizer.GraphVisualizer "/home/barnard/Code/Lisp/late/weka/gui/graphvisualizer/GraphVisualizer")

  (require :weka.gui.graphvisualizer.HierarchicalBCEngine "/home/barnard/Code/Lisp/late/weka/gui/graphvisualizer/HierarchicalBCEngine")

  (require :weka.gui.graphvisualizer.LayoutCompleteEvent "/home/barnard/Code/Lisp/late/weka/gui/graphvisualizer/LayoutCompleteEvent")

  (require :weka.gui.graphvisualizer.LayoutCompleteEventListener "/home/barnard/Code/Lisp/late/weka/gui/graphvisualizer/LayoutCompleteEventListener")

  (require :weka.gui.graphvisualizer.LayoutEngine "/home/barnard/Code/Lisp/late/weka/gui/graphvisualizer/LayoutEngine")

  (require :weka.gui.sql.ConnectionPanel "/home/barnard/Code/Lisp/late/weka/gui/sql/ConnectionPanel")

  (require :weka.gui.sql.DbUtils "/home/barnard/Code/Lisp/late/weka/gui/sql/DbUtils")

  (require :weka.gui.sql.InfoPanel "/home/barnard/Code/Lisp/late/weka/gui/sql/InfoPanel")

  (require :weka.gui.sql.InfoPanelCellRenderer "/home/barnard/Code/Lisp/late/weka/gui/sql/InfoPanelCellRenderer")

  (require :weka.gui.sql.QueryPanel "/home/barnard/Code/Lisp/late/weka/gui/sql/QueryPanel")

  (require :weka.gui.sql.ResultPanel "/home/barnard/Code/Lisp/late/weka/gui/sql/ResultPanel")

  (require :weka.gui.sql.ResultSetHelper "/home/barnard/Code/Lisp/late/weka/gui/sql/ResultSetHelper")

  (require :weka.gui.sql.ResultSetTable "/home/barnard/Code/Lisp/late/weka/gui/sql/ResultSetTable")

  (require :weka.gui.sql.ResultSetTableCellRenderer "/home/barnard/Code/Lisp/late/weka/gui/sql/ResultSetTableCellRenderer")

  (require :weka.gui.sql.ResultSetTableModel "/home/barnard/Code/Lisp/late/weka/gui/sql/ResultSetTableModel")

  (require :weka.gui.sql.SqlViewer "/home/barnard/Code/Lisp/late/weka/gui/sql/SqlViewer")

  (require :weka.gui.sql.SqlViewerDialog "/home/barnard/Code/Lisp/late/weka/gui/sql/SqlViewerDialog")

  (require :weka.gui.sql.event.ConnectionEvent "/home/barnard/Code/Lisp/late/weka/gui/sql/event/ConnectionEvent")

  (require :weka.gui.sql.event.ConnectionListener "/home/barnard/Code/Lisp/late/weka/gui/sql/event/ConnectionListener")

  (require :weka.gui.sql.event.HistoryChangedEvent "/home/barnard/Code/Lisp/late/weka/gui/sql/event/HistoryChangedEvent")

  (require :weka.gui.sql.event.HistoryChangedListener "/home/barnard/Code/Lisp/late/weka/gui/sql/event/HistoryChangedListener")

  (require :weka.gui.sql.event.QueryExecuteEvent "/home/barnard/Code/Lisp/late/weka/gui/sql/event/QueryExecuteEvent")

  (require :weka.gui.sql.event.QueryExecuteListener "/home/barnard/Code/Lisp/late/weka/gui/sql/event/QueryExecuteListener")

  (require :weka.gui.sql.event.ResultChangedEvent "/home/barnard/Code/Lisp/late/weka/gui/sql/event/ResultChangedEvent")

  (require :weka.gui.sql.event.ResultChangedListener "/home/barnard/Code/Lisp/late/weka/gui/sql/event/ResultChangedListener")

  (require :weka.gui.streams.InstanceCounter "/home/barnard/Code/Lisp/late/weka/gui/streams/InstanceCounter")

  (require :weka.gui.streams.InstanceEvent "/home/barnard/Code/Lisp/late/weka/gui/streams/InstanceEvent")

  (require :weka.gui.streams.InstanceJoiner "/home/barnard/Code/Lisp/late/weka/gui/streams/InstanceJoiner")

  (require :weka.gui.streams.InstanceListener "/home/barnard/Code/Lisp/late/weka/gui/streams/InstanceListener")

  (require :weka.gui.streams.InstanceLoader "/home/barnard/Code/Lisp/late/weka/gui/streams/InstanceLoader")

  (require :weka.gui.streams.InstanceProducer "/home/barnard/Code/Lisp/late/weka/gui/streams/InstanceProducer")

  (require :weka.gui.streams.InstanceSavePanel "/home/barnard/Code/Lisp/late/weka/gui/streams/InstanceSavePanel")

  (require :weka.gui.streams.InstanceTable "/home/barnard/Code/Lisp/late/weka/gui/streams/InstanceTable")

  (require :weka.gui.streams.InstanceViewer "/home/barnard/Code/Lisp/late/weka/gui/streams/InstanceViewer")

  (require :weka.gui.streams.SerialInstanceListener "/home/barnard/Code/Lisp/late/weka/gui/streams/SerialInstanceListener")

  (require :weka.gui.treevisualizer.Colors "/home/barnard/Code/Lisp/late/weka/gui/treevisualizer/Colors")

  (require :weka.gui.treevisualizer.Edge "/home/barnard/Code/Lisp/late/weka/gui/treevisualizer/Edge")

  (require :weka.gui.treevisualizer.NamedColor "/home/barnard/Code/Lisp/late/weka/gui/treevisualizer/NamedColor")

  (require :weka.gui.treevisualizer.Node "/home/barnard/Code/Lisp/late/weka/gui/treevisualizer/Node")

  (require :weka.gui.treevisualizer.NodePlace "/home/barnard/Code/Lisp/late/weka/gui/treevisualizer/NodePlace")

  (require :weka.gui.treevisualizer.PlaceNode1 "/home/barnard/Code/Lisp/late/weka/gui/treevisualizer/PlaceNode1")

  (require :weka.gui.treevisualizer.PlaceNode2 "/home/barnard/Code/Lisp/late/weka/gui/treevisualizer/PlaceNode2")

  (require :weka.gui.treevisualizer.TreeBuild "/home/barnard/Code/Lisp/late/weka/gui/treevisualizer/TreeBuild")

  (require :weka.gui.treevisualizer.TreeDisplayEvent "/home/barnard/Code/Lisp/late/weka/gui/treevisualizer/TreeDisplayEvent")

  (require :weka.gui.treevisualizer.TreeDisplayListener "/home/barnard/Code/Lisp/late/weka/gui/treevisualizer/TreeDisplayListener")

  (require :weka.gui.treevisualizer.TreeVisualizer "/home/barnard/Code/Lisp/late/weka/gui/treevisualizer/TreeVisualizer")

  (require :weka.gui.visualize.AttributePanel "/home/barnard/Code/Lisp/late/weka/gui/visualize/AttributePanel")

  (require :weka.gui.visualize.AttributePanelEvent "/home/barnard/Code/Lisp/late/weka/gui/visualize/AttributePanelEvent")

  (require :weka.gui.visualize.AttributePanelListener "/home/barnard/Code/Lisp/late/weka/gui/visualize/AttributePanelListener")

  (require :weka.gui.visualize.BMPWriter "/home/barnard/Code/Lisp/late/weka/gui/visualize/BMPWriter")

  (require :weka.gui.visualize.ClassPanel "/home/barnard/Code/Lisp/late/weka/gui/visualize/ClassPanel")

  (require :weka.gui.visualize.JComponentWriter "/home/barnard/Code/Lisp/late/weka/gui/visualize/JComponentWriter")

  (require :weka.gui.visualize.JPEGWriter "/home/barnard/Code/Lisp/late/weka/gui/visualize/JPEGWriter")

  (require :weka.gui.visualize.LegendPanel "/home/barnard/Code/Lisp/late/weka/gui/visualize/LegendPanel")

  (require :weka.gui.visualize.MatrixPanel "/home/barnard/Code/Lisp/late/weka/gui/visualize/MatrixPanel")

  (require :weka.gui.visualize.PNGWriter "/home/barnard/Code/Lisp/late/weka/gui/visualize/PNGWriter")

  (require :weka.gui.visualize.Plot2D "/home/barnard/Code/Lisp/late/weka/gui/visualize/Plot2D")

  (require :weka.gui.visualize.Plot2DCompanion "/home/barnard/Code/Lisp/late/weka/gui/visualize/Plot2DCompanion")

  (require :weka.gui.visualize.PlotData2D "/home/barnard/Code/Lisp/late/weka/gui/visualize/PlotData2D")

  (require :weka.gui.visualize.PostscriptGraphics "/home/barnard/Code/Lisp/late/weka/gui/visualize/PostscriptGraphics")

  (require :weka.gui.visualize.PostscriptWriter "/home/barnard/Code/Lisp/late/weka/gui/visualize/PostscriptWriter")

  (require :weka.gui.visualize.PrintableComponent "/home/barnard/Code/Lisp/late/weka/gui/visualize/PrintableComponent")

  (require :weka.gui.visualize.PrintableHandler "/home/barnard/Code/Lisp/late/weka/gui/visualize/PrintableHandler")

  (require :weka.gui.visualize.PrintablePanel "/home/barnard/Code/Lisp/late/weka/gui/visualize/PrintablePanel")

  (require :weka.gui.visualize.ThresholdVisualizePanel "/home/barnard/Code/Lisp/late/weka/gui/visualize/ThresholdVisualizePanel")

  (require :weka.gui.visualize.VisualizePanel "/home/barnard/Code/Lisp/late/weka/gui/visualize/VisualizePanel")

  (require :weka.gui.visualize.VisualizePanelEvent "/home/barnard/Code/Lisp/late/weka/gui/visualize/VisualizePanelEvent")

  (require :weka.gui.visualize.VisualizePanelListener "/home/barnard/Code/Lisp/late/weka/gui/visualize/VisualizePanelListener")

  (require :weka.gui.visualize.VisualizeUtils "/home/barnard/Code/Lisp/late/weka/gui/visualize/VisualizeUtils")

  (require :weka.gui.visualize.plugins.VisualizePlugin "/home/barnard/Code/Lisp/late/weka/gui/visualize/plugins/VisualizePlugin")

)
