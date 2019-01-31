cat("[SOURCES][INFO] Loading required libraries\n")
source("pkgChecker.R", chdir = TRUE)

cat("[SOURCES][INFO] Loading required sources\n")
source("utils.R", chdir = TRUE)
source("source/data/Dataset.R", chdir=TRUE)

#LOAD CLUSTERING ALGORITHMS CLASSES
source("source/clustering/Cluster.R", chdir=TRUE)
source("source/clustering/ClusterDistribution.R", chdir=TRUE)
source("source/clustering/ClusterData.R", chdir=TRUE)
source("source/clustering/BinaryFisherClustering.R", chdir=TRUE)
source("source/clustering/MultiTypeClustering.R", chdir=TRUE)
source("source/clustering/FSClustering.R", chdir=TRUE)

#LOAD MODEL CLASSES
source("source/models/utility/ModelFit.R", chdir = TRUE)
source("source/models/utility/DefaultModelFit.R", chdir = TRUE)
source("source/models/trainFunctions/TrainFunction.R", chdir= TRUE)
source("source/models/trainFunctions/TwoClass.R", chdir = TRUE)
source("source/models/summaryFunction/SummaryFunction.R",chdir = TRUE)
source("source/models/summaryFunction/NoProbability.R", chdir=TRUE)
source("source/models/summaryFunction/UseProbability.R", chdir=TRUE)
source("source/models/utility/ModelFormula.R", chdir=TRUE)
source("source/models/utility/DefaultModelFit.R",chdir=TRUE)
source("source/models/utility/ModelFit.R",chdir=TRUE)
source("source/models/Model.R", chdir=TRUE)
source("source/models/LoadModels.R",chdir=TRUE)
source("source/models/ModelEntry.R",chdir=TRUE)
source("source/models/ModelsList.R",chdir=TRUE)
source("source/models/Predictions.R", chdir=TRUE)
source("source/models/ExecutedModels.R", chdir=TRUE)
source("source/models/ExecutedModelsList.R", chdir=TRUE)
source("source/models/ModelInfo.R", chdir=TRUE)
source("source/models/PerformanceOutput.R", chdir=TRUE)

#LOAD PREDICTION CLASSES
source("source/prediction/Prediction.R", chdir=TRUE)
source("source/prediction/PredictionList.R", chdir=TRUE)
source("source/prediction/PredictionHandler.R", chdir=TRUE)
source("source/prediction/VotingScheme.R", chdir=TRUE)
source("source/prediction/ClassWeightedVoting.R", chdir = TRUE)
source("source/prediction/ClassMajorityVoting.R", chdir=TRUE)
source("source/prediction/ProbMajorityVoting.R", chdir=TRUE)
source("source/prediction/PerformanceMeasures.R", chdir=TRUE)

#LOAD PREDICTION ALGORITHMS
source("source/optimization/MinFunction.R", chdir = TRUE)
source("source/optimization/FPFN.R", chdir = TRUE)
source("source/optimization/WeightsOptimizer.R", chdir = TRUE)
source("source/optimization/NSGA2.R", chdir = TRUE)
source("source/optimization/OptimizationOutput.R", chdir=TRUE)
source("source/optimization/OptimizationConfig.R", chdir=TRUE)

#LOAD MEASURES ALGORITHMS
source("source/measures/MeasureFunction.R", chdir = TRUE)
source("source/measures/Accuracy.R", chdir = TRUE)
source("source/measures/Kappa.R", chdir = TRUE)
source("source/measures/MCC.R", chdir = TRUE)
source("source/measures/NPV.R", chdir = TRUE)
source("source/measures/PPV.R", chdir = TRUE)
source("source/measures/Precision.R", chdir = TRUE)
source("source/measures/Recall.R", chdir = TRUE)
source("source/measures/Sensitivity.R", chdir = TRUE)
source("source/measures/Specificity.R", chdir = TRUE)


source("D2MCS.R",chdir=TRUE)

# sources <- list.files(pattern = "*.R$", all.files = TRUE, recursive = TRUE)
# sources <- sources[which( !sources %in% c("pkgChecker.R","D2MCS_bak.R","sources.R","main.R"),arr.ind = TRUE )]
# cat("Loading required packages and libraries\n")
# invisible(sapply(sources, source, chdir=TRUE))

#Required for strcmpi()
#library(pracma)

#Required for information.gain()
#library("rJava")
#library("FSelector")