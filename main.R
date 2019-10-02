setwd("/mnt/Research/Leiden/D2-MCS/")

source("sources.R")

set.seed(1234)
data <- Dataset$new( filepath ="<path_to_dataset>", header=TRUE, sep="\t", skip = 1, 
                     normalize.names=TRUE, positive.class="Active", class.index = 1 )
data$createPartition(nfolds = 4)

subset.train <- data$getSubset(num.folds = c(1:2), opts = list(remove.na=TRUE, remove.const=TRUE) )

data$executePartition(4)

ignore.classifiers <- c("awnb","awtan","manb","nbDiscrete","nbSearch","tan","tanSearch","vbmpRadial","lssvmPoly", "polr",
                        "vglmAdjCat","vglmContRatio","vglmCumulative","dnn","elm","mlpKerasDecayCost","mlpKerasDropoutCost", 
                        "mxnet","mxnetAdam","pam","Rborist","RSimca","FRBCS.CHI","FH.GBML","FRBCS.W",
                        "PRIM","SLAVE","ada","bartMachine","chaid","C5.0Cost","lssvmLinear","avNNet",
                        "nnet","pcaNNet","lvq","rf","knn","deepboost","ordinalRF")

trFunction <- TwoClass$new(method = "cv", number = 10, savePredictions = "final", 
                           classProbs = TRUE, allowParallel = TRUE, verboseIter = FALSE)

data$getSubset(c(1:2))$getPositiveClass()


cluster.subset <- readRDS(file = file.path(getwd(),"datasets","cluster.rds") )
train.subset <- readRDS(file = file.path(getwd(),"datasets","test.rds") )
test.subset <- readRDS(file = file.path(getwd(),"datasets","test.rds") )


#BEGIN
Benchmarking <- D2MCS$new( path = "<path_to_models>", trainFunction = trFunction )
Benchmarking$train( train.set = train.subset, ig.classifiers = ignore.classifiers, metric = "MCC" )
classify <- Benchmarking$classify( test.set = test.subset, voting.scheme = ClassWeightedVoting$new(), positive.class = "Active" )
classify$computePerformance( ob = test.subset$getClass(), list(MCC$new(), PPV$new(), Accuracy$new()) )

nsga <- Benchmarking$optimize( opt.set = test.subset, 
                               voting.scheme = ClassWeightedVoting$new(), 
                               opt.algorithm = list(NSGAII$new( min.function  = FPFN$new(), 
                                                                n.generations = 100,#25000, 
                                                                n.iteractions = 1 ),
                                                    SMSEMOA$new( min.function = FPFN$new(), 
                                                                 n.generations = 100,#25000, 
                                                                 n.iteractions = 1 ) 
                                                    ), positive.class = "Active" )


nsga <- Benchmarking$optimize( opt.set = test.subset, 
                               voting.scheme = ClassWeightedVoting$new(), 
                               opt.algorithm = list(SMSEMOA$new( min.function = FPFN$new(), 
                                                                 n.generations = 100,#25000, 
                                                                 n.iteractions = 1 ) 
                               ), positive.class = "Active" )
 

a <- PerformanceComparator$new( test.set = test.subset, d2mcs.models = Benchmarking$getTrainedModels(), 
                                op.results = nsga, pareto.optimal = EuclideanDistance$new(), 
                                measures = list(MCC$new(), PPV$new(), Accuracy$new()) )
x <- a$showResults()
a$plotResults()



