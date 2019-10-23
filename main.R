setwd("/mnt/Research/Leiden/D2-MCS/")

source("sources.R")

set.seed(1234)
data <- Dataset$new( filepath ="/mnt/Research/Corpus/D4N/FCFP_6/TRAINNING/full_set_FCFP_6_physchem.csv", 
                     header=TRUE, sep="\t", skip = 1, normalize.names=TRUE, 
                     positive.class="Active", class.index = 1 )
 
data$createPartitions(num.folds = 4)
subset.1_2 <- data$createSubset( num.folds = 1:2, 
                                 opts = list(remove.na=TRUE, remove.const=TRUE) )
subset.2_3 <- data$createSubset( num.folds = c(2:3), 
                                 opts = list(remove.na=TRUE, remove.const=FALSE) )
subset.4 <- data$createSubset( num.folds = 4, 
                               opts = list(remove.na=TRUE, remove.const=FALSE) )

 
saveRDS( subset.1_2,file.path(getwd(),"datasets","subset1_2.rds") )
saveRDS( subset.2_3,file.path(getwd(),"datasets","subset2_3.rds") )
saveRDS( subset.4,file.path(getwd(),"datasets","subset4.rds") )

ignore.classifiers <- c( "awnb","awtan","manb","nbDiscrete","nbSearch","tan","tanSearch",
                         "vbmpRadial","lssvmPoly", "polr","vglmAdjCat","vglmContRatio",
                         "vglmCumulative","dnn","elm","mlpKerasDecayCost","mlpKerasDropoutCost",
                         "mxnet","mxnetAdam","pam","Rborist","RSimca","FRBCS.CHI","FH.GBML","FRBCS.W",
                         "PRIM","SLAVE","ada","bartMachine","chaid","C5.0Cost","lssvmLinear","avNNet",
                         "nnet","pcaNNet","lvq","rf","knn","deepboost","ordinalRF" )

trFunction <- TwoClass$new( method = "cv", number = 10, savePredictions = "final",
                           classProbs = TRUE, allowParallel = TRUE, verboseIter = FALSE )

subset.1_2 <- readRDS( file = file.path(getwd(),"datasets","subset1_2.rds") )
subset.2_3 <- readRDS( file = file.path(getwd(),"datasets","subset2_3.rds") )
subset.4 <- readRDS( file = file.path(getwd(),"datasets","subset4.rds" ) )



cluster.binaryReal <- BinaryRealTypeStrategy$new( subset = subset.1_2, 
                                                  heuristic = list( FisherTestHeuristic$new(), 
                                                                    KendallHeuristic$new() ) )
cluster.binaryReal$execute(verbose = TRUE)
cluster.binaryReal$plot()
cluster.binaryReal$saveCSV("plots",num.clusters = c(4,3))
binaryRealTrain <- cluster.binaryReal$createTrain(subset.2_3)

cluster.binary <- SimpleStrategy$new( subset = subset.1_2, 
                                      heuristic = FisherTestHeuristic$new() )
cluster.binary$execute(verbose = TRUE)
binaryTrain <- cluster.binary$createTrain(subset = subset.2_3)
cluster.binary$saveCSV(dir.path = "plots")


#BEGIN
Benchmarking <- D2MCS$new( dir.path =  "models/BinaryFisherCluster_test/", 
                           trainFunction = trFunction )
Benchmarking$train( train.set = train.subset, 
                    ig.classifiers = ignore.classifiers, metric = "MCC" )
classify <- Benchmarking$classify( test.set = test.subset, 
                                   voting.scheme = ClassWeightedVoting$new(), 
                                   positive.class = "Active" )

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



