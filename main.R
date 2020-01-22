source("sources.R")

set.seed(1000)

data.loader <- DatasetLoader$new()
data <- data.loader$load(filepath ="<path_to_dataset_file>.csv",
                          header=TRUE, sep="\t",skip = 1, normalize.names=TRUE,
                          positive.class="<positive_class_value>",
                          class.index = "<number_class_index>")

data$createPartitions(num.folds = 4)

subset.1_2 <- data$createSubset( num.folds = 1:2,
                                  opts = list(remove.na=TRUE, remove.const=TRUE) )
subset.2_3 <- data$createSubset( num.folds = c(2:3),
                                 opts = list(remove.na=TRUE, remove.const=FALSE) )
subset.4 <- data$createSubset( num.folds = 4,
                               opts = list(remove.na=TRUE, remove.const=FALSE) )

ignore.classifiers <- c( "awnb","awtan","manb","nbDiscrete","nbSearch","tan",
                         "tanSearch","vbmpRadial","lssvmPoly", "polr",
                         "vglmAdjCat","vglmContRatio","vglmCumulative","dnn",
                         "elm","mlpKerasDecayCost","mlpKerasDropoutCost",
                         "mxnet","mxnetAdam","pam","Rborist","RSimca",
                         "FRBCS.CHI","FH.GBML","FRBCS.W","PRIM","SLAVE","ada",
                         "bartMachine","chaid","C5.0Cost","lssvmLinear","avNNet",
                         "nnet","pcaNNet","lvq","rf","knn","deepboost","ordinalRF" )

trFunction <- TwoClass$new( method = "cv", number = 10, savePredictions = "final",
                            classProbs = TRUE, allowParallel = TRUE, verboseIter = FALSE )

cluster.binary <- SimpleStrategy$new( subset = subset.1_2,
                                      heuristic = FisherTestHeuristic$new() )
cluster.binary$execute(verbose = TRUE)
binaryTrain <- cluster.binary$createTrain(subset = subset.2_3,
                                          include.unclustered = TRUE)


#BEGIN
Benchmarking <- D2MCS$new(dir.path= "models/BinaryFisherCluster_test/")

trained.models <- Benchmarking$train(train.set = binaryTrain, train.function = trFunction,
                                     ex.classifiers = c("nb","naive_bayes","ranger"),
                                     metrics = c("MCC","PPV") )

cmvs <- Benchmarking$classify(train.output = trained.models,
                              subset = subset.4, voting.scheme= ClassWeightedVoting$new(),
                              metric="MCC" )

cmvs$getPerformance(test.set = subset.4,measures = list(MCC$new(), PPV$new()))

#-----------------------------------[X]-----------------------------------
nsga <- Benchmarking$optimize( opt.set = subset.4,
                               voting.scheme = ClassWeightedVoting$new(),
                               opt.algorithm = list(NSGAII$new( min.function  = FPFN$new(),
                                                                n.generations = 10,#25000,
                                                                n.iteractions = 1,
                                                                popSize = 10),
                                                    SMSEMOA$new( min.function = FPFN$new(),
                                                                 n.generations = 10,#25000,
                                                                 n.iteractions = 1,
                                                                 popSize = 10 ) ) )

#Benchmarking$getBestPerformanceByCluster()
                    #ig.classifiers = ignore.classifiers, metric = "MCC" )
# Benchmarking$train( train.set = train.subset,
#                     ig.classifiers = ignore.classifiers, metric = "MCC" )

cmvs <- Benchmarking$classify( test.set = subset.4,
                               voting.scheme = ClassMajorityVoting$new() )
cmvs$savePredictions(dir.path = "/mnt/Research/Leiden/D2-MCS/saveFolder/")
Benchmarking$getPredictions()

cwvs <- Benchmarking$classify( test.set = subset.4,
                               voting.scheme = ClassWeightedVoting$new() )





cmvs$getPerformance(test.set = subset.4,
                    measures= list(MCC$new(), PPV$new(), Accuracy$new()) )

cwvs$getPerformance(test.set = subset.4,
                    measures= list(MCC$new(), PPV$new(), Accuracy$new()) )

Benchmarking$getBestPerformanceByCluster()

nsga <- Benchmarking$optimize( opt.set = test.subset,
                               voting.scheme = ClassWeightedVoting$new(),
                               opt.algorithm = list(NSGAII$new( min.function  = FPFN$new(),
                                                                n.generations = 100,#25000,
                                                                n.iteractions = 1 ),
                                                    SMSEMOA$new( min.function = FPFN$new(),
                                                                 n.generations = 100,#25000,
                                                                 n.iteractions = 1 )
                                                    ), positive.class = "Active" )



Benchmarking <- D2MCS$new(dir.path= "models/BinaryFisherCluster_test/",
                          trainFunction= trFunction)
train.mcc <- Benchmarking$train(train.set = binaryTrain,
                                ex.classifiers = c("nb","naive_bayes","ranger"),
                                metric= "MCC" )

train.ppv <- Benchmarking$train(train.set = binaryTrain,
                                ex.classifiers = c("nb","naive_bayes","ranger"),
                                metric= "PPV" )

smsemoa <- Benchmarking$optimize( train.output = binaryTrain,
                                  opt.set = subset.4,
                                  voting.scheme = ClassWeightedVoting$new(),
                                  opt.algorithm = list( SMSEMOA$new( min.function = FPFN$new(),
                                                                     n.generations = 10,#25000,
                                                                     n.iteractions = 1,
                                                                     popSize = 10 ) ) )





