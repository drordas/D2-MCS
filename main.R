source("sources.R")

set.seed(1234)
#data <- Dataset$new(filepath ="/mnt/Research/Corpus/D4N/FCFP_6/TRAINNING/full_set_FCFP_6_physchem.csv",
#                    header=TRUE, sep="\t",skip = 1, normalize.names=TRUE, classIndex = 1)

# data$executePartition(4)
# subset.cluster <- data$getSubset(c(1,2))
# 
# fisherC <- BinaryFisherClustering$new(subset.cluster)
# fisherC$execute()
# fisherC$plot(file.name = "BinaryFischerClusteringDispersion" ) 

ignore.classifiers <- c("awnb","awtan","manb","nbDiscrete","nbSearch","tan","tanSearch","vbmpRadial","lssvmPoly", "polr",
                        "vglmAdjCat","vglmContRatio","vglmCumulative","dnn","elm","mlpKerasDecayCost","mlpKerasDropoutCost", 
                        "mxnet","mxnetAdam","pam","Rborist","RSimca","FRBCS.CHI","FH.GBML","FRBCS.W",
                        "PRIM","SLAVE","ada","bartMachine","chaid","C5.0Cost","lssvmLinear","avNNet",
                        "nnet","pcaNNet","lvq","rf","knn","deepboost")


#execute.classifiers <- c("ranger")#,"knn","ranger","nb")
#subset.group <- fisherC$createSubset( subset = data$getSubset(c(2,3)) )

trFunction <- TwoClass$new(method = "cv", number = 10, savePredictions = "final", 
                           classProbs = TRUE, allowParallel = TRUE, verboseIter = FALSE)

#saveRDS(subset.cluster,file = file.path("datasets","clustering.rds")) ## CLUSTERING SUBSET
#saveRDS(subset.group,file = file.path("datasets","train.rds")) ## TRAIN SUBSET
#saveRDS(data$getSubset(4),file = file.path("datasets","test.rds")) ## TEST SUBSET

cluster.subset <- readRDS(file = file.path(getwd(),"datasets","clustering.rds") )
train.subset <- readRDS(file = file.path(getwd(),"datasets","train.rds") )
test.subset <- readRDS(file = file.path(getwd(),"datasets","test.rds") )

#source("sources.R")
Benchmarking <- D2MCS$new(path = "models/BinaryFisherCluster/",trainFunction = trFunction)
#Benchmarking$train(train.subset, ex.classifiers =  execute.classifiers, metric="MCC")
Benchmarking$train(train.subset, num.clusters = 3,  ig.classifiers= ignore.classifiers, metric="PPV")

#Benchmarking$plotTrain()

VotingSystem <- ClassWeightedVoting$new(majority.class = "Active")
Benchmarking$classify(test.set = test.subset, voting.scheme = VotingSystem  )
Benchmarking$computeFinalPerformance(positive.class = "Active" )
Benchmarking$getFinalPerformance("PPV")
Benchmarking$plotTest()


Benchmarking$removeAll()
Benchmarking$savePredictions(option = "all", filename = "MCC_ClassWeightedVoting.csv")
