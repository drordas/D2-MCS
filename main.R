source("sources.R")

set.seed(1234)
data <- Dataset$new(filepath ="/mnt/Research/Corpus/D4N/FCFP_6/TRAINNING/full_set_FCFP_6_physchem.csv",
                    header=TRUE, sep="\t",skip = 1, normalize.names=TRUE, classIndex = 1)
data$getNcol()
data$getNrow()

data$executePartition(4)
# bfc <- BinaryFisherClustering$new(dataset = data$getSubset(c(1,2)) )
# bfc$execute()
# clustered <- bfc$createSubset(subset = data$getSubset(c(2,3)), cluster = 2)
# bfc$plot(dir.path = getwd(),file.name = "bfc")

ignore.classifiers <- c("awnb","awtan","manb","nbDiscrete","nbSearch","tan","tanSearch","vbmpRadial","lssvmPoly", "polr",
                        "vglmAdjCat","vglmContRatio","vglmCumulative","dnn","elm","mlpKerasDecayCost","mlpKerasDropoutCost", 
                        "mxnet","mxnetAdam","pam","Rborist","RSimca","FRBCS.CHI","FH.GBML","FRBCS.W",
                        "PRIM","SLAVE","ada","bartMachine","chaid","C5.0Cost","lssvmLinear","avNNet",
                        "nnet","pcaNNet","lvq","rf","knn","deepboost","ordinalRF")


trFunction <- TwoClass$new(method = "cv", number = 10, savePredictions = "final", 
                           classProbs = TRUE, allowParallel = TRUE, verboseIter = FALSE)

cluster.subset <- readRDS(file = file.path(getwd(),"datasets","cluster.rds") )
train.subset <- readRDS(file = file.path(getwd(),"datasets/BinaryFisherClustering","train.rds") )
test.subset <- readRDS(file = file.path(getwd(),"datasets","test.rds") )

test.subset$getClass()


#BEGIN
Benchmarking <- D2MCS$new( path = "models/BinaryFisherCluster_prev/", trainFunction = trFunction )
Benchmarking$train( train.set = train.subset, ig.classifiers = ignore.classifiers, metric = "PPV" )
Benchmarking$classify( test.set = test.subset, voting.scheme = ClassWeightedVoting$new(), positive.class = "Active" )
#output <- Benchmarking$getPerformance(real.values = test.subset$getClass() )
Benchmarking$computePerformance(real.values = test.subset$getClass(), list(MCC$new(),PPV$new(),Accuracy$new()) )

a <- Benchmarking$optimize( opt.set = test.subset, 
                            voting.scheme = ClassWeightedVoting$new(), 
                            opt.algorithm = NSGAII$new( min.function = FPFN$new(), 
                                                        n.generations = 1, 
                                                        n.iteractions = 2 ), #generations=100
                            positive.class = "Active" )



write.csv(test.subset$getInstances(),file = "/mnt/Research/Leiden/D2-MCS/subset.csv", row.names = FALSE)

aa <- read.csv("/mnt/Research/Leiden/D2-MCS/subset.csv")
index.active <- which(aa[,1] == "Active")
index.inactive <- which(aa[,1] == "Inactive")
max <- round(length(index.active) * 0.01)

actives.selected <- sample(index.active,max,replace=FALSE)
total.index <- sort(c(actives.selected,index.inactive), decreasing = FALSE)
test.corpus <- aa[ total.index, ]

models.c1 <- readRDS("/mnt/Research/Leiden/D2-MCS/models/BinaryFisherCluster_prev/MCC/C[1-3]/ranger.rds")
models.c2 <- readRDS("/mnt/Research/Leiden/D2-MCS/models/BinaryFisherCluster_prev/MCC/C[2-3]/wsrf.rds")
models.c3 <- readRDS("/mnt/Research/Leiden/D2-MCS/models/BinaryFisherCluster_prev/MCC/C[3-3]/ORFlog.rds")

ppv.c1 <- readRDS("/mnt/Research/Leiden/D2-MCS/models/BinaryFisherCluster_prev/PPV/C[1-3]/ORFridge.rds")
ppv.c2 <- readRDS("/mnt/Research/Leiden/D2-MCS/models/BinaryFisherCluster_prev/PPV/C[2-3]/ORFpls.rds")
ppv.c3 <- readRDS("/mnt/Research/Leiden/D2-MCS/models/BinaryFisherCluster_prev/PPV/C[3-3]/ORFridge.rds")

results.c1 <- predict( models.c1$getTrainModel(), test.corpus, type="raw" )
results.c2 <- predict( models.c2$getTrainModel(), test.corpus, type="raw" )
results.c3 <- predict( models.c3$getTrainModel(), test.corpus, type="raw" )

ppv.resulst.c1 <- predict( ppv.c1$getTrainModel(), test.corpus, type="raw" )
ppv.resulst.c2 <- predict( ppv.c3$getTrainModel(), test.corpus, type="raw" )
ppv.resulst.c3 <- predict( ppv.c3$getTrainModel(), test.corpus, type="raw" )

results.c1 <- as.numeric( as.character( factor( results.c1, levels=c("Inactive","Active"),  labels = c(0, 1) ) ) )
results.c2 <- as.numeric( as.character( factor( results.c2, levels=c("Inactive","Active"),  labels = c(0, 1) ) ) )
results.c3 <- as.numeric( as.character( factor( results.c3, levels=c("Inactive","Active"),  labels = c(0, 1) ) ) )

#results.combined <- rowSums( t(t( cbind(results.c1,results.c2,results.c3) ) * default.weights) )

ppv.resulst.c1 <- as.numeric( as.character( factor( ppv.resulst.c1, levels=c("Inactive","Active"),  labels = c(-1, 1) ) ) )
ppv.resulst.c2 <- as.numeric( as.character( factor( ppv.resulst.c2, levels=c("Inactive","Active"),  labels = c(-1, 1) ) ) )
ppv.resulst.c3 <- as.numeric( as.character( factor( ppv.resulst.c3, levels=c("Inactive","Active"),  labels = c(-1, 1) ) ) )
ppv.results.combined <- rowSums( t(t( cbind(ppv.resulst.c1,ppv.resulst.c2,ppv.resulst.c3) ) * default.weights) )

for ( i in 1:length(results.combined) ){
  if( results.combined[i] > 0 ) 
    results.final <- c(results.final,"Active")
  else results.final <- c(results.final,"Inactive")
}
results.final <- as.factor(results.final)

ppv.results.final <- c()
for ( i in 1:length(ppv.results.combined) ){
  if( ppv.results.combined[i] > 0 ) 
    ppv.results.final <- c(ppv.results.final,"Active")
  else ppv.results.final <- c(ppv.results.final,"Inactive")
}
ppv.results.final <- as.factor(ppv.results.final)


caret::confusionMatrix(data = results.final, test.corpus[,1], positive="Active" )
caret::confusionMatrix(data = ppv.results.final, test.corpus[,1], positive="Active" )

