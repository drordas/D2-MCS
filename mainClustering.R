set.seed(1234)
data <- Dataset$new(filepath ="/mnt/Research/Corpus/D4N/FCFP_6/TRAINNING/full_set_FCFP_6_physchem.csv",
                    header=TRUE, sep="\t",skip = 1, normalize.names=TRUE, classIndex = 1)

subset.cluster <- data$getSubset(c(1,2))
subset.test <- data$getSubset(4)
saveRDS(subset.cluster,file = file.path("datasets/","cluster.rds")) ## CLUSTERING SUBSET
saveRDS(subset.test,file = file.path("datasets/","test.rds")) ## TEST SUBSET

# Only Fisher

BFClustering <- BinaryFisherClustering$new(dataset = subset.cluster)
BFClustering$execute()
BFClustering$plot(dir.path = "models/BinaryFisherCluster", file.name = "BinaryFisherCluster" )
binaryFisher.train = BFClustering$createSubset(subset= data$getSubset(c(2,3)))
saveRDS(binaryFisher.train,file = file.path("datasets/BinaryFisherClustering","train.rds")) ## TRAINING SUBSET

# Fisher + Kendall Method
MultiTypeKendall <- MultiTypeClustering$new(dataset = subset.cluster)
MultiTypeKendall$execute(positive.class = "Active",method = "kendall")
MultiTypeKendall$plot(dir.path ="models/MultiTypeKendall", file.name = "MultiTypeKendall")
kendall.train <- MultiTypeKendall$createSubset(subset = data$getSubset(c(2,3)) )
saveRDS(kendall.train,file = file.path("datasets/MultiTypeKendall","train.rds")) ## TRAINING SUBSET

# Fisher + Spearman Method

MultiTypePearson <- MultiTypeClustering$new(dataset = subset.cluster)
MultiTypePearson$execute(positive.class = "Active", method = "pearson")
MultiTypePearson$plot(dir.path ="models/MultiTypePearson", file.name = "MultiTypePearson")
pearson.train <- MultiTypePearson$createSubset(subset = data$getSubset(c(2,3)) )
saveRDS(pearson.train,file = file.path("datasets/MultiTypePearson","train.rds")) ## TRAINING SUBSET

#FSelectionClustering (IG)

FSelectionClustering <- FSClustering$new(dataset = subset.cluster)
FSelectionClustering$execute(method="IG")
FSelectionClustering$plot(dir.path ="models/FSelectionClustering", file.name = "FSelectionClustering")
ig.train <- MultiTypeKendall$createSubset(subset = data$getSubset(c(2,3)) )
saveRDS(ig.train,file = file.path("datasets/FSelectionClustering","train.rds")) ## TRAINING SUBSET
