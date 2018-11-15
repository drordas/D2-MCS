set.seed(1234)
data <- Dataset$new(filepath ="/mnt/Research/Corpus/D4N/FCFP_6/TRAINNING/full_set_FCFP_6_physchem.csv",
                    header=TRUE, sep="\t",skip = 1, normalize.names=TRUE, classIndex = 1)

subset.cluster <- data$getSubset(c(1,2))
subset.test <- data$getSubset(4)

# Fisher + Kendall Method

MultiTypeKendall <- MultiTypeClustering$new(dataset = subset.cluster)
MultiTypeKendall$execute(positive.class = "Active",method = "kendall")
MultiTypeKendall$plot(dir.path ="models/MultiTypeKendall", file.name = "MultiTypeKendall")
MultiTypeKendall$getDistribution()
kendall.train <- MultiTypeKendall$createSubset(subset = data$getSubset(c(2,3)) )

saveRDS(subset.cluster,file = file.path("datasets/MultiTypeKendall","cluster.rds")) ## CLUSTERING SUBSET
saveRDS(kendall.train,file = file.path("datasets/MultiTypeKendall","train.rds")) ## TRAIN SUBSET
saveRDS(data$getSubset(4),file = file.path("datasets/MultiTypeKendall","test.rds")) ## TEST SUBSET

# Fisher + Spearman Method

MultiTypePearson <- MultiTypeClustering$new(dataset = subset.cluster)
MultiTypePearson$execute(positive.class= "Active",method= "pearson")
MultiTypePearson$plot(dir.path ="models/MultiTypePearson", file.name = "MultiTypePearson")
a <- MultiTypePearson$getDistribution()
pearson.train <- MultiTypePearson$createSubset(subset = data$getSubset(c(2,3)) )

saveRDS(subset.cluster,file = file.path("datasets/MultiTypePearson","cluster.rds")) ## CLUSTERING SUBSET
saveRDS(pearson.train,file = file.path("datasets/MultiTypePearson","train.rds")) ## TRAIN SUBSET
saveRDS(data$getSubset(4),file = file.path("datasets/MultiTypePearson","test.rds")) ## TEST SUBSET