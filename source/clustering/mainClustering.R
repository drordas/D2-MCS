# setwd("/Users/mpegea/GitKraken/D2-MCS")
# 
# source("sources.R")
# 
# data <- Dataset$new(filepath ="/Users/mpegea/GitKraken/Datasets/FCFP_6/TRAINNING/full_set_FCFP_6_physchem.csv", header=TRUE, sep="\t",skip = 1, classIndex = 1)
# 
# set.seed("123")
# data$executePartition(4)
# 
# subset.cluster <- data$getSubset(c(1,2))



# ------------------------------- BinaryFisherClustering ----------------------------------
# fisherC <- BinaryFisherClustering$new(subset.cluster)
# fisherC$execute()
# fisherC$getDistribution(cluster = 4, includeClass = "END")
# fisherC$createSubset(cluster=25, subset = data$getSubset(c(2,3)))
# fisherC$createSubset(cluster=25, subset = data$getSubset(c(2,3)))$getNumClusters()
# fisherC$createSubset(cluster=25, subset = data$getSubset(c(2,3)))$getAt(26)$getFeatures()
# fisherC$createSubset(cluster=25, subset = data$getSubset(c(2,3)))$getAt(1)$getClass()
# fisherC$plot(savePath = "/Users/mpegea/GitKraken/D2-MCS/plots/fisher_dispersion.pdf")
# -----------------------------------------------------------------------------------------



# -------------------------------- MultiTypeClustering ------------------------------------
# multiC <- MultiTypeClustering$new(subset.cluster)
# multiC$execute(positive.class = "Active", method = "kendall")
# multiC$plot(savePath = "/Users/mpegea/GitKraken/D2-MCS/plots/MutiTypeClustering_Kendall_Dispersion.pdf")
# multiC$getDistribution(fisherK = 8, corK = 2, includeClass = "NONE")
# multiC$createSubset(subset = data$getSubset(c(2,3)))
# multiC$createSubset(subset = data$getSubset(c(2,3)))$getAt(3)
# multiC$createSubset(subset = data$getSubset(c(2,3)))$getAt(4)$getFeatures()
# multiC$createSubset(fisherK = 5, corK = 5, subset = data$getSubset(c(2,3)))$getAt(7)$getFeatures()
# -------------------------------------------- --------------------------------------------



# ------------------------------------ FSClustering ---------------------------------------
# FSC <- FSClustering$new(subset.cluster)
# FSC$execute()
# FSC$plot(savePath = "/Users/mpegea/GitKraken/D2-MCS/plots/FSClustering_Dispersion.pdf")
# FSC$getDistribution(cluster = 10, includeClass = "END")
# FSC$createSubset(cluster = 5, subset = data$getSubset(c(2,3)))
# FSC$createSubset(cluster = 5, subset = data$getSubset(c(2,3)))$getAt(3)
# FSC$createSubset(cluster = 5, subset = data$getSubset(c(2,3)))$getAt(3)$getFeatures()
# -----------------------------------------------------------------------------------------
