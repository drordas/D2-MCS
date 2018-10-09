
setwd("/Users/mpegea/GitKraken/D2-MCS")

source("sources.R")

data <- Dataset$new(filepath ="/Users/mpegea/GitKraken/Datasets/FCFP_6/TRAINNING/full_set_FCFP_6_physchem.csv", header=TRUE, sep="\t",skip = 1, classIndex = 1)

set.seed("123")
data$executePartition(4)

subset.cluster <- data$getSubset(c(1,2))


# fisherC <- BinaryFisherClustering$new(subset.cluster)
# fisherC$execute()
# fisherC$getDistribution( includeClass = "BEGIN")
# fisherC$createSubset(cluster=2, subset = data$getSubset(c(2,3)))
# fisherC$plot(savePath = "/Users/mpegea/GitKraken/D2-MCS/plots/dispersion.pdf")
# fisherC$getDistribution(cluster = 5, includeClass = "END")


multiC <- MultiTypeClustering$new(subset.cluster)
multiC$execute(positiveElement = "Active", method = "kendall")
multiC$plot(savePath = "/Users/mpegea/GitKraken/D2-MCS/plots/MutiTypeClustering_Dispersion.pdf", method = "kendall")
multiC$getDistribution(fisherK = 2, includeClass = "BEGIN")