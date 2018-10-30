setwd("/Users/mpegea/GitKraken/D2-MCS")

source("sources.R")

data <- Dataset$new(filepath ="/Users/mpegea/GitKraken/Datasets/FCFP_6/TRAINNING/full_set_FCFP_6_physchem.csv", header=TRUE, sep="\t",skip = 1, classIndex = 1)

set.seed("123")
data$executePartition(4)

subset.cluster <- data$getSubset(c(1,2))



# ------------------------------- BinaryFisherClustering ----------------------------------
fisherC <- BinaryFisherClustering$new(subset.cluster)
fisherC$execute()
fisherC$getDistribution(cluster = 4, includeClass = "END")
fisherC$createSubset(cluster=25, subset = data$getSubset(c(2,3)))
fisherC$createSubset(cluster=25, subset = data$getSubset(c(2,3)))$getNumClusters()
fisherC$createSubset(cluster=25, subset = data$getSubset(c(2,3)))$getAt(26)$getFeatures()
fisherC$createSubset(cluster=25, subset = data$getSubset(c(2,3)))$getAt(1)$getClass()
fisherC$plot(savePath = "/Users/mpegea/GitKraken/D2-MCS/plots/dispersion.pdf")
# -----------------------------------------------------------------------------------------



# -------------------------------- MultiTypeClustering ------------------------------------
multiC <- MultiTypeClustering$new(subset.cluster)
multiC$execute(positive.class = "Active", method = "kendall")
multiC$plot(savePath = "/Users/mpegea/GitKraken/D2-MCS/plots/MutiTypeClustering_Kendall_Dispersion.pdf")
multiC$getDistribution(fisherK = 8, corK = 2, includeClass = "NONE")
multiC$createSubset(subset = data$getSubset(c(2,3)))
multiC$createSubset(subset = data$getSubset(c(2,3)))$getAt(3)
multiC$createSubset(subset = data$getSubset(c(2,3)))$getAt(4)$getFeatures()
multiC$createSubset(fisherK = 5, corK = 5, subset = data$getSubset(c(2,3)))$getAt(7)$getFeatures()
# -------------------------------------------- --------------------------------------------



# ------------------------------------ FSClustering ---------------------------------------
library("rJava")
library("FSelector")

FSC <- FSClustering$new(subset.cluster)
FSC$execute()
FSC$plot(savePath = "/Users/mpegea/GitKraken/D2-MCS/plots/FSClustering_Dispersion.pdf")

#Pruebas
igTest <- information.gain(as.formula(sprintf("`%s` ~.", subset.cluster$getClassName())), subset.cluster$getInstances(ignore.class = FALSE)) #OK
dim(igTest) # [1] 2132 1
length(igTest$attr_importance[igTest$attr_importance==0]) #1800
length(igTest$attr_importance[igTest$attr_importance!=0]) #332
which(igTest$attr_importance != 0, arr.ind = TRUE)
length(which(igTest$attr_importance != 0, arr.ind = TRUE))
row.names(ig)[which(ig$attr_importance != 0, arr.ind = TRUE)] #OK

# Objetivo:
#Named num [1:2053] 6.84e-01 3.91e-01 7.66e-02 6.17e-01 9.68e-07 ...
#- attr(*, "names")= chr [1:2053] "CM#FCFP_6#17" "CM#FCFP_6#991735244" "CM#FCFP_6#-885550502" "CM#FCFP_6#1175638033" ...
str(igTest$attr_importance)
str(row.names(igTest))
igTest <- information.gain(as.formula(sprintf("`%s` ~.", subset.cluster$getClassName())), subset.cluster$getInstances(ignore.class = FALSE)) #OK
v <- igTest$attr_importance
names(v) <- row.names(igTest)
str(v)

# -----------------------------------------------------------------------------------------
