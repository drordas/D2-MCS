library("R6")
library("tools")

IGFeatureClustering <- R6Class(
  classname = "IGFeatureClustering",
  portable = TRUE,
  inherit = Cluster,
  public = list(
    initialize = function(dataset, maxClusters = 50){
      
    },
    execute = function(){

    },
    plot = function(file.name = NULL){
     
    },
    getDistribution = function(cluster, group , includeClass = "NONE" ){
      
    },
    createSubset = function(cluster = NULL, subset = NULL){
    
    }
  ),
  private = list(
    computeTable = function(corpus){
      
    },
    computeTest = function(corpus){
     
    },
    removeUnnecesary = function(corpus){

    },
    getUnnecesary = function(corpus){

    }

    #Attribute definition
    
  )
)