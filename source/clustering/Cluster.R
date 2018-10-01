library("R6")
Cluster <- R6Class(
  classname = "Cluster",
  portable = TRUE,                   
  public = list(
    initialize = function(maxClusters = 50){
      private$maxClusters <- maxClusters
    },
    isBinary = function(column){
      lvl <- levels(as.factor(column))
      (length(lvl) == 2 && 
          length(intersect(levels(as.factor(column)),c("0","1") )) == 2 )
    },
    execute = function(){
      stop("[CLUSTER][Error] Function 'execute' must be implemented\n")
    },
    getMaxClusters = function(){
      private$maxClusters
    },
    setMaxClusters = function(max){
      if(max > 1) 
        private$maxClusters <- max
      else cat("[CLUSTER][Error] number of clusters must be greater than 1\n. Assuming default value\n")
    },
    getNumClusters = function(){
      stop("[CLUSTER][Error] Function 'getNumClusters must be implemented in inherited class\n'")
    },
    getDistribution = function(group = NULL, includeClass = FALSE, classPosition = NULL){
      stop("[CLUSTER][Error] Function 'gestBestDistribution must be implemented in inherited class\n'")
    },
    plot = function(savePath){
      stop("[CLUSTER][Error] Function 'printPlot must be implemented in inherited class\n'")
    }
  ),
  private = list(
    maxClusters = 50
  )
)