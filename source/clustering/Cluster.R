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
    plot = function(file.name){
      stop("[CLUSTER][Error] Function 'printPlot must be implemented in inherited class\n'")
    },
    getDefaultHeuristic = function(){
      private$defaultHeuristic
    }
  ),
  
  private = list(
    defaultHeuristic = function(corpus, k){
      sorted.corpus <- sort(corpus, decreasing = TRUE)
      cluster.index <- vector(length = length(corpus) )
      cluster.sum <- vector(length = k )
      cluster.sum[1:k] <- 0
      aux <- c()
      for(i in 1:length(sorted.corpus)){
        aux[1:k] <- sorted.corpus[i]
        cluster <- (which.min((cluster.sum+aux)-(max(cluster.sum))))
        cluster.index[i] <- cluster
        cluster.sum[cluster] <- cluster.sum[cluster]+sorted.corpus[i]
      }
      cluster.index
    },
    maxClusters = 50
  )
)