StrategyConfiguration <- R6::R6Class(
  classname = "StrategyConfiguration",
  portable = TRUE,
  public = list(
    initialize = function(){ },
    minNumClusters = function(...){
      message("[",class(self)[1],"][INFO] Using default configuration: 2 clusters minimun")
      2
    },
    maxNumClusters = function(...){
      message("[",class(self)[1],"][INFO] Using default maxCluster configuration: 50 clusters maximun")
      50
    }
  )
)