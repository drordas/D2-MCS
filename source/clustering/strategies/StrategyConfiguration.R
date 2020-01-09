StrategyConfiguration <- R6::R6Class(
  classname = "StrategyConfiguration",
  portable = TRUE,
  public = list(
    initialize = function(){ },
    getName = function() { class(self)[1] },
    minNumClusters = function(...){
      message( yellow( "[StrategyConfiguration][INFO] Using default configuration: 2 clusters minimun" ) )
      2
    },
    maxNumClusters = function(...){
      message( yellow( "[StrategyConfiguration][INFO] Using default maxCluster configuration: 50 clusters maximun" ) )
      50
    }
  )
)