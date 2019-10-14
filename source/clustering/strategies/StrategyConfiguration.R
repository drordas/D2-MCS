library("R6")
StrategyConfiguration <- R6Class(
  classname = "StrategyConfiguration",
  portable = TRUE,
  public = list(
    initialize = function(){ },
    getName = function() { class(self)[1] },
    minNumClusters = function(...){
      message( yellow( "[StrategyConfiguration][WARNING] Using the configuration for default: 2 clusters minimun" ) )
      2
    },
    maxNumClusters = function(...){
      message( yellow( "[StrategyConfiguration][WARNING] Using the configuration for default: 50 clusters maximun" ) )
      50
    }
  )
)