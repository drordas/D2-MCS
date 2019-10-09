library("R6")
StrategyConfiguration <- R6Class(
  classname = "StrategyConfiguration",
  portable = TRUE,
  public = list(
    initialize = function(name = NULL){ private$name <- name },
    getName = function() { private$name },
    minNumClusters = function(...){
      message( yellow( "[StrategyConfiguration][WARNING] Using the configuration for default: 2 clusters minimun" ) )
      2
    },
    maxNumClusters = function(...){
      message( yellow( "[StrategyConfiguration][WARNING] Using the configuration for default: 50 clusters maximun" ) )
      50
    }
  ),
  private = list( name = NULL )
)