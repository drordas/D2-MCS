library("R6")
OutputOptimizer <- R6Class(
  classname = "OutputOptimizer",
  portable = TRUE,
  public = list(
    initialize = function ( name ){
      if(missing(name) || is.null(name) || !is.character(name) )
        stop("[OutputOptimizer][ERROR] Optimizer name should be defined. Aborting\n")
      
      private$optimizing.name <- name
      private$optimized.weights <- NULL
    },
    execute = function(values){
      stop("[OutputOptimizer][ERROR] Method is abstract. Should be implemented in inherited class\n")
    },
    getResult = function(){ private$optimized.weights }
  ),
  private = list(
    optimizing.name = NULL,
    optimized.weights = NULL
  )
)
