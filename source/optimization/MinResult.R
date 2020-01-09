MinResult <- R6::R6Class(
  classname = "MinResult",
  portable = TRUE,
  public = list(
    initialize = function (name, n.positive, n.negative){
      if (!is.numeric(n.positive) && !is.numeric(n.negative))
        stop("[MinResult][ERROR] Arguments 'n.positive' and 'n.negative' must be numeric\n")
      
      private$name <- name
      private$n.positive <- n.positive
      private$n.negative <- n.negative
      private$values <- NULL
    },
    getNumPositives = function(){ private$n.positive },
    getNumNegatives = function(){ private$n.negative },
    getValues = function() {private$values },
    getConfusionMatrix = function( pareto.distance = NULL) { 
      stop("[MinResult][ERROR] Method 'getConfusionMatrix' is abstract. Must be implemented in inherited class\n")
    },
    getFP = function() {
      stop("[MinResult][ERROR] Method 'getFP' is abstract. Must be implemented in inherited class\n")
    },
    getFN = function() {
      stop("[MinResult][ERROR] Method 'getFN' is abstract. Must be implemented in inherited class\n")
    },
    getTN = function() {
      stop("[MinResult][ERROR] Method 'getTN' is abstract. Must be implemented in inherited class\n")
    },
    getTP = function() {
      stop("[MinResult][ERROR] Method 'getTP' is abstract. Must be implemented in inherited class\n")
    },
    getName = function(){ private$name }
  ),
  private = list(
    name = NULL,
    n.positive = NULL,
    n.negative = NULL,
    values = NULL
  )
)