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
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    getFP = function() {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    getFN = function() {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    getTN = function() {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    getTP = function() {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
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