GenericHeuristic <- R6::R6Class(
  classname = "GenericHeuristic",
  portable = TRUE,
  public = list(
    initialize = function() { },
    heuristic = function(col1, col2, column.names = NULL, ...) {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    isBinary = function(column) {
      length(levels(factor(column))) == 2
    }
  )
)