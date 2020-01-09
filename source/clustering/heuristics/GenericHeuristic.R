GenericHeuristic <- R6::R6Class(
  classname = "GenericHeuristic",
  portable = TRUE,
  public = list(
    initialize = function() { },
    getName = function() { class(self)[1]},
    heuristic = function(col1, col2, column.names = NULL, ...) {
      stop("[GenericHeuristic][ERROR] Function 'heuristic must be implemented in inherited class'")
    }
  ),
  private = list(
    isBinary = function(column) {
      all(levels(factor(column)) %in% c("0","1"))
    }
  )
)