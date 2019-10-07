Heuristic <- R6Class(
  classname = "Heuristic",
  portable = TRUE,
  public = list(
    initialize = function(name = NULL) {
      private$name <- name
    },
    getName = function() {
      private$name
    },
    heuristic = function(col1, col2, namesColums = NULL, ...) {
      stop("[Heuristic][ERROR] Function 'heuristic must be implemented in inherited class'")
    }
  ),
  private = list(
    name = NULL,
    isBinary = function(column) {
      all(levels(factor(column)) %in% c("0","1"))
    }
  )
)