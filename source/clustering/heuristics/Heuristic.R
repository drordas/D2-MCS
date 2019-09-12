library("R6")
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
    heuristic = function(subset, ...) {
      stop("[Heuristic][ERROR] Function 'heuristic must be implemented in inherited class'")
    }
  ),
  private = list(
    name = NULL
  )
)