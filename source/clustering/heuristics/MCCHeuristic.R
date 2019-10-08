library("R6")
MCCHeuristic <- R6Class(
  classname = "MCCHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "MCCHeuristic")
    },
    # Heuristic valid for discrete variables
    heuristic = function(col1, col2, column.names = NULL) {
      if (!(private$isBinary(col1) && private$isBinary(col2))) {
        warning("[", super$getName(), "][WARNING] Values must be binary. Return NA")
        NA
      } else { mccr(col1, col2) }
    }
  )
)