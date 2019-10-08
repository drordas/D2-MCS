library("R6")
MultinformationHeuristic <- R6Class(
  classname = "MultinformationHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "MultinformationHeuristic")
    },
    # Heuristic valid for discrete variables
    heuristic = function(col1, col2, column.names = NULL) {
      if (!(private$isBinary(col1) && private$isBinary(col2))) {
        warning("[", super$getName(), "][WARNING] Values must to be binary. Return NA")
        NA
      } else { mutinformation(col1, col2) }
    }
  )
)