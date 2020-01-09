MultinformationHeuristic <- R6::R6Class(
  classname = "MultinformationHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    initialize = function() { },
    # Heuristic valid for discrete variables
    heuristic = function(col1, col2, column.names = NULL) {
      if (!(private$isBinary(col1) && private$isBinary(col2))) {
        warning("[", super$getName(), "][WARNING] Values must to be binary. Return NA")
        NA
      } else { infotheo::mutinformation(col1, col2) }
    }
  )
)