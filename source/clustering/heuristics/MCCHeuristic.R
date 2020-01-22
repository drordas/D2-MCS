MCCHeuristic <- R6::R6Class(
  classname = "MCCHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    initialize = function() { },
    # Heuristic valid for discrete variables
    heuristic = function(col1, col2, column.names = NULL) {
      if (!(private$isBinary(col1) && private$isBinary(col2))) {
        warning("[",class(self)[1],"][WARNING] Columns must be binary. ",
                "Returning NA")
        NA
      } else { mccr::mccr(col1, col2) }
    }
  )
)