PearsonHeuristic <- R6::R6Class(
  classname = "PearsonHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    initialize = function() { },
    # Heuristic valid for both discrete and continuous variables
    heuristic = function(col1, col2, column.names = NULL) {
      cor(col1, col2, method = "pearson")
    }
  )
)