library("R6")
ChiSquareHeuristic <- R6Class(
  classname = "ChiSquareHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "ChiSquareHeuristic")
    },
    # Heuristic valid for both discrete and continuous variables
    heuristic = function(col1, col2, column.names = NULL) {
      # WARNING! Chi-squared approximation may be incorrect
      chisq.test(col1, col2)$p.value
    }
  )
)