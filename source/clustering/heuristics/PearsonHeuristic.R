library("R6")
PearsonHeuristic <- R6Class(
  classname = "PearsonHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "PearsonHeuristic")
    },
    # Heuristic valid for both discrete and continuous variables
    heuristic = function(col1, col2, namesColums = NULL) {
      cor(col1, col2)
    }
  )
)