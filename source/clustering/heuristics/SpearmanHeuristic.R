library("R6")
SpearmanHeuristic <- R6Class(
  classname = "SpearmanHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "SpearmanHeuristic")
    },
    # Heuristic valid for both discrete and continuous variables
    heuristic = function(col1, col2, column.names = NULL) {
      cor.test(col1, col2, method = "spearman", exact = FALSE)$p.value
    }
  )
)