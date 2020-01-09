SpearmanHeuristic <- R6::R6Class(
  classname = "SpearmanHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    initialize = function() { },
    # Heuristic valid for both discrete and continuous variables
    heuristic = function(col1, col2, column.names = NULL) {
      cor.test(col1, col2, method = "spearman", exact = FALSE)$p.value
    }
  )
)