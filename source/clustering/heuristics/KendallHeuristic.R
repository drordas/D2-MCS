library("R6")
KendallHeuristic <- R6Class(
  classname = "KendallHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "KendallHeuristic")
    },
    # Heuristic valid for continuous variables
    heuristic = function(col1, col2, column.names = NULL) {
      if (private$isBinary(col1) || !private$isBinary(col2)) {
        warning("[", super$getName(), "][WARNING] Columns must be real. Return NA")
        NA
      } else {
        unname(cor.test(col1, col2, method = "kendall")$estimate, force = TRUE)
      }
    }
  )
)