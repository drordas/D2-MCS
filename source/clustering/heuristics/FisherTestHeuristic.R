library("R6")
FisherTestHeuristic <- R6Class(
  classname = "FisherTestHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "FisherTestHeuristic")
    },
    # Heuristic valid for discrete variables
    heuristic = function(col1, col2, namesColums = NULL) {
      if (!private$isBinary(col1) || !private$isBinary(col2)) {
        warning("[", super$getName(), "][WARNING] Columns must to be binary. Return NA")
        NA
      } else {
        fisher.test(table(col1, col2))$p.value
      }
    }
  )
)