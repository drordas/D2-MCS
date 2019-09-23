library("R6")
GainRatioHeuristic <- R6Class(
  classname = "GainRatioHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "GainRatioHeuristic")
    },
    # Heuristic valid for continuous variables
    heuristic = function(col1, col2, namesColums = NULL) {
      data <- as.data.frame(cbind(col1, col2))
      names(data) <- namesColums
      gain.ratio(as.formula(sprintf("`%s` ~.", namesColums[2])), data)$attr_importance
    }
  )
)