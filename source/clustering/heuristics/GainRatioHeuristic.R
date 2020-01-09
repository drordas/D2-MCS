GainRatioHeuristic <- R6::R6Class(
  classname = "GainRatioHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    initialize = function() { },
    # Heuristic valid for continuous variables (col2) where col1 is binary or categorical
    heuristic = function(col1, col2, column.names = NULL) {
      data <- as.data.frame(cbind(col1, col2))
      names(data) <- column.names
      gain.ratio(as.formula(sprintf("`%s` ~.", column.names[2])), data)$attr_importance
    }
  )
)