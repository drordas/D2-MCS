library("R6")
OddsRatioSHeuristic <- R6Class(
  classname = "OddsRatioSHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    initialize = function() { },
    heuristic = function(col1, col2, column.names= NULL) {
      col1 <- as.integer(col1[, 1]) - 1
      if (!private$isBinary(col1) || !private$isBinary(col2)) {
        warning("[", super$getName(), "][WARNING] Columns must to be binary. Return NA")
        NA
      } else { sqrt(odds.ratio(col1, col2)$p) }
    }
  )
)