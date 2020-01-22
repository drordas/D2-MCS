OddsRatioSHeuristic <- R6::R6Class(
  classname = "OddsRatioSHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    initialize = function() { },
    heuristic = function(col1, col2, column.names= NULL) {
      col1 <- as.integer(col1[, 1]) - 1
      if (!private$isBinary(col1) || !private$isBinary(col2)) {
        warning("[",class(self)[1],"][WARNING] Columns must to be binary. ",
                "Returning NA")
        NA
      } else { sqrt(odds.ratio(col1, col2)$p) }
    }
  )
)