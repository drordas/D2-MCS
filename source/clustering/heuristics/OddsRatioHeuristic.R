OddsRatioHeuristic <- R6::R6Class(
  classname = "OddsRatioHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    initialize = function() { },
    heuristic = function(col1, col2, column.names= NULL) {
      if (!private$isBinary(col1) || !private$isBinary(col2)) {
        message("[",class(self)[1],"][WARNING] Columns must be binary. ",
                "Returning NA")
        NA
      } else {
        # data <- as.data.frame(cbind(col1, col2))
        # names(data) <- column.names
        tryCatch(
        questionr::odds.ratio(table(col1,col2))$p,
        error = function(e) {
          message("[",class(self)[1],"][ERROR] Error occurred calculating ",
                  "odds.ratio heuristic: '", e, "' . Returning NA")
          NA
        })
      }
    }
  )
)