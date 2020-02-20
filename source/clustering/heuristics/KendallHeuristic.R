KendallHeuristic <- R6::R6Class(
  classname = "KendallHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    initialize = function() { },
    # Heuristic valid for continuous variables
    heuristic = function(col1, col2, column.names = NULL) {
      if (private$isBinary(col1) || !private$isBinary(col2)) {
        message("[",class(self)[1],"][WARNING] Columns must be real. ",
                "Returning NA")
        NA
      } else {
        tryCatch(
        unname(stats::cor.test(col1, col2, method = "kendall")$estimate, force = TRUE),
        error = function(e) {
          message("[",class(self)[1],"][ERROR] Error occurred calculating ",
                  "kendall heuristic: '", e, "' . Returning NA")
          NA
        })
      }
    }
  )
)