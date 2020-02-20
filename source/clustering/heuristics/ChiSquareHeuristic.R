ChiSquareHeuristic <- R6::R6Class(
  classname = "ChiSquareHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    initialize = function() { },
    # Heuristic valid for both discrete and continuous variables
    heuristic = function(col1, col2, column.names = NULL) {
      tryCatch(
      # WARNING! Chi-squared approximation may be incorrect
      stats::chisq.test(col1, col2)$p.value,
      error = function(e) {
        message("[",class(self)[1],"][ERROR] Error occurred calculating ",
                "chi.square heuristic: '", e, "' . Returning NA")
        NA
      })
    }
  )
)