SpearmanHeuristic <- R6::R6Class(
  classname = "SpearmanHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    initialize = function() { },
    # Heuristic valid for both discrete and continuous variables
    heuristic = function(col1, col2, column.names = NULL) {
      if (!is.numeric(col1) || !is.numeric(col2)) {
        message("[",class(self)[1],"][WARNING] Columns must be 'numeric' type. ",
                "Returning NA")
        NA
      } else {
        tryCatch(
        stats::cor.test(col1, col2, method = "spearman", exact = FALSE)$p.value,
        error = function(e) {
          message("[",class(self)[1],"][ERROR] Error occurred calculating ",
                  "spearman heuristic: '", e, "' . Returning NA")
          NA
        })
      }
    }
  )
)