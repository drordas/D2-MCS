library("R6")
GainRatioHeuristic <- R6Class(
  classname = "GainRatioHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "GainRatioHeuristic")
    },
    heuristic = function(subset, ...) {
      corpus <- subset$removeUnnecesary(ignore.class = FALSE)
      className <- subset$getClassName()
      gainr <- gain.ratio(as.formula(sprintf("`%s` ~.", className)), corpus)
      gainr.values <- gainr$attr_importance
      names(gainr.values) <- row.names(gainr)
      gainr.values    
    }
  )
)