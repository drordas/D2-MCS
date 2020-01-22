CombinedMetrics <- R6::R6Class(
  classname = "CombinedMetrics",
  portable = TRUE,
  public = list(
    initialize = function(required.metrics) {
      if ( is.null(required.metrics) || !is.character(required.metrics) || length(required.metrics) < 2 ) {
        stop("[", class(self)[1], "][FATAL] Required.metrics parameter must be ",
             "defined as 'character' type. Aborting...")
      }
      private$required.metrics <- required.metrics
    },
    getRequiredMetrics = function() { private$required.metrics },
    getFinalPrediction = function(raw.pred, prob.pred, positive.class, negative.class) {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    required.metrics = c()
  )
)