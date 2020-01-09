Methodology <- R6::R6Class(
  classname = "Methodology",
  portable = TRUE,
  public = list(
    initialize = function(required.metrics) {
      if ( missing(required.metrics) && !is.character(required.metrics) && length(required.metrics) > 2 ) {
        stop("[", class(self)[1], "][FATAL] Required.metrics are incorrect. Must be a 'character' type. Aborting...")
      }
      private$required.metrics <- required.metrics
    },
    getName = function() { class(self)[1] },
    getRequiredMetrics = function() { private$required.metrics },
    compute = function(raw.pred, prob.pred, positive.class, negative.class, cutoff) {
      stop("[", class(self)[1], "][FATAL] Class is abstract.",
           " Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    required.metrics = c()
  )
)