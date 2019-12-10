TrainOutput <- R6Class(
  classname = "TrainOutput",
  portable = TRUE,
  public = list(
    initialize = function(models, class.values, positive.class) {
      if ( missing(models) && !is.list(models) ) {
        stop("[", class(self)[1], "][ERROR] Models are incorrect. Must be a 'list' type. Aborting...")
      }
      if ( missing(class.values) && !is.character(class.values) && length(class.values) < 2 ) {
        stop("[", class(self)[1], "][ERROR] Class.values are incorrect. Must be a 'character' type. Aborting...")
      }
      if ( missing(positive.class) && !is.character(positive.class) && !positive.class %in% class.values ) {
        stop("[", class(self)[1], "][ERROR] Positive.class are incorrect. Must be a 'character' type. Aborting...")
      }

      private$models <- models
      private$class.values <- class.values
      private$positive.class <- positive.class
    },
    getModels = function(metric) {
      if ( missing(metric) && !is.list(metric) && !metric %in% self$getMetrics() ) {
        stop("[",class(self)[1],"][ERROR] Metric are incorrect. Must be a 'list' type. Aborting...")
      }
      private$models[[metric]]
    },
    getMetrics = function() { names(private$models) },
    getClassValues = function() { private$class.values },
    getPositiveClass = function() { private$positive.class },
    getSize = function() { length(names(private$models)) }
  ),
  private = list(
    models = NULL,
    class.values = NULL,
    positive.class = NULL
  )
)