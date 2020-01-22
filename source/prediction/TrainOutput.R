TrainOutput <- R6::R6Class(
  classname = "TrainOutput",
  portable = TRUE,
  public = list(
    initialize = function(models, class.values, positive.class) {
      if ( is.null(models) || !is.list(models) ) {
        stop("[", class(self)[1], "][FATAL] Models parameter must be defined as ",
             "'list' type. Aborting...")
      }
      if ( is.null(class.values) || !is.character(class.values) && length(class.values) < 2 ) {
        stop("[", class(self)[1], "][FATAL] Class.values parameter must be defined as ",
             "'list' character. Aborting...")
      }
      if ( is.null(positive.class) || !is.character(positive.class) || !positive.class %in% class.values ) {
        stop("[", class(self)[1], "][FATAL] Positive.class parameter must be defined as ",
             "'list' character. Aborting...")
      }

      private$models <- models
      private$class.values <- class.values
      private$positive.class <- positive.class
    },
    getModels = function(metric) {
      if ( is.null(metric) || is.list(metric) || !metric %in% self$getMetrics() ) {
        stop("[",class(self)[1],"][FATAL] Metric not defined of invalid. Aborting...")
      }
      private$models[[metric]]
    },
    getPerformance = function(metric=NULL){
      if (is.null(metric)){ metric <- self$getMetrics() }

      if(all(metric %in% self$getMetrics())){
        if(length(metric)==1) {
          sapply(self$getModels(metric), function(model){model$model.performance})
        }else{
          sapply(self$getMetrics(), function(metric) {
              sapply(self$getModels(metric),
                     function(model){ model$model.performance} )  } )
        }
      }else{ message("[",class(self)[1],"][WARNING] Metric not defined or invalid") }
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