TrainOutput <- R6Class(
  classname = "TrainOutput",
  portable = TRUE,
  public = list(
    initialize = function (models, weights, metric){
      if( !inherits(models,"ModelsList") )
        stop("[D2MCSOutput][ERROR] Models are incorrect. Must be a 'ModelsList' type. Aborting...\n")
      
      private$model <- models      
      private$weights <- weights
      private$metric <- metric
    },
    getWeights = function() { as.numeric(private$weights) },
    getMetric = function() { private$metric },
    getModels = function() { private$model }
  ),
  private = list(
    weights = NULL,
    metric = NULL,
    model = NULL
  )
)