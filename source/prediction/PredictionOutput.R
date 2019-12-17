PredictionOutput <- R6Class(
  classname = "PredictionOutput",
  portable = TRUE,
  public = list(
    initialize = function(predictions, type, target) {
      private$predictions <- predictions
      private$type <- type
      private$target <- target
    },
    getPredictions = function() { private$predictions },
    getType = function() { private$type },
    getTarget = function() { private$target }
  ),
  private = list(
    predictions = NULL,
    type = NULL,
    target = NULL
  )
)