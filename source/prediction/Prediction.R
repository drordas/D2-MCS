library("R6")
Prediction <- R6Class(
  classname = "Prediction",
  portable = TRUE,                   
  public = list(
    initialize = function(model){#, pred.type){
      if( !"Model" %in% class(model) )
        stop("[Prediction][ERROR] Parameter model must be defined as 'Model' Object\n")
      
      #if(is.null(pred.type) || !pred.type %in% c("response","raw", "prob") )
      #  stop("[Prediction][ERROR] Parameter pred.type must be defined as 'raw' of 'prob' value\n")
      
      private$class.results <- NULL
      private$prob.results <- NULL
      private$model.name <- model$getName()
      private$model.performance <- model$getPerformance()
      private$model.trained <- model$getTrainedModel()
    },
    execute = function( pred.values ){
      if( !missing(pred.values) && !is.null(pred.values) ){
        private$class.results <- predict(object = private$model.trained, newdata=pred.values, type="raw" )
        private$prob.results <- predict(object = private$model.trained, newdata=pred.values, type="prob" )
      }
      else stop("[Prediction][ERROR] Parameter pred.values is missing or empty\n")
    },
    getClassPrediction = function(){
      private$class.results
    },
    getProbPrediction = function(){
      private$prob.results
    },
    getModelName = function(){
      private$model.name
    },
    getModelPerformance = function(){
      private$model.performance
    }
  ),
  private = list(
    model.name = NULL,
    model.performance = NULL,
    model.trained = NULL,
    class.results = NULL,
    prob.results = NULL,
    pred.type = NULL
  )
)