library("R6")
Prediction <- R6Class(
  classname = "Prediction",
  portable = TRUE,                   
  public = list(
    initialize = function(model){
      if( !"ModelData" %in% class(model) )
        stop("[Prediction][ERROR] Parameter model must be defined as 'ModelData' Object\n")
      
      private$class.results <- NULL
      private$prob.results <- NULL
      private$model.name <- model$getModelMethod()
      private$model.performance <- model$getBestModelPerformance()
      private$model.trained <- model$getTrainModel()
      private$model.pkgs <- model$getModelPkgs()
    },
    execute = function( pred.values ){
      if( !missing(pred.values) && !is.null(pred.values) ){
        loadPackages(private$model.pkgs)
        private$class.results <- predict(object = private$model.trained, newdata=pred.values, type="raw" )
        private$prob.results <- predict(object = private$model.trained, newdata=pred.values, type="prob" )
        #unloadPackages(private$model.pkgs)
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
    model.pkgs = NULL,
    model.name = NULL,
    model.performance = NULL,
    model.trained = NULL,
    class.results = NULL,
    prob.results = NULL,
    pred.type = NULL
  )
)