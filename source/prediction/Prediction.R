library("R6")
Prediction <- R6Class(
  classname = "Prediction",
  portable = TRUE,                   
  public = list(
    initialize = function(model, class.values, positive.class){
      if (!"ModelData" %in% class(model) )
        stop("[Prediction][ERROR] Parameter model must be defined as 'ModelData' Object. Aborting... \n")
      
      if (length(class.values) != 2 )
        stop("[Prediction][ERROR] Incorrect argument. Class should be binary. Aborting... \n")
      
      if (length(positive.class) != 1)
        stop("[Prediction][ERROR] Incorrect argument. Positive class values should be a character string. Aborting... \n")
      
      private$class.results <- NULL
      private$prob.results <- NULL
      private$binary.results <- NULL
      private$model.name <- model$getModelMethod()
      private$model.performance <- model$getBestModelPerformance()
      private$model.trained <- model$getTrainModel()
      private$model.pkgs <- model$getModelPkgs()
      private$class.values <- class.values
      private$positive.class <- positive.class
      private$negative.class <- class.values[which(class.values != positive.class)]
    },
    execute = function( pred.values ){
      if( !missing(pred.values) && !is.null(pred.values) ){
        loadPackages(private$model.pkgs, quiet = TRUE)
        private$class.results <- predict(object = private$model.trained, newdata=pred.values, type="raw" )
        private$prob.results <- predict(object = private$model.trained, newdata=pred.values, type="prob" )
        private$binary.results <- as.numeric( as.character( factor( private$class.results,
                                                                    levels=c(private$negative.class,private$positive.class), 
                                                                    labels = c(0, 1) ) ) )
        unloadPackages(private$model.pkgs)
      }
      else stop("[Prediction][ERROR] Parameter pred.values is missing or empty\n")
    },
    getClassPrediction = function(){
      private$class.results
    },
    getBinaryPrediction = function(){
      private$binary.results
    },
    getProbPrediction = function(){
      private$prob.results
    },
    getModelName = function(){
      private$model.name
    },
    getModelPerformance = function(){
      private$model.performance
    },
    getPositiveClass = function(){
      private$positive.class
    },
    getNegativeClass = function(){
      private$negative.class
    }
  ),
  private = list(
    model.pkgs = NULL,
    model.name = NULL,
    model.performance = NULL,
    model.trained = NULL,
    class.results = NULL,
    binary.results = NULL,
    class.values = NULL, 
    positive.class = NULL,
    negative.class = NULL,
    prob.results = NULL,
    pred.type = NULL
  )
)