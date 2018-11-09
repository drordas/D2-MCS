library("R6")
TrainFunction <- R6Class(
  classname = "TrainFunction",
  portable = TRUE,  
  public = list(
    initialize = function(method, number, savePredictions, classProbs,
                          allowParallel, verboseIter){
      private$method <- method
      private$folds <- number
      private$savePredictions <- savePredictions
      private$classProbs <- classProbs
      private$allowParallel <- allowParallel
      private$verboseIter <- verboseIter
    },
    create = function(summaryFunction, search.method = "grid", class.probs){
      stop("[TrainFunction][ERROR] create is abstract. Must be implemented in inherited class\n")
    },
    getResamplingMethod = function(){
      private$method
    },
    getNumberFolds = function(){
      private$folds
    },
    getSavePredictions = function(){
      private$savePredictions
    },
    getClassProbs = function(){
      private$classProbs
    },
    getAllowParallel = function(){
      private$allowParallel
    },
    getVerboseIter = function(){
      private$verboseIter
    },
    getTrFunction = function(){
      stop("[TrainFunction][ERROR] getTrFunction is abstract. Must be implemented in inherited class\n")
    },
    getMeasures = function(){
      stop("[TrainFunction][ERROR] getMeasures is abstract. Must be implemented in inherited class\n")
    },
    getType = function(){
      stop("[TrainFuntion][ERROR] getType is abstract. Must be implemented in inherited class\n")
    },
    setSummaryFunction = function (summaryFunction){
      stop("[TrainFuntion][ERROR] setSummaryFunction is abstract. Must be implemented in inherited class\n")
    },
    setClassProbs = function(class.probs){
      stop("[TrainFuntion][ERROR] setClassProbs is abstract. Must be implemented in inherited class\n")
    }
  ),
  private = list(
    method = NULL,
    folds = NULL,
    savePredictions = NULL,
    classProbs = NULL,
    allowParallel = NULL,
    verboseIter = NULL
  )
)