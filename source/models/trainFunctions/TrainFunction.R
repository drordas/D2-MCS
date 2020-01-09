TrainFunction <- R6::R6Class(
  classname = "TrainFunction",
  portable = TRUE,
  public = list(
    initialize = function(method, number, savePredictions, classProbs,
                          allowParallel, verboseIter, seed){
      private$method <- method
      private$folds <- number
      private$savePredictions <- savePredictions
      private$classProbs <- classProbs
      private$allowParallel <- allowParallel
      private$verboseIter <- verboseIter
      if(!is.numeric(seed)){
        private$seed <- .Random.seed[ceiling(runif(1,0,length(.Random.seed)))]
        message("[",class(self)[1],"][INFO] Using random seed '",private$seed,"'")
      }else {
        private$seed <- seed
        message("[",class(self)[1],"][INFO] Using static seed '",private$seed,"'")
      }
    },
    create = function(summaryFunction, search.method = "grid", class.probs){
      stop("[",class(self)[1],"][FATAL] create function is abstract. ",
           "Must be implemented in inherited class")
    },
    getResamplingMethod = function(){ private$method },
    getNumberFolds = function(){ private$folds },
    getSavePredictions = function(){ private$savePredictions },
    getClassProbs = function(){ private$classProbs },
    getAllowParallel = function(){ private$allowParallel },
    getVerboseIter = function(){ private$verboseIter },
    getTrFunction = function(){
      stop("[",class(self)[1],"][FATAL] getTrFunction is abstract. ",
           "Must be implemented in inherited class")
    },
    getMeasures = function(){
      stop("[",class(self)[1],"][FATAL] getMeasures is abstract. ",
           "Must be implemented in inherited class")
    },
    getType = function(){
      stop("[",class(self)[1],"][FATAL] getType is abstract. ",
           "Must be implemented in inherited class")
    },
    getSeed = function(){ private$seed },
    setSummaryFunction = function (summaryFunction){
      stop("[",class(self)[1],"][FATAL] setSummaryFunction is abstract. ",
           "Must be implemented in inherited class")
    },
    setClassProbs = function(class.probs){
      stop("[",class(self)[1],"][FATAL] setClassProbs is abstract. ",
           "Must be implemented in inherited class")
    }
  ),
  private = list(
    method = NULL,
    folds = NULL,
    savePredictions = NULL,
    classProbs = NULL,
    allowParallel = NULL,
    verboseIter = NULL,
    seed = NULL
  )
)