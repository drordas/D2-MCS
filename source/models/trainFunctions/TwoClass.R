library("R6")
library("caret")
source("TrainFunction.R")
TwoClass <- R6Class(
  classname = "TwoClass",
  portable = TRUE,
  inherit = TrainFunction,
  public = list(
    initialize = function(method, number, savePredictions, classProbs,
                          allowParallel, verboseIter){
      
      super$initialize(method, number, savePredictions, classProbs, allowParallel, verboseIter)
    },
    create = function(summaryFunction, searchMethod = "grid"){
      if( is.null(summaryFunction) ||  !"SummaryFunction" %in% class(summaryFunction) )
        stop("[TwoClass][ERROR] SummaryFunction must be defined as 'SummaryFunction' type\n")
      else{
        private$summaryFunction <- summaryFunction$execute
        if ( searchMethod %in% c("grid","random") ){
          private$search <- searchMethod
        }else cat("[TwoClass][WARNING] Erroneous search method selected. Only 'random' or 'grid' search method are available. Assuming grid method\n")
        private$trFunction <- trainControl( method=super$getResamplingMethod(), number = super$getNumberFolds(), 
                                            savePredictions = super$getSavePredictions(), classProbs = super$getClassProbs(),
                                            summaryFunction = private$summaryFunction, search = private$search,
                                            allowParallel = super$getAllowParallel(), verboseIter = super$getVerboseIter() )
        private$measures <- summaryFunction$getMeasures()
      }
    },
    getTrFunction = function(){
      if(is.null(private$trFunction) )
        cat("[TwoClass][WARNING] TrainFunction is not created. Execute create method first. Returning NULL\n")
      private$trFunction
    },
    setClassProbs = function(classProbs){
      if ( is.null(private$trFunction) ){
        private$classProbs <- classProbs
        if( !is.null(private$summaryFunction) ) 
          self$create(summaryFunction,private$search)
        else cat("[TwoClass][WARNING] SummaryFunction is not defined. Unable to create TrainFunction\n")
      }else private$trFunction$classProbs <- classProbs
    },
    getMeasures = function(){
      private$measures
    },
    getType = function(){
      private$type
    },
    setSummaryFunction = function (summaryFunction){
      if(is.null(summaryFunction) || !"SummaryFunction" %in% class(summaryFunction) ){
        cat("[TwoClass]][WARNING] SummaryFunction is null or incorrect type. Method ignored\n")
      }else{ 
        if(is.null(private$trFunction)){
          self$create(private$summaryFunction, private$search)
        }else private$trFunction$summaryFunction <- summaryFunction$execute
      }
    }
  ),
  private = list(
    measures = NULL,
    search = "grid",
    trFunction = NULL,
    summaryFunction = NULL,
    type = "Bi-Class"
  )
)