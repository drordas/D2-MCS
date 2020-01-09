TwoClass <- R6::R6Class(
  classname = "TwoClass",
  portable = TRUE,
  inherit = TrainFunction,
  public = list(
    initialize = function(method, number, savePredictions, classProbs,
                          allowParallel, verboseIter, seed=NULL){

      super$initialize(method, number, savePredictions, classProbs,
                       allowParallel, verboseIter, seed)
    },
    create = function(summaryFunction, search.method = "grid", class.probs){
      if( is.null(summaryFunction) ||  !"SummaryFunction" %in% class(summaryFunction) )
        stop("[",class(self)[1],"][ERROR] SummaryFunction must be defined as 'SummaryFunction' type\n")
      else{
        private$summaryFunction <- summaryFunction$execute
        if ( search.method %in% c("grid","random") ){
          private$search <- search.method
        }else{
          message("[",class(self)[1],"][WARNING] Invalid search method. ",
                  "Only 'random' or 'grid' search method are available. ",
                  "Assuming grid method")

        }
        class.probability <- ifelse( ( !missing(class.probs) &&
                                       is.logical(class.probs)),
                                     class.probs,  super$getClassProbs() )

        private$trFunction <- caret::trainControl( method=super$getResamplingMethod(),
                                                   number = super$getNumberFolds(),
                                                   savePredictions = super$getSavePredictions(),
                                                   classProbs = class.probability,
                                                   summaryFunction = private$summaryFunction,
                                                   search = private$search,
                                                   allowParallel = super$getAllowParallel(),
                                                   verboseIter = super$getVerboseIter() )

        private$measures <- summaryFunction$getMeasures()
      }
    },
    getTrFunction = function(){
      if(is.null(private$trFunction) )
        message("[",class(self)[1],"][WARNING] TrainFunction is not created. ",
                "Execute create method first. Returning 'NULL' value")
      private$trFunction
    },
    setClassProbs = function(class.probs){
      if(missing(class.probs) || !is.logical(class.probs))
        message("[",class(self)[1],"][WARNING] Class probabilities not defined or ",
            "erroneous. Ignoring operation")
      else{
        if ( is.null(private$trFunction) ){
          private$classProbs <- class.probs
          if( !is.null(private$summaryFunction) )
            self$create(summaryFunction,private$search)
          else message("[",class(self)[1],"][WARNING] SummaryFunction is not defined.",
                   " Unable to create TrainFunction")
        }else private$trFunction$classProbs <- class.probs
      }
    },
    getMeasures = function(){ private$measures },
    getType = function(){ private$type },
    setSummaryFunction = function (summaryFunction){
      if(is.null(summaryFunction) || !inherit(summaryFunction,"SummaryFunction") ){
        message("[",class(self)[1],"]][WARNING] SummaryFunction is null or ",
                "incorrect type. Method ignored")
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