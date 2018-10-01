library("R6")
source("Model.R")
ranger <- R6Class(
  classname = "ranger",
  inherit = Model,
  portable = TRUE,                   
  public = list(
    initialize = function(dir = NULL, method = NULL, family = NULL, 
                          description = NULL, pkgName = NULL, trFunction = NULL, fit = NULL,
                          metric = NULL){ 
      super$initialize(dir,method,family,description,pkgName,trFunction,fit,metric)
      cat("[",super$getName(),"][INFO] '",family,"' '",description,"' created!\n", sep="")
    },
    train(dataset = NULL){
      if(is.null(dataset) || ! ("Subset" %in% class(dataset) ) )
        stop("[",super$getName(),"][ERROR] Dataset is not correct, Must be a 'Subset' class\n")
      if( !super$isTrained() ){
        trainedModel <- caret::train( super$getFormula(),data=dataset, method=super$getName(),
                                      trControl=super$getTrainFunction(), metric=getMetric() )
      }else cat("[",super$getName(),"][WARNING] Not implemented yet. Avoid execution\n")
    }
  ),
  private = list(
    trainedModel <- NULL
  )
)