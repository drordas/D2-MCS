library("R6")
source("Model.R")
knn <- R6Class(
  classname = "knn",
  inherit = Model,
  portable = TRUE,                   
  public = list(
    initialize = function(dir = NULL, method = NULL, family = NULL, 
                          description = NULL, pkgName = NULL, trFunction = NULL, fit = NULL,
                          metric = NULL){ 
      super$initialize(dir,method,family,description,pkgName,trFunction,fit,metric)
      cat("[",method,"][INFO] '",family,"' '",description,"' created!\n", sep="")
      private$isTrained = FALSE
    }
  ),
  private = list(
    isTrained = FALSE
  )
)