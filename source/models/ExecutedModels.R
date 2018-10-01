library("R6")
ExecutedModels <- R6Class(
  classname = "ExecutedModels",
  portable = TRUE,                   
  public = list(
    initialize = function(num.clusters){
      private$executed <- data.frame("method"=as.character(),"performance"=as.numeric(), "time"=as.numeric() )
    },
    insert = function(model){
      if("Model" %in% class(model))
        private$executed <- rbind(private$executed,c(model$getName(), model$getPerformance(), model$getElapsedTime() ) )
    },
    getAll = function(){ private$executed },
    dim = function()  { dim(private$executed) },
    nrow = function() { nrow(private$executed) },
    ncol = function() { ncol(private$executed) },
    isEmpty = function() { nrow(private$executed) == 0 }
  ),
  private = list( executed = NULL )
)