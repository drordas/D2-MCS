library("R6")
ModelInfo <- R6Class(
  classname = "ModelInfo",
  portable = TRUE,                   
  public = list(
    initialize = function(){
      private$executed <- data.frame("method"=character(),"performance"=numeric(), 
                                     "time"=numeric(), stringsAsFactors = FALSE )
    },
    insert = function(modelName,modelPerformance,modelElapsedTime){
      #if("Model" %in% class(model))
      private$executed <- rbind(private$executed,c(modelName,modelPerformance, modelElapsedTime ), stringsAsFactors = FALSE )
      names(private$executed) <- c("method","performance","time")
    },
    getAll = function() { private$executed },
    getNames = function() { private$executed$method },
    getPerformances = function(modelName = NULL) { 
      if( !is.null(modelName) && !missing(modelName) ){
        private$executed$performance[ private$executed$method==modelName ]
      }else private$executed$performance 
    },
    getTimes = function() { private$executed$performance },
    dim = function()  { dim(private$executed) },
    nrow = function() { nrow(private$executed) },
    ncol = function() { ncol(private$executed) },
    isEmpty = function() { nrow(private$executed) == 0 }
  ),
  private = list( executed = NULL )
)