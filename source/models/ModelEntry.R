library("R6")
ModelEntry <- R6Class(
  classname = "ModelEntry",
  portable = TRUE,                   
  public = list(
    initialize = function(name, object = NULL, performance, path = NULL){
      private$name <- name
      private$metric <- performance
      private$path <- path
      if( is.null(object) && !is.null(path) && file.exists(path) && (file_ext(path) %in% tolower("rds")) ){
        private$object <- readRDS(path)
      }else private$object <- object
    },
    getPerformance = function(){ private$metric },
    getName = function(){ private$name },
    getObject = function(){ private$object },
    getPath = function(){ private$path },
    save = function(){
      if( "Model" %in% class(private$object) )
        private$object$saveModel()
      else cat("[ModelEntry][WARNING] Cannot save '",private$name,"' model. Object is not correct.\n",sep="")
    },
    removeModel = function(){ 
      if( inherits(private$object,"Model") && file.exists(private$path) ){
        file.remove(private$path)
        private$object <- NULL
      }else cat("[ModelEntry][WARNING] Cannot remove '",private$name,"' model. Object is not correct.\n", sep="") 
    }
  ),
  private = list(
    name = NULL,
    object = NULL,
    metric = NULL,
    path = NULL
  )
)