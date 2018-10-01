library("R6")
ModelEntry <- R6Class(
  classname = "ModelEntry",
  portable = TRUE,                   
  public = list(
    initialize = function(name, object, performance, path){
      private$name <- name
      private$object <- object
      private$metric <- performance
      private$path <- path
    },
    getPerformance = function(){ private$metric },
    getName = function(){ private$name },
    getObject = function(){ private$object },
    getPath = function(){ private$path },
    save = function(){
      if( "Model" %in% class(private$object) )
        private$object$saveModel()
      else cat("[ModelEntry][WARNING] Cannot save ModelEntry. Object is not correct.\n")
    }
  ),
  private = list(
    name = NULL,
    object = NULL,
    metric = NULL,
    path = NULL
  )
)