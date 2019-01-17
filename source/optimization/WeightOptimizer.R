library("R6")
WeightOptimizer <- R6Class(
  classname = "WeightOptimizer",
  portable = TRUE,
  public = list(
    initialize = function ( name, dependences = NULL ){
      if(missing(name) || is.null(name) || !is.character(name) )
        stop("[OutputOptimizer][ERROR] Optimizer name should be defined. Aborting\n")
      
      private$name <- name
      private$weights <- NULL
      
      lapply(dependences, function(pkg){
        if (! pkg %in% installed.packages() ){
          cat("[",private$name,"][WARNING] ",pkg," package required and not installed. Performing installation...\n",sep="")
          suppressMessages(install.packages(pkg,repos="https://ftp.cixug.es/CRAN/", dependencies = TRUE))
        }
        if(!pkg %in% loaded_packages() ){
          cat("[",private$name,"][WARNING] Loading ",pkg," package....\n",sep="")
          library(pkg,character.only = TRUE,warn.conflicts = FALSE, quietly = TRUE)
        } 
      })
    },
    execute = function(values){
      stop("[OutputOptimizer][ERROR] Method is abstract. Should be implemented in inherited class\n")
    },
    getResult = function(){ private$weights }
    getName = function(){ private$name }
  ),
  private = list(
    name = NULL,
    weights = NULL
  )
)
