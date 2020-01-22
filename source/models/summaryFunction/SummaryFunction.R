SummaryFunction <- R6::R6Class(
  classname = "SummaryFunction",
  portable = TRUE,
  public = list(
    initialize = function(measures){
      if(is.null(measures))
        stop("[",class(self)[1],"][FATAL] Measures were not defined. Aborting...")
      private$measures <- measures
    },
    execute = function(){
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    getMeasures = function(){
      private$measures
    }
  ),
  private = list(
    measures = NULL
  )
)