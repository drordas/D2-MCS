SummaryFunction <- R6::R6Class(
  classname = "SummaryFunction",
  portable = TRUE,                   
  public = list(
    initialize = function(measures){
      if(is.null(measures))
        stop("[",class(self)[1],"][FATAL] Measures not defined")
      private$measures <- measures
    },
    execute = function(){
      stop("[",class(self)[1],"][FATAL] Function 'execute' must be implemented")
    },
    getMeasures = function(){
      private$measures
    }
  ),
  private = list(
    measures = NULL
  )
)