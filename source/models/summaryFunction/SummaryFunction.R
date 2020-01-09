SummaryFunction <- R6::R6Class(
  classname = "SummaryFunction",
  portable = TRUE,                   
  public = list(
    initialize = function(measures){
      #stop("[ERROR][SummaryFunction] Function 'initialize' must be implemented\n")
      if(is.null(measures))
        stop("[ERROR][SummaryFunction] Measures not defined.\n")
      private$measures <- measures
    },
    execute = function(){
      stop("[ERROR][SummaryFunction] Function 'execute' must be implemented\n")
    },
    getMeasures = function(){
      private$measures
    }
  ),
  private = list(
    measures = NULL
  )
)