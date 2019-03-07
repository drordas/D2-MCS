MeasureFunction <- R6Class(
  classname = "MeasureFunction",
  portable = TRUE,
  public = list(
    initialize = function (name = NULL, performance = NULL){
      if( is.null(name) )
        stop("[MeasureFunction][ERROR] Measure name is not defined or is incorrect\n")
      
      if ( !is.null(performance) && !inherits(performance.output,c("MinResult","Classifier","ModelPerformance") ) )
        stop("[MeasureFunction][ERROR] Performance argument should be of type 'MeasureFunction' or 'ClassifierPerformance'\n")

      private$performance <- performance
      private$name <- name
    },
    compute = function(performance = NULL){
      stop("[MeasureFunction][ERROR] Method 'execute' is abstract. Should be implemented in inherited class\n")
    },
    getName = function(){ private$name }
  ),
  private = list(
    name = NULL,
    performance = NULL
  )
)