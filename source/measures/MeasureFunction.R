MeasureFunction <- R6::R6Class(
  classname = "MeasureFunction",
  portable = TRUE,
  public = list(
    initialize = function (name = NULL, performance = NULL){
      if( is.null(name) )
        stop("[MeasureFunction][FATAL] Measure name is not defined or is incorrect")

      if ( !is.null(performance) && !inherits(performance.output,c("MinResult","Classifier","ModelPerformance") ) )
        stop("[MeasureFunction][FATAL] Performance argument should be of type 'MeasureFunction' or 'ClassifierPerformance'")

      private$performance <- performance
      private$name <- name
    },
    compute = function(performance = NULL){
      stop("[MeasureFunction][FATAL] Method 'execute' is abstract. Should be implemented in inherited class")
    },
    getName = function(){ private$name }
  ),
  private = list(
    name = NULL,
    performance = NULL
  )
)