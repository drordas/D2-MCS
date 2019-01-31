MeasureFunction <- R6Class(
  classname = "MeasureFunction",
  portable = TRUE,
  public = list(
    initialize = function (name = NULL, performance = NULL){
      if( is.null(name) )
        stop("[MeasureFunction][ERROR] Measure name is not defined or is incorrect\n")
      
      if (is.null(performance) || !inherits(performance,"PerformanceOutput"))
        private$performance <- NULL
      else private$performance <- performance
      
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