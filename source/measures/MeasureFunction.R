MeasureFunction <- R6::R6Class(
  classname = "MeasureFunction",
  portable = TRUE,
  public = list(
    initialize = function(performance = NULL){
      if ( !is.null(performance) && !inherits(performance, c("MinResult", "ConfMatrix") ) )
        stop("[",class(self)[1],"][FATAL] Performance parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      private$performance <- performance
    },
    compute = function(performance = NULL){
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    performance = NULL
  )
)