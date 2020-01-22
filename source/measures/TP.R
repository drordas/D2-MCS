TP <- R6::R6Class(
  classname = "TP",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function(performance.output = NULL){
      super$initialize(performance.output)
    },
    compute = function(performance.output = NULL){
      if ( is.null(super$performance) && !inherits(performance.output, c("MinResult", "ConfMatrix") ) )
        stop("[",class(self)[1],"][FATAL] performance.output parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      if ( !is.null(performance.output) && inherits(performance.output, c("MinResult", "ConfMatrix")) )
        output <- as.character(performance.output$getTP())
      else output <- as.character(super$performance$getTP())

      names(output) <- class(self)[1]
      output
    }
  )
)