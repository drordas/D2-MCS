Recall <- R6::R6Class(
  classname = "Recall",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function(performance.output = NULL){
      super$initialize(performance.output)
    },
    compute = function(performance.output = NULL){
      if ( is.null(private$performance) && !inherits(performance.output, c("MinResult", "ConfMatrix") ) )
        stop("[",class(self)[1],"][FATAL] Performance output parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      if( !is.null(performance.output) && inherits(performance.output, c("MinResult", "ConfMatrix")) )
        output <- performance.output$getConfusionMatrix()$byClass["Recall"]
      else output <- private$performance$getConfusionMatrix()$byClass["Recall"]

      names(output) <- class(self)[1]
      output
    }
  )
)