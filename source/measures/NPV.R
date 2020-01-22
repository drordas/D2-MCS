NPV <- R6::R6Class(
  classname = "NPV",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function(performance.output = NULL){
      super$initialize(performance.output)
    },
    compute = function(performance.output){
      if ( is.null(super$performance) && !inherits(performance.output, c("MinResult", "ConfMatrix") ) )
        stop("[",class(self)[1],"][FATAL] Performance output parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      if( !is.null(performance.output) && inherits(performance.output, c("MinResult", "ConfMatrix")) )
        performance.output$getConfusionMatrix()$byClass["Neg Pred Value"]
      else super$getConfusionMatrix()$byClass["Neg Pred Value"]
    }
  )
)