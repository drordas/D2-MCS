Recall <- R6::R6Class(
  classname = "Recall",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function (performance.output = NULL){
      super$initialize("Recall",performance.output)
    },
    compute = function(performance.output = NULL){
      if ( is.null(super$performance) && !inherits(performance.output,c("MinResult","Classifier","ModelPerformance","ConFMatrix") ) )
        stop("[",private$name,"][FATAL] Classifier object not included or invalid")

      if( !is.null(performance.output) && inherits(performance.output,c("MinResult","Classifier","ModelPerformance","ConFMatrix")) )
        performance.output$getConfusionMatrix()$byClass["Recall"]
      else super$performance$getConfusionMatrix()$byClass["Recall"]
    }
  )
)