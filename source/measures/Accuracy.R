Accuracy <- R6Class(
  classname = "Accuracy",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function (performance.output = NULL){
      super$initialize("Accuracy",performance.output)
    },
    compute = function(performance.output = NULL){
      if ( is.null(super$performance) && !inherits(performance.output,c("MinResult","Classifier","ModelPerformance","ConFMatrix") ) )
        stop("[",private$name,"][ERROR] Classifier object not included or invalid\n")
      
      if( !is.null(performance.output) && inherits(performance.output,c("MinResult","Classifier","ModelPerformance","ConFMatrix")) )
        performance.output$getConfusionMatrix()$overall["Accuracy"]
      else super$performance$getConfusionMatrix()$overall["Accuracy"]
    }
  )
)