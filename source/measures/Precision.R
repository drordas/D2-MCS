Precision <- R6Class(
  classname = "Precision",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function (performance.output = NULL){
      super$initialize("Precision",performance.output)
    },
    compute = function(performance.output = NULL){
      if(is.null(super$performance) && (is.null(performance.output) || !inherits(performance.output,"PerformanceOutput") ) )
        stop("[Precision][ERROR] PerformanceOutput object not included or invalid\n")
      
      if( !is.null(performance.output) && inherits(performance.output,"PerformanceOutput") )
        performance.output$getConfusionMatrix()$byClass["Precision"]
      else super$performance$getConfusionMatrix()$byClass["Precision"]
    }
  )
)