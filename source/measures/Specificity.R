Specificity <- R6Class(
  classname = "Specificity",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function (performance.output = NULL){
      super$initialize("Specificity",performance.output)
    },
    compute = function(performance.output = NULL){
      if(is.null(super$performance) && (is.null(performance.output) || !inherits(performance.output,"PerformanceOutput") ) )
        stop("[Sensitivity][ERROR] PerformanceOutput object not included or invalid\n")
      
      if( !is.null(performance.output) && inherits(performance.output,"PerformanceOutput") )
        performance.output$getConfusionMatrix()$byClass["Specificity"]
      else super$performance$getConfusionMatrix()$byClass["Specificity"]
    }
  )
)