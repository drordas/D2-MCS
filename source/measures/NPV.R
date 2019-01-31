NPV <- R6Class(
  classname = "NPV",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function (performance.output = NULL){
      super$initialize("NPV",performance.output)
    },
    compute = function(performance.output){
      if(is.null(super$performance) && (is.null(performance.output) || !inherits(performance.output,"PerformanceOutput") ) )
        stop("[NPV][ERROR] PerformanceOutput object not included or invalid\n")
      
      if( !is.null(performance.output) && inherits(performance.output,"PerformanceOutput") )
        performance.output$getConfusionMatrix()$byClass["Neg Pred Value"]
      else super$getConfusionMatrix()$byClass["Neg Pred Value"]
    }
  )
)