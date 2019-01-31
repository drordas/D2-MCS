Kappa <- R6Class(
  classname = "Kappa",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function (performance.output = NULL){
      super$initialize("Kappa",performance.output)
    },
    compute = function(performance.output = NULL){
      if(is.null(super$performance) && (is.null(performance.output) || !inherits(performance.output,"PerformanceOutput") ) )
        stop("[Kappa][ERROR] PerformanceOutput object not included or invalid\n")
      
      if( !is.null(performance.output) && inherits(performance.output,"PerformanceOutput") )
        performance.output$getConfusionMatrix()$overall["Kappa"]
      else super$performance$getConfusionMatrix()$overall["Kappa"]
    }
  )
)