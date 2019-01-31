MCC <- R6Class(
  classname = "MCC",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function (performance.output = NULL){
      super$initialize("MCC",performance.output)
    },
    compute = function(performance.output = NULL){
      if(is.null(super$performance) && (is.null(performance.output) || !inherits(performance.output,"PerformanceOutput") ) )
        stop("[MCC][ERROR] PerformanceOutput object not included or invalid\n")
      
      if( !is.null(performance.output) && inherits(performance.output,"PerformanceOutput") )
         output <- mltools::mcc(preds= performance.output$getPredictions(), actuals = performance.output$getObserved() )
      else output <- mltools::mcc(preds= super$performance$getPredictions(), actuals = super$performance$getObserved() )
      names(output) <- super$getName()
      output
    }
  )
)