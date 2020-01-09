MCC <- R6::R6Class(
  classname = "MCC",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function (performance.output = NULL){
      super$initialize("MCC",performance.output)
    },
    compute = function(performance.output = NULL){

      if ( is.null(super$performance) && !inherits(performance.output,c("MinResult","Classifier","ModelPerformance", "ConFMatrix") ) )
        stop("[",private$name,"][FATAL] Classifier object not included or invalid")

      if( inherits(performance.output,c("MinResult","Classifier","ModelPerformance","ConFMatrix")) )
         output <- mltools::mcc( TP = performance.output$getTP(), FP = performance.output$getFP(), FN = performance.output$getFN(), TN = performance.output$getTN() )
      else output <- mltools::mcc( TP = super$performance$getTP(), FP = super$performance$getFP(), FN = super$performance$getFN(), TN = super$performance$getTN() )
      names(output) <- super$getName()
      output
    }
  )
)