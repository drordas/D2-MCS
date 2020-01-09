FN <- R6::R6Class(
  classname = "FN",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function(performance.output = NULL){
      super$initialize("FN", performance.output)
    },
    compute = function(performance.output = NULL){
      if ( is.null(super$performance) && !inherits(performance.output, c("MinResult", "Classifier", "ModelPerformance", "ConFMatrix") ) )
        stop("[", private$name, "][ERROR] Classifier object not included or invalid")
      
      if ( !is.null(performance.output) && inherits(performance.output, c("MinResult", "Classifier", "ModelPerformance", "ConFMatrix")) ) {
        as.character(performance.output$getFN())
      } else {
        as.character(super$performance$getFN())
      }
    }
  )
)