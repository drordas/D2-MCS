TP <- R6::R6Class(
  classname = "TP",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function(performance.output = NULL){
      super$initialize("TP", performance.output)
    },
    compute = function(performance.output = NULL){
      if ( is.null(super$performance) && !inherits(performance.output,c("MinResult","Classifier","ModelPerformance","ConFMatrix") ) )
        stop("[", private$name, "][ERROR] Classifier object not included or invalid")
      
      if ( !is.null(performance.output) && inherits(performance.output,c("MinResult","Classifier","ModelPerformance","ConFMatrix")) ) {
        as.character(performance.output$getTP())
      } else {
        as.character(super$performance$getTP())
      }
    }
  )
)