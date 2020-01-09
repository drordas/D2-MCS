NPV <- R6::R6Class(
  classname = "NPV",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function (performance.output = NULL){
      super$initialize("NPV",performance.output)
    },
    compute = function(performance.output){
      if ( is.null(super$performance) && !inherits(performance.output,c("MinResult","Classifier","ModelPerformance","ConFMatrix") ) )
        stop("[",private$name,"][ERROR] Classifier object not included or invalid\n")
      
      if( !is.null(performance.output) && inherits(performance.output,c("MinResult","Classifier","ModelPerformance","ConFMatrix")) )
        performance.output$getConfusionMatrix()$byClass["Neg Pred Value"]
      else super$getConfusionMatrix()$byClass["Neg Pred Value"]
    }
  )
)