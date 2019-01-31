PPV <- R6Class(
  classname = "PPV",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    initialize = function (performance.output = NULL){
      super$initialize("PPV",performance.output)
    },
    compute = function(performance.output = NULL){
      if(is.null(super$performance) && (is.null(performance.output) || !inherits(performance.output,"PerformanceOutput") ) )
        stop("[PPV][ERROR] PerformanceOutput object not included or invalid\n")
      
      if( !is.null(performance.output) && inherits(performance.output,"PerformanceOutput") )
        output <- performance.output$getConfusionMatrix()$byClass["Pos Pred Value"]
      else output <- super$performance$getConfusionMatrix()$byClass["Pos Pred Value"]
      
      names(output) <- super$getName()
      output
    }
  )
)