FisherTestHeuristic <- R6::R6Class(
  classname = "FisherTestHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    initialize = function() { },
    # Heuristic valid for discrete variables
    heuristic = function(col1, col2, column.names = NULL) {
      if ( private$isBinary(col1) && private$isBinary(col2) ) {
        fisher.test(table(col1, col2))$p.value
      } else {
        if(!private$isBinary(col1))
          message( yellow(paste0("[", super$getName(), "][WARNING] Feature '",
                                 column.names[1],"' is not binary. Returning NA")))
        else message( yellow(paste0("[", super$getName(), "][WARNING] Class '",
                                    column.names[2],"' is not binary. Returning NA")))
        NA 
      }
    }
  )
)