FisherTestHeuristic <- R6::R6Class(
  classname = "FisherTestHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    initialize = function() { },
    # Heuristic valid for discrete variables
    heuristic = function(col1, col2, column.names = NULL) {
      if ( private$isBinary(col1) && private$isBinary(col2) ) {
        col2.factor <- factor(col2)
        col1.factor <- factor(col1, levels =  levels(col2.factor) )
        stats::fisher.test(col1.factor, col2.factor)$p.value
        #stats::fisher.test(table(col1.factor, col2.factor))$p.value
      } else {
        if(!private$isBinary(col1))
          message("[",class(self)[1],"][WARNING] Column '",
                  column.names[1],"' is not binary. Returning NA")
        else message("[", class(self)[1],"][WARNING] Column '",
                     column.names[2],"' is not binary. Returning NA")
        NA
      }
    }
  )
)