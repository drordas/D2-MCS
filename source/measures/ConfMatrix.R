ConFMatrix <- R6Class(
  classname = "ConFMatrix",
  portable = TRUE,
  public = list(
    initialize = function (confMatrix){
      if( !inherits(confMatrix,"confusionMatrix") )
        stop("[ConFMatrix][ERROR] Argument must be a caret::confusionMatrix object. Aborting\n")
      
      private$positive.class <- confMatrix$positive
      private$negative.class <- colnames(confMatrix$table)[which(colnames(confMatrix$table)!=confMatrix$positive)]
      private$confusionMatrix <- confMatrix
    },
    getConfusionMatrix = function() { private$confusionMatrix },
    getTP = function() { private$confusionMatrix$table[private$positive.class, private$positive.class] },
    getTN = function() { private$confusionMatrix$table[private$negative.class, private$negative.class] },
    getFP = function() { private$confusionMatrix$table[private$negative.class, private$positive.class] },
    getFN = function() { private$confusionMatrix$table[private$positive.class, private$negative.class] }
  ),
  private = list(
    confusionMatrix = NULL,
    positive.class = NULL,
    negative.class = NULL
  )
)