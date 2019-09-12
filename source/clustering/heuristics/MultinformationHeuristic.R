library("R6")
MultinformationHeuristic <- R6Class(
  classname = "MultinformationHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "MultinformationHeuristic")
    },
    heuristic = function(subset, ...) {
      if (dim(subset$removeUnnecesaryReal())[2] > 0) {
        corpus <- subset$removeUnnecesaryBinary()
      }
      class <- subset$getClass()
      mutinf <- sapply(corpus,function(e, class) {
        mutinformation(class, e)
      }, class)
      names(mutinf) <- names(corpus)
      mutinf
    }
  )
)