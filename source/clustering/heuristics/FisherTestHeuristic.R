library("R6")
FisherTestHeuristic <- R6Class(
  classname = "FisherTestHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "FisherTestHeuristic")
    },
    heuristic = function(subset, ...) {
      corpus <- subset$getBinaryFeatures()
      class <- subset$getClass()
      sapply(corpus, function(col) {fisher.test(table(col, class))$p.value})
    }
  )
)