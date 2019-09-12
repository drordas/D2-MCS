library("R6")
OddsRatioSHeuristic <- R6Class(
  classname = "OddsRatioSHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "OddsRatioSHeuristic")
    },
    heuristic = function(subset, ...) {
      if (dim(subset$removeUnnecesaryReal())[2] > 0) {
        corpus <- subset$removeUnnecesaryBinary()
      }
      class <- subset$getClass()
      oddsrs.result <- sapply(corpus, function(e, class) {
        odds.ratio(class, e)$p
      }, class)
      names(oddsrs.result) <- names(corpus)
      sqrt(oddsrs.result)  
    }
  )
)