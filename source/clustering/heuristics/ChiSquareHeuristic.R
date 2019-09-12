library("R6")
ChiSquareHeuristic <- R6Class(
  classname = "ChiSquareHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "ChiSquareHeuristic")
    },
    heuristic = function(subset, ...) {
      corpus <- subset$removeUnnecesary(ignore.class = TRUE)
      class <- subset$getClass()
      # WARNING! Chi-squared approximation may be incorrect
      chisq.result <- sapply(corpus,function(e, class){
        chisq.test(class, e)$p.value
      }, class)
      names(chisq.result) <- names(corpus)
      chisq.result
    }
  )
)