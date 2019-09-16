library("R6")
PearsonHeuristic <- R6Class(
  classname = "PearsonHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "PearsonHeuristic")
    },
    heuristic = function(subset, ...) {
      i <- eval.parent(substitute(alist(...))[["i"]])
      j <- eval.parent(substitute(alist(...))[["j"]])
      corpus <- eval.parent(substitute(alist(...))[["corpus"]])
      if (missing(corpus)) {
        corpus <- subset$removeUnnecesaryReal()  
      }
      
      if (!is.numeric(i) && !is.character(i)) {
        stop("[", super$getName(), "][ERROR] i parameter must to be numeric or character")
      }
      if (!is.numeric(j) && !is.character(j)) {
        stop("[", super$getName(), "][ERROR] j parameter must to be numeric or character")
      }
      cor(corpus[, i], corpus[, j])
    }
  )
)