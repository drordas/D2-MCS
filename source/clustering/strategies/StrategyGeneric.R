library("R6")
StrategyGeneric <- R6Class(
  "StrategyGeneric",
  public = list(
    initialize = function(name = NULL, subset, heuristic) {
      if (!"Subset" %in% class(subset)) {
        stop("[StrategyGeneric][ERROR] subset parameter must be defined as 'Subset' type")
      }
      if (is.list(heuristic)) {
        for (h in heuristic) {
          if (!"Heuristic" %in% class(h)) {
            stop("[StrategyGeneric][ERROR] heuristic parameter must be defined as a list of 'Heuristic' type objects")
          }
        }
      } else {
        stop("[StrategyGeneric][ERROR] heuristic parameter must be defined as a list of 'Heuristic' type objects")
      }
      private$name <- name
      private$subset <- subset
      private$heuristic <- heuristic
    },
    getName = function() {
      private$name
    },
    getHeuristic = function() {
      private$heuristic
    },
    getSubset = function() {
      private$subset
    },
    getAllDistribution = function() {
      private$all.distribution
    },
    getBestDistribution = function() {
      private$best.distribution
    },
    setName = function(name) {
      private$name <- name
    },
    setHeuristic = function(heuristic) {
      if (!"Heuristic" %in% class(heuristic)) {
        stop("[StrategyGeneric][ERROR] heuristic parameter must be defined as 'Heuristic' type")
      }
      private$heuristic <- heuristic
    },
    execute = function(...) {
      stop("[StrategyGeneric][ERROR] I am an abstract interface method")
    },
    getDistribution = function(cluster = NULL, group = NULL, includeClass = "NONE", ...)  {
      stop("[StrategyGeneric][ERROR] I am an abstract interface method")
    },
    createSubset = function(subset, cluster = NULL, ...) {
      stop("[StrategyGeneric][ERROR] I am an abstract interface method")
    },
    plot = function(dir.path = NULL, file.name = NULL, ...) {
      stop("[StrategyGeneric][ERROR] I am an abstract interface method")
    }
  ),
  private = list(
    name = NULL,
    subset = NULL,
    heuristic = NULL,
    all.distribution = NULL,
    best.distribution = NULL
  )
)