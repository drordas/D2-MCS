Cluster <- R6Class(
  classname = "Cluster",
  portable = TRUE,                   
  public = list(
    initialize = function(strategy) {
      if (!"StrategyGeneric" %in% class(strategy)) {
        stop("[Cluster][ERROR] strategy parameter must be defined as 'StrategicGeneric' type")
      }
      private$strategy <- strategy
      private$subset <- private$strategy$getSubset()
    },
    getSubset = function() {
      private$subset
    },
    getStrategy = function() {
      private$strategy
    },
    getAllDistribution = function() {
      private$all.distribution
    },
    getBestDistribution = function() {
      private$best.distribution
    },
    execute = function(...) {
      private$strategy$execute(...)
      private$all.distribution <- private$strategy$getAllDistribution()
      private$best.distribution <- private$strategy$getBestDistribution()
    },
    getDistribution = function(cluster = NULL, group = NULL, includeClass = "NONE", ...) {
      private$strategy$getDistribution(cluster = cluster, group = group, includeClass = includeClass, ...)
    },
    createSubset = function(subset, cluster = NULL, ...) {
      private$strategy$createSubset(subset = subset, cluster = cluster, ...)
    },
    plot = function(dir.path = NULL, file.name = NULL, ...) {
      private$strategy$plot(dir.path = dir.path, file.name = file.name, ...)
    }
  ),
  private = list(
    subset = NULL,
    strategy = NULL,
    all.distribution = NULL,
    best.distribution = NULL
  )
)