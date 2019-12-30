SimpleVoting <- R6Class(
  classname = "SimpleVoting",
  portable = TRUE,
  inherit = VotingStrategy,
  public = list(
    initialize = function(metric = NULL, cutoff = NULL) {
      if (!is.null(metric) && (!is.character(metric) || length(metric) != 1)) {
        stop("[", class(self)[1], "][INFO] Invalid values of metric. Aborting...")
      }
      if (!is.null(cutoff) && !is.numeric(cutoff)) {
        stop("[", class(self)[1], "][INFO] Invalid values of cutoff. Aborting...")
      }
      super$initialize()
      private$metric <- metric
      private$cutoff <- cutoff

    },
    getMetric = function() { private$metric },
    getCutoff = function() { private$cutoff },
    execute = function(predictions, metric = NULL) {
      stop("[", class(self)[1], "][ERROR] Class is abstract.",
           " Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    metric = NULL,
    cutoff = NULL
  )
)