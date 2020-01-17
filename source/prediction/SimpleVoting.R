SimpleVoting <- R6::R6Class(
  classname = "SimpleVoting",
  portable = TRUE,
  inherit = VotingStrategy,
  public = list(
    initialize = function(cutoff = NULL) {
      if (!is.null(cutoff) && !is.numeric(cutoff)) {
        stop("[", class(self)[1], "][FATAL] Invalid values of cutoff. Aborting...")
      }
      super$initialize()
      private$cutoff <- cutoff
    },
    getCutoff = function() { private$cutoff },
    execute = function(predictions, metric = NULL) {
      stop("[", class(self)[1], "][FATAL] Class is abstract.",
           " Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list( cutoff = NULL )
)