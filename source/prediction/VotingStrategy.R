VotingStrategy <- R6::R6Class(
  classname = "VotingStrategy",
  portable = TRUE,
  public = list(
    initialize = function() {
      #private$final.pred <- FinalPred$new()
    },
    getVotingSchemes = function() { private$voting.schemes },
    getMetrics = function() { private$metrics },
    execute = function(predictions, ...) {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    getName = function() { class(self)[1] }
  ),
  private = list(
    voting.schemes = NULL,
    metrics = NULL
  )
)