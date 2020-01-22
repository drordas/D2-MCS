VotingStrategy <- R6::R6Class(
  classname = "VotingStrategy",
  portable = TRUE,
  public = list(
    initialize = function() {
      #private$final.pred <- FinalPred$new()
    },
    getVotingSchemes = function(){
      stop("[", class(self)[1], "][FATAL] Class is abstract.",
           " Method should be implemented in inherited class. Aborting...")
    },
    getMetrics = function() {
      stop("[", class(self)[1], "][FATAL] Class is abstract.",
           " Method should be implemented in inherited class. Aborting...")
    },
    execute = function(predictions, ...) {
      stop("[", class(self)[1], "][FATAL] Class is abstract.",
           " Method should be implemented in inherited class. Aborting...")
    },
    getName = function() { class(self)[1] }#,
  )
)