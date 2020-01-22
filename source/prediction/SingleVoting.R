SingleVoting <- R6::R6Class(
  classname = "SingleVoting",
  portable = TRUE,
  inherit = VotingStrategy,
  public = list(
    initialize = function(voting.schemes, metrics) {
      if (!all(sapply(voting.schemes, function(voting) {
                inherits(voting, "SimpleVoting")
          })))
      {
        stop("[", class(self)[1], "][FATAL] Invalid voting scheme type. Must be a ",
             "SimpleVoting object. Aborting...")
      }

      if ( !all(is.character(metrics)) ) {
        stop("[", class(self)[1], "][FATAL] Invalid metric values. ",
             "Aborting execution... ")
      }

      super$initialize()
      private$voting.schemes <- voting.schemes
      private$metrics <- metrics
    },
    getVotingSchemes = function() { private$voting.schemes },
    getMetrics = function() { private$metrics },
    execute = function(predictions, verbose = FALSE) {

      if ( !all(sapply(predictions, function(pred) {
        !inherits(pred, "ClusterPrediction") } )) )
      {
        stop("[", class(self)[1], "][FATAL] Invalid prediction type. Must be a ",
             "ClusterPrediction object. Aborting...")
      }

      if ( any(sapply(predictions, function(pred) { pred$size() <= 0 } ))) {
        stop("[", class(self)[1], "][FATAL] Cluster predictions were not",
             " computed. Aborting...")
      }

      if (!any(self$getMetrics() %in% names(predictions))) {
        stop("[", class(self)[1], "][FATAL] metrics are incorrect. ",
             "Must be: [",paste(names(predictions), collapse = ", "),
             "]. Aborting...")
      }
      single.votings <- list()
      for (preds in seq_len(length(predictions))){
        metric <- names(predictions)[preds]
        votings.list <- list()
        for(voting.scheme in private$voting.schemes){
          voting.name <- class(voting.scheme)[1]
          message("[", class(self)[1], "][INFO] ------------------------------",
                  "-------------------------")
          message("[", class(self)[1], "][INFO] Executing '",voting.name,
                  "' for '",metric,"' metric with '",
                  voting.scheme$getCutoff(),"' cutoff")
          message("[", class(self)[1], "][INFO] ------------------------------",
                  "-------------------------")
          voting.scheme$execute(predictions[[preds]])
          list.element <- list(voting.scheme)
          names(list.element) <- paste0(voting.name)
          votings.list[[as.character(voting.scheme$getCutoff())]] <- list.element
        }
        single.votings[[metric]] <- votings.list
      }
      single.votings
    }
  ),
  private = list(
    voting.schemes = NULL,
    metrics = NULL
  )
)