SingleVoting <- R6::R6Class(
  classname = "SingleVoting",
  portable = TRUE,
  inherit = VotingStrategy,
  public = list(
    initialize = function(voting.schemes, metrics) {
      if (!all(sapply(voting.schemes, function(voting) {
                inherits(voting, "SimpleVoting")
          }))) {
        stop("[", class(self)[1], "][FATAL] Voting schemes parameter must be a ",
             "list comprised of 'SimpleVoting' objects. Aborting...")
      }

      if ( !all(is.character(metrics)) ) {
        stop("[", class(self)[1], "][FATAL] Metrics parameter must be a list ",
             "comprised of 'character' objects. Aborting... ")
      }

      super$initialize()
      private$voting.schemes <- voting.schemes
      private$metrics <- metrics
    },
    execute = function(predictions, verbose = FALSE) {

      if ( !all(sapply(predictions, function(pred) {
        !inherits(pred, "ClusterPrediction") } )) ) {
        stop("[", class(self)[1], "][FATAL] Predictions parameter must be a ",
             "list comprised of 'ClusterPrediction' objects. Aborting...")
      }

      if ( any(sapply(predictions, function(pred) { pred$size() <= 0 } ))) {
        stop("[", class(self)[1], "][FATAL] Cluster predictions were not ",
             "computed. Aborting...")
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