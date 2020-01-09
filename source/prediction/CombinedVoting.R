CombinedVoting <- R6::R6Class(
  classname = "CombinedVoting",
  portable = TRUE,
  inherit = VotingStrategy,
  public = list(
    initialize = function(voting.scheme, combined.metrics, methodology, metrics, cutoff = NULL) {
      if (!inherits(voting.scheme, "SimpleVoting")) {
        stop("[", class(self)[1], "][FATAL] Invalid voting.scheme type. Must be a ",
             "SimpleVoting object. Aborting...")
      }
      if (!inherits(combined.metrics, "CombinedMetrics")) {
        stop("[", class(self)[1], "][FATAL] Invalid combined.metrics type. Must be a ",
             "CombinedMetrics object. Aborting...")
      }
      if (!inherits(methodology, "Methodology")) {
        stop("[", class(self)[1], "][FATAL] Invalid methodology type. Must be a ",
             "SimpleVoting object. Aborting...")
      }

      if ( !all(is.character(metrics), length(metrics) >= 2) ) {
        stop("[", class(self)[1], "][FATAL] Invalid values of metrics ")
      }

      if (!is.null(cutoff) && !is.numeric(cutoff)) {
        stop("[", class(self)[1], "][FATAL] Invalid values of cutoff. Aborting...")
      }

      super$initialize()
      private$voting.scheme <- voting.scheme
      private$combined.metrics <- combined.metrics
      private$methodology <- methodology
      private$metrics <- metrics
      private$cutoff <- cutoff
    },
    getVotingScheme = function() { private$voting.scheme },
    getCombinedMetrics = function() { private$combined.metrics },
    getMethodology = function() { private$methodology },
    getMetrics = function() { private$metrics },
    getCutoff = function() { private$cutoff },
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

      if (!all(self$getMetrics() %in% names(predictions))) {
        stop("[", class(self)[1], "][FATAL] predictions are incorrect. ",
             "Must have the required metrics: ",
             paste(self$getMetrics(), collapse = ", "), ". Aborting...")
      }

      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Refresh final predictions.")
      }

      private$final.pred <- list(prob = data.frame(), raw = c())

      predictions <- predictions[self$getMetrics()]

      private$positive.class <- predictions[[1]]$getPositiveClass()
      private$class.values <- predictions[[1]]$getClassValues()
      negative.class <- setdiff(self$getClassValues(),
                                self$getPositiveClass())

      all.raw.pred <- data.frame(matrix(nrow = length(predictions[[1]]$getAll()[[1]]$getPrediction(type = "raw")),
                                        ncol = 0))
      all.prob.pred <- data.frame(matrix(nrow = length(predictions[[1]]$getAll()[[1]]$getPrediction(type = "raw")),
                                         ncol = 0))

      for (pos in seq_len(length(predictions))) {
        metric <- names(predictions)[[pos]]
        predictions.metric <- predictions[[pos]]
        private$voting.scheme$execute(predictions = predictions.metric,
                                      metric = metric,
                                      cutoff = self$getCutoff(), verbose = verbose)
        all.raw.pred <- cbind(all.raw.pred,
                              private$voting.scheme$getPrediction("raw"))
        names(all.raw.pred)[length(all.raw.pred)] <- metric

        clusterPredictions <- sapply(predictions.metric$getAll(), function(x) {
          x$getPrediction(type = "prob")
        })
        names(clusterPredictions) <- rep_len(x = metric,
                                             length(clusterPredictions))
        all.prob.pred <- cbind(all.prob.pred,
                               clusterPredictions)
      }
      final.raw.pred <- c()
      final.prob.pred <- data.frame()

      for (row in seq_len(dim(all.raw.pred)[1])) {
        row.raw.pred <- all.raw.pred[row,]
        row.prob.pred <- all.prob.pred[row,]
        names(row.raw.pred) <- names(all.raw.pred)
        names(row.prob.pred) <- names(all.prob.pred)
        if (self$getCombinedMetrics()$getFinalPrediction(raw.pred = row.raw.pred,
                                                         prob.pred = row.prob.pred,
                                                         positive.class = self$getPositiveClass(),
                                                         negative.class = negative.class,
                                                         cutoff = self$getCutoff())) {
          final.raw.pred <- c(final.raw.pred, self$getPositiveClass())

        } else { final.raw.pred <- c(final.raw.pred, negative.class) }

        prob.pred <- self$getMethodology()$compute(raw.pred = row.raw.pred,
                                                   prob.pred = row.prob.pred,
                                                   positive.class = self$getPositiveClass(),
                                                   negative.class = negative.class,
                                                   cutoff = self$getCutoff())

        final.prob.pred <- rbind(final.prob.pred, data.frame(prob.pred, abs(1 - prob.pred)))
      }

      private$final.pred$raw <- factor(final.raw.pred,
                                       levels = self$getClassValues())
      private$final.pred$raw  <- relevel(self$getFinalPred()$raw,
                                         ref = self$getPositiveClass())

      private$final.pred$raw <- as.data.frame(self$getFinalPred()$raw,
                                              row.names = row.names(final.prob.pred))

      colnames(private$final.pred$raw) <- c("Target Value")

      private$final.pred$prob <- final.prob.pred
      names(private$final.pred$prob) <- c(self$getPositiveClass(), setdiff(self$getClassValues(),
                                                                           self$getPositiveClass()))
    }
  ),
  private = list(
    voting.scheme = NULL,
    combined.metrics = NULL,
    methodology = NULL,
    metrics = NULL,
    cutoff = NULL
  )
)