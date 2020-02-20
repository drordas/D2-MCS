CombinedVoting <- R6::R6Class(
  classname = "CombinedVoting",
  portable = TRUE,
  inherit = VotingStrategy,
  public = list(
    initialize = function(voting.schemes, combined.metrics, methodology, metrics) {
      if (!inherits(voting.schemes, "SimpleVoting")) {
        stop("[", class(self)[1], "][FATAL] Voting.schemes parameter must be ",
             "defined as 'SimpleVoting' type. Aborting...")
      }
      if (!inherits(combined.metrics, "CombinedMetrics")) {
        stop("[", class(self)[1], "][FATAL] Combined.metrics parameter must be ",
             "defined as 'CombinedMetrics' type. Aborting...")
      }
      if (!inherits(methodology, "Methodology")) {
        stop("[", class(self)[1], "][FATAL] Methodology parameter must be ",
             "defined as 'Methodology' type. Aborting...")
      }

      if ( !all(is.character(metrics), length(metrics) >= 2) ) {
        stop("[", class(self)[1], "][FATAL] Invalid values of metrics. Aborting...")
      }

      super$initialize()
      private$voting.schemes <- voting.schemes
      private$combined.metrics <- combined.metrics
      private$methodology <- methodology
      private$metrics <- metrics
      private$final.pred <- FinalPred$new()
    },
    getCombinedMetrics = function() { private$combined.metrics },
    getMethodology = function() { private$methodology },
    getFinalPred = function(type= NULL, target = NULL, filter = NULL) {
      if( any(is.null(type), !(type %in% c("raw","prob")) )){
        private$final.pred
      }else{
        if(!is.logical(filter)){
          message("[", class(self)[1], "][WARNING] Filter parameter must be ",
                  "defined as 'logical' type. Aborting...")
          filter <- FALSE
        }
        class.values <- private$final.pred$getClassValues()

        switch(type,
               "prob" = {
                 if (is.null(target) || !(target %in% class.values )) {
                   message("[", class(self)[1], "][WARNING] Target not ",
                           "specified or invalid. Using '",
                           private$final.pred$getPositiveClass(),
                           "' as default value")
                   target <- private$final.pred$getPositiveClass()
                 }
                 if (filter) {
                   private$final.pred$getProb()[private$final.pred$getRaw() == target,
                                                target, drop = FALSE]
                 } else {
                   private$final.pred$getProb()[, target, drop = FALSE]
                 }
               },
               "raw" = {
                 if (filter) {
                   private$final.pred$getRaw()[private$final.pred$getRaw() == target,
                                               ,drop = FALSE]
                 } else { private$final.pred$getRaw() }
               }
        )
      }
    },
    execute = function(predictions, verbose = FALSE) {

      if ( is.null(predictions) || !is.vector(predictions) ||
           !all(sapply(predictions, function(pred) {
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

      predictions <- predictions[self$getMetrics()]

      positive.class <- predictions[[1]]$getPositiveClass()
      class.values <- predictions[[1]]$getClassValues()
      negative.class <- setdiff(class.values,
                                positive.class)

      all.raw.pred <- data.frame(matrix(nrow = length(predictions[[1]]$getAll()[[1]]$getPrediction(type = "raw")),
                                        ncol = 0))
      all.prob.pred <- data.frame(matrix(nrow = length(predictions[[1]]$getAll()[[1]]$getPrediction(type = "raw")),
                                         ncol = 0))

      for (pos in seq_len(length(predictions))) {
        metric <- names(predictions)[[pos]]
        predictions.metric <- predictions[[pos]]
        private$voting.schemes$execute(predictions = predictions.metric,
                                      verbose = verbose)
        all.raw.pred <- cbind(all.raw.pred,
                              self$getVotingSchemes()$getFinalPred(type = "raw"))
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
                                                         positive.class = positive.class,
                                                         negative.class = negative.class)) {
          final.raw.pred <- c(final.raw.pred, positive.class)

        } else { final.raw.pred <- c(final.raw.pred, negative.class) }

        prob.pred <- self$getMethodology()$compute(raw.pred = row.raw.pred,
                                                   prob.pred = row.prob.pred,
                                                   positive.class = positive.class,
                                                   negative.class = negative.class)

        final.prob.pred <- rbind(final.prob.pred, data.frame(prob.pred, abs(1 - prob.pred)))
      }

      private$final.pred$set( prob = final.prob.pred, raw = final.raw.pred,
                              class.values = class.values,
                              positive.class = positive.class )

      combined.voting <- list(self)
      names(combined.voting) <- class(self$getMethodology())[1]
      combined.voting <- list(combined.voting)
      names(combined.voting) <- as.character(self$getVotingSchemes()$getCutoff())
      combined.voting <- list(combined.voting)
      names(combined.voting) <-  paste0(self$getMetrics(),
                                        collapse = "-")
      combined.voting
    }
  ),
  private = list(
    combined.metrics = NULL,
    methodology = NULL,
    final.pred = NULL
  )
)