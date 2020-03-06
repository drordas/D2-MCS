MinimizeFN <- R6::R6Class(
  classname = "MinimizeFN",
  portable = TRUE,
  inherit = CombinedMetrics,
  public = list(
    initialize = function(required.metrics = c("MCC", "PPV")) {
      if (any(is.null(required.metrics),
              !is.character(required.metrics),
              length(required.metrics) < 2)) {
        stop("[", class(self)[1], "][FATAL] Invalid values of required.metrics. Aborting...")
      }
      super$initialize(required.metrics = required.metrics)
    },
    getFinalPrediction = function(raw.pred, prob.pred, positive.class, negative.class) {
      if (is.null(raw.pred) || !is.list(raw.pred)) {
        stop("[", class(self)[1], "][FATAL] Raw.pred parameter must be defined ",
             "as 'list' type. Aborting...")
      }
      if (!all(self$getRequiredMetrics() %in% names(raw.pred))) {
        stop("[", class(self)[1], "][FATAL] Raw.pred parameter must have required metrics. ",
             paste(self$getRequiredMetrics(), collapse = " "), ". Aborting...")
      }

      if (is.null(prob.pred) || !is.list(prob.pred)) {
        stop("[", class(self)[1], "][FATAL] Prob.pred parameter must be defined ",
             "as 'list' type. Aborting...")
      }
      if (!all(self$getRequiredMetrics() %in% names(prob.pred))) {
        stop("[", class(self)[1], "][FATAL] Prob.pred parameter must have required metrics. ",
             paste(self$getRequiredMetrics(), collapse = " "), ". Aborting...")
      }

      if (is.null(positive.class) || (!is.character(positive.class) && !is.numeric(positive.class))) {
        stop("[", class(self)[1], "][FATAL] Positive class parameter must be defined. Aborting...")
      }
      if (is.null(negative.class) || (!is.character(negative.class) && !is.numeric(negative.class))) {
        stop("[", class(self)[1], "][FATAL] Negative class parameter must be defined. Aborting...")
      }

      ifelse(all(raw.pred[self$getRequiredMetrics()] == negative.class),
             FALSE,
             TRUE)
    }
  )
)