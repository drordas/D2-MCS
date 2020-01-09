MaximizeFN <- R6::R6Class(
  classname = "MaximizeFN",
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
    getFinalPrediction = function(raw.pred, prob.pred, positive.class, negative.class, cutoff = NULL) {
      if (missing(raw.pred) && !is.list(raw.pred)) {
        stop("[", class(self)[1], "][FATAL] raw.pred are incorrect. Must be a 'list' type. Aborting...")
      }
      if (!self$getRequiredMetrics() %in% names(raw.pred)) {
        stop("[", class(self)[1], "][FATAL] raw.pred are incorrect. Must have required metrics. ",
             paste(self$getRequiredMetrics(), collapse = " "), ". Aborting...")
      }

      if (missing(prob.pred) && !is.list(prob.pred)) {
        stop("[", class(self)[1], "][FATAL] prob.pred are incorrect. Must be a 'list' type. Aborting...")
      }
      if (!self$getRequiredMetrics() %in% names(prob.pred)) {
        stop("[", class(self)[1], "][FATAL] prob.pred are incorrect. Must have required metrics. ",
             paste(self$getRequiredMetrics(), collapse = " "), ". Aborting...")
      }

      if (missing(positive.class) && !is.character(positive.class)) {
        stop("[", class(self)[1], "][FATAL] Positive class are incorrect. Must be a 'character' type. Aborting...")
      }
      if (missing(negative.class) && !is.character(negative.class)) {
        stop("[", class(self)[1], "][FATAL] Negative class are incorrect. Must be a 'character' type. Aborting...")
      }

      ifelse(all(raw.pred[self$getRequiredMetrics()] == negative.class),
             FALSE,
             TRUE)
    }
  )
)