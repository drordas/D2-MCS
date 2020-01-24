ProbBasedMethodology <- R6::R6Class(
  classname = "ProbBasedMethodology",
  portable = TRUE,
  inherit = Methodology,
  public = list(
    initialize = function(required.metrics = c("MCC", "PPV")) {
      if (any(is.null(required.metrics),
              !is.character(required.metrics),
              length(required.metrics) < 2)) {
        stop("[", class(self)[1], "][FATAL] Invalid values of required.metrics. Aborting...")
      }
      super$initialize(required.metrics = required.metrics)
    },
    compute = function(raw.pred, prob.pred, positive.class, negative.class) {
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

      if (is.null(positive.class) || !is.character(positive.class)) {
        stop("[", class(self)[1], "][FATAL] Positive class parameter must be defined ",
             "as 'character' type. Aborting...")
      }
      if (is.null(negative.class) || !is.character(negative.class)) {
        stop("[", class(self)[1], "][FATAL] Negative class parameter must be defined ",
             "as 'character' type. Aborting...")
      }

      prod(prob.pred[, which(names(prob.pred) %in% self$getRequiredMetrics())])
    }
  )
)