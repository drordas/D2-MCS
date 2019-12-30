VotingStrategy <- R6Class(
  classname = "VotingStrategy",
  portable = TRUE,
  public = list(
    initialize = function() {
      private$final.pred <- list(prob = data.frame(),
                                 raw = c())
    },
    execute = function(predictions, ...) {
      stop("[", class(self)[1], "][ERROR] Class is abstract.",
           " Method should be defined in inherited class. Aborting...")
    },
    getName = function() { class(self)[1] },
    getPositiveClass = function() { private$positive.class },
    getClassValues = function() { private$class.values },
    getFinalPred = function() { private$final.pred },
    getPrediction = function(type = NULL, target = NULL, filter = FALSE) {
      if (is.null(self$getFinalPred()) || is.null(self$getPositiveClass())) {
        stop("[", class(self)[1], "][ERROR] Predictions not found.",
             "Voting method has not been executed. Aborting...")
      }

      if (is.null(type) || !type %in% c("raw", "prob")) {
        message(yellow(paste0("[", class(self)[1], "][WARNING] Probability type ",
                              "missing or incorrect. Should be 'raw' or 'prob'.",
                              " Assuming 'prob' by default")))
        type <- "prob"
      }

      if (!is.logical(filter)) {
        stop("[", class(self)[1], "][ERROR] Filter is incorrect. Must be a 'logical' type. Aborting...")
      }

      switch(type,
             "prob" = {
               if (is.null(target) || !(target %in% names(self$getFinalPred()$prob))) {
                 message(yellow(paste0("[", class(self)[1], "][WARNING] Target not ",
                                       "specified or invalid. Using '",
                                       names(self$getFinalPred()$prob)[1],
                                       "' as default value")))
                 target <- names(self$getFinalPred()$prob)[1]
               }
               if (filter) {
                 self$getFinalPred()$prob[self$getFinalPred()$raw == target,
                                          target,
                                          drop = FALSE]
               } else {
                 self$getFinalPred()$prob[, target, drop = FALSE]
               }
             },
             "raw" = {
               if (filter) {
                 self$getFinalPred()$raw[self$getFinalPred()$raw == target, ,drop = FALSE]
               } else {
                 self$getFinalPred()$raw
               }
             }
      )
    }
  ),
  private = list(
    positive.class = NULL,
    class.values = NULL,
    final.pred = NULL
  )
)