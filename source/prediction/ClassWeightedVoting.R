ClassWeightedVoting <- R6Class(
  classname = "ClassWeightedVoting",
  portable = TRUE,
  inherit = SimpleVoting,
  public = list(
    initialize = function(metric = NULL, cutoff = NULL, weights = NULL) {
      if (!is.null(metric) && (!is.character(metric) || length(metric) != 1)) {
        stop("[", class(self)[1], "][INFO] Invalid values of metric. Aborting...")
      } else {
        message("[", class(self)[1], "][WARNING] Metric value has not been implemented")
      }

      if (!is.null(cutoff) && !dplyr::between(cutoff, 0, 1)) {
        if (!dplyr::between(cutoff, 0, 1) )
          message("[", class(self)[1], "][WARNING] Cutoff value should be in ",
                  "the interval between 0 and 1. Assuming 0.5.")
          cutoff <- .5
      } else {
        message("[", class(self)[1], "][WARNING] Cut-off method has not been implemented")
      }
      super$initialize(metric = metric, cutoff = cutoff)
      private$weights <- weights
    },
    getWeights = function() { private$weights },
    setWeights = function(weights) {
      if (missing(weights) || is.null(weights)) {
        message("[", super$getName(), "][WARNING] Weights values not changed due",
                "to inconsistency error")
      } else {
        private$weights <- data.frame(matrix(NA, nrow = 1, ncol = 0),
                                      stringsAsFactors = FALSE)
        colNames <- c()
        for (i in 1:length(weights)) {
          private$weights <- cbind(self$getWeights(),
                                   data.frame(as.numeric(weights[i]),
                                              stringsAsFactors = FALSE))
          colNames <- c(colNames, paste0("CLUSTER ", i))
        }
        names(private$weights) <- colNames
      }
    },
    execute = function(predictions, metric = NULL, cutoff = NULL, weights = NULL, verbose = FALSE) {
      if (!inherits(predictions, "ClusterPredictions")) {
        stop("[", class(self)[1], "][ERROR] Invalid prediction type. Must be a ",
             "ClusterPrediction object. Aborting...")
      }

      if (predictions$size() <= 0) {
        stop("[", class(self)[1], "][ERROR] Cluster predictions were not",
             "computed. Aborting...")
      }

      if (missing(metric) || !is.character(metric)) {
        if (is.null(self$getMetric())) {
          stop("[", class(self)[1], "][ERROR] Metric attribute not set or invalid.")
        }
      } else {
        message("[", class(self)[1], "][INFO] Metric attribute set on execute method.",
                " Assigning the value of metric: ", metric)
        private$metric <- metric
      }

      if (missing(cutoff) || !dplyr::between(cutoff, 0, 1)) {
        if (is.null(self$getCutoff())) {
          message("[", class(self)[1], "][WARNING] Cutoff value should be in ",
                  "the interval between 0 and 1. Assuming 0.5.")
          private$cutoff <- .5
        }
      } else {
        if (is.null(self$getCutoff())) {
          message("[", class(self)[1], "][INFO] Cutoff attribute set on execute method.",
                  " Assigning the value of cutoff: ", cutoff)
          private$cutoff <- cutoff
        } else {
          message("[", class(self)[1], "][WARNING] Cutoff has been previously assigned.",
                  " Keeping initial cutoff value: ", self$getCutoff())
        }
      }

      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Refresh final predictions.")
      }
      private$final.pred <- list(prob = data.frame(),
                                 raw = c())

      class.values <- ifelse(is.factor(predictions$getClassValues()),
                             levels(predictions$getClassValues()),
                             predictions$getClassValues())
      binary.pred <- do.call(cbind, lapply(predictions$getAll(), function(x, col.index) {
        pred <- x$getPrediction("raw", predictions$getPositiveClass())
        data.frame(varhandle::to.dummy(pred, predictions$getPositiveClass())[, col.index],
                   row.names = row.names(pred))
      }, col.index = which(class.values == predictions$getPositiveClass())))

      if (is.null(weights) || length(weights) != ncol(binary.pred)) {
        if (isTRUE(verbose)) {
          message( "[", class(self)[1], "][WARNING] Weight values are missing or",
                   " incorrect. Assuming default model performance values" )
        }
        private$weights <- sapply(predictions$getAll(), function(x) {
          x$getModelPerformance()
        })
      } else { private$weights <- weights }

      sum.weights <- sum(self$getWeights())
      weighted.predictions <- as.data.frame(as.matrix(binary.pred) %*% diag(self$getWeights()))
      colnames(weighted.predictions) <- sprintf("[C.%d]", seq(1, ncol(weighted.predictions)))

      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Performing voting with '~",
                paste0(round(self$getWeights(), digits = 4), collapse = ", ~"),
                "' weights and cutoff of ", self$getCutoff())
      }
      private$positive.class <- predictions$getPositiveClass()
      private$class.values <- predictions$getClassValues()

      for (row in 1:nrow(weighted.predictions)) {
        row.sum <- sum(weighted.predictions[row, ] / sum.weights)
        private$final.pred$prob <- rbind(self$getFinalPred()$prob,
                                         data.frame(row.sum, abs(row.sum - 1)))
        if (row.sum > self$getCutoff()) {
          private$final.pred$raw <- c(self$getFinalPred()$raw,
                                      self$getPositiveClass())
        } else {
          private$final.pred$raw <- c(self$getFinalPred()$raw,
                                      setdiff(self$getClassValues(),
                                             self$getPositiveClass()))
        }
      }

      private$final.pred$raw <- factor(self$getFinalPred()$raw,
                                       levels = predictions$getClassValues())
      private$final.pred$raw <- relevel(self$getFinalPred()$raw,
                                        ref = predictions$getPositiveClass())
      private$final.pred$raw <- as.data.frame(self$getFinalPred()$raw,
                                              row.names = row.names(weighted.predictions))
      colnames(private$final.pred$raw) <- c("Target Value")

      names(private$final.pred$prob) <- c(predictions$getPositiveClass(),
                                          setdiff(predictions$getClassValues(),
                                                  predictions$getPositiveClass()))
      private$final.pred$prob <- as.data.frame(self$getFinalPred()$prob,
                                               row.names = row.names(weighted.predictions))
    }
  ),
  private = list(
    weights = NULL
  )
)