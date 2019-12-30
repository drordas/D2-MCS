ClassMajorityVoting <- R6::R6Class(
  classname = "ClassMajorityVoting",
  portable = TRUE,
  inherit = SimpleVoting,
  public = list(
    initialize = function(metric = NULL, cutoff = NULL, class.tie = "first") {
      if (!is.null(metric) && (!is.character(metric) || length(metric) != 1)) {
        stop("[", class(self)[1], "][INFO] Invalid values of metric. Aborting...")
      } else {
        message("[", class(self)[1], "][WARNING] Metric value has not been implemented")
      }
      if (!is.null(cutoff) && !dplyr::between(cutoff, 0, 1)) {
        if (!dplyr::between(cutoff, 0, 1) )
          message("[", class(self)[1], "][WARNING] Cutoff value should be in ",
                  "the interval between 0 and 1")
        cutoff <- .5
      } else {
        message("[", class(self)[1], "][WARNING] Cut-off method has not been implemented")
      }
      if (is.null(class.tie) && !is.character(class.tie)) {
        stop("[", class(self)[1], "][INFO] Invalid values of class.tie. Aborting...")
      }
      super$initialize(metric = metric, cutoff = cutoff)
      private$class.tie <- class.tie
      private$majority.class <- NULL
    },
    getMajorityClass = function() { private$majority.class },
    getClassTie = function() { private$class.tie },
    execute = function(predictions, metric = NULL, cutoff = NULL, majority.class = NULL, verbose = FALSE){
      if (!inherits(predictions, "ClusterPredictions")) {
        stop("[", class(self)[1], "][ERROR] Invalid prediction type. Must be a ",
             "ClusterPrediction object. Aborting...")
      }

      if (predictions$size() <= 0) {
        stop("[",class(self)[1],"][ERROR] Cluster predictions were not computed",
             "Aborting...")
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

      private$majority.class <- predictions$getPositiveClass()
      if (is.null(majority.class) || !(majority.class %in% predictions$getClassValues())) {
        if (isTRUE(verbose)) {
          message("[", class(self)[1], "][WARNING] Majority class not set or invalid.",
                  " Assuming default value: ", predictions$getPositiveClass())
        }
      } else { private$majority.class <- majority.class }

      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Performing voting using '",
                self$getMajorityClass(), "' as majority class")
      }

      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Refresh final predictions.")
      }
      private$final.pred <- list(prob = data.frame(),
                                 raw = c())
      raw.pred <- do.call(cbind, lapply(predictions$getAll(),function(x) {
        pred <- x$getPrediction("raw")
        data.frame(pred, row.names = row.names(pred) )
      }))

      prob.pred <- do.call(cbind, lapply(predictions$getAll(), function(x) {
        pred <- x$getPrediction("prob", predictions$getPositiveClass())
        data.frame(pred, row.names = row.names(pred))
      }))

      private$class.values <- predictions$getClassValues()
      private$positive.class <- predictions$getPositiveClass()

      for (row in 1:nrow(prob.pred)) {
        row.summary <- table(as.matrix(raw.pred[row, ]))
        max.values <- names(which(row.summary == max(row.summary)))

        if (length(max.values) > 1) {
          if (self$getMajorityClass() %in% max.values) {
            message("[", class(self)[1], "][INFO] Found Tie. Resolving using ",
                    "'majority class' solver")
            entry <- self$getMajorityClass()
            private$final.pred$raw <- c(self$getFinalPred()$raw, entry)
          } else {
            message("[", class(self)[1], "][INFO] Found Tie. Resolving using '",
                    self$getClassTie(), "' tie solver")
            entry <- which.max(rank(x = row.summary, ties.method = self$getClassTie()))
            private$final.pred$raw <- c(self$getFinalPred()$raw, entry)
          }
        } else {
          entry <- max.values
          private$final.pred$raw <- c(self$getFinalPred()$raw, entry)
        }

        mean.row <- rowMeans(prob.pred[row, ])
        private$final.pred$prob <- rbind(self$getFinalPred()$prob,
                                         data.frame(mean.row, abs(mean.row - 1)))
      }
      private$final.pred$raw <- factor(self$getFinalPred()$raw,
                                       levels = predictions$getClassValues())

      private$final.pred$raw  <- relevel(self$getFinalPred()$raw,
                                         ref = predictions$getPositiveClass())
      private$final.pred$raw <- as.data.frame(self$getFinalPred()$raw,
                                              row.names = row.names(raw.pred))
      colnames(private$final.pred$raw) <- c("Target Value")

      names(private$final.pred$prob) <- c(predictions$getPositiveClass(),
                                          setdiff(predictions$getClassValues(),
                                                  predictions$getPositiveClass()))
      private$final.pred$prob <- as.data.frame(self$getFinalPred()$prob,
                                               row.names = row.names(prob.pred))
    }
  ),
  private = list(
    class.tie = NULL,
    majority.class = NULL
  )
)