ClassWeightedVoting <- R6::R6Class(
  classname = "ClassWeightedVoting",
  portable = TRUE,
  inherit = SimpleVoting,
  public = list(
    initialize = function(metric = NULL, cutoff = NULL, weights = NULL) {
      if (!is.null(metric) && (!is.character(metric) || length(metric) != 1)) {
        stop("[", class(self)[1], "][FATAL] Invalid values of metric. Aborting...")
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
    execute = function(predictions, metric = NULL, cutoff = NULL, weights = NULL,
                       verbose = FALSE) {
      if (!inherits(predictions, "ClusterPredictions")) {
        stop("[", class(self)[1], "][FATAL] Invalid prediction type. Must be a ",
             "ClusterPrediction object. Aborting...")
      }

      if (predictions$size() <= 0) {
        stop("[", class(self)[1], "][FATAL] Cluster predictions were not ",
             "computed. Aborting...")
      }

      if (missing(metric) || !is.character(metric)) {
        if (is.null(self$getMetric())) {
          stop("[", class(self)[1], "][FATAL] Metric attribute not set or invalid.")
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
        message("[", class(self)[1], "][INFO] Performing voting with '~",
                paste0(round(self$getWeights(), digits = 4), collapse = ", ~"),
                "' weights and cutoff of ", self$getCutoff())
      }


      if (is.null(weights) || length(weights) != ncol(binary.pred)) {
        if (isTRUE(verbose)) {
          message( "[", class(self)[1], "][WARNING] Weight values are missing or",
                   " incorrect. Assuming default model performance values" )
        }
        private$weights <- sapply(predictions$getAll(), function(x) {
          x$getModelPerformance()
        })
      } else { private$weights <- weights }

      private$final.pred <- list(prob = data.frame(), raw = c() )
      private$positive.class <- predictions$getPositiveClass()
      private$class.values <- predictions$getClassValues()

      class.values <- ifelse(is.factor(predictions$getClassValues()),
                             levels(predictions$getClassValues()),
                             predictions$getClassValues())

      raw.pred <- do.call(cbind, lapply(predictions$getAll(), function(x, col.index) {
        x$getPrediction("raw", predictions$getPositiveClass())
      }))

      prob.pred <- do.call(cbind, lapply(predictions$getAll(), function(x, col.index) {
        x$getPrediction("prob", predictions$getPositiveClass())
      }))

      for(row in seq_len(nrow(raw.pred))){
        values <- unique( factor(as.matrix(raw.pred[row,]),
                          levels = predictions$getClassValues()))

        row.sum <- c()
        for(val in values){
          row.sum <- c(row.sum,sum(self$getWeights()[which(raw.pred[row,]==val)]))
        }

        names(row.sum) <- values
        winner.class <- names(row.sum)[which(row.sum==max(row.sum))]

        if(length(winner.class)!=1){
          print(winner.class)
          print(names(row.sum))
          stop("[",class(self)[1],"][FATAL] Tie found. Untied method under ",
               "development")
        }else{
          winner.prob <- weighted.mean(prob.pred[row,which(raw.pred[row,]==winner.class)],
                                       self$getWeights()[which(raw.pred[row,]==winner.class)])
          private$final.pred$prob <- rbind(private$final.pred$prob,
                                           data.frame(winner.prob,1-winner.prob))
          private$final.pred$raw <- c(private$final.pred$raw,winner.class)
        }
      }

      names(private$final.pred$prob) <- c(private$positive.class,
                                          setdiff(private$class.values,
                                                  private$positive.class))
      row.names(private$final.pred$prob) <- row.names(prob.pred)

      private$final.pred$raw <- factor(self$getFinalPred()$raw,
                                       levels = private$class.values)

      private$final.pred$raw  <- relevel(self$getFinalPred()$raw,
                                         ref = private$positive.class)

      private$final.pred$raw <- as.data.frame(self$getFinalPred()$raw,
                                              row.names = row.names(prob.pred))
      colnames(private$final.pred$raw) <- c("Target Value")


      #classWeight.pred.prob <<- private$final.pred$prob
      #classWeight.pred.raw <<- private$final.pred$raw
    }
  ),
  private = list(
    weights = NULL
  )
)