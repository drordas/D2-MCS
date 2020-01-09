ProbAverageWeightedVoting <- R6::R6Class(
  classname = "ProbAverageWeightedVoting",
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
                  "the interval between 0 and 1")
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
    execute = function(predictions, metric = NULL, cutoff = NULL,
                       weights = NULL, verbose = FALSE ) {
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
                " Assigning metric: '", metric,"'")
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

      if (is.null(weights) || length(weights) != ncol(binary.pred)) {
        if (isTRUE(verbose)) {
          message( "[", class(self)[1], "][WARNING] Weight values are missing or",
                   " incorrect. Assuming default model performance values" )
        }
        private$weights <- sapply(predictions$getAll(), function(x) {
          1+x$getModelPerformance()
        })
      } else { private$weights <- weights }


      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Performing voting using '",
                self$getClassTie(), "' as tie solving")
      }

      private$final.pred <- list(prob = data.frame(), raw = c())
      private$class.values <- predictions$getClassValues()
      private$positive.class <- predictions$getPositiveClass()

      prob.pred <- do.call(cbind, lapply(predictions$getAll(), function(x) {
        pred <- x$getPrediction("prob", predictions$getPositiveClass())
        data.frame(pred, row.names = row.names(pred))
      }))

      pred.weight <- as.data.frame(apply(prob.pred,1,function(row, weights) {
        weighted.mean(row,weights) }, weights=self$getWeights() )
      )


      private$final.pred$prob <- data.frame( pred.weight,(1-pred.weight),
                                             row.names = row.names(prob.pred) )
      names(private$final.pred$prob) <- c(self$getPositiveClass(),
                                          setdiff(self$getClassValues(),
                                                  self$getPositiveClass()))

      prob.raw <- apply(self$getFinalPred()$prob,1,function(row) {
        max.value <- which(row==max(row))

        if(length(max.value)==1){
          if(row[max.value] > private$cutoff){
            names(row)[max.value]
          }else{
            aux <- row[-max.value]
            max.aux <- which(aux==max(aux))
            if(length(max.aux)==1){
              names(row)[aux]
            }else{
              if( !( self$getClassTie() %in% names(row)[max.aux] )  ){
                message("[",class(self)[1],"][WARNING] Tie class does not match",
                        "Using random tie solver.")
                sample(names(row)[aux],1)
              }else{
                message("[",class(self)[1],"][INFO] Using '", self$getClassTie(),
                        "' to solve tie.")
                self$getClassTie()
              }
            }
          }
        }else{
          if( !( self$getClassTie() %in% names(row)[max.value] )  ){
            message("[",class(self)[1],"][WARNING] Tie class does not match",
                    "Using random tie solver.")
            sample(names(row)[max.value],1)
          }else{
            message("[",class(self)[1],"][INFO] Using '", self$getClassTie(),
                    "' to solve tie.")
            self$getClassTie()
          }
        }
      } )

      private$final.pred$raw  <- factor(prob.raw,levels = self$getClassValues())
      private$final.pred$raw  <- relevel(self$getFinalPred()$raw,
                                         ref = self$getPositiveClass())

      private$final.pred$raw <- as.data.frame(self$getFinalPred()$raw,
                                              row.names = row.names(prob.pred))
      colnames(private$final.pred$raw) <- c("Target Value")

      #avgWeight.pred.prob <<- private$final.pred$prob
      #avgWeight.pred.raw <<- private$final.pred$raw
    }
  ),
  private = list(
    final.prediction = NULL,
    weights = NULL
  )
)