ClassWeightedVoting <- R6::R6Class(
  classname = "ClassWeightedVoting",
  portable = TRUE,
  inherit = SimpleVoting,
  public = list(
    initialize = function(cutoff = 0.5, weights = NULL) {
      super$initialize(cutoff = cutoff)
      private$weights <- weights
    },
    getWeights = function() { private$weights },
    setWeights = function(weights) {
      if (missing(weights) || is.null(weights)) {
        message("[", super$getName(), "][WARNING] Weights values not changed due ",
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
    execute = function(predictions, verbose = FALSE ) {
      if (!inherits(predictions, "ClusterPredictions")) {
        stop("[", class(self)[1], "][FATAL] Predictions parameter must be defined ",
             "as 'ClusterPrediction' type. Aborting...")
      }

      if (predictions$size() <= 0) {
        stop("[", class(self)[1], "][FATAL] Cluster predictions were not ",
             "computed. Aborting...")
      }

      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Performing voting with '~",
                paste0(round(self$getWeights(), digits = 4), collapse = ", ~"),
                "' weights and cutoff of ", self$getCutoff())
      }

      if ( any( is.null(private$weights),
                length(private$weights) != predictions$size() ) )
      {
        if (isTRUE(verbose)) {
          message( "[", class(self)[1], "][WARNING] Weight values are missing or ",
                   "incorrect. Assuming default model performance values" )
        }
        private$weights <- sapply(predictions$getAll(), function(x) {
          x$getModelPerformance()
        })
      }

      final.raw <- c()
      final.prob <- data.frame()

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
          stop("[",class(self)[1],"][FATAL] Tie found. Untied method under ",
               "development")
        }else{
          winner.prob <- weighted.mean(prob.pred[row,which(raw.pred[row,]==winner.class)],
                                       self$getWeights()[which(raw.pred[row,]==winner.class)])
          final.prob <- rbind(final.prob, data.frame(winner.prob,1-winner.prob))

          if ( identical(winner.class,predictions$getPositiveClass()) &&
               winner.prob < self$getCutoff() ){
            winner.class <- setdiff(predictions$getClassValues(),
                                    predictions$getPositiveClass())
          }
          final.raw <- c(final.raw,winner.class)
        }
      }

      private$final.pred$set( final.prob, final.raw,
                              predictions$getClassValues(),
                              predictions$getPositiveClass() )
    }
  ),
  private = list(
    weights = NULL
  )
)