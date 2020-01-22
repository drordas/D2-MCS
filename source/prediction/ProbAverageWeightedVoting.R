ProbAverageWeightedVoting <- R6::R6Class(
  classname = "ProbAverageWeightedVoting",
  portable = TRUE,
  inherit = SimpleVoting,
  public = list(
    initialize = function(cutoff = NULL, weights = NULL) {
      if (!is.null(cutoff) && !dplyr::between(cutoff, 0, 1)) {
        if (!dplyr::between(cutoff, 0, 1) )
          message("[", class(self)[1], "][WARNING] Cutoff value should be in ",
                  "the interval between 0 and 1")
        cutoff <- 0.5
      } else {
        message("[", class(self)[1], "][WARNING] Cutoff method has not been implemented")
      }

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
        stop("[", class(self)[1], "][FATAL] Predictions parameter must be defined",
             "as 'ClusterPrediction' type. Aborting...")
      }

      if (predictions$size() <= 0) {
        stop("[",class(self)[1],"][FATAL] Cluster predictions were not computed. ",
             "Aborting...")
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


      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Performing voting using '",
                self$getClassTie(), "' as tie solving")
      }

      final.prob <- data.frame()
      final.raw <- c()

      prob.pred <- do.call(cbind, lapply(predictions$getAll(), function(x) {
        pred <- x$getPrediction("prob", predictions$getPositiveClass())
        data.frame(pred, row.names = row.names(pred))
      }))

      pred.weight <- as.data.frame(apply(prob.pred,1,function(row, weights) {
        weighted.mean(row,weights) }, weights=self$getWeights() )
      )


      final.prob <- data.frame( pred.weight,(1-pred.weight),
                                row.names = row.names(prob.pred) )

      names(final.prob) <- c( predictions$getPositiveClass(),
                              setdiff( predictions$getClassValues(),
                                       predictions$getPositiveClass()) )

      final.raw <- c()

      for (pos in seq_len(nrow(final.prob))){
        row <- final.prob[pos, ]
        max.col <- which(row==max(row))
        if( length(max.col) == 1 ){
          max.value <- names(row)[max.col]
          if( identical(max.value,predictions$getPositiveClass()) &&
              row[max.col] < self$getCutoff()){
            entry <- setdiff( predictions$getClassValues(),
                              predictions$getPositiveClass() )
          }else { entry <- names(row)[max.col] }
        }else{
          max.values <- names(row)[max.col]
          if( is.null(self$getClassTie()) ||
              !(self$getClassTie() %in% max.values) ){
            message("[", class(self)[1], "][INFO] Tie solver not found. ",
                    "Resolving tie using first occurrence.")
            entry <- max.values[1]
          }else{
            message("[", class(self)[1], "][INFO] Tie solver found. ",
                    "Resolving tie using '",self$getClassTie(),"'.")
            entry <- self$getClassTie()
          }
        }
        final.raw <- c(final.raw, entry)
      }

      private$final.pred$set( final.prob, final.raw,
                              predictions$getClassValues(),
                              predictions$getPositiveClass() )

      print(private$final.pred$getRaw()[1:15])
    }
  ),
  private = list(
    weights = NULL
  )
)