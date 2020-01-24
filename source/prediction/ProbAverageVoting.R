ProbAverageVoting <- R6::R6Class(
  classname = "ProbAverageVoting",
  portable = TRUE,
  inherit = SimpleVoting,
  public = list(
    initialize = function(cutoff = 0.5, class.tie = NULL, majority.class=NULL) {
      if (all(!is.null(class.tie),!is.character(class.tie))) {
        stop("[", class(self)[1], "][FATAL] Invalid class tie value. Aborting...")
      }

      super$initialize(cutoff = cutoff)
      private$class.tie <- class.tie
      private$majority.class <- majority.class
    },
    getMajorityClass = function() { private$majority.class },
    getClassTie = function() { private$class.tie },
    execute = function(predictions, verbose = FALSE){
      if (!inherits(predictions, "ClusterPredictions")) {
        stop("[", class(self)[1], "][FATAL] Predictions parameter must be defined ",
             "as 'ClusterPrediction' type. Aborting...")
      }

      if (predictions$size() <= 0) {
        stop("[",class(self)[1],"][FATAL] Cluster predictions were not computed.",
             " Aborting...")
      }

      if( is.null(private$majority.class) ||
          ! (private$majority.class %in% predictions$getClassValues()) ){
        message("[",class(self)[1],"][WARNING] Majority class unset or invalid.",
                " Assuming '",predictions$getPositiveClass(),"' by default")
        private$majority.class <- predictions$getPositiveClass()
      }

      if(any(is.null(private$class.tie),
             !(private$class.tie %in% predictions$getClassValues()))) {
        message("[",class(self)[1],"][INFO] Class tie unset or invalid")
        private$class.tie <- NULL
      }

      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Performing voting using '",
                self$getClassTie(), "' as tie solving")
      }

      prob.pred <- do.call(cbind, lapply(predictions$getAll(), function(x) {
        pred <- x$getPrediction("prob", predictions$getPositiveClass())
        data.frame(pred, row.names = row.names(pred))
      }))

      prob.mean <- rowMeans(prob.pred)
      final.prob <- data.frame( prob.mean,(1-prob.mean),
                                row.names = row.names(prob.pred) )
      names(final.prob) <- c( predictions$getPositiveClass(),
                              setdiff( predictions$getClassValues(),
                                       predictions$getPositiveClass()) )
      final.raw <- c()

      for ( pos in seq_len(nrow(final.prob)) ) {
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

      private$final.pred$set( final.prob, final.raw, predictions$getClassValues(),
                              predictions$getPositiveClass() )
    }
  ),
  private = list(
    final.prediction = NULL,
    majority.class = NULL,
    class.tie = NULL
  )
)