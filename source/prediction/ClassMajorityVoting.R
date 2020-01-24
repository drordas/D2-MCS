ClassMajorityVoting <- R6::R6Class(
  classname = "ClassMajorityVoting",
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
        stop("[",class(self)[1],"][FATAL] Cluster predictions were not computed. ",
             "Aborting...")
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
                self$getMajorityClass(), "' as majority class")
      }

      final.raw <- c()
      final.prob <- data.frame()

      raw.pred <- do.call(cbind, lapply(predictions$getAll(), function(x) {
        pred <- x$getPrediction("raw")
        data.frame(pred, row.names = row.names(pred) )
      }))

      prob.pred <- do.call(cbind, lapply(predictions$getAll(), function(x) {
        pred <- x$getPrediction("prob", predictions$getPositiveClass())
        data.frame(pred, row.names = row.names(pred))
      }))
      for (row in 1:nrow(prob.pred)) {
        row.summary <- table(as.matrix(raw.pred[row, ]))
        max.values <- names(which(row.summary == max(row.summary)))

        if (length(max.values) > 1) {
          if (self$getMajorityClass() %in% max.values) {
            message("[", class(self)[1], "][INFO] Found Tie. Resolving using",
                    " 'majority class' solver")
            entry <- self$getMajorityClass()
          } else {
            entry <- self$getClassTie()
            if( any(is.null(self$getClassTie()),
                    !(self$getClassTie() %in% max.values))){
              message("[", class(self)[1], "][INFO] Resolving tie using first",
                      "occurrence.")
              entry <- max.values[1]
            }
          }
        } else { entry <- max.values }

        mean.row <- rowMeans(prob.pred[row, which(raw.pred[row,]==entry)])
        if( identical(entry,predictions$getPositiveClass()) &&
            mean.row < self$getCutoff() )
        {
          entry <- setdiff(predictions$getClassValues(), predictions$getPositiveClass())
        }

        final.prob <- rbind(final.prob,data.frame(mean.row, abs(mean.row - 1)))
        final.raw <- c(final.raw, entry)
      }

      private$final.pred$set( final.prob, final.raw,
                              predictions$getClassValues(),
                              predictions$getPositiveClass() )
    }
  ),
  private = list(
    class.tie = NULL,
    majority.class = NULL
  )
)