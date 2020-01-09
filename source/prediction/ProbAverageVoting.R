ProbAverageVoting <- R6::R6Class(
  classname = "ProbAverageVoting",
  portable = TRUE,
  inherit = SimpleVoting,
  public = list(
    initialize = function(metric = NULL, cutoff = NULL, class.tie = "first") {
      if (!is.null(metric) && (!is.character(metric) || length(metric) != 1)) {
        stop("[", class(self)[1], "][FATAL] Invalid values of metric. Aborting...")
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
        stop("[", class(self)[1], "][FATAL] Invalid values of class.tie. Aborting...")
      }
      super$initialize(metric = metric, cutoff = cutoff)
      private$class.tie <- class.tie
    },
    getMajorityClass = function() { private$majority.class },
    getClassTie = function() { private$class.tie },
    execute = function(predictions, metric = NULL, cutoff = NULL, verbose = FALSE){
      if (!inherits(predictions, "ClusterPredictions")) {
        stop("[", class(self)[1], "][FATAL] Invalid prediction type. Must be a ",
             "ClusterPrediction object. Aborting...")
      }

      if (predictions$size() <= 0) {
        stop("[",class(self)[1],"][FATAL] Cluster predictions were not computed",
             "Aborting...")
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

      prob.mean <- rowMeans(prob.pred)
      private$final.pred$prob <- data.frame( prob.mean,(1-prob.mean),
                                             row.names = row.names(prob.pred) )
      names(private$final.pred$prob) <- c(predictions$getPositiveClass(),
                                          setdiff(predictions$getClassValues(),
                                                  predictions$getPositiveClass()))

      prob.raw <- apply(private$final.pred$prob,1,  function(row) {
        #names(private$final.pred$prob)[which.max(rank(row,ties.method = self$getClassTie() )==1)]
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
          #if(row[,max.value[1]]>cutoff){
            if( !( self$getClassTie() %in% names(row)[max.value] )  ){
              message("[",class(self)[1],"][WARNING] Tie class does not match",
                      "Using random tie solver.")
              sample(names(row)[max.value],1)
            }else{
              message("[",class(self)[1],"][INFO] Using '", self$getClassTie(),
                      "' to solve tie.")
              self$getClassTie()
            }
          #}
        }

      } )

      private$final.pred$raw  <- factor(prob.raw,levels = predictions$getClassValues())
      private$final.pred$raw  <- relevel(self$getFinalPred()$raw,
                                         ref = predictions$getPositiveClass())

      private$final.pred$raw <- as.data.frame(self$getFinalPred()$raw,
                                              row.names = row.names(prob.pred))
      colnames(private$final.pred$raw) <- c("Target Value")

      #avg.pred.prob <<- private$final.pred$prob
      #avg.pred.raw <<- private$final.pred$raw
    }
  ),
  private = list(
    final.prediction = NULL,
    class.tie = NULL
  )
)