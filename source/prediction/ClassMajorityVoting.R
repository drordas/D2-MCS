ClassMajorityVoting <- R6::R6Class(
  classname = "ClassMajorityVoting",
  portable = TRUE,
  inherit = VotingScheme,
  public = list(
    initialize = function(class.tie = "first", cutoff=.5){
      super$initialize()
      private$class.tie <- class.tie
      private$final.pred <- list(prob=data.frame(),raw=c())
      private$positive.class <- NULL
      private$class.values <- NULL
      private$weights <- NULL
      if( is.null(cutoff) || !dplyr::between(cutoff,0,1) ){
        if (!dplyr::between(cutoff,0,1) )
          message("[",class(self)[1],"][WARNING] Cutoff value is not valid.",
                  "Must be a not-null in the interval between 0 and 1")
        private$cutoff <- .5
      }else{ private$cutoff <- cutoff }
      message("[",class(self)[1],"][INFO] Cut-off method has not been implemented")
    },
    execute = function(predictions, majority.class=NULL, cutoff=.5, verbose=FALSE){
      if(!inherits(predictions,"ClusterPredictions")){
        stop("[",class(self)[1],"][ERROR] Invalid prediction type. Must be a ",
             "ClusterPrediction object. Aborting...")
      }

      if(predictions$size()<=0){
        stop("[",class(self)[1],"][ERROR] Cluster predictions were not computed",
             "Aborting...")
      }

      private$majority.class <- predictions$getPositiveClass()
      if( is.null(majority.class) || !(majority.class %in% predictions$getClassValues()) ){
        if(isTRUE(verbose)){
          message("[",class(self)[1],"][WARNING] Majority class not set of invalid.",
                  " Assuming default value: ",predictions$getPositiveClass())
        }
      }else private$majority.class <- majority.class

      if( is.null(cutoff) || !between(cutoff,0,1) ){
        message("[",class(self)[1],"][WARNING] Cutoff value is not valid.",
             "Must be a not-null in the interval between 0 and 1")
      }else { private$cutoff <- cutoff }

      if(isTRUE(verbose)){
        message("[",class(self)[1],"][INFO] Performing voting using '",
                private$majority.class,"' as majority class")
      }

      raw.pred <- do.call(cbind,lapply(predictions$getAll(),function(x) {
        pred <- x$getPrediction("raw")
        data.frame(pred, row.names = row.names(pred) )
      }))

      prob.pred <- do.call(cbind,lapply(predictions$getAll(),function(x) {
        pred <- x$getPrediction("prob",predictions$getPositiveClass())
        data.frame(pred,row.names = row.names(pred))
      }))

      private$class.values <- predictions$getClassValues()
      private$positive.class <- predictions$getPositiveClass()
      #negative.class <- setdiff( private$class.values, private$positive.class )

      for (row in 1:nrow(raw.pred)) {
        row.summary <- table(as.matrix(raw.pred[row, ]))
        max.values <- names(which(row.summary == max(row.summary)))

        if ( length(max.values) > 1 ){
          if ( private$majority.class %in% max.values ){
            message("[",class(self)[1],"][INFO] Found Tie. Resolving using ",
                    "'majority class' solver")
            entry <- private$majority.class
            private$final.pred$raw <- c(private$final.pred$raw, entry)
          }else{
            message("[",class(self)[1],"][INFO] Found Tie. Resolving using '",
                    private$class.tie,"' tie solver")
            entry <- which.max(rank(x=row.summary, ties.method=private$class.tie))
            private$final.pred$raw <- c(private$final.pred$raw, entry)
          }
        }else{
          entry <- max.values
          private$final.pred$raw <- c(private$final.pred$raw,entry)
        }

        mean.row <- rowMeans(prob.pred[row, ])
        private$final.pred$prob <- rbind(private$final.pred$prob,
                                         data.frame(mean.row,abs(mean.row-1)))
        # if( entry %in% predictions$getPositiveClass() ){
        #   up.index <- which(prob.row > private$cutoff )
        #   neg.index <- which(prob.row <= private$cutoff )
        #
        #   row.result <- ifelse( (length(up.index) > 0),
        #                        prob.row[up.index]/length(up.index),
        #                        prob.row[neg.index]/length(neg.index) )
        #
        #   private$final.pred$prob <- rbind(private$final.pred$prob,
        #                                    data.frame(row.result,abs(row.result-1)))
        #
        # }else{ private$final.pred$prob <- rbind(private$final.pred$prob,c(0,1)) }
      }

      private$final.pred$raw <- factor(private$final.pred$raw,
                                       levels= predictions$getClassValues())
      relevel(private$final.pred$raw,ref = predictions$getPositiveClass())
      private$final.pred$raw <- as.data.frame(private$final.pred$raw,
                                              row.names=row.names(raw.pred))

      df.names <- c(predictions$getPositiveClass(),
                    setdiff(predictions$getClassValues(),
                            predictions$getPositiveClass()))
      names(private$final.pred$prob) <- df.names

      private$final.pred$prob <- as.data.frame(private$final.pred$prob,
                                              row.names=row.names(prob.pred))
      colnames(private$final.pred$raw) <- c("Target Value")

    },
    getPrediction = function(type=NULL, target=NULL){
      if(is.null(private$final.pred) || is.null(private$positive.class)){
        stop("[",class(self)[1],"][ERROR] Predictions not found.",
             "Voting method has not been executed. Aborting...")
      }

      if( is.null(type) || !type %in% c("raw","prob") ){
        message(yellow(paste0("[",class(self)[1],"][WARNING] Probability type ",
                              "missing or incorrect. Should be 'raw' or 'prob'.",
                              " Assuming 'prob' by default")))
        type <- "prob"
      }

      switch (type,
              "prob"= {
                if(is.null(target) || !(target %in% names(private$final.pred$prob) ) ){
                  message(yellow(paste0("[",class(self)[1],"][WARNING] Target not ",
                                        "specified or invalid. Using '",
                                        names(private$final.pred$prob)[1],"' as default value")))
                  target <- names(private$final.pred$prob)[1]
                }
                private$final.pred$prob[,target, drop=FALSE]
              },
              "raw" = {private$final.pred$raw}
      )
    },
    getPositiveClass = function(){ private$positive.class },
    getClassValues = function() { private$class.values },
    getCutoff = function() {private$cutoff},
    getWeights = function(){private$weights}
  ),
  private = list(
    positive.class = NULL,
    class.values = NULL,
    final.pred = NULL,
    class.tie = "first",
    majority.class = NULL,
    weights = NULL,
    cutoff = NULL
  )
)