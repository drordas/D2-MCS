ClassWeightedVoting <- R6Class(
  classname = "ClassWeightedVoting",
  portable = TRUE,
  inherit = VotingScheme,
  public = list(
    initialize = function(class.tie = "first", weights = NULL){ 
      super$initialize(private$voting.name) 
      private$class.tie <- class.tie
      private$weights <- weights
      private$final.pred <- NULL
      private$positive.class <- NULL
      private$class.values <- NULL
    },
    execute = function(predictions, weights = NULL){
      if(!inherits(predictions,"ClusterPredictions")){
        stop("[",class(self)[1],"][ERROR] Invalid prediction type. Must be a ",
                     "ClusterPrediction object. Aborting...")
      }
      if(predictions$size()<=0){
        stop("[",class(self)[1],"][ERROR] Cluster predictions were not executed.",
             "Aborting...")
      }
      
      binary.pred <- sapply(predictions$getAll(),function(x) { 
                              x$getPrediction("bin",predictions$getPositiveClass()) 
                            })

      if ( is.null(private$weights) || length(private$weights) != ncol(binary.pred) ){
        message("[",class(self)[1],"][INFO] Weight values are missing/incorrect.",
                " Assuming default model performance values")
        private$weights <- sapply(predictions$getAll(),function(x) { 
                                    x$getModelPerformance()
                                  })
      }else{ private$weights <- weights }
      
      sum.weights <- sum(private$weights)
      weighted.predictions <- as.data.frame(binary.pred %*% diag(private$weights))
      
      message("[",class(self)[1],"][INFO] Computing '",class(self)[1],"' with '~",
              paste0(round(private$weights, digits=4),collapse=", ~"),"' weights")
      
      private$final.pred <- list(prob=data.frame(),raw=c(),bin=data.frame())
      
      for(row in 1:nrow(weighted.predictions)) { 
        row.sum <- sum(weighted.predictions[row,]/sum.weights)
        private$final.pred$prob <- rbind(private$final.pred$prob,
                                          data.frame(row.sum,abs(row.sum-1)))
        private$final.pred$raw <- if(row.sum>.5) {
                                          c(private$final.pred$raw,
                                            prediction.list$class[1]) }
                                   else { c(private$final.pred$raw,
                                            prediction.list$class[2]) }
        private$final.pred$bin <- if(row.sum>.5) {
                                    rbind(private$final.pred$bin,c(1,0))} 
                                   else { rbind(private$final.pred$bin,c(0,1)) }
      }
      private$positive.class <- predictions$getPositiveClass()
      private$class.values <- predictions$getClassValues()
      df.names <- c(predictions$getPositiveClass(),
                        setdiff(predictions$getClassValues(),
                                predictions$getPositiveClass()))
      names(private$final.pred$prob) <- df.names
      names(private$final.pred$bin) <- df.names
      private$final.pred$raw <- factor(private$final.pred$raw, 
                                       levels= predictions$getClassValues())
    },
    setWeights = function(weights){
      if(missing(weights) || is.null(weights))
        cat("[",super$getName(),"][WARNING] Weights values not changed due to inconsistency error'\n", sep="")
      else{
        private$weights <- data.frame(matrix(NA, nrow=1, ncol=0),stringsAsFactors = FALSE)
        colNames <- c()
        for (i in 1:length(weights) ){
          private$weights <- cbind(private$weights, data.frame(as.numeric(weights[i]), stringsAsFactors = FALSE))
          colNames <- c(colNames,paste0("CLUSTER ",i) )
        }
        names(private$weights) <- colNames
      }
    },
    getPrediction = function(type=NULL, target=NULL){
      if(is.null(private$final.pred) || is.null(private$positive.class)){
        stop("[",class(self)[1],"][ERROR] Predictions not found.",
             "Voting method has not been executed. Aborting...")
      }
      
      if( is.null(type) || !type %in% c("raw","prob","bin") ){
        message(yellow(paste0("[",class(self)[1],"][WARNING] Probability type ",
                              "missing or incorrect. Should be 'raw', 'prob' ",
                              "or 'bin'. Assuming 'prob' by default")))
        type <- "prob"
      }
      switch (type,
              "prob"= {
                if(is.null(target) || !(target %in% names(private$final.pred$prob) ) ){
                  message(yellow(paste0("[",class(self)[1],"][WARNING] Target not ",
                                        "specified or invalid. Using '",
                                        names(private$final.pred$prob)[1],
                                        "' as default")))
                  target <- names(private$final.pred$prob)[1]
                }
                private$final.pred$prob[,target]},
              "bin" = {
                if( is.null(target) || !(target %in% names(private$final.pred$bin) ) )
                {
                  message(yellow(paste0("[",class(self)[1],"][WARNING] Target not ",
                                        "specified or invalid. Using '",
                                        names(private$final.pred$bin)[1],
                                        "' as default value")))
                  target <- names(private$final.pred$bin)[1]
                }
                private$final.pred$bin[,target]
              },
              "raw" = {private$final.pred$raw}
      )
    },
    getWeights = function(){private$weights},
    getPositiveClass = function(){ private$positive.class },
    getClassValues = function() { private$class.values }
  ),
  private = list(
    voting.name = "ClassWeightedVoting",
    positive.class = NULL,
    class.values = NULL,
    final.pred = NULL,
    class.tie = "first",
    majority.class = NULL,
    weights = NULL
  )
)