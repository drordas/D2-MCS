ClassWeightedVoting <- R6Class(
  classname = "ClassWeightedVoting",
  portable = TRUE,
  inherit = VotingScheme,
  public = list(
    initialize = function(class.tie = "first", weights = NULL){ 
      super$initialize(private$voting.name) 
      private$class.tie <- class.tie
      private$weights <- weights
    },
    execute = function(predictions, weights = NULL){
      if( !"PredictionList" %in% class(predictions) )
        stop("[",super$getName(),"][ERROR] Unsupported format. Parameter should be a PredictionList object\n", sep="")
      
      binary.predictions <- as.matrix(predictions$getBinaryPredictions())
      summary <- table(sapply(binary.predictions,class))
      
      if( !is.null(weights) && length(weights) == ncol(binary.predictions) )
        private$weights <- weights
      
      if ( is.null(private$weights) || length(private$weights) != ncol(binary.predictions) ){
        cat("[",super$getName(),"][INFO] Weight values are missing/incorrect. Assuming default model performance values\n", sep="")
        private$weights <- predictions$getModelPerformances()
      }
      
      sum.weights <- sum(private$weights)

      
      if( length(names(summary)) > 1 ) stop("[",super$getName(),"][ERROR] Different prediction values\n")
      
      weighted.predictions <- as.data.frame( binary.predictions %*% diag(as.vector(private$weights)) )
      
      cat("[",super$getName(),"][INFO] Executing '",super$getName(),"' with '~",paste0(round(as.vector(private$weights), digits=4),collapse=", ~"),"' weights\n", sep="")
      
      final.prediction <- as.factor(as.vector(rowSapply(weighted.predictions, function(row){ ifelse(sum(row) / sum(sum.weights)<=.5, 0, 1) }, unlist=TRUE ) ))
      if (length(levels(final.prediction)) < 2 ) levels(final.prediction) <- c(0, 1)
      final.prediction
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
    getWeights = function(){ private$weights }
  ),
  private = list(
    voting.name = "ClassWeightedVoting",
    class.tie = "first",
    majority.class = NULL,
    weights = NULL
  )
)