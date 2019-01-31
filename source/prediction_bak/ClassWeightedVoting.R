ClassWeightedVoting <- R6Class(
  classname = "ClassWeightedVoting",
  portable = TRUE,
  inherit = VotingScheme,
  public = list(
    initialize = function(majority.class, class.tie = "first", weights = NULL){ 
      private$majority.class <- majority.class
      super$initialize(private$voting.name) 
      private$class.tie <- class.tie
      private$weights <- weights
    },
    execute = function(predictions, weights = NULL){
      if( !"PredictionList" %in% class(predictions) )
        stop("[",super$getName(),"][ERROR] Unsupported format. Parameter should be a PredictionList object\n", sep="")
      
      class.predictions <- predictions$getClassPredictions()
      summary <- table(sapply(class.predictions,class))
      
      if( !is.null(weights) && length(weights) == ncol(class.predictions) )
        private$weights <- weights
      
      if ( is.null(private$weights) || length(private$weights) != ncol(class.predictions) ){
        cat("[",super$getName(),"][INFO] Weight values are missing/incorrect. Assuming default model performance values\n", sep="")
        private$weights <- predictions$getModelPerformances()
      }
      
      class.predictions <- predictions$getClassPredictions()

      if( length(names(summary)) > 1 )
        stop("[",super$getName(),"][ERROR] Different prediction values\n")
      
      if ( !"factor" %in% names(summary) )
        stop("[",super$getName(),"][ERROR] Prediction values must be factor. Aborting\n")
      
      voting.result <- data.frame(character(), stringsAsFactors = FALSE)

      for(row in 1:nrow(class.predictions)){
        prob.values <- unlist(class.predictions[row, ])
        class.values <- table(prob.values)
        result.values <- split(rep(0,length(class.values)),names(class.values))

        for (i in 1:length(prob.values)) 
          result.values[prob.values[i]] <- (result.values[[prob.values[i]]] + private$weights[i])
        
        df <- as.data.frame(result.values)
        final.prediction <- names(df)[which(df == max(df), arr.ind= TRUE)[, "col"]]
        
        if ( length(final.prediction) > 1 ){
          if ( private$majority.class %in% final.prediction )
            voting.result <- rbind(voting.result, private$majority.class, stringsAsFactors=FALSE )
          else voting.result <- rbind(voting.result, names(class.values)[max.col(df, ties.method = private$class.tie )], stringsAsFactors=FALSE )
        }else voting.result <- rbind(voting.result, final.prediction, stringsAsFactors=FALSE )
      }
      ret <- data.frame(class.predictions,voting.result)
      names(ret) <- c( names(class.predictions) , private$voting.name )
      ret
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
    class.tie = NULL,
    majority.class = NULL,
    weights = NULL
  )
)