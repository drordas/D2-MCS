library("R6")
ClassWeightedVoting <- R6Class(
  classname = "ClassWeightedVoting",
  portable = TRUE,
  inherit = VotingScheme,
  public = list(
    initialize = function(majority.class, class.tie = "first", weights = NULL){ 
      #if( is.null(majority.class) || missing(majority.class) )
      #  stop("[",super$getName(),"][ERRROR] Majority class parameter should be defined")
      
      private$majority.class <- majority.class
      super$initialize(private$voting.name) 
      private$class.tie <- class.tie
      
      #if ( !missing(weights) && !is.null(weights) )
      private$weigths <- weigths
      #else private$weigths <- NULL
    },
    execute = function(predictions){
      if( !"PredictionList" %in% class(predictions) )
        stop("[",super$getName(),"][ERROR] Unsupported format. Parameter should be a PredictionList object\n", sep="")
      
      class.predictions <- predictions$getClassPredictions()
      summary <- table(sapply(class.predictions,class))
      
      if ( is.null(private$weigths) || length(private$weigths) != ncol(class.predictions) ){
        cat("[",super$getName(),"][INFO] Weight values are missing/incorrect. Assuming default model performance values\n", sep="")
        private$weigths <- predictions$getModelPerformances()
      }
      
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
          result.values[prob.values[i]] <- (result.values[[prob.values[i]]] + private$weigths[i])
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
    setWeights = function(weigths){
      if ( !missing(weigths) && !is.null(weigths) )
        private$weigths <- c(private$weigths, weigths)
      else cat("[",super$getName(),"][WARNING] Weights values not changed due to inconsistency error'\n", sep="")
    },
    getWeights = function(){ private$weigths }
  ),
  private = list(
    voting.name = "ClassWeightedVoting",
    class.tie = NULL,
    majority.class = NULL,
    weigths = NULL
  )
)