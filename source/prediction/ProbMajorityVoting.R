library("R6")
ProbMajorityVoting <- R6Class(
  classname = "ProbMajorityVoting",
  portable = TRUE,
  inherit = VotingScheme,
  public = list(
    initialize = function(selected.class){ 
      super$initialize(private$voting.name)
      
      if( is.null(selected.class) || missing(selected.class) )
        stop("[",super$getName(),"][ERRROR] Majority class parameter should be defined")
      
      private$majority.class <- selected.class
    },
    execute = function(predictions){
      if(!"PredictionList" %in% class(predictions))
        stop("[",super$getName(),"][ERROR] Unsupported format. Parameter should be a PredictionList object\n")
      
      all.predictions <- predictions$getProbPredictions()

      summary <- table(sapply(all.predictions,class))
      
      if( length(names(summary)) > 1 )
        stop("[",super$getName(),"][ERROR] Different prediction values\n")
      
      if ( !"numeric" %in% names(summary))
        stop("[",super$getName(),"][ERROR] Prediction values must be factor. Aborting\n")

      if( !private$majority.class %in% names(all.predictions) )
        stop("[",super$getName(),"][ERROR] Selected class not found. Aborting execution\n")
      
      cat("[",super$getName(),"][INFO] Voting strategy selected: '", super$getName() ,"'\n", sep="")
      
      voting.result <- data.frame(numeric(), stringsAsFactors = FALSE)
      
      prob.predictions <- all.predictions[,grepl(private$majority.class,names(all.predictions)) ]
      
      for(row in 1:nrow(prob.predictions)){
        #prob.values <- unlist(prob.predictions[row, names(prob.predictions) %in% super$getPrevalenceClass(),] )
        prob.values <- unlist(prob.predictions[row, ] )
        prod.final <- prod(prob.values, na.rm = TRUE)
        voting.result <- rbind(voting.result, prod.final, stringsAsFactors = FALSE )
      }
      ret <- cbind(prob.predictions,voting.result)
      names(ret) <- c(names(prob.predictions),private$voting.name)
      ret
    }
  ),
  private = list(
    voting.name = "ProbMajorityVoting",
    final.prediction = NULL,
    majority.class = NULL
  )
)