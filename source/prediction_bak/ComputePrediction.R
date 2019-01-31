library("R6")
PredictionHandler <- R6Class(
  classname = "PredictionHandler",
  portable = TRUE,                   
  public = list(
    initialize = function(predictions, voting.scheme){
      
      if( "D2MCS" %in% class(predictions) )
        private$predictions <- predictions$getPredictions()
      else if( "PredictionList" %in% class(predictions) )
        private$predictions <- predictions
      else  stop("[ComputePrediction][ERROR] Parameter result should be defined as 'PredictionList' Class. Aborting execution")
      
      if (!missing(voting.scheme) && "VotingScheme" %in% class(voting.scheme) )
        private$voting.scheme <- voting.scheme
      
      private$final.prediction <- NULL
    },
    executeVoting = function(voting.scheme){
      voting <- private$voting.scheme
      #result <- data.frame(matrix(NA, nrow=nrow(private$predictions), ncol=1), stringsAsFactors = FALSE)
      if (missing(voting.scheme) && is.null(private$voting.scheme))
        stop("[ComputePrediction][ERROR] Voting Scheme not defined\n")
      
      if(!missing(voting.scheme) && "VotingScheme" %in% class(voting.scheme))
        voting <- voting.scheme
      
      private$final.prediction <- voting$execute(private$predictions)
    },
    computePerformance = function(){
      if( is.null(private$final.prediction) || !is.data.frame(private$final.prediction) )
        stop("[ComputePrediction][ERROR] Voting scheme should be executed first to obtain predicion results\n")
      
      
    },
    getFinalPrediction = function(option= "all"){
      if (!option %in% c("prediction","all","final") )
        cat("[ComputePrediction][WARNING] Incorrect option ('prediction','final','all'). Assuming 'all' as default value\n", sep="")
      
      private$final.prediction
    },
    save = function(path, option = "all"){
      if ( is.null(private$final.prediction) )
        stop("[ComputePrediction][WARNING] Final prediction is not computed. Aborting ...\n")
      
      if (!option %in% c("prediction","all","final") )
        cat("[ComputePrediction][WARNING] Incorrect option ('prediction','final','all'). Assuming 'all' as default value\n", sep="")
      
      all <- cbind( private$predictions$getPredictions(), private$final.prediction, stringsAsFactors = FALSE )
      names(all)[length(names(all))] <- private$name
      write.csv(all,file=path, append = FALSE, sep = ",")
    }
  ),
  private = list(
    predictions = NULL,
    final.prediction = NULL,
    voting.scheme = NULL
  )
)