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
      #voting <- private$voting.scheme

      if (missing(voting.scheme) && is.null(private$voting.scheme))
        stop("[ComputePrediction][ERROR] Voting Scheme not defined\n")
      
      if(!missing(voting.scheme) && "VotingScheme" %in% class(voting.scheme))
        private$voting.scheme <- voting.scheme
        #voting <- voting.scheme
      
      private$final.prediction <- private$voting.scheme$execute(private$predictions)
    },
    computePerformance = function(new.data){
      if( is.null(private$final.prediction) || !is.data.frame(private$final.prediction) )
        stop("[ComputePrediction][ERROR] Voting scheme should be executed first to obtain predicion results\n")
      
      if (missing(new.data) || is.null(new.data) || !"Subset" %in% class(new.data) ) 
        stop("[ComputePrediction][ERROR] Test data not defined or incorrect object type (Subset class)\n")
      
      if(new.data$getNrow() != nrow(private$final.prediction) )
        stop("[ComputePrediction][ERROR] Distinct dimension of predicted and real values\n")
      
      real.values <- new.data$getClass()
      pred.values <- private$final.prediction[,ncol(private$final.prediction)]
      
      if( !"factor" %in% class(pred.values) ) 
        pred.values <- as.factor(pred.values)
      
      cf <- caret::confusionMatrix(real.values,pred.values, positive=private$voting.scheme$getPrevalenceClass(), mode="everything")
      mcc <- mltools::mcc(FP= cf$table[1,2], TP = cf$table[1,1], TN = cf$table[2,2], FN = cf$table[2,1]  )
      PerformanceMeasures$new(cf=cf,mcc= mcc)
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