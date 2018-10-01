library("R6")
ClassMajorityVoting <- R6Class(
  classname = "ClassMajorityVoting",
  portable = TRUE,
  inherit = VotingScheme,
  public = list(
    initialize = function(majority.class, class.tie = "first"){ 
      super$initialize(private$voting.name) 
      if( is.null(majority.class) || missing(majority.class) )
        stop("[",super$getName(),"][ERRROR] Majority class parameter should be defined")
      
      private$majority.class <- majority.class
      private$class.tie <- class.tie
    },
    execute = function(predictions){
      if(!"PredictionList" %in% class(predictions))
        stop("[",super$getName(),"][ERROR] Unsupported format. Parameter should be a PredictionList object\n")
      
      class.predictions <- predictions$getClassPredictions()
      summary <- table(sapply(class.predictions,class))
      class.values <- names(table(sapply(test1, unique)))
      
      if(!private$majority.class %in% class.values)
        stop("[",super$getName(),"][ERROR] Majority class not found in predicted values (",paste0(class.values,collapse = ","),")\n")

      if(length(names(summary)) > 1 )
        stop("[",super$getName(),"][ERROR] Different prediction values\n")
      
      if (!"factor" %in% names(summary))
        stop("[",super$getName(),"][ERROR] Prediction values must be factor. Aborting\n")
      
      #if( !super$getPrevalenceClass() %in% unique(unlist(lapply(class.predictions,levels))) )
      #  stop("[",super$getName(),"][ERROR] Majority class not found\n")
      
      cat("[",super$getName(),"][INFO] Voting strategy selected: '", super$getName() ,"'\n", sep="")
      
      voting.result <- data.frame(character(), stringsAsFactors = FALSE)
      
      for(row in 1:nrow(class.predictions)){
        prob.values <- unlist(class.predictions[row, ])
        class.values <- table(prob.values)
        result.values <- split(rep(0,length(class.values)),names(class.values))
        for (i in 1:length(prob.values)) 
          result.values[[prob.values[i]]] <- result.values[[prob.values[i]]] + 1
        
        df <- as.data.frame(result.values)
        final.prediction <- names(df)[which(df == max(df), arr.ind=T)[, "col"]]
        if ( length(final.prediction) > 1 ){
          if ( private$majority.class %in% final.prediction )
            voting.result <- rbind(voting.result, private$majority.class, stringsAsFactors=FALSE )
          else voting.result <- rbind(voting.result, names(class.values)[max.col(df, ties.method = private$class.tie )], stringsAsFactors=FALSE )
        }else voting.result <- rbind(voting.result, final.prediction, stringsAsFactors=FALSE )
      }
      ret <- data.frame(class.predictions,voting.result)
      names(ret) <- c( names(class.predictions) , private$voting.name )
      ret
    }
  ),
  private = list(
    voting.name = "ClassMajorityVoting",
    class.tie = NULL,
    majority.class = NULL
  )
)