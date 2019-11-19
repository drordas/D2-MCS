library("R6")
ClassMajorityVoting <- R6Class(
  classname = "ClassMajorityVoting",
  portable = TRUE,
  inherit = VotingScheme,
  public = list(
    initialize = function(class.tie = "first"){ 

      super$initialize(private$voting.name) 
      private$class.tie <- class.tie
      private$final.pred <- NULL
      private$positive.class <- NULL
      private$class.values <- NULL
    },
    execute = function(predictions, majority.class=NULL){
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
        message("[",class(self)[1],"][WARNING] Majority class not set of invalid.",
                " Assuming default value: ",predictions$getPositiveClass())
      }else private$majority.class <- majority.class
      
      message("[",class(self)[1],"][INFO] Computing '",class(self)[1],
              "' scheme using '",private$majority.class,"' as majority class")
      
      private$final.pred <- list(prob=data.frame(),raw=c(),bin=data.frame())
      
      raw.pred <- sapply(predictions$getAll(),function(x) { 
                            x$getPrediction("raw",predictions$getPositiveClass()) 
                     })
      prob.pred <- sapply(predictions$getAll(),function(x) { 
                            x$getPrediction("prob",predictions$getPositiveClass()) 
                     })
      private$class.values <- predictions$getClassValues()
      private$positive.class <- predictions$getPositiveClass()
      negative.class <- setdiff( private$class.values, private$positive.class )
      
      for (row in 1:nrow(raw.pred)) {
        row.summary <- table(raw.pred[row, ])
        max.values <- names(which(row.summary == max(row.summary)))
        if ( length(max.values) > 1 ){
          if ( private$majority.class %in% max.values ){
            message("[",class(self)[1],"][INFO] Found Tie. Resolving using ",
                    "Majority class method")
            entry <- private$majority.class
            private$final.pred$raw <- c(private$final.pred$raw, entry)
          }else{
            message("[",class(self)[1],"][INFO] Found Tie. Resolving using ",
                    "Resolving using ",private$class.tie," tie solver")
            entry <- which.max(rank(x=row.summary, ties.method=private$class.tie))
            private$final.pred$raw <- c(private$final.pred$raw, entry)
          }
        }else{ 
          entry <- max.values
          private$final.pred$raw <- c(private$final.pred$raw,entry) 
        }
        
        prob.row <- prob.pred[row, ]
        if( entry %in% predictions$getPositiveClass() ){
          values <- prob.row[prob.row>.5]
          result <- sum(values)/length(values)
          private$final.pred$prob <- rbind(private$final.pred$prob, c(result,abs(result-1)))
        }else{ private$final.pred$prob <- rbind(private$final.pred$prob,c(0,1)) }
      }
      
      names(private$final.pred$prob) <- c(private$positive.class,negative.class)
      private$final.pred$raw <- factor(private$final.pred$raw, 
                                       levels= predictions$getClassValues())
      
      col.index <- which(levels(private$final.pred$raw)==private$positive.class)
      private$final.pred$bin <- varhandle::to.dummy(private$final.pred$raw, 
                                                    private$positive.class)[,col.index]
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
    getPositiveClass = function(){ private$positive.class },
    getClassValues = function() { private$class.values }
  ),
  private = list(
    voting.name = "ClassMajorityVoting",
    positive.class = NULL,
    class.values = NULL,
    final.pred = NULL,
    class.tie = "first",
    majority.class = NULL,
    weights = NULL
  )
)