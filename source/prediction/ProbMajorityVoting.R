library("R6")
ProbMajorityVoting <- R6Class(
  classname = "ProbMajorityVoting",
  portable = TRUE,
  inherit = VotingScheme,
  public = list(
    initialize = function(metric, class.tie = "first"){
      super$initialize(metric)
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
      
      private$final.pred <- list(prob=data.frame(), raw=c(), bin=data.frame())
      
      private$class.values <- predictions$getClassValues()
      private$positive.class <- predictions$getPositiveClass()
      negative.class <- setdiff( private$class.values, 
                                 private$positive.class )
      
      mean.pred <- apply(sapply(predictions$getAll(), function(x) { 
                          x$getPrediction("prob",private$positive.class)
                        } ), 1 , mean)
      
      for (row in 1:length(mean.pred)){
        mean.value <- mean.pred[row]
        private$final.pred$prob <- rbind(private$final.pred$prob, 
                                         data.frame(mean.value,abs(mean.value-1)))
        if(mean.pred == (mean.value-1) ){
          if(private$majority.class %in% private$positive.class){
            private$final.pred$raw <- c(private$final.pred$raw, private$positive.class) 
          }else{ private$final.pred$raw <- c(private$final.pred$raw, negative.class)}
        }else{
          if(mean.pred >.5){
            private$final.pred$raw <- c(private$final.pred$raw, private$positive.class)  
          }else{ private$final.pred$raw <- c(private$final.pred$raw, negative.class) }
        }
      }
      
      names(private$final.pred$prob) <- c(private$positive.class,negative.class)
      private$final.pred$raw <- factor( private$final.pred$raw,
                                        levels= private$class.values)
            
      col.index <- which(levels(private$final.pred$raw)==private$positive.class)
      private$final.pred$bin <- varhandle::to.dummy(private$final.pred$raw, 
                                                    private$positive.class)[,col.index]
    }
  ),
  private = list(
    final.prediction = NULL,
    majority.class = NULL
  )
)