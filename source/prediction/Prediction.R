library("R6")
Prediction <- R6Class(
  classname = "Prediction",
  portable = TRUE,                   
  public = list(
    initialize = function(model, class.values, positive.class){
      if ( !inherits(model,"list") || length(model) != 5 )
        stop("[",class(self)[1],"][ERROR] Model must be defined as a list of four ",
             "elements. Aborting...")
      
      if (length(class.values) != 2 )
        stop("[",class(self)[1],"][ERROR] Incorrect argument. Target class", 
             "should be binary. Aborting... ")
      
      if (!is.character(positive.class) && length(positive.class) != 1)
        stop("[",class(self)[1],"][ERROR] Incorrect argument. ",
             "Positive class values should be a character string. Aborting...")
      private$results <- list(raw=NULL, prob=NULL, bin=NULL)
      private$model <- model
      private$positive.class <- positive.class
      private$negative.class <- setdiff(class.values,positive.class)
    },
    execute = function(pred.values){
      if( !missing(pred.values) && !is.null(pred.values) && 
          inherits(pred.values,"data.frame") )
      {
        loadPackages(private$model$model.libs)
        private$results$raw <- predict(object = private$model$model.data, 
                                       newdata=pred.values, type="raw" )
        private$results$prob <- predict(object = private$model$model.data, 
                                        newdata=pred.values, type="prob" )
        col.index <- which(levels(private$results$raw)==private$positive.class)
        private$results$bin <- varhandle::to.dummy(private$results$raw, 
                                                   private$positive.class)[,col.index]
      }
      else stop("[",class(self)[1],"][ERROR] Prediction values are missing or ",
                "incorrect. Aborting..")
    },
    getPrediction = function(type=NULL, target=NULL){
      if( is.null(type) || !type %in% c("raw","prob","bin") ){
        message(yellow(paste0("[",class(self)[1],"][WARNING] Probability type ",
                              "missing or incorrect. Should be 'raw', 'prob' ",
                              "or 'bin'. Assuming 'raw' by default")))
        type <- "raw"
      }
      class.values <- c(private$positive.class,private$negative.class)
      
      switch (type,
        "prob"= {
          if(is.null(target) || !(target %in% class.values) ){
            message(yellow(paste0("[",class(self)[1],"][WARNING] Target not ",
                                  "specified or invalid. Using '",
                                  private$positive.class,"' as default value")))
            target <- private$positive.class
          }
          private$results$prob[,target]},
        "bin" = {
          if( is.null(target) || !(target %in% class.values ) ){
            message(yellow(paste0("[",class(self)[1],"][WARNING] Target not ",
                                  "specified or invalid. Using '",
                                  private$positive.class,"' as default value")))
            target <- private$positive.class
          }
          if (target %in% private$positive.class)
            private$results$bin
          else abs(private$results$bin-1)
          },
        "raw" = {private$results$raw}
      )
    },
    getModelName = function(){ private$model$model.name },
    getModelPerformance = function(){ private$model$model.performance }
  ),
  private = list(
    results = NULL,
    model = NULL,
    positive.class = NULL,
    negative.class = NULL
  )
)