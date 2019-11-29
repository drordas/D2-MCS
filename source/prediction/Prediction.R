library("R6")
Prediction <- R6Class(
  classname = "Prediction",
  portable = TRUE,
  public = list(
    initialize = function(model){
      if ( !inherits(model,"list") || length(model) != 5 )
        stop("[",class(self)[1],"][ERROR] Model must be defined as a list of four ",
             "elements. Aborting...")

      private$results <- list(raw=NULL, prob=NULL)
      private$model <- model
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
        # col.index <- which(levels(private$results$raw)==private$positive.class)
        # private$results$bin <- varhandle::to.dummy(private$results$raw,
        #                                            private$positive.class)[,col.index]
      }
      else stop("[",class(self)[1],"][ERROR] Prediction values are missing or ",
                "incorrect. Aborting..")
    },
    getPrediction = function(type=NULL, target=NULL){
      if( is.null(type) || !type %in% c("raw","prob") ){
        message(yellow(paste0("[",class(self)[1],"][WARNING] Probability type ",
                              "missing or incorrect. Should be 'raw' or 'prob' ",
                              ". Assuming 'raw' by default")))
        type <- "raw"
      }
      switch (type,
        "prob"= {
          class.names <- names(private$results$prob)
          if(is.null(target) || !(target %in% class.names ) ){
            message(yellow(paste0("[",class(self)[1],"][WARNING] Target not ",
                                  "specified or invalid. Using '",
                                  class.names[1],"' as default value")))
            target <- class.names[1]
          }
          private$results$prob[,target]
         },
        "raw" = {private$results$raw}
      )
    },
    getModelName = function(){ private$model$model.name },
    getModelPerformance = function(){ private$model$model.performance }
  ),
  private = list(
    results = NULL,
    model = NULL
  )
)