ClusterPredictions <- R6::R6Class(
  classname = "ClusterPredictions",
  portable = TRUE,                   
  public = list(
    initialize = function(class.values,positive.class){
      
      if(!(positive.class %in% class.values))
        stop("[",class(self)[1],"][ERROR] Positive class not found. Should be ",
             paste0(class.values,collapse="or "),". Aborting...")
      
      private$positive.class <- positive.class
      private$class.values <- class.values
      private$pred <- list()
    },
    add = function(prediction){
      if( "Prediction" %in% class(prediction) ){
        private$pred <- append(private$pred,prediction)
      }else stop("[",class(self)[1],"][ERROR] Prediction parameter must be ",
                 "defined as 'Prediction' object. Aborting... ")
    },
    get = function(position){
      if( position > 0 && position <= length(private$pred) ){
        private$pred[[position]]
      }else stop("[",class(self)[1],"][ERROR] Position exceeds list size. Aborting...")
    },
    getAll = function(){ private$pred },
    size = function(){ length(private$pred) },
    getPositiveClass = function(){ private$positive.class },
    getClassValues =function() { private$class.values }
  ),
  private = list(
    pred = NULL,
    positive.class = NULL,
    class.values = NULL
  )
)