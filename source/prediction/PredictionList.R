library("R6")
PredictionList <- R6Class(
  classname = "PredictionList",
  portable = TRUE,                   
  public = list(
    initialize = function(metric){
      private$results.pred <- vector(mode="list")
      
      if(missing(metric) || is.null(metric))
        stop("[PredictionList][ERROR] Model metric parameter should be defined. Aborting\n")
      
      private$metric <- metric
    },
    addPrediction = function(prediction){
      if( "Prediction" %in% class(prediction) ){
        private$results.pred <- append(private$results.pred,prediction)
      }else stop("[PredictionList][ERROR] Prediction parameter must be defined as 'Prediction' Object\n")
    },
    getPredictionAt = function(position){
      if( position > 0 && position <= length(private$results.pred) ){
        private$results.pred[[position]]
      }else stop("[PredictionList][ERROR] Position exceeds list bounds\n")
    },
    size = function(){ length(private$results.pred) },
    getClassPredictions = function(){
      allPreds <- data.frame(matrix(NA, nrow=1, ncol=0),stringsAsFactors = FALSE)
      colNames <- c()
      for( i in 1:length(private$results.pred) ){
        allPreds <- cbind(allPreds,data.frame(private$results.pred[[i]]$getClassPrediction()), stringsAsFactors = FALSE)
        colNames <- c(colNames,paste0("CLUSTER ",i) )
      }
      names(allPreds) <- colNames
      allPreds
    },
    getProbPredictions = function(){
      allPreds <- data.frame(matrix(NA, nrow=1, ncol=0),stringsAsFactors = FALSE)
      for( i in 1:length(private$results.pred) )
        allPreds <- cbind(allPreds,data.frame(private$results.pred[[i]]$getProbPrediction()), stringsAsFactors = FALSE)
      allPreds
    },
    getBinaryPredictions = function(){
      allPreds <- data.frame(matrix(NA, nrow=1, ncol=0),stringsAsFactors = FALSE)
      colNames <- c()
      for( i in 1:length(private$results.pred) ){
        allPreds <- cbind(allPreds,data.frame(private$results.pred[[i]]$getBinaryPrediction()), stringsAsFactors = FALSE)
        colNames <- c(colNames,paste0("CLUSTER ",i) )
      }
      names(allPreds) <- colNames
      allPreds
    },
    getMetric = function(){ private$metric },
    getModelPerformances = function(){
      allPerf <- data.frame(matrix(NA, nrow=1, ncol=0),stringsAsFactors = FALSE)
      colNames <- c()
      for (i in 1:length(private$results.pred) ){
        allPerf <- cbind(allPerf,data.frame(private$results.pred[[i]]$getModelPerformance()), stringsAsFactors = FALSE)
        colNames <- c(colNames,paste0("CLUSTER ",i) )
      }
      names(allPerf) <- colNames
      allPerf
    }
  ),
  private = list(
    results.pred = NULL,
    metric = NULL
  )
)