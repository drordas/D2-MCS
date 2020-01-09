PerformanceMeasures <- R6::R6Class(
  classname = "PerformanceMeasures",
  portable = TRUE,                   
  public = list(
    initialize = function(cf){
      if("confusionMatrix" %in% class(cf)){
        private$cf <- cf
        private$mcc <- mltools::mcc(TP=cf$table[1,1],FP=cf$table[1,2],TN=cf$table[2,2],FN=cf$table[2,1])
      }else stop("[PerformanceMeasures][ERROR] Incorrect type of Confusion Matrix\n")
    },
    getMCC = function(){ private$mcc },
    getConfMatrix = function(){ private$cf$table},
    getAccuracy = function(){ private$cf$overall["Accuracy"] },
    getKappa = function(){ private$cf$overall["Kappa"] },
    getSensitivity = function(){ private$cf$byClass["Sensitivity"] },
    getSpecificity = function(){ private$cf$byClass["Specificity"] },
    getPrecision = function(){ private$cf$byClass["Precision"] },
    getPPV = function() { private$cf$byClass["Pos Pred Value"] },
    getNPV = function() { private$cf$byClass["Neg Pred Value"] },
    getMeasure = function(measure){
      if (missing(measure) || is.null(measure))
        stop("[PerformanceMeasures][ERROR] Measure should be specified\n")
      else{
        switch (toupper(measure),
          "MCC" = { self$getMCC() },
          "Accuracy" = { self$getAccuracy() },
          "Kappa" = { self$getKappa() },
          "Sensitivity" = { self$getSensitivity() },
          "Specificity" = { self$getSpecificity() },
          "Precision" = { self$getPrecision() },
          "PPV" = { self$getPPV() },
          "NPV" = { self$getNPV() }
        )
      }
    }
  ),
  private = list(
    cf = NULL,
    mcc = NULL
  )
)