library("R6")
PerformanceMeasures <- R6Class(
  classname = "PerformanceMeasures",
  portable = TRUE,                   
  public = list(
    initialize = function(cf, mcc){
      private$cf <- cf
      private$mcc <- mcc
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