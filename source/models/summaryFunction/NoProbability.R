library("R6")
library("mltools")
library("caret")
library("ModelMetrics")
source("SummaryFunction.R")
NoProbability <- R6Class(
  classname = "NoProbability",
  inherit = SummaryFunction,
  portable = TRUE,
  public = list(
    initialize = function(){
      super$initialize(c("Kappa","Accuracy","TCR_9", "MCC", "PPV"))
    },
    execute = function(data, lev = NULL, model = NULL){
      lvls <- levels(data$obs)
      if(length(lvls) > 2)
        stop(paste("Your outcome has", length(lvls),
                   "levels. The defaultSummary() function isn't appropriate."))
      
      if (!all(levels(data[, "pred"]) == lvls))
        stop("classSummary:: levels of observed and predicted data do not match")
      
      data$y = as.numeric(data$obs == lvls[2])
      data$z = as.numeric(data$pred == lvls[2])
      
      confMat <- caret::confusionMatrix(table(data$z, data$y),positive="1")
      fn_tcr_9 <- (9*confMat$table[1,2] + confMat$table[2,1]) / (9 * (confMat$table[1,2] + confMat$table[2,2]) + confMat$table[2,1] + confMat$table[1,1] )
      mcc <- mltools::mcc(TP=confMat$table[1,1],FP=confMat$table[1,2],TN=confMat$table[2,2],FN=confMat$table[2,1])
      ppv <- (confMat$table[1,1] / (confMat$table[1,1] + confMat$table[1,2]) )
      out <- c(confMat$overall['Kappa'],confMat$overall['Accuracy'],fn_tcr_9, mcc, ppv)
      names(out) <- c("Kappa","Accuracy","TCR_9", "MCC", "PPV")
      out
    },
    getMeasures = function(){
      super$getMeasures()
    }
  )
)