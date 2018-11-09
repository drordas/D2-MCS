source("SummaryFunction.R")
UseProbability <- R6Class(
  classname = "UseProbability",
  inherit = SummaryFunction,
  portable = TRUE, 
  public = list(
    initialize = function(){
      super$initialize(c("ROC", "Sens", "Spec","Kappa","Accuracy","TCR_9","MCC","PPV"))
    },
    execute = function(data, lev = NULL, model = NULL){
      lvls <- levels(data$obs)
      if( length(lvls) > 2 )
        stop( paste("Your outcome has", length(lvls),
                    "levels. The UseProbability function isn't appropriate."))
      if (!all(levels(data[, "pred"]) == lvls))
        stop("levels of observed and predicted data do not match")
      
      data$y = as.numeric(data$obs == lvls[2])
      data$z = as.numeric(data$pred == lvls[2])
      rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0, 1), data[, lvls[1]])
      confMat <- caret::confusionMatrix(table(data$z, data$y),positive="1")
      mcc <- mltools::mcc(TP=confMat$table[1,1],FP=confMat$table[1,2],TN=confMat$table[2,2],FN=confMat$table[2,1])
      ppv <- (confMat$table[1,1] / (confMat$table[1,1] + confMat$table[1,2]) )
      fn_tcr_9 <- ( 9*confMat$table[1,2] + confMat$table[2,1]) / (9 * (confMat$table[1,2] + confMat$table[2,2]) + 
                                                                    confMat$table[2,1] + confMat$table[1,1] )
      out <- c(rocAUC,
               caret::sensitivity(data[, "pred"], data[, "obs"], lev[1]),
               caret::specificity(data[, "pred"], data[, "obs"], lev[2]),
               confMat$overall['Kappa'],
               confMat$overall['Accuracy'],
               fn_tcr_9, mcc, ppv)
      names(out) <- c("ROC", "Sens", "Spec","Kappa","Accuracy","TCR_9","MCC","PPV")
      out
    },
    getMeasures = function(){
      super$getMeasures()
    }
  )
)