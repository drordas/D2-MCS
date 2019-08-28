ClassifierPerformance <- R6Class(
  classname = "ClassifierPerformance",
  portable = TRUE,
  public = list(
    initialize = function (preds, obs){
      if(is.null(preds) || !is.list(preds) || is.null(obs))
        stop("[ClassifierPerformance][ERROR] Input parameters are incorrect.")
      
      if(length(preds$preds) != length(obs) )
        stop("[ClassifierPerformance][ERROR] Number of Predictions and Observations mismatch")
      
      if(!all.equal(sort(levels(preds$preds)),sort(levels(obs))))
        stop("[ClassifierPerformance][ERROR] Levels of Predictions and Observations mismatch")
      
      private$positive.class <- preds$positive
      private$negative.class <- preds$negative
      private$metric <- preds$metric
      #private$preds <- preds$preds
      private$preds <- relevel(preds$preds,preds$negative)
      #private$obs <- obs
      private$models <- preds$models
      private$obs <- relevel(obs,preds$negative)
      private$weights <-preds$weights
      private$confusionMatrix <- caret::confusionMatrix(data=private$preds,reference=private$obs,positive=preds$positive)
    },
    getWeights = function() { private$weights },
    getPredictions = function() { private$preds },
    getObserved = function() { private$obs },
    getFP = function() { private$confusionMatrix$table[2,1]},
    getFN = function() { private$confusionMatrix$table[1,2]},
    getTN = function() { private$confusionMatrix$table[1,1]},
    getTP = function() { private$confusionMatrix$table[2,2]},
    getConfusionMatrix = function( ) { private$confusionMatrix },
    getNumPositives = function( ) { sum(private$confusionMatrix$table[,2])},
    getNumNegatives = function( ) { sum(private$confusionMatrix$table[,1])},
    getName = function() { private$name },
    getPositiveClass = function() { private$positive.class},
    getNegativeClass = function() { private$negative.class},
    getMetric = function(){ private$metric },
    getModels = function(){ private$models }
  ),
  private = list(
    confusionMatrix = NULL,
    positive.class = NULL,
    negative.class = NULL,
    obs = NULL,
    preds = NULL,
    weights = NULL,
    metric = NULL,
    models = NULL,
    name = "Classifier"
  )
)