D2MCSPerformance <- R6Class(
  classname = "D2MCSPerformance",
  portable = TRUE,
  public = list(
    initialize = function (preds, obs, models, metric, weights, positive.class, negative.class){
      
      if(length(levels(preds)) != 2 || length(levels(obs)) != 2 || length(levels(preds)) != length(levels(obs)) )
        stop("[D2MCSPerformance][ERROR] Class values incorrect or missmatch. Aborting...\n")
      
      if( all(levels(preds) %in% c("0","1")) || all(levels(preds) %in% c(0,1))  ){
        preds.levels <- as.numeric( levels(preds) )
        preds <- factor(preds, levels=c(0,1), labels=c(negative.class,positive.class))
      }else preds <- relevel(preds,ref=negative.class)
      
      if( all(levels(obs) %in% c("0","1")) ){
        obs.levels <- as.numeric( levels(obs) )
        obs <- factor(obs, levels=c(0,1), labels=c(negative.class,positive.class))
      }else obs <- relevel(obs,ref=negative.class)
    
      ifelse( is.data.frame(weights), private$weights <- as.numeric(weights), private$weights <- weights  ) 
      private$confusionMatrix <- caret::confusionMatrix( preds, obs, positive=positive.class )
      private$positive.class <- positive.class
      private$negative.class <- negative.class
      private$metric <- metric
      private$preds <- preds
      private$model <- models
    },
    getWeights = function() { private$weights },
    getPredictions = function() { private$preds },
    getMetric = function() { private$metric },
    getConfusionMatrix = function() { private$confusionMatrix },
    getTP = function() { private$confusionMatrix$table[private$positive.class, private$positive.class] },
    getTN = function() { private$confusionMatrix$table[private$negative.class, private$negative.class] },
    getFP = function() { private$confusionMatrix$table[private$negative.class, private$positive.class] },
    getFN = function() { private$confusionMatrix$table[private$positive.class, private$negative.class] },
    getPositiveClass = function() { private$positive.class }, 
    getNegativeClass = function() { private$negative.class },
    getModels = function() { private$model }
  ),
  private = list(
    positive.class = NULL,
    confusionMatrix = NULL,
    negative.class = NULL,
    weights = NULL,
    preds = NULL,
    model = NULL,
    metric = NULL
  )
)