Optimizers <- R6Class(
  classname = "Optimizers",
  portable = TRUE,
  public = list(
    initialize = function (voting.scheme, cluster.models, metric, optimizers, positive.class, negative.class){
      
      if (!inherits(voting.scheme,"VotingScheme"))
        stop("[Optimizers][ERROR] Voting.scheme argument missing or invalid. Should inherit from 'VotingScheme' class. Aborting...")
      
      #if (!inherits(prediction.cluster,"PredictionList"))
      #  stop("[Optimizers][ERROR] Prediction.cluster argument missing or invalid. Should inherit from 'PredictionList' class. Aborting...")
      
      if ( !is.null(optimizers) && !all(sapply(optimizers,inherits, "MinResult")  ) ) 
        stop("[Optimizers][ERROR] Optimizers arguments missing or invalid. Should inherit from 'MinResult' class. Aborting...")
      
      private$optimizers <- optimizers
      private$voting.scheme <- voting.scheme
      private$cluster.models <- cluster.models
      private$positive.class <- positive.class
      private$negative.class <- negative.class
      private$metric <- metric
    },
    getPositiveClass = function() {private$positive.class},
    getNegativeClass = function() {private$negative.class},
    getVotingMethod  = function(){ private$voting.scheme },
    getOptimizers  = function(){ private$optimizers },
    getMetric = function(){ private$metric },
    getModels = function(){ private$cluster.models },
    computePerformance = function( pareto.distance) {
      sapply(nsga$getOptimizers(), function (x){ 
        cf <- ConFMatrix$new(x$getConfusionMatrix(pareto)) 
        
      }, pareto = pareto.distance)
    }
  ),
  private = list(
    voting.scheme  = NULL,
    positive.class = NULL,
    negative.class = NULL,
    cluster.models = NULL,
    metric = NULL,
    optimizers = NULL
  )
)