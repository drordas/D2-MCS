Optimizers <- R6::R6Class(
  classname = "Optimizers",
  portable = TRUE,
  public = list(
    initialize = function (voting.scheme, cluster.models, metric, 
                           optimizers, positive.class, negative.class)
    {

      if (!inherits(voting.scheme,"VotingScheme"))
        stop("[",class(self)[1],"][FATAL] Voting scheme argument missing or invalid.",
             "Should inherit from 'VotingScheme' class. Aborting...")
      
      if ( !is.null(optimizers) && !all(sapply(optimizers,inherits, "MinResult")  ) ) 
        stop("[",class(self)[1],"][FATAL] Optimizers are missing or invalid.",
             "Should inherit from 'MinResult' class. Aborting...")

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
    computePerformance = function( pareto.distance=NULL) {
      sapply(self$getOptimizers(), function (x) { 
        cf <- ConFMatrix$new(x$getConfusionMatrix(pareto.distance)) 
      })
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