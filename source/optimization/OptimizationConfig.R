OptimizationConfig <- R6Class(
  classname = "OptimizationConfig",
  portable = TRUE,
  public = list(
    initialize = function (weights, models){
      if(length(weights) < 1 || is.null(models) || !"ModelsList" %in%  class(models) || models$size() < 1 )
        stop("[OptimizationConfig][ERROR] Input parameters are incorrect\n")
      
      private$weights <- weights
      private$models <- models
    },
    getWeights = function(){ private$weights },
    getModels = function(){ private$models }
  ),
  private = list(
    weights = NULL,
    models = NULL
  )
)