ModelPerformance <- R6::R6Class(
  classname = "ModelPerformance",
  portable = TRUE,
  public = list(
    initialize = function (model.name, conf.mat, weights){
      if(is.null(model.name) || !is.character(model.name))
        stop("[",class(self)[1],"][FATAL] Model name missing or incorrect. Aborting...")

      #if (!inherits(conf.mat,"confusionMatrix"))
      # stop("[ModelPerformance][ERROR] Invalid confusionMatrix argument. Aborting... \n")

      if (!inherits(conf.mat,"ConfMatrix"))
        stop("[",class(self)[1],"][FATAL] Invalid confusionMatrix argument. Aborting...")

      private$weights <- weights
      private$name <- model.name
      private$conf.mat <- conf.mat
    },
    getConfusionMatrix = function(){ private$conf.mat$getConfusionMatrix() },
    getTP = function(){ private$conf.mat$getTP() },
    getTN = function(){ private$conf.mat$getTN() },
    getFP = function(){ private$conf.mat$getFP() },
    getFN = function(){ private$conf.mat$getFN() },
    getName= function(){ private$name },
    getWeights = function() { private$weights }
  ),
  private = list(
    conf.mat = NULL,
    positive.class = NULL,
    negative.class = NULL,
    weights = NULL,
    name = NULL
  )
)