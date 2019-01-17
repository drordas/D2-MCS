OptimizationOutput <- R6Class(
  classname = "OptimizationOutput",
  portable = TRUE,
  public = list(
    initialize = function (optimizer = NULL){
      if(is.null(optimizer) || !"WeightsOptimizer" %in% class(optimizer) ){
        stop("[OptimizationOutput][ERROR] Optimizer name should be defined. Aborting...\n")
      }

      private$method <- optimizer$getName()
      private$n.objectives <- optimizer$getNumObjectives()
      private$pareto.front <- optimizer$getParetoValues()
      private$opt.values <- optimizer$getOptimizedValues()
    },
    getParetoFront = function(){
      private$pareto.front
    },
    getOptimizedValues = function(){
      private$opt.values
    },
    getMethodName = function(){
      private$method
    },
    plotPareto = function(reference.values){
      cat("[",private$method,"] Pending implementation\n")
    }
  ),
  private = list(
    method = NULL,
    n.objectives = NULL,
    pareto.front = NULL,
    opt.values = NULL
  )
)