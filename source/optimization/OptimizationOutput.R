OptimizationOutput <- R6::R6Class(
  classname = "OptimizationOutput",
  portable = TRUE,
  public = list(
    initialize = function (optimizer = NULL, n.positive= NULL, n.negative = NULL){
      if(is.null(optimizer) || !"WeightsOptimizer" %in% class(optimizer) ){
        stop("[",class(self)[1],"][FATAL] Optimizer name should be defined. Aborting...")
      }

      private$method <- optimizer$getName()
      private$n.objectives <- optimizer$getNumObjectives()
      private$pareto.front <- optimizer$getParetoValues()
      private$opt.values <- optimizer$getOptimizedValues()
      private$n.positive <- n.positive
      private$n.negative <- n.negative
    },
    getParetoFronts = function(){
      private$pareto.front
    },
    getOptimizedValues = function(){
      private$opt.values
    },
    getMethodName = function(){
      private$method
    },
    getBestSolution = function(pareto.distance = NULL){
      if( !is.null(pareto.distance) ){
        if( !inherits(pareto.distance,"ParetoDistance") ){
           message("[OptimizationOutput][WARNING] Input parameter must inherit from ParetoDistance class. Using default method")
           method <- EuclideanDistance$new()
        }else method <- pareto.distance
      }else method <- EuclideanDistance$new()

      message("[OptimizationOutput][INFO] Executing method '",method$getName(),"'")
      method$compute(private$pareto.front[,-which(colnames(private$pareto.front)=="p.front")])
      method$solve.ties()
    },
    getNumPositives = function(){
      private$n.positive
    },
    getNumNegatives = function(){
      private$n.negative
    },
    plotPareto = function(reference.values){
      if( ( ncol(private$pareto.front) - 1  ) == 2 ){
        colnames <-  names(private$pareto.front)
        ggplot(private$pareto.front, aes_string(x=colnames[1], y=colnames[2] ) ) + labs(color = "Pareto Nº") +
          geom_point(aes_string(color=as.factor(private$pareto.front[,3]))) +
          stat_smooth(aes_string( x=colnames[1], y=colnames[2], color=as.factor(private$pareto.front[,3]) ), method="lm", se=FALSE )
      }else message("[",class(self)[1],"][ERROR] 3D plot not implemented yet")
    }
  ),
  private = list(
    method = NULL,
    n.objectives = NULL,
    pareto.front = NULL,
    opt.values = NULL,
    n.positive = NULL,
    n.negative = NULL
  )
)