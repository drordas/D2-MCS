OptimizationOutput <- R6Class(
  classname = "OptimizationOutput",
  portable = TRUE,
  public = list(
    initialize = function (optimizer = NULL, n.positive= NULL, n.negative = NULL){
      if(is.null(optimizer) || !"WeightsOptimizer" %in% class(optimizer) ){
        stop("[OptimizationOutput][ERROR] Optimizer name should be defined. Aborting...\n")
      }

      private$method <- optimizer$getName()
      private$n.objectives <- optimizer$getNumObjectives()
      private$pareto.front <- optimizer$getParetoValues()
      private$opt.values <- optimizer$getOptimizedValues()
      # distances <- private$euclidean.distance( private$pareto.front[,1:private$n.objectives] )
      # private$best.solution <- cbind( private$pareto.front[which(distances==min(distances)), ],
      #                                 private$opt.values[ which(distances==min(distances)), -nrow(private$opt.values) ] )
      # names(private$best.solution) <- c(names(private$pareto.front),paste0("X",seq(1,(nrow(private$opt.values)-1),1)))
      private$n.positive <- n.positive
      private$n.negative <- n.negative
      private$best.solution <- NULL
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
    computeBestSolution = function(distance = NULL, tie.solver = NULL){
      if( is.null(distance) || !is.function(distance) ){
        distances <- private$euclidean.distance( private$pareto.front[,1:private$n.objectives] )
        private$best.solution <- cbind( private$pareto.front[which(distances==min(distances)), ],
                                        private$opt.values[ which(distances==min(distances)), -nrow(private$opt.values) ] )
        names(private$best.solution) <- c(names(private$pareto.front),paste0("X",seq(1,(nrow(private$opt.values)-1),1)))
      }else{
        distances <- private$distance( private$pareto.front[,1:private$n.objectives] )
        private$best.solution <- cbind( private$pareto.front[which(distances==min(distances)), ],
                                        private$opt.values[ which(distances==min(distances)), -nrow(private$opt.values) ] )
        names(private$best.solution) <- c(names(private$pareto.front),paste0("X",seq(1,(nrow(private$opt.values)-1),1)))
      }
      
      if( is.null(tie.solver) || !is.function(tie.solver) )
        private$best.solution <- private$best.solution[1, ]
      else private$best.solution <- tie.solver(private$best.solution)
    },
    getBestSolution = function(){
      if( is.null(private$best.solution) )
        self$computeBestSolution()
      
      private$best.solution
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
      }else cat("[OptimizationOutput][ERROR] 3D plot not implemented yet\n")
    }
  ),
  private = list(
    euclidean.distance = function(pareto.front){
      distances <- apply(pareto.front,1,function(row){ sqrt(sum(row^2)) })
      pareto.front[ which(distances==min(distances)), ]
    },
    method = NULL,
    n.objectives = NULL,
    pareto.front = NULL,
    opt.values = NULL,
    best.solution = NULL,
    n.positive = NULL,
    n.negative = NULL 
  )
)