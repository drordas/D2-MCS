MOOData <- R6Class(
  classname = "MOOData",
  inherit = MinResult,
  portable = TRUE,
  public = list(
    initialize = function (name, pareto.front, population, n.positive, n.negative){
      if (!inherits(pareto.front,"data.frame"))
        stop("[MOOData][ERROR] Pareto front must be a data.frame\n")

      if (!inherits(population,"list"))
        stop("[MOOData][ERROR] Population must be a list\n")
      
      super$initialize(name,n.positive, n.negative)
      private$pareto.front <- pareto.front
      private$population <- population
      private$conf.matrix <- NULL
    },
    plotPareto = function(){
      if( ( ncol(private$pareto.front) - 1  ) == 2 ){
        colnames <-  names(private$pareto.front)
        ggplot(private$pareto.front, aes_string(x=colnames[1], y=colnames[2] ) ) + labs(color = "Pareto Nº") + 
          geom_point(aes_string(color=as.factor(private$pareto.front[,3]))) +
          stat_smooth(aes_string( x=colnames[1], y=colnames[2], color=as.factor(private$pareto.front[,3]) ), method="lm", se=FALSE ) 
      }else cat("[MOOData][ERROR] 3D plot not implemented yet\n")
    },
    getBestSolution = function(pareto.distance = NULL){
      if( !is.null(pareto.distance) ){
        if( !inherits(pareto.distance,"ParetoDistance") ){
          cat("[MOOData][WARNING] Input parameter must inherit from ParetoDistance class. Using default method\n")
          method <- EuclideanDistance$new()   
        }else method <- pareto.distance
      }else method <- EuclideanDistance$new()
      
      cat("[MOOData][INFO] Executing method '",method$getName(),"'\n", sep="")
      method$compute(private$pareto.front[,-which(colnames(private$pareto.front)=="p.front")])
      method$solve.ties()
    },
    getParetoValues = function(){ private$pareto.front },
    getNumPositives = function(){ private$n.positive },
    getNumNegatives = function(){ private$n.negative },
    getConfusionMatrix = function( pareto.distance = NULL) { 
      stop("[",private$name,"][ERROR] Method 'getConfusionMatrix' is abstract. Must be implemented in inherited class\n")
    },
    getFP = function(){
      if (is.null(private$conf.matrix)) self$getConfusionMatrix(private$pareto.distance)
      private$conf.matrix$table[2,1]
    },
    getFN = function(){
      if (is.null(private$conf.matrix)) self$getConfusionMatrix(private$pareto.distance)
      private$conf.matrix$table[1,2]
    },
    getTP = function(){
      if (is.null(private$conf.matrix)) self$getConfusionMatrix(private$pareto.distance)
      private$conf.matrix$table[2,2]
    },
    getTN = function(){
      if (is.null(private$conf.matrix)) self$getConfusionMatrix(private$pareto.distance)
      private$conf.matrix$table[1,1]
    }
  ),
  private = list(
    pareto.front = NULL,
    population = NULL,
    conf.matrix = NULL
  )
)