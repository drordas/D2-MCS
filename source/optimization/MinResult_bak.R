MinResult <- R6::R6Class(
  classname = "MinResult",
  portable = TRUE,
  public = list(
    initialize = function (name, pareto.front, n.positive, n.negative){
      if (!inherits(pareto.front,"data.frame"))
        stop("[MinResult][ERROR] Pareto front must be a data.frame\n")

      if (!is.numeric(n.positive) && !is.numeric(n.negative))
        stop("[MinResult][ERROR] Arguments 'n.positive' and 'n.negative' must be numeric\n")

      private$name <- name
      private$pareto.front <- pareto.front
      private$n.positive <- n.positive
      private$n.negative <- n.negative
    },
    plotPareto = function(){
      if( ( ncol(private$pareto.front) - 1  ) == 2 ){
        colnames <-  names(private$pareto.front)
        ggplot(private$pareto.front, aes_string(x=colnames[1], y=colnames[2] ) ) + labs(color = "Pareto Nº") +
          geom_point(aes_string(color=as.factor(private$pareto.front[,3]))) +
          stat_smooth(aes_string( x=colnames[1], y=colnames[2], color=as.factor(private$pareto.front[,3]) ), method="lm", se=FALSE )
      }else cat("[MinResult][ERROR] 3D plot not implemented yet\n")
    },
    getBestSolution = function(pareto.distance = NULL){
      if( !is.null(pareto.distance) ){
        if( !inherits(pareto.distance,"ParetoDistance") ){
          cat("[MinResult][WARNING] Input parameter must inherit from ParetoDistance class. Using default method\n")
          method <- EuclideanDistance$new()
        }else method <- pareto.distance
      }else method <- EuclideanDistance$new()

      cat("[MinResult][INFO] Executing method '",method$getName(),"'\n", sep="")
      method$compute(private$pareto.front[,-which(colnames(private$pareto.front)=="p.front")])
      method$solve.ties()
    },
    getParetoValues = function(){ private$pareto.front },
    getNumPositives = function(){ private$n.positive },
    getNumNegatives = function(){ private$n.negative },
    getConfusionMatrix = function( pareto.distance = NULL) {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    getFP = function() {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    getFN = function(){
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    getTN = function() {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    getTP = function() {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    getName = function(){ private$name }
  ),
  private = list(
    name = NULL,
    pareto.front = NULL,
    n.positive = NULL,
    n.negative = NULL
  )
)
