MinFunction <- R6::R6Class(
  classname = "MinFunction",
  portable = TRUE,
  public = list(
    initialize = function ( name, objective.names ){
      if( is.null(name) )
        stop("[",class(self)[1],"][FATAL] Name must be provided. Aborting...")

      if( is.null(objective.names) )
        stop("[",class(self)[1],"][FATAL] Number of objectives is null. Aborting...")

      private$conf.matrix <- NULL
      private$name <- name
      private$n.objectives <- length(objective.names)
      private$objectives <- objective.names
    },
    computeMeasure = function(conf.matrix){
      stop("[",class(self)[1],"][FATAL] Fitness class is abstract. Method 'computeMeasure' should be immplemented inherited class. Aborting")
    },
    pack = function(pareto.front, n.positive, n.negative){
      stop("[",class(self)[1],"][FATAL] Fitness class is abstract. Method 'pack' should be immplemented inherited class. Aborting")
    },
    getName = function(){ private$name },
    getNumOjectives = function(){ private$n.objectives },
    getOjectiveNames = function(){ private$objectives }
  ),
  private = list(
    conf.matrix = NULL,
    n.objectives = NULL,
    objectives = NULL,
    name = NULL
  )
)