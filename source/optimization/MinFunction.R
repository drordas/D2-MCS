MinFunction <- R6::R6Class(
  classname = "MinFunction",
  portable = TRUE,
  public = list(
    initialize = function ( name, objective.names ){
      if( is.null(name) )
        stop("[Fitness][ERROR] Name must be provided. Aborting...\n")
      
      if( is.null(objective.names) )
        stop("[Fitness][ERROR] Number of objectives is null. Aborting...\n")
      
      private$conf.matrix <- NULL
      private$name <- name
      private$n.objectives <- length(objective.names)
      private$objectives <- objective.names
    },
    computeMeasure = function(conf.matrix){
      stop("[Fitness][ERROR] Fitness class is abstract. Method 'computeMeasure' should be immplemented inherited class. Aborting\n")
    },
    pack = function(pareto.front, n.positive, n.negative){
      stop("[Fitness][ERROR] Fitness class is abstract. Method 'pack' should be immplemented inherited class. Aborting\n")
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