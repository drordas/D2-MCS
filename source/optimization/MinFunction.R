MinFunction <- R6Class(
  classname = "MinFunction",
  portable = TRUE,
  public = list(
    initialize = function ( name, n.objectives ){
      if( is.null(name) )
        stop("[Fitness][ERROR] Name must be provided. Aborting...\n")
      
      if( is.null(n.objectives) || !is.numeric(n.objectives) || n.objectives < 2 )
        stop("[Fitness][ERROR] Number of objectives must be greater or equals 2. Aborting...\n")
      
      private$conf.matrix <- NULL
      private$name <- name
      private$n.objectives <- n.objectives
    },
    computeMeasure = function(conf.matrix){
      stop("[Fitness][ERROR] Fitness class is abstract. Method computeMeasure should be immplemented inherited class. Aborting\n")
    },
    getName = function(){ private$name },
    getNumOjectives = function(){ private$n.objectives }
  ),
  private = list(
    conf.matrix = NULL,
    n.objectives = NULL,
    name = NULL
  )
)