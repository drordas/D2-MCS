GenericStrategy <- R6Class(
  classname = "GenericStrategy",
  public = list(
    initialize = function( subset, heuristic, description, configuration) {

      if ( missing(description) ){ stop(red("[GenericStrategy][ERROR] Strategy description not defined.")) }

      if ( !inherits(subset,"Subset") ) {
        stop(red("[GenericStrategy][ERROR] Subset parameter must inherit from 'Subset' class."))
      }
      if(is.list(heuristic)){
        if( length(Filter( function(x) inherits(x,"GenericHeuristic"), heuristic) ) == 0 ) {
          stop( red("[GenericStrategy][ERROR] Adequate heuristics not found",
                    "(must inherit from 'GenericHeuristic' class).") )
        }
      }else{
        if( inherits(heuristic,"GenericHeuristic") ) {
          heuristic <- list(heuristic)
        }else { stop(red("[GenericStrategy][ERROR] Heuristics is not correct",
                         "(must inherit from 'GenericHeuristic' class).")) }
      }

      if( !inherits(configuration,"StrategyConfiguration") ){
        stop( "[GenericStrategy][ERROR] Configuration parameter must",
              "inherit from 'StrategyConfiguration' class." )
      }

      private$description <- description
      private$subset <- subset
      private$heuristic <- heuristic
      private$configuration <- configuration
    },
    getName = function() { class(self)[1] },
    getDescription = function() { private$description },
    getHeuristic = function() { private$heuristic },
    getConfiguration = function() { private$configuration },
    getBestClusterDistribution = function() { private$best.distribution },
    getUnclustered = function() { private$not.distribution },
    setValidHeuristics = function(valid.heuristics) {
      private$valid.heuristics <- valid.heuristics
    },
    execute = function(verbose, ...) {
      stop(red("[GenericStrategy][ERROR] I am an abstract interface method"))
    },
    getDistribution = function(num.clusters = NULL, num.groups = NULL,
                               include.unclustered = FALSE )  {
      stop(red("[GenericStrategy][ERROR] I am an abstract interface method"))
    },
    createTrain = function(subset, num.cluster = NULL, num.groups = NULL,
                           include.unclustered = FALSE) {
      stop(red("[GenericStrategy][ERROR] I am an abstract interface method"))
    },
    plot = function(dir.path = NULL, file.name = NULL, ...) {
      stop(red("[GenericStrategy][ERROR] I am an abstract interface method"))
    },
    saveCSV = function(dir.path, name, num.clusters= NULL){
      stop(red("[GenericStrategy][ERROR] I am an abstract interface method"))
    }
  ),
  private = list(
    description = NULL,
    subset = NULL,
    heuristic = NULL,
    configuration = NULL,
    all.distribution = NULL,
    best.distribution = NULL,
    not.distribution = NULL
  )
)