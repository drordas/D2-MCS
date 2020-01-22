ClusteringStrategy <- R6::R6Class(
  classname = "ClusteringStrategy",
  public = list(
    initialize = function( subset, heuristic, description, configuration) {

      if ( is.null(description) || !is.character(description)) {
        stop("[",class(self)[1],"][FATAL] Strategy description parameter must ",
             "be defined as 'character' type. Aborting...")
      }

      if ( !inherits(subset,"Subset") ) {
        stop("[",class(self)[1],"][FATAL] Subset parameter must be inherit from ",
             "'Subset' class. Aborting...")
      }

      if(is.list(heuristic)){
        if( length(Filter( function(x) inherits(x,"GenericHeuristic"), heuristic) ) == 0 ) {
          stop("[",class(self)[1],"][FATAL] Adequate heuristics not found ",
              "(must inherit from 'GenericHeuristic' class). Aborting...")
        }
      }else{
        if( inherits(heuristic,"GenericHeuristic") ) {
          heuristic <- list(heuristic)
        }else { stop("[",class(self)[1],"][FATAL] Heuristics is not correct ",
                     "(must inherit from 'GenericHeuristic' class). Aborting...") }
      }

      if( !inherits(configuration,"StrategyConfiguration") ){
        stop("[",class(self)[1],"][FATAL] Configuration parameter must be ",
             "inherit from 'StrategyConfiguration' class. Aborting...")
      }

      private$description <- description
      private$subset <- subset
      private$heuristic <- heuristic
      private$configuration <- configuration
    },
    getDescription = function() { private$description },
    getHeuristic = function() { private$heuristic },
    getConfiguration = function() { private$configuration },
    getBestClusterDistribution = function() { private$best.distribution },
    getUnclustered = function() { private$not.distribution },
    execute = function(verbose, ...) {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    getDistribution = function(num.clusters = NULL, num.groups = NULL,
                               include.unclustered = FALSE )  {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    createTrain = function(subset, num.cluster = NULL, num.groups = NULL,
                           include.unclustered = FALSE) {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    plot = function(dir.path = NULL, file.name = NULL, ...) {
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    saveCSV = function(dir.path, name, num.clusters = NULL){
      stop("[",class(self)[1],"][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
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