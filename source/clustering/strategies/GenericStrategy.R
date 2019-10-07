GenericStrategy <- R6Class(
  classname = "GenericStrategy",
  public = list(
    initialize = function(name, subset, heuristic) {
      if ( missing(name) ){ stop(red("[GenericStrategy][ERROR] Strategy name not defined.")) }
      
      if ( !inherits(subset,"Subset") ){
        stop(red("[GenericStrategy][ERROR] Subset parameter must inherit from 'Subset' class."))
      }
      if(is.list(heuristic)){ 
        heuristic <- Filter( function(x) inherits(x,"Heuristic"), heuristic )
        if( length(heuristic) == 0 ) {
          stop( red("[GenericStrategy][ERROR] Adequate heuristics not found (must inherit from 'Heuristic' class).") )
        }
      }else{ 
        if( inherits(heuristic,"Heuristic") ) { 
          heuristic <- list(heuristic) 
        }else { stop(red("[GenericStrategy][ERROR] Heuristics is not correct (must inherit from 'Heuristic' class).")) }
      }
      private$name <- name
      private$subset <- subset
      private$heuristic <- heuristic
    },
    getName = function() { private$name },
    getHeuristic = function() { private$heuristic },
    getSubset = function() { private$subset },
    getAllDistributions = function() { private$all.distribution }, #TO BE DELETED. NO SENSE RETURNING ALL DE DISTRIBUTIONS.
    getBestClusterDistribution = function() { private$best.distribution },
    getUnclustered = function() { private$not.distribution },
    setName = function(name) { private$name <- name },
    execute = function(...) {
      stop(red("[GenericStrategy][ERROR] I am an abstract interface method"))
    },
    getDistribution = function(num.clusters = NULL, num.groups = NULL )  {
      stop(red("[GenericStrategy][ERROR] I am an abstract interface method"))
    },
    createSubset = function(subset, cluster = NULL, ...) {
      stop(red("[GenericStrategy][ERROR] I am an abstract interface method"))
    },
    plot = function(dir.path = NULL, file.name = NULL, ...) {
      stop(red("[GenericStrategy][ERROR] I am an abstract interface method"))
    }
  ),
  private = list(
    name = NULL,
    subset = NULL,
    heuristic = NULL,
    all.distribution = NULL,
    best.distribution = NULL,
    not.distribution = NULL
  )
)