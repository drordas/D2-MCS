NSGAII <- R6Class(
  classname = "NSGAII",
  portable = TRUE,
  inherit = WeightsOptimizer,
  public = list(
    initialize = function (cpus = 1L, min.function, init.population = NULL, generations=20, popSize=100){
      super$initialize(name= "NSGAII", dependences=c("ecr"), min.function =  min.function)
      private$init.weights <- init.population
      private$generations <- generations
      private$popSize <- popSize
      private$optimize <- NULL
      private$cpus = cpus
    },
    execute = function(init.weights=NULL, fitness){
      if ( missing(fitness) || !is.function(fitness))
        stop("[",super$getName(),"][ERROR] Fitness function should be provided to perform optimization process\n")
      
      if( is.null(init.weights) && is.null(private$init.weights) )
        stop("[",super$getName(),"][ERROR] Initial weights should be previously defined\n\n")
      
      if(!is.null(init.weights)) private$init.weights <- init.weights
      
      private$init.weights <- private$parse.population(init.weights)
      min.function <- super$getMinFunction()

      cat("[",super$getName(),"][INFO] Executing NSGAII Algorithm with ",private$generations,
          " generation/s comprising ",private$popSize," individuals\n",sep="")
      
      lower <- rep( 0, length(private$init.weights) )
      upper <- rep( as.double(length(private$init.weights)), length(private$init.weights) )

      private$optimize <- ecr( fitness.fun = fitness, minimize=TRUE, n.objectives = min.function$getNumOjectives(),
                               n.dim = length(private$init.weights), lower = lower, upper = upper,
                               mu = 100L, lambda = 100L, representation = "float", survival.strategy = "plus",
                               parent.selector = ecr::selSimple,
                               mutator = setup(ecr::mutPolynomial, eta = 25, p = 0.2, lower = lower, upper = upper),
                               recombinator = setup(ecr::recSBX, eta = 15, p = 0.7, lower = lower, upper = upper),
                               initial.solutions = list(as.double(names(table(private$init.weights)))),
                               survival.selector = ecr::selNondom,
                               terminators = list(stopOnIters( private$generations )),
                               min.function = min.function$computeMeasure )
      
      cat("[",super$getName(),"][INFO] Finish execution of NSGAII Algorithm with ",private$generations,
          " generation/s comprising ",private$popSize," individuals\n",sep="")
    },
    getParetoValues = function(){
      if( is.null(private$optimize) || is.null(private$optimize$pareto.front) )
        stop("[",super$getName(),"][ERROR] Multi-Objective Algorithm not executed or failed\n")
      else{ 
        names(private$optimize$pareto.front) <- super$getMinFunction()$objectives
        return(private$optimize$pareto.front)
      }
    },
    getOptimizedValues = function(){
      if( is.null(private$optimize) || is.null(private$optimize$pareto.set) )
        stop("[",super$getName(),"][ERROR] Multi-Objective Algorithm not executed or failed\n")
      else return ( as.data.frame(do.call(rbind,strsplit(unlist(sapply(private$optimize$pareto.set,paste0,collapse=",")),","))) )
    },
    finalize= function(){ unloadPackages(c("ecr")) }
  ),
  private = list(
    parse.population = function(init.population){
      if( !"matrix" %in% class(init.population) ){
        if(is.vector(init.population)) 
          init.population <- matrix( as.numeric(init.population), nrow = 1, ncol= length(init.population) )
        else init.population <- matrix( as.numeric(init.population), nrow = nrow(init.population), ncol= ncol(init.population) )
      }
      init.population
    },
    cpus = 1L,
    optimize = NULL,
    popSize = 100,
    generations = 20,
    init.weights = NULL,
    final.weights = NULL
  )
)