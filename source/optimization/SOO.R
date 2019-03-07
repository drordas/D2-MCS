SOO <- R6Class(
  classname = "SOO",
  portable = TRUE,
  inherit = WeightsOptimizer,
  public = list(
    initialize = function (cpus = 1L, min.function, init.population = NULL, n.generations=20, maximize = FALSE){
      super$initialize(name= "SOO", dependences=NULL, min.function =  min.function)
      private$init.weights <- init.population
      private$generations <- n.generations
      private$optimize <- NULL
      private$cpus <- cpus
      private$maximize <- maximize
    },
    execute = function(init.weights=NULL, fitness){
      if ( missing(fitness) || !is.function(fitness))
        stop("[",private$name,"][ERROR] Fitness function should be provided to perform optimization process\n")
      
      if( is.null(init.weights) && is.null(private$init.weights) )
        stop("[",private$name,"][ERROR] Initial weights should be previously defined\n\n")
      
      if(!is.null(init.weights)) private$init.weights <- init.weights
      
      private$init.weights <- as.vector(private$parse.population(init.weights))
      
      cat("[",private$name,"][INFO] Executing ",private$name," Algorithm with ",private$generations,
          " generation\n",sep="")
      
      lower <- rep( 0, length(private$init.weights) )
      upper <- rep( as.numeric(length(private$init.weights)), length(private$init.weights) )
      
      private$optimize <- optimx(par = private$init.weights,
                                 fn = fitness, 
                                 gr = NULL,
                                 method = c("BFGS","CG","Nelder-Mead","L-BFGS-B","nlm","nlminb","Rcgmin",
                                            "Rvmmin","newuoa","bobyqa"),
                                 lower = lower, upper = upper,
                                 control = list(maximize=private$maximize, follow.on= FALSE, kkt=FALSE),
                                 min.function = private$min.function$computeMeasure )
      cat("[",private$name,"][INFO] Finish execution of ",private$name," Algorithm with ",private$generations,
          " generations\n",sep="")
    },
    getResult = function(n.positive, n.negative){
      private$min.function$pack(private$optimize,n.positive,n.negative)
    }
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
    generations = 20,
    maximize = FALSE,
    init.weights = NULL,
    final.weights = NULL
  )
)