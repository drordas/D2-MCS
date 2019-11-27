SMSEMOA <- R6Class(
  classname = "SMSEMOA",
  portable = TRUE,
  inherit = WeightsOptimizer,
  public = list(
    initialize = function (cpus = 1L, min.function, init.population = NULL, ref.point=NULL, n.generations=20, n.iteractions=1, popSize=100){
      super$initialize(name= "SMSEMOA", dependences=c("ecr"), min.function =  min.function)
      private$init.weights <- init.population
      private$n.iteractions <- n.iteractions
      private$generations <- n.generations
      private$ref.point <- ref.point
      private$popSize <- popSize
      private$optimize <- NULL
      private$cpus <- cpus
    },
    execute = function(init.weights=NULL, fitness){
      if ( missing(fitness) || !is.function(fitness))
        stop("[",private$name,"][ERROR] Fitness function should be provided to perform optimization process\n")
      
      if( is.null(init.weights) && is.null(private$init.weights) )
        stop("[",private$name,"][ERROR] Initial weights should be previously defined\n")
      
      if (is.null(private$min.function$getNumOjectives()) && is.null(private$ref.point) )
        stop("[",private$name,"][ERROR] Default hypervolumne reference point can only be generated if number of objectives is passed.\n")
      
      if ( is.null(private$ref.point) ) private$ref.point <- rep(11,private$min.function$getNumOjectives())
      
      if(!is.null(init.weights)) private$init.weights <- init.weights
      
      private$init.weights <- private$parse.population(init.weights)
      
      message("[",private$name,"][INFO] Executing ",private$name,
              " Algorithm with ",private$generations," generation/s comprising ",
              private$popSize," individuals")
      
      lower <- rep( 0, length(private$init.weights) )
      upper <- rep( as.double(length(private$init.weights)), length(private$init.weights) )
      
      private$optimize <- sapply( 1:private$n.iteractions, function(x) {
              message("[",private$name,"][INFO] Executing iteration ",x," of ",
                      private$n.iteractions)
              list(ecr::ecr( fitness.fun = fitness, minimize=TRUE, 
                             n.objectives= private$min.function$getNumOjectives(),
                             n.dim = length(private$init.weights), 
                             lower= lower, upper = upper,
                             mu = 100L, lambda = 1L, representation= "float", 
                             survival.strategy = "plus",
                             parent.selector = ecr::selSimple,
                             mutator = setup(ecr::mutPolynomial,eta = 25,p = 0.2, 
                                             lower = lower, upper = upper),
                             recombinator = setup(ecr::recSBX, eta = 15, p = 0.7, 
                                                  lower = lower, upper = upper), log.pop = TRUE,
                             initial.solutions = list(as.double(names(table(private$init.weights)))),
                             survival.selector = setup(ecr::selDomHV, ref.point= private$ref.point),
                             terminators = list(stopOnIters( private$generations )),
                             min.function = private$min.function$computeMeasure ))
      })
      
      message("[",private$name,"][INFO] Finish execution of ",private$name,
              " Algorithm with ",private$generations," generation/s comprising ",
              private$popSize," individuals")
    },
    getResult = function(n.positive, n.negative){
      pareto.front <- private$getParetoValues()
      population <- private$getLastPopulation()
      private$min.function$pack(super$getName(),pareto.front,population,n.positive,n.negative)
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
    getParetoValues = function(){
      if( is.null(private$optimize) || length(private$optimize) == 0 )
        stop("[",private$name,"][ERROR] Multi-Objective Algorithm not executed or failed\n")
      else{
        as.data.frame(do.call(rbind,lapply(1:length(private$optimize), function(x) {
          df <- data.frame(private$optimize[[x]]$pareto.front,x)
          names(df) <- c(private$min.function$getOjectiveNames(),"p.front")
          df
        })))
      }
    },
    getLastPopulation = function(){
      if( is.null(private$optimize) || length(private$optimize) == 0 )
        stop("[",super$getName(),"][ERROR] Multi-Objective Algorithm not executed or failed\n")
      else{ 
        out <- do.call(c, lapply(private$optimize, function(iteration) {
          l <- do.call(rbind,lapply(iteration$last.population, function(pop){
            values <- as.vector( attributes(pop)$fitness )
            weigths <- as.vector(pop)
            c(values,weigths)
          }))
          l <- as.data.frame(l)
          names (l) <- c(private$min.function$getOjectiveNames(),paste0("W",seq(1:(ncol(l)-2))))
          list(l)}
        ))
        out
      }
    },
    cpus = 1L,
    optimize = NULL,
    popSize = 100,
    generations = 20,
    init.weights = NULL,
    final.weights = NULL,
    ref.point = NULL,
    n.iteractions = NULL
  )
)