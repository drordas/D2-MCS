NSGAII <- R6::R6Class(
  classname = "NSGAII",
  portable = TRUE,
  inherit = WeightsOptimizer,
  public = list(
    initialize = function (cpus = 1L, min.function, init.population = NULL, 
                           n.generations=20, n.iteractions=1, popSize=100)
    {
      super$initialize(name= class(self)[1], dependences=c("ecr"), 
                       min.function =  min.function)
      private$init.weights <- init.population
      private$n.iteractions <- n.iteractions
      private$generations <- n.generations
      private$popSize <- popSize
      private$optimize <- NULL
      private$cpus <- cpus
    },
    execute = function(init.weights=NULL, fitness){
      if ( missing(fitness) || !is.function(fitness))
        stop("[",super$getName(),"][ERROR] Fitness function should be provided",
             " to perform optimization process\n")
      
      if( is.null(init.weights) && is.null(private$init.weights) )
        stop("[",super$getName(),"][ERROR] Initial weights should be",
             "previously defined\n")
      
      if(!is.null(init.weights)) private$init.weights <- init.weights

      private$init.weights <- private$parse.population(init.weights)

      message("[",super$getName(),"][INFO] Executing Optimization Algorithm with ",
              private$generations," generation/s comprising ",
              private$popSize," individuals")
      
      lower <- rep( 0, length(private$init.weights) )
      upper <- rep( as.double(length(private$init.weights)), length(private$init.weights) )
      
      private$optimize <- sapply( 1:private$n.iteractions, function(x) {
            message("[",super$getName(),"][INFO] Executing iteration ",x,
                    " of ",private$n.iteractions)
            list(ecr::ecr( fitness.fun = fitness, minimize=TRUE, 
                           n.objectives = private$min.function$getNumOjectives(),
                           n.dim = length(private$init.weights), 
                           lower= lower, upper= upper, mu = 100L, lambda = 100L, 
                           representation = "float", survival.strategy = "plus",
                           parent.selector = ecr::selSimple,
                           mutator= setup(ecr::mutPolynomial, eta= 25, p= 0.2, 
                                          lower= lower, upper= upper),
                           recombinator= setup(ecr::recSBX, eta = 15, p = 0.7, 
                                               lower = lower, upper = upper), 
                           log.pop = TRUE, 
                           initial.solutions= list(as.double(names(table(private$init.weights)))),
                           survival.selector= ecr::selNondom,
                           terminators= list(stopOnIters( private$generations )),
                           min.function= private$min.function$computeMeasure ))
      })
      
      message( "[",super$getName(),"][INFO] Execution finished with ",
               private$generations," generation/s comprising ",private$popSize,
               " individuals")
    },
    getResult = function(n.positive, n.negative){
      pareto.front <- private$getParetoValues()
      population <- private$getLastPopulation()
      private$min.function$pack(super$getName(),pareto.front,population,
                                n.positive,n.negative)
    }
  ),
  private = list(
    parse.population = function(init.population){
      if( !"matrix" %in% class(init.population) ){
        if(is.vector(init.population)) 
          init.population <- matrix(as.numeric(init.population), nrow = 1, 
                                    ncol= length(init.population) )
        else init.population <- matrix(as.numeric(init.population), 
                                       nrow = nrow(init.population), 
                                       ncol= ncol(init.population) )
      }
      init.population
    },
    getParetoValues = function(){
      if( is.null(private$optimize) || length(private$optimize) == 0 )
        stop("[",super$getName(),"][ERROR] Optimization Algorithm not executed or failed")
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
        stop("[",super$getName(),"][ERROR] Optimization Algorithm not executed or failed")
      else{
        out <- do.call(c, lapply(private$optimize, function(iteration) {
          l <- do.call(rbind,lapply(iteration$last.population, function(pop){
            values  <- as.vector(attributes(pop)$fitness)
            weigths <- as.vector(pop)
            c(values,weigths)
          }))
          l <- as.data.frame(l)
          names (l) <- c(private$min.function$getOjectiveNames(),paste0("W",seq(1:(ncol(l)-2))))
          list(l)
          }
        ))
        return(out)
      }
    },
    cpus = 1L,
    optimize = NULL,
    popSize = 100,
    generations = 20,
    init.weights = NULL,
    final.weights = NULL,
    n.iteractions = NULL
  )
)