WeightsOptimizer <- R6::R6Class(
  classname = "WeightsOptimizer",
  portable = TRUE,
  public = list(
    initialize = function ( name, dependences = NULL, min.function ){
      if(missing(name) || is.null(name) || !is.character(name) )
        stop("[",class(self)[1],"][FATAL] Optimizer name should be defined. Aborting...")

      if(missing(min.function) || !"MinFunction" %in% class(min.function) )
        stop("[",class(self)[1],"][FATAL] Minimization function incorrect. ",
             "Must inherit from 'MinFunction'. Aborting...")

      private$name <- name
      private$min.function <- min.function
      private$dependences <- dependences
      loadPackages(private$dependences)
    },
    execute = function(fitness = NULL){
      stop("[",class(self)[1],"][FATAL] Method 'execute' is abstract.",
           "Should be implemented in inherited class")
    },
    #plot = function(){
    #  stop("[WeightsOptimizer][ERROR] Method 'plot' is abstract. Should be implemented in inherited class\n")
    #},
    getName = function(){ private$name },
    getNumObjectives = function(){ private$min.function$getNumOjectives() }#,
    #getParetoValues = function(){
    #  stop("[WeightsOptimizer][ERROR] Method 'getParetoValues' is abstract. Should be implemented in inherited class\n")
    #},
    #getOptimizedValues = function(){
    #  stop("[WeightsOptimizer][ERROR] Method 'getOptimizedValues' is abstract. Should be implemented in inherited class\n")
    #}
  ),
  private = list(
    getMinFunction = function() { private$min.function },
    voting.scheme = NULL,
    name = NULL,
    min.function = NULL,
    dependences = NULL
  )
)
