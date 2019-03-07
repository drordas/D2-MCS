WeightsOptimizer <- R6Class(
  classname = "WeightsOptimizer",
  portable = TRUE,
  public = list(
    initialize = function ( name, dependences = NULL, min.function ){
      if(missing(name) || is.null(name) || !is.character(name) )
        stop("[WeightsOptimizer][ERROR] Optimizer name should be defined. Aborting...\n")

      if(missing(min.function) || !"MinFunction" %in% class(min.function) )
        stop("[WeightsOptimizer][ERROR] Minimization function incorrect. Must inherits from MinFunction. Aborting...\n")
      
      private$name <- name
      private$min.function <- min.function
      private$dependences <- dependences
      loadPackages(private$dependences)
    },
    execute = function(fitness = NULL){
      stop("[WeightsOptimizer][ERROR] Method 'execute' is abstract. Should be implemented in inherited class\n")
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
