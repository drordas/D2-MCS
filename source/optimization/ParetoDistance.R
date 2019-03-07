ParetoDistance <- R6Class(
  classname = "ParetoDistance",
  portable = TRUE,
  public = list(
    initialize = function (name){
      if(is.null(name) || !is.character(name))
        stop("[ParetoDistance][ERROR] Named not defined. Aborting")

      private$name <- name
      private$pareto.front <- NULL
    },
    compute = function(pareto.front){
      if(is.null(pareto.front) || !is.data.frame(pareto.front))
        stop("[ParetoDistance][ERROR] Pareto Front not included or incorrect. Must be a data.frame. Aborting")
      
      if( "p.front" %in% colnames(pareto.front) )
        private$pareto.front <- pareto.front[,-(which( colnames(pareto.front) == "p.front" )) ]
      
    },
    solve.ties = function(){
      cat("[ParetoDistance][ERROR] Using default method. Obtainning the first occurence\n")
      return(private$pareto.front[1, ])
    },
    getName = function(){ private$name }
  ),
  private = list(
    name = NULL,
    pareto.front = NULL,
    best = NULL
  )
)