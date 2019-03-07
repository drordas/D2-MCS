FPFNdata <- R6Class(
  classname = "FPFNdata",
  portable = TRUE,
  inherit = MinResult,
  public = list(
    initialize = function (alg.name, pareto.front, n.positive, n.negative){
      super$initialize(paste0(alg.name," - FPFN"), pareto.front, n.positive, n.negative)
      private$conf.matrix <- NULL
      private$pareto.distance <- NULL
    },
    getConfusionMatrix = function( pareto.distance = NULL) { 
      private$pareto.distance <- pareto.distance
      solution <- super$getBestSolution(pareto.distance)
      if( !is.data.frame(solution) || nrow(solution) > 1  )
        stop("[",private$name,"][ERROR] Solution is incorrect. Should be a single-row data.frame")

      fp <- solution[,"FP"]
      fn <- solution[,"FN"]
      df <- matrix( c(private$n.negative - fp,fp, fn,private$n.positive-fn), nrow = 2, ncol = 2, dimnames = list( c("0","1"),c("0","1") ) )
      private$conf.matrix <- caret::confusionMatrix(as.table(df),positive="1")
      private$conf.matrix
    },
    getFP = function(){
      if (is.null(private$conf.matrix)) self$getConfusionMatrix(private$pareto.distance)
      private$conf.matrix$table[2,1]
    },
    getFN = function(){
      if (is.null(private$conf.matrix)) self$getConfusionMatrix(private$pareto.distance)
      private$conf.matrix$table[1,2]
    },
    getTP = function(){
      if (is.null(private$conf.matrix)) self$getConfusionMatrix(private$pareto.distance)
      private$conf.matrix$table[2,2]
    },
    getTN = function(){
      if (is.null(private$conf.matrix)) self$getConfusionMatrix(private$pareto.distance)
      private$conf.matrix$table[1,1]
    }
  ),
  private = list(
    conf.matrix = NULL,
    pareto.distance = NULL
  )
)