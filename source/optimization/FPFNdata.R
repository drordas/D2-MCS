FPFNdata <- R6::R6Class(
  classname = "FPFNdata",
  portable = TRUE,
  inherit = MOOData,
  public = list(
    initialize = function (alg.name, pareto.front, population, n.positive, n.negative){
      super$initialize(paste0(alg.name," - FPFN"), pareto.front, population, 
                       n.positive, n.negative)
      private$conf.matrix <- NULL
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
    getValues = function(pareto.distance = NULL){
      private$pareto.distance <- pareto.distance
      solution <- super$getBestSolution(private$pareto.distance)

      pareto.values <- private$pareto.front[rownames(solution),"p.front"]
      population.index <- private$population[[pareto.values]]
      join <- inner_join(population.index,solution,by=c("FP","FN"))
      join[,-which(names(join) %in% c("FP","FN") )]
    }
  ),
  private = list(
    conf.matrix = NULL,
    pareto.distance = NULL
  )
)