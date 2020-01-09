MinimumFP <- R6::R6Class(
  classname = "MinimumFP",
  inherit = ParetoDistance,
  portable = TRUE,
  public = list(
    initialize = function (){
      super$initialize("Minimum FP")
    },
    compute = function(pareto.front){
      super$compute(pareto.front)
      
      distances <- apply(pareto.front,1,function(row){ row["FP"] })
      private$pareto.front <- pareto.front[ which(distances==min(distances)), ]
    },
    solve.ties = function(){
      private$pareto.front[1,]
    }
  )
)