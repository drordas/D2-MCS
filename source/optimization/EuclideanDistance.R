EuclideanDistance <- R6::R6Class(
  classname = "EuclideanDistance",
  inherit = ParetoDistance,
  portable = TRUE,
  public = list(
    initialize = function (){
      super$initialize("Euclidean Distance")
    },
    compute = function(pareto.front){
      super$compute(pareto.front)

      distances <- apply(pareto.front,1,function(row){ sqrt(sum(row^2)) })
      private$pareto.front <- pareto.front[ which(distances==min(distances)), ]
    },
    solve.ties = function(){
      private$pareto.front[1,]
    }
  )
)