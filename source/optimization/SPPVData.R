SPPVData <- R6::R6Class(
  classname = "SPPVData",
  portable = TRUE,
  inherit = SOOData,
  public = list(
    initialize = function (optimx, n.positive, n.negative){
      super$initialize("PPV", optimx, n.positive, n.negative)
      private$conf.matrix <- NULL
    },
    getConfusionMatrix = function( pareto.distance = NULL) { 
      stop("PENDING IMPLEMENTATION\n")
    }
  ),
  private = list(
    conf.matrix = NULL,
    pareto.distance = NULL
  )
)