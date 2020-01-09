SOOData <- R6::R6Class(
  classname = "SOOData",
  inherit = MinResult,
  portable = TRUE,
  public = list(
    initialize = function (min.objective,optimx, n.positive, n.negative){
      if(!inherits(optimx,"optimx") )
        stop("[",class(self)[1],"][FATAL] Optimx must inherit from optimx class")

      row <- optimx[which(optimx$value==min(optimx$value))[1],]
      super$initialize(paste0(rownames(row)," - ",min.objective),n.positive, n.negative)

      private$optimx <- optimx
      private$solution <- list(result=row$value, values=row[1:which(names(private$optimx)=="value")-1]  )
      private$conf.matrix <- NULL
    },
    getValues = function(){ private$solution },
    getNumPositives = function(){ private$n.positive },
    getNumNegatives = function(){ private$n.negative },
    getConfusionMatrix = function() {
      stop("[",private$name,"][FATAL] Method 'getConfusionMatrix' is abstract. Must be implemented in inherited class")
    },
    getFP = function(){
      if (is.null(private$conf.matrix)) self$getConfusionMatrix()
      private$conf.matrix$table[2,1]
    },
    getFN = function(){
      if (is.null(private$conf.matrix)) self$getConfusionMatrix()
      private$conf.matrix$table[1,2]
    },
    getTP = function(){
      if (is.null(private$conf.matrix)) self$getConfusionMatrix()
      private$conf.matrix$table[2,2]
    },
    getTN = function(){
      if (is.null(private$conf.matrix)) self$getConfusionMatrix()
      private$conf.matrix$table[1,1]
    }
  ),
  private = list(
    optimx = NULL,
    solution = NULL,
    conf.matrix = NULL
  )
)