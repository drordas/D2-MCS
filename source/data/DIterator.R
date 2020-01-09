DIterator <- R6::R6Class(
  classname = "DIterator",
  portable = TRUE,
  public = list(
    initialize = function (data, chunk.size, verbose){
      private$chunk.size  <- chunk.size
      private$read.chunk  <- chunk.size
      private$verbose <- verbose
      private$start <- 0
      private$end <- 0
      private$data <- data
    },
    getNext = function(){
      if( self$isLast()  ) return (NULL)
      if( (private$start + private$chunk.size) > nrow(private$data) ){
        private$end <- private$start + (nrow(private$data) - private$start)
        private$read.chunk <- seq(private$start,private$end)
      }else {
        private$end <- (private$start + private$chunk.size)
        private$read.chunk <- seq(private$start,private$end)
      }

      data.chunk <- private$data[private$read.chunk,]

      if(isTRUE(private$verbose)){
        message("[",class(self)[1],"][INFO] Readed lines ",
                private$start," to ",private$end,
                " [",format(private$end-private$start,scientific = FALSE),"]")
      }
      private$start <- private$end + 1
      data.chunk
    },
    isLast = function(){ private$end >= nrow(private$data) },
    finalize = function(){}
  ),
  private = list(
    chunk.size = NULL,
    verbose = FALSE,
    start = NULL,
    end = 0,
    read.chunk = 0,
    data = 0
  )
)