FIterator <- R6Class(
  classname = "FIterator",
  portable = TRUE,
  public = list(
    initialize = function (config.params, chunk.size, verbose){
      private$params <- config.params
      private$chunk.size  <- chunk.size
      private$read.chunk  <- chunk.size
      private$verbose <- verbose
      private$con <- file(description = config.params$file.path, open="r")
      private$start <- config.params$start
      private$index <- 0
    },
    getNext = function(){
      if( is.null(private$con) || !isOpen(private$con) || self$isLast() ){
        return(NULL)
      }
      data.chunk <- read.table(private$con,
                               nrows= private$chunk.size,
                               skip= private$start, header=FALSE,
                               sep= private$params$sep,
                               col.names= private$params$feature.names,
                               stringsAsFactors=FALSE)

      if(isTRUE(private$verbose)){
        initial <- (private$index * private$chunk.size) + private$start
        message("[",class(self)[1],"][INFO] Readed lines ",
                initial," to ",initial + private$chunk.size,
                " [",format(private$chunk.size,scientific = FALSE),"]")
      }
      private$start <- 0
      private$index <- private$index + 1
      private$read.chunk <- nrow(data.chunk)
      data.chunk
    },
    isLast = function(){ private$read.chunk != private$chunk.size },
    finalize = function(){
      if( !is.null(private$con) && isOpen(private$con) ){
        message("[",class(self)[1],"][INFO] Closing connection")
        close(private$con)
        private$con <- NULL
      }else{
        message("[",class(self)[1],"][INFO] Finalize")
      }
    }
  ),
  private = list(
    params = NULL,
    chunk.size = NULL,
    verbose = FALSE,
    con = NULL,
    start = NULL,
    index = 0,
    read.chunk = 0
  )
)