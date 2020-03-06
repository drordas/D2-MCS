HDSubset <- R6::R6Class(
  classname = "HDSubset",
  portable = TRUE,
  public = list(
    initialize = function(file.path, feature.names, feature.id, start.at=0,
                          sep= ",", chunk.size) {
      if (is.null(feature.names) || ncol(feature.names) ==0){
        stop("[",class(self)[1],"][FATAL] Dataset has not being preloaded. ",
             "Aborting...")
      }
      private$chunk.size <- chunk.size
      private$conection <- NULL
      private$file.path <- file.path
      private$feature.names <- names(feature.names)
      private$index <- 0
      private$sep <- sep
      if(isFALSE(feature.id))
        private$feature.id <- NULL
      else private$feature.id <- feature.id

      if(!is.numeric(start.at) || start.at <0){
        message("[",class(self)[1],"][WARNING] Starting point must be a ",
                "non-negative numeric value. Assuming 0 as default value")
        private$start.at <- 0
      }else private$start.at <- start.at
    },
    getColumnNames = function() { private$feature.names },
    getNcol = function() {length(private$feature.names)},
    getID = function() {private$feature.names[private$feature.id] },
    getIterator = function(chunk.size=private$chunk.size,verbose=FALSE) {
      if(!is.numeric(chunk.size)){
        message("[",class(self)[1],"][WARNING] Chunk size is not valid. ",
                "Assuming default value")
        chunk.size <- private$chunk.size
      }

      if(!is.logical(verbose)){
        message("[",class(self)[1],"][WARNING] Verbose type is not valid ",
                "Assuming 'FALSE' as default value")
        verbose <- FALSE
      }

      it.params <- list( file.path=private$file.path,
                         feature.names=private$feature.names,
                         start=private$start.at, sep=private$sep,
                         col.names= self$getColumnNames())
      FIterator$new(it.params, chunk.size, verbose=verbose)
    },
    finalize = function() {
      if(!is.null(private$conetion))
        close(private$conection)
      private$conection <-NULL
    },
    isBlinded = function() {TRUE}
  ),
  private = list(
    feature.names = NULL,
    file.path = NULL,
    conection = NULL,
    index = 0,
    start.at = 0,
    sep=0,
    data.chunk = NULL,
    chunk.size = 100000,
    feature.id = NULL
  )
)