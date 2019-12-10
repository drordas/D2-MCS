Subset <- R6Class(
  classname = "Subset",
  portable = TRUE,
  public = list(
    initialize = function(dataset, feature.id=NULL, class.index,
                          class.values, positive.class){
      if ( any(is.null(dataset), nrow(dataset) == 0, !is.data.frame(dataset)) ) {
        stop("[",class(self)[1],"][ERROR] Dataset empty or incorrect ",
             "(must be a data.frame). Aborting.")
      }

      if( missing(class.index) || is.null(class.index) ||
          !(class.index %in% c(1:nrow(dataset))) ){
        stop("[",class(self)[1],"][ERROR] Class index missing or incorrect. Must be between 1 and",
              nrow(dataset)," Aborting.")
      }

      if (!(positive.class %in% as.character(unique(dataset[,class.index])) ) ) {
        stop("[",class(self)[1],"][ERROR] Positive Class is incorrect. Must be '",
             paste0(as.character(unique(dataset[,class.index]))))
      }

      if (! all(class.values %in% as.character(unique(dataset[,class.index])) ) ) {
        stop("[",class(self)[1],"][ERROR] Class values missmatch. Must be '",
             paste0(as.character(unique(dataset[,class.index]))))
      }

      private$data <- dataset
      private$class.index <- class.index
      private$feature.id <- feature.id
      private$positive.class <- positive.class
      private$class.name <- names(private$data)[private$class.index]
      private$class.values <- class.values
      private$feature.names <- -(private$class.index)
    },
    getFeatureNames = function() { names(private$data[,-private$class.index]) },
    getFeatures = function (feature.names=NULL){
      if(is.vector(feature.names) && length(feature.names) > 0){
        private$data[,intersect(names(private$data[,-private$class.index]), feature.names)]
      }else { private$data[,-private$class.index] }
    },
    getID = function(){
      if(!is.null(private$feature.id))
        private$feature.names[private$feature.id]
      else private$feature.id
    },
    getIterator = function(chunk.size=private$chunk.size, verbose=FALSE) {
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
      DIterator$new(data = private$data,chunk.size = chunk.size,
                    verbose = verbose)
    },
    getClassValues = function() { private$data[, private$class.index] },
    getClassIndex = function() { private$class.index },
    getClassName = function() { private$class.name },
    getNcol = function() { ncol(private$data) },
    getNrow = function() { nrow(private$data) },
    getPositiveClass = function() { private$positive.class },
    isBlinded = function() {FALSE}
  ),
  private = list(
    data = NULL,
    class.index = NULL,
    class.name = NULL,
    feature.names = NULL,
    class.values = NULL,
    positive.class = NULL,
    chunk.size = 10000,
    feature.id = NULL
  )
)