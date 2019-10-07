Subset <- R6Class(
  classname = "Subset",
  portable = TRUE,
  public = list(
    initialize = function(dataset, class.index, class.values, positive.class) {
      if ( any(is.null(dataset), nrow(dataset) == 0, !is.data.frame(dataset)) ) { 
        stop("[Subset][ERROR] Dataset empty or incorrect (must be a data.frame). Aborting.")
      }
      
      if( missing(class.index) || is.null(class.index) || 
          !(class.index %in% c(1:nrow(dataset))) ){
        stop("[Subset][ERROR] Class index missing or incorrect. Must be between 1 and",
              nrow(dataset),"\n. Aborting.")
      }

      if (!(positive.class %in% as.character(unique(dataset[,class.index])) ) ) { 
        stop("[Subset][ERROR] Positive Class is incorrect. Must be '",
             paste0(as.character(unique(dataset[,class.index]))),"'\n")
      } 
      
      if (! all(class.values %in% as.character(unique(dataset[,class.index])) ) ) { 
        stop("[Subset][ERROR] Class values missmatch. Must be '",
             paste0(as.character(unique(dataset[,class.index]))),"'\n")
      } 
      
      private$data <- dataset
      private$class.index <- class.index
      private$positive.class <- positive.class
      private$class.name <- names(private$data)[private$class.index]
      private$class.values <- class.values
      private$feature.names <- -(private$class.index)
      
      private$binary.features <- Filter(function(x) { all(levels(factor(x)) %in% c("0","1")) },
                                                      private$data[,-private$class.index] )
      private$real.features <- Filter(function(x) { !all(levels(factor(x)) %in% c("0","1")) },
                                                    private$data[,-private$class.index] )
    },
    getFeatureNames = function() { names(private$data[,-private$class.index]) },
    getFeatures = function(feature.names=NULL){
      if(is.vector(feature.names) && length(feature.names) > 0){
        private$data[,self$getFeatureNames()[match(self$getFeatureNames(), feature.names, nomatch = FALSE)] ]
      }else { private$data[,-private$class.index] }
    },
    getBinaryFeatures = function() { 
      if ( nrow(private$binary.features) == 0 ){
        message(yellow("[Subset][WARNING] Dataset without Binary Features. Returning empty data.frame"))
      }
      private$binary.features 
    },
    getRealFeatures = function() { 
      if ( nrow(private$real.features) == 0 ){
        message(yellow("[Subset][WARNING] Dataset without Binary Features. Returning empty data.frame"))
      }
      private$real.features 
    },
    getClassValues = function() { private$data[, private$class.index] },
    getClassIndex = function() { private$class.index },
    getClassName = function() { private$class.name },
    getNcol = function() { ncol(private$data) },
    getNrow = function() { nrow(private$data) },
    getPositiveClass = function() { private$positive.class }
  ),
  private = list(
    data = NULL,
    class.index = NULL,
    class.name = NULL,
    feature.names = NULL,
    class.values = NULL,
    binary.features = NULL,
    real.features = NULL,
    positive.class = NULL
  )
)