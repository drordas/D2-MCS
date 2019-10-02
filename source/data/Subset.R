Subset <- R6Class(
  classname = "Subset",
  portable = TRUE,
  public = list(
    initialize = function(dataset, class.index, class.values, positive.class) {
      if (is.null(dataset) || is.null(class.index)) { stop(red("[Subset][ERROR] Dataset not defined or empty")) }
      if (!is.character(positive.class)) { stop(red("[Subset][ERROR] Positive Class not defined")) } 
      
      private$data <- dataset
      private$class.index <- class.index
      private$positive.class <- positive.class
      private$class.name <- names(private$data)[private$class.index]
      private$class.values <- class.values
      private$feature.names <- -(private$class.index)
      
      private$binary.features <- Filter(function(x) { !( any(as.integer(unique(x)) != unique(x)) || 
                                                         length(unique(x)) > 2 || min(x) != 0 || 
                                                         max(x) != 1)},private$data[,-private$class.index] )
      private$real.features <- Filter(function(x) { ( any(as.integer(unique(x)) != unique(x)) || 
                                                      length(unique(x)) > 2 || min(x) != 0 || 
                                                      max(x) != 1)},private$data[,-private$class.index] )
    },
    getAllFeatures = function() { private$data[,-private$class.index] },
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
    getPositiveClass = function() { private$positive.class },
    # setPositiveClass = function(positive.class) { 
    #   if( positive.class %in% private$class.values){
    #     private$positiveClass <- positive.class 
    #   }else{ message(red("[Subset][ERROR] Positive class value not found. Task not done\n")) }
    # } NO SENSE. BETTER CHANGE CLASS VALUES ON DATASET CLASS TO GENERATE UNIFORMIZED SUBSETS.
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