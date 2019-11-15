TrainSet <- R6Class(
  classname = "TrainSet",
  portable = TRUE,
  public = list(
    initialize = function(cluster.dist, class.name, class.values, positive.class) {

      if ( !is.list(cluster.dist) && length(cluster.dist) == 0 ) {
        stop("[TrainSet][ERROR] Clusters empty or incorrect (must be a list). Aborting.")
      }

      if ( !(positive.class %in% as.character(unique(class.values))) ) { 
        stop("[TrainSet][ERROR] Positive Class is incorrect. Must be '",
             paste0(as.character(unique(class.values))))
      } 

      private$clusters <- cluster.dist
      private$positive.class <- positive.class
      private$class.name <- class.name
      private$class.values <- class.values
    },
    getPositiveClass = function() { private$positive.class },
    getClassName = function() { private$class.name },
    getClassValues = function() { private$class.values },
    getFeatureNames = function(num.cluster) {
      if ( any( missing(position), !is.numeric(position),
                !position %in% c(1:length(private$clusters)) )) {
        stop("[TrainSet][ERROR] Position not defined or incorrect.",
             "Must be included between 1 and ",length(private$clusters))
      }
      names(private$clusters[[num.cluster]])
    },
    getFeatureValues = function(num.cluster){
      if ( any(missing(num.cluster),!is.numeric(num.cluster),
               !num.cluster %in% c(1:length(private$clusters))) ) 
      {
        stop("[TrainSet][ERROR] Position not defined or incorrect.",
             "Must be included between 1 and ",length(private$clusters))
      }
      private$clusters[[num.cluster]]
    },
    getInstances = function(num.cluster){
      if ( any(missing(num.cluster),!is.numeric(num.cluster),
               !num.cluster %in% c(1:length(private$clusters))) ) 
      {
        stop("[TrainSet][ERROR] Position not defined or incorrect.",
             "Must be included between 1 and ",length(private$clusters))
      }
      instances <- cbind(private$clusters[[num.cluster]],private$class.values)
      names(instances)[length(instances)] <- private$class.name
      instances
    },
    getNumClusters = function() { length(private$clusters) }
  ),
  private = list(
    clusters = list(),
    class.name = NULL,
    class.values = NULL,
    positive.class = NULL
  )
)