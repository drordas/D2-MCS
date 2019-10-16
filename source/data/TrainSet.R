TrainSet <- R6Class(
  classname = "TrainSet",
  portable = TRUE,
  public = list(
    initialize = function(clusters, class.name, class.values, positive.class) {

      if ( !is.list(clusters) && length(clusters) == 0 ) {
        stop(red("[TrainSet][ERROR] Clusters empty or incorrect (must be a list). Aborting."))
      }

      if ( !(positive.class %in% as.character(unique(class.values))) ) { 
        stop(red("[TrainSet][ERROR] Positive Class is incorrect. Must be '",
                  paste0(as.character(unique(class.values)))))
      } 
      
      if ( !all(class.values %in% as.character(unique(class.values))) ) { 
        stop(red("[TrainSet][ERROR] Class values missmatch. Must be '",
                  paste0(as.character(unique(class.values)))))
      } 
      
      private$clusters <- clusters
      private$positive.class <- positive.class
      private$class.name <- class.name
      private$class.values <- class.values
    },
    getClassName = function() { private$class.name },
    getClassValues = function() { private$data[, private$class.index] },
    getFeaturesNames = function(position) {
      if ( missing(position) || !is.numeric(position) || !position %in% c(1:length(private$clusters)) ) {
        stop("[TrainSet][ERROR] Position not defined or incorrect. Must be included in: 1 <= position <= ",length(private$clusters))
      }
      names(private$clusters[[position]])
    },
    getFeatures = function(position){
      if ( missing(position) || !is.numeric(position) || !position %in% c(1:length(private$clusters)) ) {
        stop("[TrainSet][ERROR] Position not defined or incorrect. Must be included in: 1 <= position <= ",length(private$clusters))
      }
      private$clusters[[position]]
    },
    getNumClusters = function()  {
      length(private$clusters)
    }
  ),
  private = list(
    clusters = list(),
    class.name = NULL,
    class.values = NULL,
    positive.class = NULL
  )
)