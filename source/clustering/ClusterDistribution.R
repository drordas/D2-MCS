library("R6")
library("tictoc")
ClusterDistribution <- R6Class(
  classname = "ClusterDistribution",
  portable = TRUE,                   
  public = list(
    initialize = function(){
      distribution <- vector(mode="list")
    },
    add = function(instances, classIndex){
      if (missing(instances) || missing(classIndex) || is.null(instances) || 
          !is.data.frame(instances) || !is.numeric(classIndex) || !classIndex %in% c(1:ncol(instances)) )
        stop("[ClusterDistribution][ERROR] Instances and/or classIndex have incorrect values\n")
      private$distribution <- append( private$distribution, Subset$new( dataset = instances, classIndex = classIndex) )
    },
    removeAt = function(position){
      if (length(private$distribution) == 0)
        stop("[ClusterDistribution][ERROR] Distribution is empty\n")

      if(missing(position) || !is.numeric(position) || !position %in% c(1:length(private$distribution) ) )
        stop("[ClusterDistribution][ERROR] Position not defined or incorrect. Must be included in: 1 <= position<=",length(private$distribution),"\n")
      cat("[ClusterDistribution][INFO] Removing group at position ",position,"\n")
      private$distribution <- private$distribution[-position]
    },
    getAt = function(position){
      if (length(private$distribution) == 0)
        stop("[ClusterDistribution][ERROR] Distribution is empty\n")
      
      if(missing(position) || !is.numeric(position) || !position %in% c(1:length(private$distribution) ) )
        stop("[ClusterDistribution][ERROR] Position not defined or incorrect. Must be included in: 1 <= position<=",length(private$distribution),"\n")
      
      private$distribution[[position]]
    },
    getNumClusters = function(){
      length(private$distribution)
    }
  ),
  private = list(
    distribution = NULL
  )
)