ClusterData <- R6Class(
  classname = "ClusterData",
  portable = TRUE,                   
  public = list(
    initialize = function(){
      private$bestK <- integer(1)
      private$clustered <- data.frame( k=integer(), homogeneity=numeric(), dist=I(list()) )
    },
    setClusterDist = function(distribution){
      private$clustered <- clustered
    },
    addNewCluster = function(k,homogeneity,distribution){
      private$clustered <- rbind(private$clustered,
                                 data.frame(k=k,homogeneity=homogeneity,dist=I(list(distribution))))
    },
    setBestK = function(k){
      private$bestK <- k
    },
    getBestK = function(){
      private$bestK
    },
    getBestHomogeneity = function(){
      private$clustered$homogeneity[which.min(private$clustered$homogeneity)]
    },
    getClusterDist = function(){
      private$clustered
    }
  ),
  private = list(
    bestK = NA,
    clustered = NA
  )
)