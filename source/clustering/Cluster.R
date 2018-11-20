library("R6")
Cluster <- R6Class(
  classname = "Cluster",
  portable = TRUE,                   
  public = list(
    initialize = function(name = NULL, dependences = NULL, maxClusters = 50){
      if( is.character(name) )
        private$name <- name
      else stop("[CLUSTER][ERROR] Clustering not defined or incorrect\n")
      
      lapply(dependences, function(pkg){
        if (! pkg %in% installed.packages() ){
          cat("[",private$name,"][WARNING] ",pkg," package required and not installed. Performing installation...\n",sep="")
          suppressMessages(install.packages(pkg,repos="https://ftp.cixug.es/CRAN/", dependencies = TRUE))
        }
        if(!pkg %in% loaded_packages() ){
          cat("[",private$name,"][WARNING] Loading ",pkg," package....\n",sep="")
          library(pkg,character.only = TRUE,warn.conflicts = FALSE, quietly = TRUE)
        } 
      })

      private$maxClusters <- maxClusters
    },
    isBinary = function(column) {
      unique = unique(column)
      if ( !is.numeric(column) | any(is.na(column)) )
        return (FALSE)
      else return(!(any(as.integer(unique) != unique) || length(unique) > 2 || min(column) != 0 || max(column) != 1))
    },
    execute = function(...){
      stop("[CLUSTER][ERROR] Function 'execute' must be implemented\n")
    },
    getMaxClusters = function(){
      private$maxClusters
    },
    setMaxClusters = function(max){
      if(max > 1) 
        private$maxClusters <- max
      else cat("[CLUSTER][ERROR] number of clusters must be greater than 1\n. Assuming default value\n")
    },
    getNumClusters = function(){
      stop("[CLUSTER][ERROR] Function 'getNumClusters must be implemented in inherited class\n'")
    },
    getDistribution = function(group = NULL, includeClass = FALSE, classPosition = NULL){
      stop("[CLUSTER][ERROR] Function 'gestBestDistribution must be implemented in inherited class\n'")
    },
    plot = function(dir.path = NULL, file.name = NULL){
      stop("[CLUSTER][ERROR] Function 'printPlot must be implemented in inherited class\n'")
    },
    getName = function(){
      private$name
    }
  ),
  
  private = list(
    defaultHeuristic = function(corpus, k){
      sorted.corpus <- sort(corpus, decreasing = TRUE)
      cluster.index <- vector(length = length(corpus) )
      cluster.sum <- vector(length = k )
      cluster.sum[1:k] <- 0
      aux <- c()
      for(i in 1:length(sorted.corpus)){
        aux[1:k] <- sorted.corpus[i]
        cluster <- (which.min((cluster.sum+aux)-(max(cluster.sum))))
        cluster.index[i] <- cluster
        cluster.sum[cluster] <- cluster.sum[cluster]+sorted.corpus[i]
      }
      cluster.index
    },
    maxClusters = 50,
    name = NULL
  )
)