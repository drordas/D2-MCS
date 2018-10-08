library("R6")
library("caret")
library("tools")
source("Cluster.R")
BinaryFisherClustering <- R6Class(
  classname = "BinaryFisherClustering",
  portable = TRUE,
  inherit = Cluster,
  public = list(
    initialize = function(dataset, maxClusters = 50){
      if (class(dataset)[1] != "Subset" || class(dataset)[2]!= "R6" )
         stop("[CLUSTER][Error] Input corpus should be R6 Subset type\n")
      super$initialize( maxClusters )
      private$data.unbinary <- NULL
      #private$data.binary <- NULL
      private$all.distribution <- NULL
      private$best.distribution <- NULL
      private$class <- dataset$getClass()
      private$className <- dataset$getClassName()
      private$dataset <- dataset$getInstances(ignore.class = TRUE)
    },
    execute = function(){
      binaryIndex <- sapply( private$dataset, function(e){
        ( super$isBinary(e) || length( unique(e) ) == 2) 
      })

      if( dim(private$dataset[,!binaryIndex])[2] > 0 ){
        private$data.unbinary <- private$removeUnnecesary(private$dataset[,!binaryIndex])
      }
      
      onlyBinary <- private$dataset[,binaryIndex]
      private$all.distribution <- private$computeFisherTest(onlyBinary)
      
      private$best.distribution <- data.frame(cluster=integer(),features=I(list()))
      aux <- unlist(private$all.distribution$getClusterDist()[private$all.distribution$getClusterDist()$k==private$all.distribution$getBestK(), ]$dist)

      for(i in 1:private$all.distribution$getBestK() ){
        private$best.distribution <- rbind(private$best.distribution, 
                                           data.frame(cluster=i,dist=I(list(names(aux[aux==i])))) )
      }

      if(nrow(private$data.unbinary) > 0 ){
        private$best.distribution <- rbind( private$best.distribution,
                                            data.frame( cluster=(nrow(private$best.distribution)+1),
                                                        dist=I(list(names(private$data.unbinary) )) ) )
      }
      
      private$min <- min(private$all.distribution$getClusterDist()$k)
      private$max <- max(private$all.distribution$getClusterDist()$k)
    },
    plot = function(file.name = NULL){
      summary <- data.frame(k=private$all.distribution$getClusterDist()[,1],
                            dispersion=private$all.distribution$getClusterDist()[,2], 
                            row.names = NULL)
      min <- data.frame(x=summary[which.min(summary[,2]), ][, 1],y= min(summary[,2]))
      max <- data.frame(x=summary[which.max(summary[,2]), ][, 1],y= max(summary[,2]))
      ggplot(summary, aes(k,dispersion)) + geom_line() + geom_point() +
          geom_point(aes(x,y), min, fill="transparent", color="blue", shape=21, size=3,stroke=1) + 
          geom_text(aes(x,y,label=sprintf("%.3f",y)), min, hjust=-0.45, color='blue' ) +
          geom_point(aes(x,y), max, fill="transparent", color="red", shape=21, size=3,stroke=1) + 
          geom_text(aes(x,y,label=sprintf("%.3f",y)), max, hjust=-0.45, color='red' ) + 
          scale_y_continuous(limits=c(min(summary$dispersion), max( summary$dispersion) )) + 
          scale_x_continuous(breaks=seq(from=2,to=nrow(summary) + 1)) + 
          labs(x = "Number of clusters", y = "Dispersion")
      
      last_plot()
      if( !is.null(file.name) ){
        save.path <- file.path(getwd(),"plots",paste0(file.name,".pdf") )
        cat("[BinaryCluster][INFO] Plot has been succesfully saved at: ",save.path,"\n",sep="")
        ggsave(save.path,plot=last_plot(),device="pdf", limitsize = FALSE)
      } 
    },
    getDistribution = function(cluster, group , includeClass = "NONE" ){
      if( is.null(private$best.distribution) || is.null(private$all.distribution) ){
        warning("[BinaryCluster][Warning] Function 'execute()' must be called first. Automatically run execute function\n")
        self$execute()
      }
      
      if( !toupper(includeClass) %in% c("NONE","BEGIN","END") ){
        cat("[BinaryCluster][INFO] Class parameter not included. Assuming class not included\n")
        class <- "NONE"
      }else class <- toupper(includeClass)
      
      if( missing(cluster) || ( is.numeric(cluster) && (cluster == private$all.distribution$getBestK()) ) ){
        
        switch (class,
            "NONE" = { final.distr <- private$best.distribution[,2] },
            "END" =  { final.distr <- lapply(private$best.distribution[,2], function(x) {append(x,private$className)} ) },
            "BEGIN" = { final.distr <- lapply(private$best.distribution[,2], function(x) {append(x,private$className,0)} ) }
        )
        
        if( !missing(group) && !is.null(group) && is.numeric(group) && group <= length(final.distr))
          final.distr[[group]]
        else final.distr
        
      }else{
        distribution <- data.frame(cluster=integer(),features=I(list()))
        aux <- unlist(private$all.distribution$getClusterDist()[private$all.distribution$getClusterDist()$k==cluster, ]$dist)
        for( i in 1:cluster )
          distribution <- rbind(distribution, data.frame(cluster=i,dist=I(list(names(aux[aux == i])))) )
        
        if(nrow(private$data.unbinary) > 0 )
          distribution <- rbind( distribution, data.frame( cluster=cluster+1,
                                                           dist=I(list(names(private$data.unbinary) )) ) )
        switch (class,
          "NONE" = { final.distr <- distribution[,2] },
          "END" = { final.distr <- lapply(distribution[,2], function(x) {append(x,private$className)} ) },
          "BEGIN" = { final.distr <- lapply(distribution[,2], function(x) {append(x,private$className,0)} ) }
        )
        
        #ifelse(!missing(group) && !is.null(group) && is.numeric(group) && group <= length(final.distr), final.distr[[group]], final.distr ) 
        if( !missing(group) && !is.null(group) && is.numeric(group) && group <= length(final.distr))
          final.distr[[group]]
        else final.distr
      }
    },
    createSubset = function(cluster = NULL, subset = NULL){
      if( is.null(private$all.distribution) ){
        cat("[BinaryCluster][Warning] Function 'execute()' must be called first. Automatically run execute function\n")
        self$execute()
      }
      
      if( missing(subset) || is.null(subset) || !"Subset" %in% class(subset)  )
        stop("[BinaryCluster][ERROR] Subset parameter must be defined as 'Subset' object\n")
      
      if( is.null(cluster) || missing(cluster) || !is.numeric(cluster) || 
          (is.numeric(cluster) && !cluster %in% c(private$min:private$max ) ) ){
        cat("[BinaryCluster][WARNING] Incorrect cluster parameter. Should be between: ",private$min," <= cluster <= ",private$max,"\n", sep="")
        cat("                         Assuming best cluster configuration (",private$all.distribution$getBestK(),")\n", sep="")
        cluster <- private$all.distribution$getBestK()
      }
      
      distribution <- self$getDistribution(cluster = cluster,includeClass = "NONE")
      cluster.dist <- ClusterDistribution$new()
      invisible(lapply(distribution, function(group){
        cluster.dist$add(subset$getInstances(features = c(subset$getClassName(),unlist(group)) ), classIndex = 1)
      } ) )
      cluster.dist
    }
    # },
    # getNumClusters = function(){
    #   if( is.null(private$distribution) ){
    #     warning("[BinaryCluster][Warning] Function 'execute()' must be called first. Automatically run execute function\n")
    #     self$execute()
    #   }
    #   nrow(private$distribution)
    # }
  ),
  private = list(
    computeFisherTable = function(corpus){
      fisherTest <- sapply(corpus, function(c){ fisher.test(table(c,private$class))$p.value } )
    },
    computeFisherTest = function(corpus){
      binary.data <- BinaryFisherData$new()
      fisher.table <- private$computeFisherTable(corpus)
      fisher.index <- order(fisher.table, decreasing = TRUE)
      fisher.size <- length(fisher.table)
      totalGroups <- 2:super$getMaxClusters()
      for(k in totalGroups) {
        clustering <- rep(c(1:k,(k:1)),fisher.size/(2*k)+1)[1:fisher.size]
        cluster <- integer(length = length(fisher.table))
        names(cluster) <- names(corpus)
        sumGroup <- vector(k,mode="list")
        
        for (i in 1:k){ 
          sumGroup[[i]] <- fisher.table[fisher.index[clustering==i]]
          cluster[fisher.index[clustering==i]] <- i
        }
        groupMeasure <- lapply(sumGroup,sum)
        
        deltha <- max(unlist(groupMeasure)) - min(unlist(groupMeasure))
        binary.data$addNewCluster(k,deltha,cluster)
      }
      binary.data$setBestK(  binary.data$getClusterDist()[which.min(binary.data$getClusterDist()[,2]),1]  )
      binary.data
    },
    removeUnnecesary = function(corpus){
      corpus[,sapply(corpus, function(c){
              length(unique(c)) >= 2 } )]
    },
    getUnnecesary = function(corpus){
      names(corpus[,!sapply(corpus, function(c){
            length(unique(c)) >= 2 }) ])
    },
    data.unbinary = NULL,
    dataset = NULL,
    class = NULL,
    className = NULL,
    all.distribution = NULL,
    best.distribution = NULL,
    min = NULL,
    max = NULL
  )
)