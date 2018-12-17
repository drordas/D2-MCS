library("R6")
source("Cluster.R")
BinaryFisherClustering <- R6Class(
  classname = "BinaryFisherClustering",
  portable = TRUE,
  inherit = Cluster,
  public = list(
    initialize = function(dataset, maxClusters = 50){
      if (class(dataset)[1] != "Subset" || class(dataset)[2]!= "R6" )
        stop("[CLUSTER][Error] Input corpus should be R6 Subset type\n")
      
      super$initialize(name="BinaryFisherClustering", maxClusters = maxClusters )
      private$data.unbinary <- NULL
      private$all.distribution <- NULL
      private$best.distribution <- NULL
      private$class <- dataset$getClass()
      private$className <- dataset$getClassName()
      private$dataset <- dataset$getInstances(ignore.class = TRUE)
      private$heuristic <- super$defaultHeuristic
    },
    execute = function( heuristic= NULL){
      if( !missing(heuristic) && is.function(heuristic) )
        private$heuristic <- heuristic
      
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
    plot = function(dir.path = NULL, file.name = NULL){
      summary <- data.frame(k=private$all.distribution$getClusterDist()[,1],
                            dispersion= private$all.distribution$getClusterDist()[,2], 
                            row.names = NULL, check.names = FALSE)
      min <- data.frame(x=summary[which.min(summary[,2]), ][, 1],y= min(summary[,2]))
      max <- data.frame(x=summary[which.max(summary[,2]), ][, 1],y= max(summary[,2]))
      diff <- abs(max$y - min$y)
      ggplot(summary, aes(k,dispersion)) + geom_point(aes(color = dispersion),position = position_jitter()) + 
        scale_color_continuous(name="",low = "blue", high = "red", guide = FALSE ) + 
        geom_text_repel( aes(x,y,label=sprintf("%s",format(min$y,digits = 2, scientific = TRUE))), 
                         min, hjust=0.5, vjust=0, point.padding = 0.25, color='blue', size=3 ) +
        geom_text_repel( aes(x,y,label=sprintf("%s",format(max$y,digits = 2, scientific = TRUE))), 
                         max, hjust=0.5, vjust=1, point.padding = 0.25, color='red', size=3  ) + 
        scale_y_continuous( name="Dispersion (represented as logaritmic scale)", trans = "log", breaks = c( min$y,max$y ) ) + 
        scale_x_continuous(name="Number of clusters", breaks=seq(from=2,to=nrow(summary) + 1)) + 
        labs(title="Binary Data") + theme_light() + theme(axis.text.x = element_text(angle = 90, hjust = 0.5))
      
      if( !is.null(dir.path) ){
        if(!dir.exists(dir.path)) dir.create(dir.path,recursive = TRUE)
        ggsave(paste0(file.path(dir.path,file.name),".pdf"),device="pdf",plot=last_plot(), limitsize = FALSE)
        cat("[",super$getName(),"][INFO] Plot has been succesfully saved at: ",paste0(file.path(dir.path,file.name),".pdf"),"\n",sep="")
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
          distribution <- rbind( distribution, data.frame( cluster=cluster+1, dist=I(list(names(private$data.unbinary) )) ) )
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
    
    createSubset = function(cluster = NULL, subset = NULL, na.rm = FALSE){
      if(!is.logical(na.rm))
        cat("[BinaryCluster][Warning] na.rm paraneter must be a logical value (TRUE or FALSE). Assuming na.rm = TRUE.\n")
      if( is.null(private$all.distribution) ){
        cat("[BinaryCluster][Warning] Function 'execute()' must be called first. Automatically run execute function\n")
        self$execute()
      }
      if( missing(subset) || is.null(subset) || !"Subset" %in% class(subset)  )
        stop("[BinaryCluster][ERROR] Subset parameter must be defined as 'Subset' object\n")
      if( is.null(cluster) || missing(cluster) || !is.numeric(cluster) || (is.numeric(cluster) && !cluster %in% c(private$min:private$max ) ) ){
        cat("[BinaryCluster][WARNING] Incorrect cluster parameter. Should be between: ",private$min," <= cluster <= ",private$max,"\n", sep="")
        cat("                         Assuming best cluster configuration (",private$all.distribution$getBestK(),")\n", sep="")
        cluster <- private$all.distribution$getBestK()
      }
      distribution <- self$getDistribution(cluster = cluster,includeClass = "NONE")
      cluster.dist <- ClusterDistribution$new()
      if(na.rm){
        invisible(lapply(distribution, function(group){
          group.features <- subset$getInstances( features = c(unlist(group)) )
          sd.result <- apply(group.features,2,sd,na.rm=TRUE)
          sd.result <- sd.result[-which(sd.result==0,arr.ind = TRUE)]
          if(length(sd.result!=0)){
            cluster.dist$add(subset$getInstances(features = c(subset$getClassName(),names(sd.result)) ), classIndex = 1)
          }
          else{
            cluster.dist$add(subset$getInstances(features = c(subset$getClassName(),unlist(group)) ), classIndex = 1)
          }
        } ) )
      }
      else{
        invisible(lapply(distribution, function(group){
          cluster.dist$add(subset$getInstances(features = c(subset$getClassName(),unlist(group)) ), classIndex = 1)
        } ) )
      }
      cluster.dist
    }
  ),
  
  private = list(
    computeFisherTable = function(corpus){
      fisherTest <- sapply(corpus, function(c){ fisher.test(table(c,private$class))$p.value } )
    },
    
    computeFisherTest = function(corpus){
      binary.data <- ClusterData$new()
      fisher.table <- private$computeFisherTable(corpus)
      fisher.index <- order(fisher.table, decreasing = TRUE)
      fisher.size <- length(fisher.table)
      totalGroups <- 2:super$getMaxClusters()
      for(k in totalGroups){
        clustering <- private$heuristic(fisher.table, k)
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
      corpus[,sapply(corpus, function(c){length(unique(c)) >= 2 } )]
    },
    getUnnecesary = function(corpus){
      names(corpus[,!sapply(corpus, function(c){length(unique(c)) >= 2 }) ])
    },
    data.unbinary = NULL,
    dataset = NULL,
    class = NULL,
    className = NULL,
    all.distribution = NULL,
    best.distribution = NULL,
    min = NULL,
    max = NULL,
    heuristic = NULL
  )
)