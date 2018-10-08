library("R6")
library("tools")
MultiTypeClustering <- R6Class(
  classname = "MultiTypeClustering",
  portable = TRUE,
  inherit = Cluster,
  public = list(
    initialize = function(dataset, maxClusters = 50){
      if (class(dataset)[1] != "Subset" || class(dataset)[2]!= "R6" )
        stop("[CLUSTER][Error] Input corpus should be R6 Subset type\n")
      super$initialize( maxClusters )
      private$data.unbinary <- NULL
      
      private$fisher.all.distribution <- NULL
      private$fisher.best.distribution <- NULL
      private$cor.all.distribution <- NULL
      private$cor.best.distribution <- NULL
      private$best.distribution <- NULL #Contains this clustering method's best distribution (Fisher's best distribution clusters & Correlation's best distribution clusters)
      
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
      private$fisher.all.distribution <- private$computeFisherTest(onlyBinary)
      private$fisher.best.distribution <- data.frame(cluster=integer(),features=I(list()))
      aux <- unlist(private$fisher.all.distribution$getClusterDist()[private$fisher.all.distribution$getClusterDist()$k==private$fisher.all.distribution$getBestK(), ]$dist)
      for(i in 1:private$fisher.all.distribution$getBestK() ){
        private$fisher.best.distribution <- rbind(private$fisher.best.distribution, data.frame(cluster=i,dist=I(list(names(aux[aux==i])))))
      }
      if(nrow(private$data.unbinary) > 0 ){
        private$cor.all.distribution <- private$computeCorrelationTest(private$data.unbinary, "Active")
        private$cor.best.distribution <- data.frame(cluster=integer(),features=I(list())) 
        aux <- unlist(private$cor.all.distribution$getClusterDist()[private$cor.all.distribution$getClusterDist()$k==private$cor.all.distribution$getBestK(), ]$dist) 
        for(i in 1:private$cor.all.distribution$getBestK() ){
          private$cor.best.distribution <- rbind(private$cor.best.distribution, data.frame(cluster=i,dist=I(list(names(aux[aux==i])))))
        }
        private$best.distribution <- rbind(private$fisher.best.distribution,private$cor.best.distribution)
      }
      private$min <- min(private$fisher.all.distribution$getClusterDist()$k) 
      private$max <- max(private$fisher.all.distribution$getClusterDist()$k) 
    }, 
    
    
    plot = function(savePath = NULL){
      #Fisher Plot
      summary <- data.frame(k=private$fisher.all.distribution$getClusterDist()[,1],
                            dispersion=private$fisher.all.distribution$getClusterDist()[,2],
                            row.names = NULL)
      min <- data.frame(x=summary[which.min(summary[,2]), ][, 1],y= min(summary[,2]))
      max <- data.frame(x=summary[which.max(summary[,2]), ][, 1],y= max(summary[,2]))
      fisherPlot <- ggplot(summary, aes(k,dispersion)) + geom_line() + geom_point() +
        geom_point(aes(x,y), min, fill="transparent", color="blue", shape=21, size=3,stroke=1) +
        geom_text(aes(x,y,label=sprintf("%.3f",y)), min, hjust=-0.45, color='blue' ) +
        geom_point(aes(x,y), max, fill="transparent", color="red", shape=21, size=3,stroke=1) +
        geom_text(aes(x,y,label=sprintf("%.3f",y)), max, hjust=-0.45, color='red' ) +
        scale_y_continuous(limits=c(min(summary$dispersion), max( summary$dispersion) )) +
        scale_x_continuous(breaks=seq(from=2,to=nrow(summary) + 1)) +
        labs(title = "Binary Data", x = "Number of clusters", y = "Dispersion")
      
      #Cor Plot
      summary <- data.frame(k=private$cor.all.distribution$getClusterDist()[,1],
                            dispersion=private$cor.all.distribution$getClusterDist()[,2], 
                            row.names = NULL)
      min <- data.frame(x=summary[which.min(summary[,2]), ][, 1],y= min(summary[,2]))
      max <- data.frame(x=summary[which.max(summary[,2]), ][, 1],y= max(summary[,2]))
      fisherCorPlot <- ggplot(summary, aes(k,dispersion)) + geom_line() + geom_point() +
        geom_point(aes(x,y), min, fill="transparent", color="blue", shape=21, size=3,stroke=1) + 
        geom_text(aes(x,y,label=sprintf("%.3f",y)), min, hjust=-0.45, color='blue' ) +
        geom_point(aes(x,y), max, fill="transparent", color="red", shape=21, size=3,stroke=1) + 
        geom_text(aes(x,y,label=sprintf("%.3f",y)), max, hjust=-0.45, color='red' ) + 
        scale_y_continuous(limits=c(min(summary$dispersion), max( summary$dispersion) )) + 
        scale_x_continuous(breaks=seq(from=2,to=nrow(summary) + 1)) + 
        labs(title = "Unbinary Data", x = "Number of clusters", y = "Dispersion")
      
      dualPlot <-  grid.arrange(fisherPlot, fisherCorPlot, nrow = 1)
      if( !is.null(savePath) ) ggsave(savePath,plot=dualPlot,device=file_ext(savePath), limitsize = FALSE)
    },
    
    
    getDistribution = function(fisherK, corK, group, includeClass = "NONE" ){
      if( is.null(private$best.distribution) || is.null(private$cor.best.distribution) || is.null(private$fisher.all.distribution) || is.null(private$cor.all.distribution)){
        warning("[MultiTypeCluster][Warning] Function 'execute()' must be called first. Automatically run execute function\n")
        self$execute()
      }
      if( !toupper(includeClass) %in% c("NONE","BEGIN","END") ){
        cat("[MultiTypeCluster][INFO] Class parameter not included. Assuming class not included\n")
        class <- "NONE"
      }else class <- toupper(includeClass)
      if(missing(fisherK))
        fisherK <- private$fisher.all.distribution$getBestK()
      if(missing(corK))
        corK <- private$cor.all.distribution$getBestK()
      if( ( is.numeric(fisherK) && (fisherK == private$fisher.all.distribution$getBestK()) ) && ( is.numeric(corK) && (corK == private$cor.all.distribution$getBestK()) ) ){
        final.distr <- rbind(private$fisher.best.distribution, private$cor.best.distribution) 
      }
      else{
        if( ( is.numeric(fisherK) && (fisherK == private$fisher.all.distribution$getBestK()) ) || ( is.numeric(corK) && (corK == private$cor.all.distribution$getBestK()) )){
          if( ((is.numeric(fisherK) && (fisherK == private$fisher.all.distribution$getBestK())))){
            corKDistribution <- data.frame(cluster=integer(),features=I(list()))
            aux <- unlist(private$cor.all.distribution$getClusterDist()[private$cor.all.distribution$getClusterDist()$k==corK, ]$dist)
            for( i in 1:corK )
              corKDistribution <- rbind(corKDistribution, data.frame(cluster=i,dist=I(list(names(aux[aux == i])))) )
            final.distr <- rbind( private$fisher.best.distribution, corKDistribution) 
          }
          else {
            fisherKDistribution <- data.frame(cluster=integer(),features=I(list()))
            aux <- unlist(private$fisher.all.distribution$getClusterDist()[private$fisher.all.distribution$getClusterDist()$k==fisherK, ]$dist)
            for( i in 1:fisherK )
              fisherKDistribution <- rbind(fisherKDistribution, data.frame(cluster=i,dist=I(list(names(aux[aux == i])))) )
            final.distr <- rbind(fisherKDistribution,  private$cor.best.distribution) 
          }
        }
        else{
          fisherKDistribution <- data.frame(cluster=integer(),features=I(list()))
          aux <- unlist(private$fisher.all.distribution$getClusterDist()[private$fisher.all.distribution$getClusterDist()$k==fisherK, ]$dist)
          for( i in 1:fisherK )
            fisherKDistribution <- rbind(fisherKDistribution, data.frame(cluster=i,dist=I(list(names(aux[aux == i])))) )
          corKDistribution <- data.frame(cluster=integer(),features=I(list()))
          aux <- unlist(private$cor.all.distribution$getClusterDist()[private$cor.all.distribution$getClusterDist()$k==corK, ]$dist)
          for( i in 1:corK )
            corKDistribution <- rbind(corKDistribution, data.frame(cluster=i,dist=I(list(names(aux[aux == i])))) )
          final.distr <- rbind(fisherKDistribution,  corKDistribution)
        }
      }
      for(i in (fisherK+1):(fisherK+corK)){
        final.distr[i,1]=i
      }
      switch (class,
              "NONE" = { final.distr <- final.distr[,2]},
              "END" =  { final.distr <- lapply(final.distr[,2], function(x) {append(x,private$className)} ) },
              "BEGIN" ={ final.distr <- lapply(final.distr[,2], function(x) {append(x,private$className,0)} ) }
      )
      if( !missing(group) && !is.null(group) && is.numeric(group) && group <= length(final.distr))
        final.distr[[group]]
      else 
        final.distr
    },
    
    
    createSubset = function(cluster = NULL, subset = NULL){ # --- RECODE --- (Not Working!!)
      if( is.null(private$cor.all.distribution) ){
        cat("[MultiTypeClustering][Warning] Function 'execute()' must be called first. Automatically run execute function\n")
        self$execute()
      }
      
      if( missing(subset) || is.null(subset) || !"Subset" %in% class(subset)  )
        stop("[MultiTypeClustering][ERROR] Subset parameter must be defined as 'Subset' object\n")
      
      if( is.null(cluster) || missing(cluster) || !is.numeric(cluster) || 
          (is.numeric(cluster) && !cluster %in% c(private$min:private$max ) ) ){
        cat("[MultiTypeClustering][WARNING] Incorrect cluster parameter. Should be between: ",private$min," <= cluster <= ",private$max,"\n", sep="")
        cat("                         Assuming best cluster configuration (",private$cor.all.distribution$getBestK(),")\n", sep="")
        cluster <- private$cor.all.distribution$getBestK()
      }
      
      distribution <- self$getDistribution(cluster = cluster,includeClass = "NONE")
      cluster.dist <- ClusterDistribution$new()
      invisible(lapply(distribution, function(group){
        cluster.dist$add(subset$getInstances(features = c(subset$getClassName(),unlist(group)) ), classIndex = 1)
      } ) )
      cluster.dist
    }
  ),
  
  
  private = list(
    #---------------------------------------------------------------------------------------
    computeFisherTable = function(corpus){ # --- REFACTOR ---
      fisherTest <- sapply(corpus, function(c){fisher.test(table(c,private$class))$p.value })
    },
    computeFisherTest = function(corpus){ #BinaryCorpus
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
      binary.data$setBestK(  binary.data$getClusterDist()[which.min(binary.data$getClusterDist()[,2]),1])
      binary.data
    },
    #---------------------------------------------------------------------------------------
    
    
    computeCorrelationTable = function(corpus, positiveElement){
      if(missing(positiveElement))
        stop("[MultiTypeClustering][ERROR] positiveElement parameter must be defined\n")
      binaryClass <- sapply(private$class,function(elem){
        if(strcmpi(toString(elem), positiveElement))
          elem <- 1
        else
          elem <- 0
      })
      print(private$class)
      print(binaryClass)
      correlationTest <- sapply(corpus, function(c){ 
        cor.test(c,binaryClass,method = "spearman", exact = FALSE)$p.value
      }) 
    },
    
    
    computeCorrelationTest = function(corpus, positiveElement){
      unbinary.data <- BinaryFisherData$new()
      correlation.table <- private$computeCorrelationTable(corpus, positiveElement)
      cor.index <- order(correlation.table, decreasing = TRUE) 
      cor.size <- length(correlation.table)
      totalGroups <- 2:super$getMaxClusters()
      for(k in totalGroups) {
        clustering <- rep(c(1:k,(k:1)),cor.size/(2*k)+1)[1:cor.size]
        cluster <- integer(length = length(correlation.table))
        names(cluster) <- names(corpus)
        sumGroup <- vector(k,mode="list")
        for (i in 1:k){ 
          sumGroup[[i]] <- correlation.table[cor.index[clustering==i]]
          cluster[cor.index[clustering==i]] <- i
        }
        groupMeasure <- lapply(sumGroup,sum)
        deltha <- max(unlist(groupMeasure)) - min(unlist(groupMeasure))
        unbinary.data$addNewCluster(k,deltha,cluster)
      }
      unbinary.data$setBestK(unbinary.data$getClusterDist()[which.min(unbinary.data$getClusterDist()[,2]),1])
      unbinary.data
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
    
    fisher.all.distribution = NULL,
    fisher.best.distribution = NULL,
    cor.all.distribution = NULL,
    cor.best.distribution = NULL,
    best.distribution = NULL,
    
    min = NULL,
    max = NULL
  )
)