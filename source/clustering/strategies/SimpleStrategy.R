library("R6")
library("varhandle")

SimpleStrategy <- R6Class(
  classname = "SimpleStrategy",
  inherit = GenericStrategy,
  portable = TRUE,
  public = list(
    initialize = function(subset, heuristic, configuration = StrategyConfiguration$new()) {
      super$initialize( name = "SimpleStrategy", subset = subset,
                        heuristic = heuristic, configuration = configuration )
    },
    execute = function(verbose=FALSE, ...) {
      private$all.distribution <- data.frame(k = integer(), deltha = numeric(), dist = I(list()))
      class <- private$subset$getClassValues()

      colIndex <- which( levels(private$subset$getClassValues()) == private$subset$getPositiveClass() )
      class <- varhandle::to.dummy( private$subset$getClassValues(), private$subset$getPositiveClass() )[, colIndex]
      
      ##COMPUTING HEURISTIC (BETWEEN EACH FEATURE AND THE CLASS)
      corpus <- private$subset$getFeatures()
      heuristic.values <- sapply(names(corpus), function(colName, class) {
        abs(private$heuristic[[1]]$heuristic( col1 = corpus[, colName], col2 = class, 
                                              column.names = c(colName, private$subset$getClassName())) )
      }, class)
      
      heuristic.valid <- heuristic.values[complete.cases(heuristic.values)]
      notHeuristic <- setdiff(names(heuristic.values), names(heuristic.valid))
      sorted.values <- heuristic.valid[order(heuristic.valid, decreasing = TRUE)]
      
      ##DISTRIBUTE FEATURES IN CLUSTERS (2 >= k <= maxClusters)
      if(isTRUE(verbose)){
        message( "[",self$getName(),"][INFO] Performing feature clustering using '",
                 private$heuristic[[1]]$getName(),"' heuristic" )
        pb <- txtProgressBar(min = 0, max = (private$configuration$maxNumClusters()-1), style = 3 )
      }
      
      if (length(heuristic.valid) > 0) {
        for ( k in private$configuration$minNumClusters():private$configuration$maxNumClusters() ){
          clustering <- rep( c(1:k, (k:1)), length(sorted.values)/(2 * k) + 1 )[1:length(sorted.values)]
          cluster <- vector( mode = "list", length = length(sorted.values) )
          names(cluster) <- names(sorted.values)
          sumGroup <- vector(mode = "list", length = k)
          for (i in 1:k) {
            sumGroup[[i]] <- sorted.values[clustering == i]
            for (j in names(sorted.values[clustering == i])) { cluster[[j]] <- c(i) }
          }
          groupMeasure <- sapply(sumGroup, sum)
          deltha <- (max(groupMeasure) - min(groupMeasure))
          df <- data.frame( k = k, deltha = deltha, dist = I(list(cluster)))
          private$all.distribution <- rbind(private$all.distribution, df)
          if(isTRUE(verbose)) { setTxtProgressBar(pb, (k-1)) }
        }
        
        if(isTRUE(verbose)) { close(pb) }
        
        for (i in 1:nrow(private$all.distribution)){
          aux.dist <- unlist(private$all.distribution[i,]$dist, recursive = FALSE)
          aux.list <- list()
          for ( j in 1:private$all.distribution[i,]$k ) {
            aux.list <- append( aux.list,list(names(aux.dist[ aux.dist == j ])) )
            private$all.distribution[i,]$dist <- I(list(aux.list))
          }
        }
        
        bestK <- which.min(private$all.distribution$deltha)
        aux.dist <- unlist( private$all.distribution[bestK, ]$dist, 
                            recursive = FALSE )
        private$best.distribution <- data.frame( cluster= integer(), 
                                                 dist= I(list()) )
        for ( i in 1:length(aux.dist) ){
          df <- data.frame(cluster=i, dist=I(list(aux.dist[[i]])))
          private$best.distribution <- rbind(private$best.distribution, df)
        }
      }
      if (length(notHeuristic) > 0) {
        message( "[", super$getName(), "][WARNING] ",
                 length(notHeuristic)," features were incompatible with '",
                 private$heuristic[[1]]$getName(), "' heuristic." )
        private$not.distribution <- data.frame( cluster = 1, 
                                                dist = I(list(notHeuristic)))
      }   
    },
    getAllDistributions = function() {
      list(private$all.distribution)
    },
    getBestClusterDistribution = function() {
      list(private$best.distribution)
    },
    getUnclustered = function() {
      list(private$not.distribution)
    },
    getDistribution = function( num.clusters= NULL, num.groups=NULL, 
                                include.unclustered = FALSE){
      if (is.null(private$best.distribution) || is.null(private$all.distribution)) {
        stop(red("[", super$getName(), "][WARNING] Clustering not done or errorneous. Returning NULL"))
      }

      if(is.null(num.clusters)){
        message("[",super$getName(),"][INFO] Number of clusters not defined. Assuming best cluster distribution.")
        distribution <- unlist(private$all.distribution[which.min(private$all.distribution$deltha), ]$dist,recursive = FALSE)
      }else{
        if( is.numeric(num.clusters) && (num.clusters %in% c(2:tail(private$all.distribution$k,n=1))) ){
          distribution <- unlist(private$all.distribution[which(num.clusters==private$all.distribution$k), ]$dist,recursive = FALSE)
        }else{
          message("[",super$getName(),"][INFO] Number of clusters not found. Assuming best cluster distribution.")
          distribution <- unlist(private$all.distribution[which.min(private$all.distribution$deltha), ]$dist,recursive = FALSE)
        }
      }
      if ( !missing(num.groups) && is.numeric(num.groups) && 
           num.groups %in% c(1:length(distribution)) ){
          distribution <- distribution[num.groups]
      }
      if( isTRUE(include.unclustered) && nrow(private$not.distribution) ){
        distribution <- append(distribution,lapply(private$not.distribution$dist, 
                                                   function(x) {x} ))
      }
      return(distribution)
    },
    createSubset = function(subset, num.clusters = NULL, ...) {
      if ( !inherits(subset,"Subset") ) {
        stop(red("[",super$getName(),"][ERROR] Subset parameter must be a 'Subset' object"))
      }
      
      if (is.null(private$best.distribution) || is.null(private$all.distribution)) {
        stop("[",super$getName(),"][ERROR] Clustering not done or errorneous. Aborting...")
      }
      include.unclustered <- eval(substitute(alist(...))[["include.unclustered"]])
      num.groups <- eval(substitute(alist(...))[["num.groups"]])
      
      if (!is.logical(include.unclustered)){
        message("[",super$getName(),"][INFO] 'include.unclustered' parameter must contain a logical value (TRUE or FALSE). Assuming FALSE as default")
        include.unclustered <- FALSE
      }
      
      distribution <- self$getDistribution( num.clusters = num.clusters, 
                                            num.groups = num.groups,
                                            include.unclustered = include.unclustered )

      cluster.dist <- lapply(distribution, function(group) {
        instances <- subset$getFeatures(feature.names= group)
        if(subset$getClassIndex() == 1){
          instances <- cbind(subset$getClassValues(),instances)
        }else{
          if(subset$getClassIndex() >=nrow() ){
            instances <- cbind(instances,subset$getClassValues())
          }else{
            instances <- cbind( instances[1:private$class.index-1],
                                subset$getClassValues(), 
                                instances[private$class.index:ncol(instances)] )
          }
        }
        Subset$new( dataset = instances, class.index = subset$getClassIndex(),
                    class.values = as.character(unique(subset$getClassValues())),
                    positive.class = subset$getPositiveClass() )
      })
      # }
      cluster.dist
    },
    plot = function(dir.path = NULL, file.name = NULL, 
                    plotObject = list(BinaryPlot$new()), ...) {
      if (!is.list(plotObject)) {
        stop("[", super$getName(), "][ERROR] plotObject parameter must be defined as 'list' type")
      }
      if (length(plotObject) == 0) {
        stop("[", super$getName(), "][ERROR] plotObject parameter must be defined as a list of Plots")
      } else {
        for (p in plotObject) {
          if (!"Plot" %in% class(p)) {
            stop("[", super$getName(), "][ERROR] plotObject parameter must be defined as a list of 'Plot' type objects")
          }
        }
        if (length(plotObject) == 1) {
          message("[", super$getName(), "][INFO] ", super$getName(), " use one Plot. Assuming only binary plot")  
          if (!"Plot" %in% class(plotObject[[1]])) {
            stop("[", super$getName(), "][ERROR] plotObject parameter must be defined as a list of Plot")
          }
        }
      }
      summary <- data.frame(k = private$all.distribution$k,
                            dispersion = private$all.distribution$deltha,
                            row.names = NULL)
      plot <- plotObject[[1]]$plot(summary)
      if (!is.null(dir.path)) {
        if (!dir.exists(dir.path)) {
          dir.create(dir.path, recursive = TRUE)
        }
        ggsave( paste0(file.path(dir.path, file.name), ".pdf"), device = "pdf", 
                plot = plot, limitsize = FALSE )
        message("[",super$getName(),"][INFO] Plot has been succesfully saved at: ",file.path(dir.path,file.name,".pdf"))
      } else {  invisible(show(plot)) }
    },
    saveCSV = function(dir.path, name=NULL, num.clusters=NULL){
      if(missing(dir.path))
        stop("[",super$getName(),"][INFO] Path not defined. Aborting.")
      
      if(is.null(name)){
        name <- private$heuristic[[1]]$getName()
        message("[",super$getName(),"][INFO] File name not defined. Using '",name,".csv'.")
      }
      
      if(is.null(private$all.distribution) || nrow(private$all.distribution) == 0){
        stop("[",super$getName(),"][INFO] Clustering method not performed. Aborting.")
      }
      
      if (!dir.exists(dir.path)) { 
        dir.create(dir.path, recursive = TRUE) 
        if(dir.exists(dir.path)) {
          message("[",super$getName(),"][INFO] Directory has been succesfully created")
        }else {stop("[",super$getName(),"][ERROR] Cannot create directory.") }
      }

      if( is.null(num.clusters) ){
        message( "[",super$getName(),"][WARNING] Number of clusters not defined.",
                 " Saving all cluster configurations" )
        num.clusters <- (max(private$all.distribution$k)-1)
      }else{
        if( !(num.clusters %in%c(2:max(private$all.distribution$k)) ) ) {
          message( "[",super$getName(),"][WARNING] Number of clusters exceeds ",
                   "maximun number of clusters. Saving all cluster configurations." )
          num.clusters <- (max(private$all.distribution$k)-1)
        }
      }
      
      write.table( data.frame( k = private$all.distribution[c(2:num.clusters),"k"],
                               dispersion = private$all.distribution[c(2:num.clusters),"deltha"],
                               row.names = NULL), 
                   file=file.path(dir.path, paste0(name,".csv")),row.names = FALSE, 
                   col.names = TRUE, sep=";")
    }
  )
)