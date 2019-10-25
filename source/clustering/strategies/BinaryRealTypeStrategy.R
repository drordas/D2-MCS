BinaryRealTypeStrategy <- R6Class(
  classname = "BinaryRealTypeStrategy",
  inherit = GenericStrategy,
  portable = TRUE,
  public = list(
    initialize = function(subset, heuristic, configuration = StrategyConfiguration$new() ) {
      if ( !inherits(subset,"Subset" ) ) {
        stop("[",super$getName(),"][ERROR] Subset parameter must be defined as",
             "'Subset' type")
      }
      
      if ( !is.list(heuristic) || length(heuristic) != 2 ) {
        stop("[",super$getName(),"][ERROR] Heuristic parameter is not defined",
             " or incorrect. Must contain two elements.")
      }
      
      if(!any(sapply(heuristic, inherits, "GenericHeuristic"))){
        stop("[",super$getName(),"][ERROR] Defined heuristics are not correct.",
             " Must inherit from 'GenericHeuristic' class.")
      }
      
      if( is.null(heuristic[[1]]) ){
        message("[",super$getName(),"][INFO]) Heuristic for binary data not defined")
      }else { 
        message("[",super$getName(),"][INFO]) Heuristic for binary data defined",
                " as '",heuristic[[1]]$getName(),"'")
      }
      
      if (is.null(heuristic[[2]])){
        message("[",super$getName(),"][INFO]) Heuristic for real data not defined")
      }else{
          message("[",super$getName(),"][INFO]) Heuristic for real data defined",
                  " as '",heuristic[[2]]$getName(),"'")
      }
      super$initialize( subset= subset, heuristic= heuristic, "Pending...",
                        configuration= configuration )
    },
    execute = function(verbose=FALSE, ...) {
      class <- private$subset$getClassValues()
      col.index <- which( levels(private$subset$getClassValues()) == private$subset$getPositiveClass() )
      class <- varhandle::to.dummy( private$subset$getClassValues(), 
                                    private$subset$getPositiveClass() )[, col.index]
      
      minClusters <- private$configuration$minNumClusters()
      maxClusters <- private$configuration$maxNumClusters()
      
      private$best.distribution <- vector(mode = "list", length = 2)
      private$all.distribution <- vector(mode = "list", length = 2)
      private$not.distribution <- vector(mode = "list", length = 2)
      
      ##COMPUTING HEURISTIC FOR BINARY DATA (BETWEEN EACH FEATURE AND THE CLASS)
      if( !is.null(private$heuristic[[1]]) ){
        binary.data <- private$getBinaryFeatures( private$subset$getFeatures() )
        if( nrow(binary.data) > 0 ){
          binary.bestDistribution <- data.frame(cluster= integer(), dist= I(list()))
          binary.allDistribution <- data.frame(k= integer(), deltha= numeric(), 
                                               dist= I(list())) 
          
          message("[", super$getName(), "][INFO] Using '", 
                  private$heuristic[[1]]$getName(), 
                  "' heuristic to cluster binary features")
          
          bheuristic.values <- sapply(names(binary.data), function(col.name, class) {
            abs(private$heuristic[[1]]$heuristic( col1 = binary.data[, col.name], 
                                                  col2 = class, 
                                                  column.names = c(col.name, private$subset$getClassName())) )
          }, class)
          
          binary.valid <- bheuristic.values[complete.cases(bheuristic.values)]
          binary.invalid <- setdiff(names(bheuristic.values), names(binary.valid))
          binary.sorted <- binary.valid[order(binary.valid, decreasing = TRUE)]
          
          if (length(binary.valid) > 0) {
            ##DISTRIBUTE FEATURES IN CLUSTERS (2 >= k <= maxClusters)
            if(isTRUE(verbose)){
              message( "[",self$getName(),"][INFO] Performing binary feature clustering using '",
                       private$heuristic[[1]]$getName(),"' heuristic" )
              pb <- txtProgressBar(min = 0, max = (maxClusters-1), style = 3 )
            }
            
            for ( k in minClusters:maxClusters ){
              clustering <- rep( c(1:k, (k:1)), length(binary.sorted)/(2 * k) + 1 )[1:length(binary.sorted)]
              cluster <- vector( mode = "list", length = length(binary.sorted) )
              names(cluster) <- names(binary.sorted)
              sum.group <- vector(mode = "list", length = k)
              for (i in 1:k) {
                sum.group[[i]] <- binary.sorted[clustering == i]
                for (j in names(binary.sorted[clustering == i])) { 
                  cluster[[j]] <- c(i) 
                }
              }
              group.measure <- sapply(sum.group, sum)
              deltha <- (max(group.measure) - min(group.measure))
              df <- data.frame( k = k, deltha = deltha, dist = I(list(cluster)))
              binary.allDistribution <- rbind(binary.allDistribution, df)
              if(isTRUE(verbose)) { setTxtProgressBar(pb, (k-1)) }
            }
            
            if(isTRUE(verbose)) { close(pb) }
            
            for ( i in 1:nrow(binary.allDistribution )){
              aux.dist <- unlist(binary.allDistribution[i,]$dist, 
                                 recursive = FALSE)
              aux.list <- list()
              for ( j in 1:binary.allDistribution[i,]$k ) {
                aux.list <- append( aux.list,list(names(aux.dist[ aux.dist == j ])) )
                binary.allDistribution[i,]$dist <- I(list(aux.list))
              }
            }
            
            bestK <- which.min(binary.allDistribution$deltha)
            aux.dist <- unlist( binary.allDistribution[bestK, ]$dist, 
                                recursive = FALSE )
            
            binary.bestDistribution <- data.frame(cluster= integer(), dist= I(list()))
            
            for ( i in 1:length(aux.dist) ){
              df <- data.frame(cluster=i, dist=I(list(aux.dist[[i]])))
              binary.bestDistribution <- rbind(binary.bestDistribution, df)
            }
            
            private$best.distribution[[1]] <- binary.bestDistribution
            private$all.distribution[[1]] <- binary.allDistribution
          }
          
          if (length(binary.invalid) > 0) {
            message( "[", super$getName(), "][WARNING] ",
                     length(binary.invalid)," features were incompatible with '",
                     private$heuristic[[1]]$getName(), "' heuristic." )
            private$notDistribution[[1]] <- data.frame( cluster = 1, 
                                                        dist = I(list(binary.invalid)))
          } 
        }else{
          message("[", super$getName(), "][INFO] Not binary features for clustering")
        }
      }else{
        message("[", super$getName(), "][INFO] ", super$getName(),
                " has not heuristic to binary features. Assuming one cluster by default")  
        private$all.distribution[[1]] <- data.frame( k = 1, homogeneity = 0, 
                                              dist = I(list(names(binary.data))))
        private$best.distribution[[1]] <- data.frame(cluster = 1, 
                                                     dist= I(list(names(binary.data))) )
      }
      
      all.distribution <- private$all.distribution[[1]]
      best.distribution <- private$best.distribution[[1]]
      
      ##COMPUTING HEURISTIC FOR REAL DATA (BETWEEN EACH FEATURE AND THE CLASS)
      if (!is.null(private$heuristic[[2]])){
        real.data <- private$getRealFeatures( private$subset$getFeatures() )
        message("[", super$getName(), "][INFO] Using '", private$heuristic[[2]]$getName(), 
                "' heuristic to distribute real features")
        if( nrow(real.data) > 0 ){
          real.bestDistribution <- data.frame( cluster= integer(), dist= I(list()))
          real.allDistribution <- data.frame( k= integer(), deltha= numeric(), 
                                                 dist= I(list())) 
          
          rheuristic.values <- sapply(names(real.data), function(col.name, class) {
            abs(private$heuristic[[2]]$heuristic( col1 = real.data[, col.name], col2 = class, 
                                                  column.names = c(col.name, 
                                                                   private$subset$getClassName())) )
          }, class)
          
          real.valid <- rheuristic.values[complete.cases(rheuristic.values)]
          real.invalid <- setdiff(names(rheuristic.values), names(real.valid))
          real.sorted <- real.valid[order(real.valid, decreasing = TRUE)]
          
          ##DISTRIBUTE FEATURES IN CLUSTERS (2 >= k <= maxClusters)
          if(isTRUE(verbose)){
            message( "[",self$getName(),"][INFO] Performing real feature clustering using '",
                     private$heuristic[[2]]$getName(),"' heuristic" )
            pb <- txtProgressBar(min = 0, max = (maxClusters-1), style = 3 )
          }
          
          if (length(real.valid) > 0) {
            for ( k in minClusters:maxClusters ){
              clustering <- rep( c(1:k, (k:1)), 
                                 length(real.sorted)/(2 * k) + 1 )[1:length(real.sorted)]
              
              cluster <- vector( mode = "list", length = length(real.sorted) )
              names(cluster) <- names(real.sorted)
              sum.group <- vector(mode = "list", length = k)
              for (i in 1:k) {
                sum.group[[i]] <- real.sorted[clustering == i]
                for (j in names(real.sorted[clustering == i])) { cluster[[j]] <- c(i) }
              }
              group.measure <- sapply(sum.group, sum)
              deltha <- (max(group.measure) - min(group.measure))
              df <- data.frame( k = k, deltha = deltha, dist = I(list(cluster)))
              real.allDistribution <- rbind(real.allDistribution, df)
              if(isTRUE(verbose)) { setTxtProgressBar(pb, (k-1)) }
            }
            
            if(isTRUE(verbose)) { close(pb) }
            
            for ( i in 1:nrow(real.allDistribution )){
              aux.dist <- unlist(real.allDistribution[i,]$dist, recursive= FALSE)
              aux.list <- list()
              for ( j in 1:real.allDistribution[i,]$k ) {
                aux.list <- append( aux.list,list(names(aux.dist[ aux.dist == j ])) )
                real.allDistribution[i,]$dist <- I(list(aux.list))
              }
            }
            
            bestK <- which.min( real.allDistribution$deltha)
            aux.dist <- unlist( real.allDistribution[bestK, ]$dist, recursive = FALSE )
            
            real.bestDistribution <- data.frame(cluster= integer(), dist= I(list()))
            for ( i in 1:length(aux.dist) ){
              df <- data.frame(cluster=i, dist=I(list(aux.dist[[i]])))
              real.bestDistribution <- rbind(real.bestDistribution, df)
            }
            private$best.distribution[[2]] <- real.bestDistribution
            private$all.distribution[[2]] <- real.allDistribution
          }
          
          if (length(real.invalid) > 0) {
            real.notDistribution <- data.frame(cluster = integer(), 
                                               features = I(list()))
            message( "[", super$getName(), "][WARNING] ",
                     length(heuristic.invalid)," features were incompatible with '",
                     private$heuristic[[2]]$getName(), "' heuristic." )
            private$not.distribution[[2]] <- data.frame( cluster = 1, 
                                                         dist = I(list(real.invalid)))
          } 
          
        }else{
          message("[", super$getName(), "][INFO] Not real features for clustering")
          private$best.distribution[[2]] <- append(private$best.distribution[[2]],list(NULL))
          private$all.distribution[[2]] <- append(private$all.distribution[[2]],list(NULL))
        }
      }else{
        message("[", super$getName(), "][INFO] ", super$getName(),
                " has not heuristic to binary features. Assuming one cluster by default")  
        private$all.distribution[[2]] <- data.frame( k= 1, homogeneity = 0, 
                                                     dist= I(list(names(real.data))))
        private$best.distribution[[2]] <- data.frame( cluster= 1, 
                                                      dist = I(list(names(real.data))) )
      }
    },
    getDistribution = function( num.clusters= NULL, num.groups=NULL, 
                                include.unclustered = FALSE ) {
      distribution <- list() 
      if ( is.null(private$best.distribution) || 
           is.null(private$all.distribution) || 
           all(sapply(private$best.distribution,is.null)) || 
           all(sapply(private$all.distribution,is.null)) ) {
        stop("[",super$getName(),"][WARNING] Clustering not done or errorneous.",
             " Returning NULL")
      }
      
      if( is.null(num.clusters) || !is.numeric(num.clusters) ){
        #message("[",super$getName(),"][INFO] Number of clusters not defined",
        #        "or incorrect. Assuming best cluster distribution for both heuristics.")
        dist.binary <- sapply(private$best.distribution[[1]]$dist, function(x) {x})
        dist.real <- sapply(private$best.distribution[[2]]$dist, function(x) {x})
      }else{
        all.binary <- private$all.distribution[[1]]
        all.real <- private$all.distribution[[2]]
        
        if ( length(num.clusters) >= length(private$all.distribution) ){
          num.clusters <- num.clusters[c(1:length(private$all.distribution))]
        }else{
          num.clusters <- c(num.clusters,rep(0,length(private$all.distribution)-length(num.clusters)))
        }

        if ( !( num.clusters[1] %in% c(min(all.binary$k):max(all.binary$k)) ) ){
          message("[",super$getName(),"][WARNING] Number of clusters incorrect.",
                  " Must be between ",min(all.binary$k)," and ",max(all.binary$k),
                  ". Ignoring clustering for binary type features...")
          dist.binary <- NULL
        }else{
          dist.binary <- unlist( all.binary[which(all.binary$k == num.clusters[1]), ]$dist, 
                                 recursive = FALSE )
        }
        
        if ( !( num.clusters[2] %in% c(min(all.real$k):max(all.real$k)) ) ){
          message("[",super$getName(),"][INFO] Number of clusters incorrect.",
                  " Must be between ",min(all.real$k)," and ",max(all.real$k),
                  ". Ignoring clustering for real type features...")
          dist.real <- NULL
        }else{
          dist.real <- unlist( all.real[which(all.real$k == num.clusters[2]), ]$dist, 
                               recursive = FALSE )
        }
      }
 
      if ( !is.null(num.groups) && is.numeric(num.groups) ) {
        if( length(num.groups) >= length(private$all.distribution) ){
          num.groups <- num.groups[c(1:length(private$all.distribution))]
        }else{
          num.groups <- c(num.groups,rep(0,length(private$all.distribution)-length(num.groups)))
        }
        if ( !( num.groups[1] %in% c(1:length(dist.binary)) ) ){
          message( "[",super$getName(),"][WARNING] Number of clusters incorrect.",
                   " Returning all groups ..." )
          
        }else{ dist.binary <- dist.binary[num.groups[1]] }
        
        if ( !( num.groups[2] %in% c(1:length(dist.real)) ) ){
          message( "[",super$getName(),"][WARNING] Number of clusters incorrect.",
                   " Returning all groups ..." )
          
        }else{ dist.real <- dist.real[num.groups[2]] }
      }
      
      distribution <- append(distribution, c(dist.binary,dist.real) )
      
      if( isTRUE(include.unclustered) && nrow(private$not.distribution) ){
        distribution <- append(distribution,lapply(private$not.distribution$dist, 
                                                   function(x) {x} ))
      }
      return(distribution)
    },
    createTrain = function( subset, num.clusters= NULL, num.groups=NULL,
                            include.unclustered= FALSE) {
      if ( !inherits(subset,"Subset") ) {
        stop("[",super$getName(),"][ERROR] Subset parameter must be a 'Subset' object")
      }
      
      if ( is.null(private$best.distribution) || is.null(private$all.distribution) ) {
        stop("[",super$getName(),"][ERROR] Clustering not done or erroneous. Aborting...")
      }
      
      distribution <- self$getDistribution( num.clusters = num.clusters, 
                                            num.groups = num.groups,
                                            include.unclustered = include.unclustered )
      
      train.dist <- lapply(distribution, function(group) {
        subset$getFeatures(feature.names = group)
        instances <- subset$getFeatures(feature.names = group)
      })
      
      TrainSet$new( cluster.dist = train.dist, class.name= subset$getClassName(),
                    class.values = subset$getClassValues(),
                    positive.class = subset$getPositiveClass() )
    },
    plot = function(dir.path = NULL, file.name = NULL, ...) {

      binary.summary <- data.frame( k = private$all.distribution[[1]]$k,
                                    dispersion = private$all.distribution[[1]]$deltha,
                                    row.names = NULL )
      
      real.summary <- data.frame( k = private$all.distribution[[2]]$k,
                                  dispersion = private$all.distribution[[2]]$deltha,
                                  row.names = NULL )
      
      if( nrow(binary.summary) > 0 && nrow(real.summary) > 0){
        plot <- grid.arrange(BinaryPlot$new()$plot(binary.summary), 
                     BinaryPlot$new()$plot(real.summary), 
                     nrow = 2, ncol = 1)
      }else{
        if(nrow(binary.summary) > 0){
          plot <- BinaryPlot$new()$plot(binary.summary)
        }else {plot <- BinaryPlot$new()$plot(real.summary)} 
      }
      
      if (!is.null(dir.path)) {
        if (!dir.exists(dir.path)) {
          dir.create(dir.path, recursive = TRUE)
        }
        ggsave( paste0(file.path(dir.path, file.name), ".pdf"), device = "pdf", 
                plot = plot, limitsize = FALSE )
        message("[",super$getName(),"][INFO] Plot has been succesfully saved at: ",file.path(dir.path,file.name,".pdf"))
      } #else { plot }
      
    },
    saveCSV = function(dir.path, name=NULL, num.clusters=NULL){
      if(missing(dir.path))
        stop("[",super$getName(),"][INFO] Path not defined. Aborting.")
      
      if(is.null(name)){
        name <- private$heuristic[[1]]$getName()
        message("[",super$getName(),"][INFO] File name not defined. Using '",
                name,".csv'.")
      }
      
      if ( is.null(private$all.distribution) || 
           length(private$all.distribution) == 0 ) {
        stop("[",super$getName(),"][WARNING] Clustering not done or errorneous.",
             " Returning NULL")
      }
      
      if (!dir.exists(dir.path)) { 
        dir.create(dir.path, recursive = TRUE) 
        if(dir.exists(dir.path)) {
          message("[",super$getName(),"][INFO] Directory '",dir.path,"'has been succesfully created")
        }else {stop("[",super$getName(),"][ERROR] Cannot create directory '",dir.path,"'.") }
      }
      
      if( is.null(num.clusters) ){
        message( "[",super$getName(),"][WARNING] Number of clusters not defined.",
                 " Saving all cluster configurations" )
        num.clusters <- c( (max(private$all.distribution[[1]]$k)-1),
                           (max(private$all.distribution[[2]]$k)-1) )
      }else{
        if ( length(num.clusters) >= length(private$all.distribution) ){
          num.clusters <- num.clusters[c(1:length(private$all.distribution))]
        }else{ num.clusters <- sapply(private$all.distribution, 
                                      function(x) {nrow + 1 }) }
      }
        
      all.binary <- private$all.distribution[[1]]
      all.real <- private$all.distribution[[2]]
      
      if ( !( num.clusters[1] %in% c(1:nrow(all.binary)) ) ){
        message("[",super$getName(),"][WARNING] Number of clusters incorrect.",
                " Must be between ",min(all.binary$k)," and ",max(all.binary$k),
                ". Ignoring clustering for binary type features...")
        dist.binary <- data.frame( k = numeric(),dispersion = numeric(),
                                   feature_type= character() )
      }else{ 
        dist.binary <- data.frame(k=all.binary[c(1:(num.clusters[1]-1) ),"k"],
                                  dispersion=all.binary[c(1:(num.clusters[1]-1) ),"deltha"],
                                  feature_type= "binary", row.names = NULL) 
      }
      
      if ( !( num.clusters[2] %in% c(1:nrow(all.real)) ) ){
        message("[",super$getName(),"][INFO] Number of clusters incorrect.",
                " Must be between ",min(all.real$k)," and ",max(all.real$k),
                ". Ignoring clustering for real type features...")
        dist.real <- data.frame( k = numeric(),dispersion = numeric(),
                                 feature_type=character() )
      }else{
        dist.real <- data.frame(k=all.real[c(1:(num.clusters[2]-1) ),"k"],
                                dispersion=all.real[c(1:(num.clusters[2]-1) ),"deltha"],
                                feature_type="real", row.names = NULL) 
      }
      
      write.table( rbind(dist.binary,dist.real), 
                   file=file.path(dir.path,paste0(name,".csv")),
                   row.names = FALSE, col.names = TRUE, sep=";")
    }
  ),
  private = list(
    getBinaryFeatures = function(data){
      Filter( function(x) { all(levels(factor(x)) %in% c("0","1")) },  data )
    },
    getRealFeatures = function(data){
      Filter( function(x) { !all(levels(factor(x)) %in% c("0","1")) }, data )
    }
  )
)