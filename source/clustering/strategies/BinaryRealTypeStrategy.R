BinaryRealTypeStrategy <- R6::R6Class(
  classname = "BinaryRealTypeStrategy",
  inherit = ClusteringStrategy,
  portable = TRUE,
  public = list(
    initialize = function(subset, heuristic, configuration = StrategyConfiguration$new() ) {
      if ( !inherits(subset,"Subset" ) ) {
        stop("[",class(self)[1],"][FATAL] Subset parameter must be defined as ",
             "'Subset' type. Aborting...")
      }

      if ( !is.list(heuristic) || length(heuristic) != 2 ) {
        stop("[",class(self)[1],"][FATAL] Heuristic parameter is not defined ",
             "or incorrect. Must contain two elements. Aborting...")
      }

      if(!any(sapply(heuristic, inherits, "GenericHeuristic"))){
        stop("[",class(self)[1],"][FATAL] Defined heuristics are not correct. ",
             "Must be inherit from 'GenericHeuristic' class. Aborting...")
      }

      if( is.null(heuristic[[1]]) ){
        message("[",class(self)[1],"][WARNING] Heuristic for binary data not defined")
      }else {
        message("[",class(self)[1],"][INFO] Heuristic for binary data defined ",
                "as '",class(heuristic[[1]])[1],"'")
      }

      if (is.null(heuristic[[2]])){
        message("[",class(self)[1],"][WARNING] Heuristic for real data not defined")
      }else{
        message("[",class(self)[1],"][INFO] Heuristic for real data defined ",
                "as '",class(heuristic[[2]])[1],"'")
      }
      description <- "<<Pending>>"
      super$initialize( subset= subset, heuristic= heuristic,
                        description = description, configuration= configuration )
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

      binary.data <- private$getBinaryFeatures( private$subset$getFeatures() )
      ##COMPUTING HEURISTIC FOR BINARY DATA (BETWEEN EACH FEATURE AND THE CLASS)
      if( !is.null(private$heuristic[[1]]) ){
        if( nrow(binary.data) > 0 ){
          binary.bestDistribution <- data.frame(cluster= integer(), dist= I(list()))
          binary.allDistribution <- data.frame(k= integer(), deltha= numeric(),
                                               dist= I(list()))

          message("[",class(self)[1],"][INFO] Using '",
                  class(private$heuristic[[1]])[1],
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
              message( "[",class(self)[1],"][INFO] Performing binary feature clustering using '",
                       class(private$heuristic[[1]])[1],"' heuristic" )
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
            message( "[",class(self)[1],"][WARNING] ",
                     length(binary.invalid)," features were incompatible with '",
                     class(private$heuristic[[1]])[1], "' heuristic" )
            private$not.distribution[[1]] <- data.frame( cluster = 1,
                                                         dist = I(list(binary.invalid)))
          }
        }else{
          message("[",class(self)[1],"][INFO] Not binary features for clustering")
        }
      }else{
        message("[",class(self)[1],"][INFO] ", class(self)[1], " has not ",
                "heuristic to binary features. Assuming one cluster by default")
        private$all.distribution[[1]] <- data.frame( k = 1, deltha = 0,
                                              dist = I(list(names(binary.data))))
        private$best.distribution[[1]] <- data.frame(cluster = 1,
                                                     dist= I(list(names(binary.data))) )
      }

      all.distribution <- private$all.distribution[[1]]
      best.distribution <- private$best.distribution[[1]]
      real.data <- private$getRealFeatures( private$subset$getFeatures() )
      ##COMPUTING HEURISTIC FOR REAL DATA (BETWEEN EACH FEATURE AND THE CLASS)
      if (!is.null(private$heuristic[[2]])){
        message("[", class(self)[1],"][INFO] Using '", class(private$heuristic[[2]])[1],
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
            message( "[",class(self)[1],"][INFO] Performing real feature clustering using '",
                     class(private$heuristic[[2]])[1],"' heuristic" )
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
            message( "[",class(self)[1],"][WARNING] ",
                     length(real.invalid)," features were incompatible with '",
                     class(private$heuristic[[2]])[1], "' heuristic." )
            private$not.distribution[[2]] <- data.frame( cluster = 1,
                                                         dist = I(list(real.invalid)))
          }

        }else{
          message("[",class(self)[1],"][INFO] Not real features for clustering")
          private$best.distribution[[2]] <- append(private$best.distribution[[2]],list(NULL))
          private$all.distribution[[2]] <- append(private$all.distribution[[2]],list(NULL))
        }
      }else{
        message("[",class(self)[1],"][INFO] ", class(self)[1],
                " has not heuristic to real features. Assuming one cluster by default")
        private$all.distribution[[2]] <- data.frame( k= 1, deltha = 0,
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
        stop("[",class(self)[1],"][FATAL] Clustering not done or errorneous. ",
             "Aborting...")
      }

      if( is.null(num.clusters) || !is.numeric(num.clusters) ){
        dist.binary <- lapply(private$best.distribution[[1]]$dist, function(x) {x})
        dist.real <- lapply(private$best.distribution[[2]]$dist, function(x) {x})
      }else{
        all.binary <- private$all.distribution[[1]]
        all.real <- private$all.distribution[[2]]

        if ( length(num.clusters) >= length(private$all.distribution) ){
          num.clusters <- num.clusters[c(1:length(private$all.distribution))]
        }else{
          num.clusters <- c(num.clusters,rep(0,length(private$all.distribution)-length(num.clusters)))
        }

        if ( !( num.clusters[1] %in% c(min(all.binary$k):max(all.binary$k)) ) ){
          message("[",class(self)[1],"][WARNING] Number of clusters incorrect. ",
                  "Must be between ",min(all.binary$k)," and ",max(all.binary$k),
                  ". Ignoring clustering for binary type features...")
          dist.binary <- NULL
        }else{
          dist.binary <- unlist( all.binary[which(all.binary$k == num.clusters[1]), ]$dist,
                                 recursive = FALSE )
        }

        if ( !( num.clusters[2] %in% c(min(all.real$k):max(all.real$k)) ) ){
          message("[",class(self)[1],"][INFO] Number of clusters incorrect. ",
                  "Must be between ",min(all.real$k)," and ",max(all.real$k),
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
          message( "[",class(self)[1],"][WARNING] Number of clusters incorrect. ",
                   "Returning all groups ..." )

        }else{ dist.binary <- dist.binary[num.groups[1]] }

        if ( !( num.groups[2] %in% c(1:length(dist.real)) ) ){
          message( "[",class(self)[1],"][WARNING] Number of clusters incorrect. ",
                   "Returning all groups ..." )

        }else{ dist.real <- dist.real[num.groups[2]] }
      }

      distribution <- append(distribution, c(dist.binary,dist.real) )

      if( isTRUE(include.unclustered) ){
        if (!is.null(private$not.distribution[[1]]) && nrow(private$not.distribution[[1]]) > 0)
          distribution <- append(distribution,lapply(private$not.distribution[[1]]$dist,
                                                     function(x) {x} ))
        if (!is.null(private$not.distribution[[2]]) && nrow(private$not.distribution[[2]]) > 0)
          distribution <- append(distribution,lapply(private$not.distribution[[2]]$dist,
                                                     function(x) {x} ))
      }
      return(distribution)
    },
    createTrain = function( subset, num.clusters= NULL, num.groups=NULL,
                            include.unclustered= FALSE) {
      if ( !inherits(subset,"Subset") ) {
        stop("[",class(self)[1],"][FATAL] Subset parameter must be defined as ",
             "'Subset' type. Aborting...")
      }

      if ( is.null(private$best.distribution) || is.null(private$all.distribution) ) {
        stop("[",class(self)[1],"][FATAL] Clustering not done or errorneous. ",
             "Aborting...")
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

      if( nrow(binary.summary) > 1 && nrow(real.summary) > 1){
        plot <- grid.arrange(
                      BinaryPlot$new()$plot(binary.summary) +
                       labs(title = "Binary Data") + theme_light() +
                       theme(axis.text.x = element_text(angle = 90, hjust = 0.5)),
                      GenericPlot$new()$plot(real.summary) +
                       labs(title = "Real Data") + theme_light() +
                       theme(axis.text.x = element_text(angle = 90, hjust = 0.5)),
                       nrow = 2, ncol = 1 )
      }else{
        if(nrow(binary.summary) > 1){
          plot <- BinaryPlot$new()$plot(binary.summary) +
                    labs(title = "Binary Data") + theme_light() +
                    theme(axis.text.x = element_text(angle= 90, hjust= 0.5))
        }else { plot <- GenericPlot$new()$plot(real.summary) +
                          labs(title = "Real Data") + theme_light() +
                          theme(axis.text.x = element_text(angle= 90, hjust= 0.5)) }
      }

      if (!is.null(dir.path)) {
        if (!dir.exists(dir.path)) {
          dir.create(dir.path, recursive = TRUE)
          if ( dir.exists(dir.path) ) {
            message("[",class(self)[1],"][INFO] Directory '", dir.path,
                    "'has been succesfully created")
          } else {
            stop("[",class(self)[1],"][FATAL] Cannot create directory '", dir.path,
                 "'. Aborting...")
          }
        }
        ggsave( paste0(file.path(dir.path, file.name), ".pdf"), device = "pdf",
                plot = plot, limitsize = FALSE )
        message("[",class(self)[1],"][INFO] Plot has been succesfully saved at: ",
                file.path(dir.path,file.name,".pdf"))
      } #else { plot }

    },
    saveCSV = function(dir.path = NULL, name = NULL, num.clusters = NULL){
      if ( is.null(dir.path) )
        stop("[",class(self)[1],"][FATAL] Path not defined. Aborting...")

      if ( is.null(name) ) {
        name <- paste(class(private$heuristic[[1]])[1],
                      class(private$heuristic[[2]])[1],
                      sep = "-")
        message("[",class(self)[1],"][WARNING] File name not defined. Using '",
                name,".csv'")
      }

      if ( is.null(private$all.distribution) ||
           length(private$all.distribution) == 0 ) {
        stop("[",class(self)[1],"][FATAL] Clustering not done or errorneous. ",
             "Aborting...")
      }

      if ( !dir.exists(dir.path) ) {
        dir.create(dir.path, recursive = TRUE)
        if ( dir.exists(dir.path) ) {
          message("[",class(self)[1],"][INFO] Directory '", dir.path,
                  "'has been succesfully created")
        } else {
          stop("[",class(self)[1],"][FATAL] Cannot create directory '", dir.path,
               "'. Aborting...")
        }
      }

      if ( is.null(num.clusters) ) {
        message( "[",class(self)[1],"][WARNING] Number of clusters not defined. ",
                 "Saving all cluster configurations" )
        num.clusters <- list(list(1:max(private$all.distribution[[1]]$k)),
                             list(1:max(private$all.distribution[[2]]$k)))
      } else {
        if ( !(is.list(num.clusters) && length(num.clusters) >= 0)) {
          message( "[",class(self)[1],"][WARNING] Type of num.clusters not valid ",
                   "(must be NULL or list type). Saving all cluster configurations" )
          num.clusters <- list(list(1:max(private$all.distribution[[1]]$k)),
                               list(1:max(private$all.distribution[[2]]$k)))
        } else {
          if ( is.null(num.clusters[[1]]) || !is.list(num.clusters[[1]])) {
            num.clusters[[1]] <- list(1:max(private$all.distribution[[1]]$k))
          }
          if ( (length(num.clusters) >= 2 && (!is.list(num.clusters[[2]]) || is.null(num.clusters[[2]]))) ) {
            num.clusters[[2]] <- list(1:max(private$all.distribution[[2]]$k))
          }
        }
      }

      all.binary <- private$all.distribution[[1]]
      all.real <- private$all.distribution[[2]]

      if ( !all(unlist(num.clusters[[1]]) %in% all.binary$k) ) {
        message("[",class(self)[1],"][WARNING] Number of clusters incorrect. ",
                "Must be between ", min(all.binary$k), " and ", max(all.binary$k),
                ". Ignoring clustering for binary type features...")
        dist.binary <- data.frame( k = numeric(), dispersion = numeric(),
                                   feature_type = character() )
      } else {
        dist.binary <- data.frame( k = all.binary[c(all.binary$k %in% unlist(num.clusters[[1]])), "k"],
                                   dispersion = all.binary[c(all.binary$k %in% unlist(num.clusters[[1]])), "deltha"],
                                   feature_type = "binary", row.names = NULL)
      }

      if ( !all(unlist(num.clusters[[2]]) %in% all.real$k) ) {
        message("[",class(self)[1],"][WARNING] Number of clusters incorrect. ",
                "Must be between ", min(all.real$k), " and ", max(all.real$k),
                ". Ignoring clustering for real type features...")
        dist.real <- data.frame( k = numeric(), dispersion = numeric(),
                                 feature_type = character() )
      } else {
        dist.real <- data.frame(k = all.real[c(all.real$k %in% unlist(num.clusters[[2]])), "k"],
                                dispersion = all.real[c(all.real$k %in% unlist(num.clusters[[2]])), "deltha"],
                                feature_type = "real", row.names = NULL)
      }

      write.table( rbind(dist.binary, dist.real),
                   file = file.path(dir.path, paste0(name, ".csv")),
                   row.names = FALSE, col.names = TRUE, sep = ";")
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