library("R6")
MultiTypeStrategy <- R6Class(
  classname = "MultiTypeStrategy",
  inherit = StrategyGeneric,
  portable = TRUE,
  public = list(
    initialize = function(subset, heuristic, maxClusters = 50) {
      if (!"Subset" %in% class(subset)) {
        stop("[MultiTypeStrategy][ERROR] subset parameter must be defined as 'Subset' type")
      }
      if (is.list(heuristic) && length(heuristic) >= 2) {
        if (length(heuristic) > 3) {
          warning("[MultiTypeStrategy][WARNING] MultiTypeStrategy only use two Heuristic. Assuming the first two elements on the heuristic list")
        }
        if (!"Heuristic" %in% class(heuristic[[1]]) || !"Heuristic" %in% class(heuristic[[2]])) {
          stop("[MultiTypeStrategy][ERROR] heuristic parameter must be defined as a list of 'Heuristic' type")
        } else {
          heuristic <- list(heuristic[[1]], heuristic[[2]])
        }
      } else {
        stop("[MultiTypeStrategy][ERROR] heuristic parameter must be defined as a list of 'Heuristic' type")
      }
      if (!is.numeric(maxClusters)) {
        stop("[MultiTypeStrategy][ERROR] maxClusters parameter must be defined as a 'numeric' type")
      } 
      super$initialize(
        name = "MultiTypeStrategy",
        subset = subset,
        heuristic = heuristic
      )
      private$maxClusters <- maxClusters
    },
    getMaxClusters = function() {
      private$maxClusters
    },
    setMaxClusters = function(max) {
      if (max > 1) {
        private$maxClusters <- max
      } else {
        message("[", super$getName(), "][ERROR] number of clusters must be greater than 1\n. Assuming default value")
      } 
    },
    execute = function(...) {
      private$all.distribution <- vector(mode = "list", length =  2)
      clusterData <- data.frame(k = integer(), homogeneity = numeric(), dist = I(list()))
      table <- private$heuristic[[1]]$heuristic(subset = private$subset)
      index <- order(table, decreasing = TRUE)
      totalGroups <- 2:self$getMaxClusters()
      for (k in totalGroups) {
        clustering <- rep(c(1:k, (k:1)), length(table)/(2 * k) + 1)[1:length(table)] 
        cluster <- vector(mode = "list", length = length(table))
        names(cluster) <- names(private$subset$getBinaryFeatures())
        sumGroup <- vector(mode = "list", length = k)
        for (i in 1:k) {
          sumGroup[[i]] <- table[index[clustering == i]]
          for (j in index[clustering == i]) {
            cluster[[j]] <- list(i)
          }
        }
        groupMeasure <- lapply(sumGroup, sum)
        deltha <- max(unlist(groupMeasure)) - min(unlist(groupMeasure))
        clusterData <- rbind(clusterData, data.frame(k = k, homogeneity = deltha, dist = I(list(cluster))))
      }
      private$all.distribution[[1]] <- clusterData
      
      private$best.distribution <- vector(mode = "list", length =  2)
      private$best.distribution[[1]] <- data.frame(cluster = integer(), features = I(list()))
      
      aux <- unlist(private$all.distribution[[1]][private$all.distribution[[1]]$k == private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1], ]$dist)
      
      for (i in 1:private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1]) {
        private$best.distribution[[1]] <- rbind(private$best.distribution[[1]], 
                                                data.frame(cluster = i, 
                                                           dist = I(list(names(aux[aux == i])))))
      }
      if (nrow(private$subset$removeUnnecesaryReal()) > 0) {
        if (private$heuristic[[2]]$getName() == "KendallHeuristic") {
          correlation.table <- private$heuristic[[2]]$heuristic(subset = private$subset)
          kendall.neg <- correlation.table[which(correlation.table < 0)]
          kendall.pos <- correlation.table[which(correlation.table > 0)]
          deltha <- abs(sum(kendall.pos) - abs(sum(kendall.neg)))
          kendallCluster <- list(names(kendall.neg), names(kendall.pos))
          names(kendallCluster) <- c(names(table(private$subset$getClass()))[which(!names(table(private$subset$getClass())) %in% private$subset$getPositiveClass())], private$subset$getPositiveClass()) 
          private$meanPositiveTau <- mean(kendall.pos)
          private$meanNegativeTau <- abs(mean(kendall.neg))
          clusterData <- data.frame(k = integer(), homogeneity = numeric(), dist = I(list()))
          clusterData <- rbind(clusterData, data.frame(k = length(names(table(private$subset$getClass()))), homogeneity = deltha, dist = I(list(kendallCluster))))
          kendallDistribution <- data.frame(cluster = c(1:length(names(table(private$subset$getClass())))), dist = I(kendallCluster))
          private$all.distribution[[2]] <- kendallDistribution
          private$best.distribution[[2]] <- kendallDistribution
        } else {
          clusterData <- data.frame(k = integer(), homogeneity = numeric(), dist = I(list()))
          table <- private$heuristic[[2]]$heuristic(subset = private$subset)
          index <- order(table, decreasing = TRUE)
          totalGroups <- 2:self$getMaxClusters()
          for (k in totalGroups) {
            clustering <- rep(c(1:k, (k:1)), length(table)/(2 * k) + 1)[1:length(table)] 
            cluster <- vector(mode = "list", length = length(table))
            names(cluster) <- names(private$subset$removeUnnecesaryReal())
            sumGroup <- vector(mode = "list", length = k)
            for (i in 1:k) {
              sumGroup[[i]] <- table[index[clustering == i]]
              for (j in index[clustering == i]) {
                cluster[[j]] <- list(i)
              }
            }
            groupMeasure <- lapply(sumGroup, sum)
            deltha <- max(unlist(groupMeasure)) - min(unlist(groupMeasure))
            clusterData <- rbind(clusterData, data.frame(k = k, homogeneity = deltha, dist = I(list(cluster))))
          }
          private$all.distribution[[2]] <- clusterData
          private$best.distribution[[2]] <- data.frame(cluster = integer(), features = I(list())) 
          aux <- unlist(private$all.distribution[[2]][private$all.distribution[[2]]$k == private$all.distribution[[2]][which.min(private$all.distribution[[2]][, 2]), 1], ]$dist) 
          for (i in 1:private$all.distribution[[2]][which.min(private$all.distribution[[2]][, 2]), 1]) {
            private$best.distribution[[2]] <- rbind(private$best.distribution[[2]], 
                                                    data.frame(cluster = i, 
                                                               dist = I(list(names(aux[aux == i])))))
          }
        }
      }
    },
    getDistribution = function(cluster = NULL, group = NULL, includeClass = "NONE", ...) {
      if (is.null(private$best.distribution) || is.null(private$all.distribution)) {
        warning("[", super$getName(), "][WARNING] Function 'execute()' must be called first. Automatically run execute function")
        self$execute(...)
      }
      if (!toupper(includeClass) %in% c("NONE","BEGIN","END")) {
        message("[", super$getName(), "][INFO] Class parameter not included. Assuming class not included")
        class <- "NONE"
      }else {
        class <- toupper(includeClass)
      }
      if (missing(cluster) || is.null(cluster)) {
        binaryK <- private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1]
        message("[", super$getName(), "][INFO] Assuming best cluster configuration for binary features (", binaryK, ")")
        if (private$heuristic[[2]]$getName() == "KendallHeuristic") {
          realK <- private$all.distribution[[2]][length(names(table(private$subset$getClass()))), 1]  
        } else {
          realK <- private$all.distribution[[2]][which.min(private$all.distribution[[2]][, 2]), 1]  
        }
        message("[", super$getName(), "][INFO] Assuming best cluster configuration for real features (", realK, ")")
      } else {
        if (!is.list(cluster) && length(cluster) != 2) {
          stop("[", super$getName(), "][ERROR] Variable cluster must to be a list with two elements")
        }
        if (!is.null(cluster[[1]]) &&
            is.numeric(cluster[[1]]) &&
            cluster[[1]] %in% c(
              min(private$all.distribution[[1]]$k):max(private$all.distribution[[1]]$k)
            )) {
          binaryK <- cluster[[1]]
          message("[", super$getName(), "][INFO] Selected cluster configuration for binary features (", cluster[[1]], ")")
        } else {
          binaryK <- private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1]
          message("[", super$getName(), "][INFO] Assuming best cluster configuration for binary features (", binaryK, ")")
        }
        if (!is.null(cluster[[2]]) &&
            is.numeric(cluster[[2]]) && (private$heuristic[[2]]$getName() != "KendallHeuristic" &&
            cluster[[2]] %in% c(
              min(private$all.distribution[[2]]$k):max(private$all.distribution[[2]]$k)
            ))) {
          realK <- cluster[[2]]
          message("[", super$getName(), "][INFO] Selected cluster configuration for real features (", cluster[[2]], ")")
        } else {
          if (private$heuristic[[2]]$getName() == "KendallHeuristic") {
            realK <- private$all.distribution[[2]][length(names(table(private$subset$getClass()))), 1]  
          } else {
            realK <- private$all.distribution[[2]][which.min(private$all.distribution[[1]][, 2]), 1]  
          }
          message("[", super$getName(), "][INFO] Assuming best cluster configuration for real features (", realK, ")")
        }
      }
      if (binaryK == private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1] && ((private$heuristic[[2]]$getName() == "KendallHeuristic" && realK == private$all.distribution[[2]][length(names(table(private$subset$getClass()))), 1]) || (private$heuristic[[2]]$getName() != "KendallHeuristic" && realK == private$all.distribution[[2]][which.min(private$all.distribution[[2]][, 2]), 1]))) {
        final.distr <- rbind(private$best.distribution[[1]], private$best.distribution[[2]])
      } else {
        if (binaryK == private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1] || 
            ((private$heuristic[[2]]$getName() == "KendallHeuristic" && realK == private$all.distribution[[2]][length(names(table(private$subset$getClass()))), 1]) || (private$heuristic[[2]]$getName() != "KendallHeuristic" && realK == private$all.distribution[[2]][which.min(private$all.distribution[[2]][, 2]), 1]))) {
          if (binaryK == private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1]) {
            realKDistribution <- data.frame(cluster = integer(), features = I(list()))
            aux <- unlist(private$all.distribution[[2]][private$all.distribution[[2]]$k == realK, ]$dist)
            for (i in 1:realK) {
              realKDistribution <- rbind(realKDistribution, data.frame(cluster = i, dist = I(list(names(aux[aux == i])))))
            }
            final.distr <- rbind(private$best.distribution[[1]], realKDistribution) 
          } else {
            binaryKDistribution <- data.frame(cluster = integer(), features = I(list()))
            aux <- unlist(private$all.distribution[[1]][private$all.distribution[[1]]$k == binaryK,]$dist)
            for (i in 1:binaryK) {
              binaryKDistribution <- rbind(binaryKDistribution, data.frame(cluster = i, dist = I(list(names(aux[aux == i])))))
            }
            final.distr <- rbind(binaryKDistribution, private$best.distribution[[2]]) 
          }
        } else {
          binaryKDistribution <- data.frame(cluster = integer(), features = I(list()))
          aux <- unlist(private$all.distribution[[1]][private$all.distribution[[1]]$k == binaryK, ]$dist)
          for (i in 1:binaryK) {
            binaryKDistribution <- rbind(binaryKDistribution, data.frame(cluster = i, dist = I(list(names(aux[aux == i])))) )
          }
          realKDistribution <- data.frame(cluster = integer(), features = I(list()))
          aux <- unlist(private$all.distribution[[2]][private$all.distribution[[2]]$k == realK,]$dist)
          for (i in 1:realK) {
            realKDistribution <- rbind(realKDistribution, data.frame(cluster = i, dist = I(list(names(aux[aux == i])))) )
          }
          final.distr <- rbind(binaryKDistribution, realKDistribution)
        }
      }
      final.distr[,1] <- seq(1:nrow(final.distr))
      switch(class,
             "NONE" = {
               final.distr <- final.distr[, 2]
             },
             "END" =  {
               final.distr <-
                 lapply(final.distr[, 2], function(x) {
                   append(x, private$className)
                 })
             },
             "BEGIN" = {
               final.distr <-
                 lapply(final.distr[, 2], function(x) {
                   append(x, private$className, 0)
                 })
             })
      if (!missing(group) && !is.null(group) && is.numeric(group) && group <= length(final.distr)) {
        final.distr[[group]]
      } else {
        final.distr
      }
    },
    createSubset = function(subset, cluster = NULL, ...) {
      if (is.null(private$all.distribution) || is.null(private$all.distribution)) {
        warning("[", super$getName(), "][WARNING] Function 'execute()' must be called first. Automatically run execute function.")
        self$execute(...)
      }
      if (is.null(subset) || !"Subset" %in% class(subset)) {
        stop("[", super$getName(), "][ERROR] Subset parameter must be defined as 'Subset' object")
      }
      if (is.null(cluster)) {
        binaryK <- private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1]
        if (private$heuristic[[2]]$getName() == "KendallHeuristic") {
          realK <- private$all.distribution[[2]][length(names(table(private$subset$getClass()))), 1]  
        } else {
          realK <- private$all.distribution[[2]][which.min(private$all.distribution[[2]][, 2]), 1]  
        }
        message("[", super$getName(), "][INFO] Assuming best cluster configuration for binary features (", binaryK, ")")
        message("[", super$getName(), "][INFO] Assuming best cluster configuration for real features (", realK, ")")
      } else {
        if (!is.list(cluster) && length(cluster) != 2) {
          stop("[", super$getName(), "][ERROR] Variable cluster must to be a list with two elements")
        }
        if (!is.null(cluster[[1]]) &&
            is.numeric(cluster[[1]]) &&
            cluster[[1]] %in% c(
              min(private$all.distribution[[1]]$k):max(private$all.distribution[[1]]$k)
            )) {
          binaryK <- cluster[[1]]
          message("[", super$getName(), "][INFO] Selected cluster configuration for binary features (", cluster[[1]], ")")
        } else {
          binaryK <- private$all.distribution[[1]][which.min(private$all.distribution[[2]][, 2]), 1]
          message("[", super$getName(), "][INFO] Assuming best cluster configuration for binary features (", binaryK, ")")
        }
        if (!is.null(cluster[[2]]) &&
            is.numeric(cluster[[2]]) &&
            (private$heuristic[[2]]$getName() != "KendallHeuristic" &&
            cluster[[2]] %in% c(
            min(private$all.distribution[[2]]$k):max(private$all.distribution[[2]]$k)
          ))) {
          realK <- cluster[[2]]
          message("[", super$getName(), "][INFO] Selected cluster configuration for real features (", cluster[[2]], ")")
        } else {
          if (private$heuristic[[2]]$getName() == "KendallHeuristic") {
            realK <- private$all.distribution[[2]][length(names(table(private$subset$getClass()))), 1]  
          } else {
            realK <- private$all.distribution[[2]][which.min(private$all.distribution[[2]][, 2]), 1]  
          }
          message("[", super$getName(), "][INFO] Assuming best cluster configuration for real features (", realK, ")")
        }
      }
      distribution <- self$getDistribution(list(binaryK, realK), includeClass = "NONE")
      
      cluster.dist <- vector(mode = "list")
      cluster.dist <- lapply(distribution, function(group) {
        Subset$new(dataset = subset$getInstances(features = c(subset$getClassName(), unlist(group))), classIndex = 1, positive.class = subset$getPositiveClass())
      })
      cluster.dist
    },
    plot = function(dir.path = NULL, file.name = NULL, plotObjectBinary = BinaryPlot$new(), plotObjectReal = PearsonPlot$new(), ...) { 
      if (!"Plot" %in% class(plotObjectBinary)) {
        stop("[", super$getName(), "][ERROR] plotObjectBinary parameter must be defined as 'Plot' type")
      }
      summaryBinary <- data.frame(k = private$all.distribution[[1]][, 1],
                            dispersion = private$all.distribution[[1]][, 2], 
                            row.names = NULL, check.names = FALSE)
      plot1 <- plotObjectBinary$plot(summary = summaryBinary)
    
      if (!"Plot" %in% class(plotObjectReal)) {
        stop("[", super$getName(), "][ERROR] plotObjectReal parameter must be defined as 'Plot' type")
      }
      if (private$heuristic[[2]]$getName() == "KendallHeuristic") {
        summaryReal <- data.frame(interval = c(private$subset$getPositiveClass(), 
                                               names(table(private$subset$getClass()))[which(!names(table(private$subset$getClass())) %in% private$subset$getPositiveClass())]),
                                  value = round(c(private$meanPositiveTau, private$meanNegativeTau), 3))
        plot2 <- KendallPlot$new()$plot(summary = summaryReal)
      } else {
        summaryReal <- data.frame(k = private$all.distribution[[2]][, 1],
                                  dispersion = private$all.distribution[[2]][, 2], 
                                  row.names = NULL, check.names = FALSE)
        plot2 <- plotObjectReal$plot(summary = summaryReal, ...)
      }
      dualPlot <- grid.arrange(plot1, plot2, nrow = 2, ncol = 1)
      if (!is.null(dir.path)) {
        if (!dir.exists(dir.path)) {
          dir.create(dir.path, recursive = TRUE) 
        }
        ggsave(paste0(file.path(dir.path, file.name), ".pdf"), device = "pdf", plot = dualPlot, limitsize = FALSE)
        message("[", super$getName(), "][INFO] Plot has been succesfully saved at: ", file.path(dir.path, file.name), ".pdf")
      }
    }
  ),
  private = list(
    maxClusters = NULL,
    meanPositiveTau = NULL,
    meanNegativeTau = NULL
  )
)