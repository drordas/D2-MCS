library("R6")
BinaryRealTypeStrategy <- R6Class(
  classname = "BinaryRealTypeStrategy",
  inherit = StrategyGeneric,
  portable = TRUE,
  public = list(
    initialize = function(subset, heuristic, maxClusters = 50) {
      if (!"Subset" %in% class(subset)) {
        stop("[BinaryRealTypeStrategy][ERROR] subset parameter must be defined as 'Subset' type")
      }
      if (!is.list(heuristic)) {
        stop("[BinaryRealTypeStrategy][ERROR] heuristic parameter must be defined as a 'list' type")
      }
      if (length(heuristic) == 0) {
        stop("[BinaryRealTypeStrategy][ERROR] heuristic parameter must be defined as a list of Heuristics")
      } 
      if (length(heuristic) == 1) {
        if (is.null(heuristic)[[1]]) {
          stop("[BinaryRealTypeStrategy][ERROR] heuristic parameter must be defined as a list of Heuristics")
        } 
        message("[BinaryRealTypeStrategy][INFO] BinaryRealTypeStrategy will use one Heuristic. Assuming only binary strategy")  
      } else {
        if (length(heuristic) == 2) {
          if (is.null(heuristic[[1]]) && is.null(heuristic[[2]])) {
            stop("[BinaryRealTypeStrategy][ERROR] heuristic parameter must be defined as a list of Heuristics")
          } 
          if (is.null(heuristic[[1]])) {
            message("[BinaryRealTypeStrategy][INFO] BinaryRealTypeStrategy will use one Heuristic. Assuming only real strategy")  
          } else {
            if (is.null(heuristic[[2]])) {
              message("[BinaryRealTypeStrategy][INFO] BinaryRealTypeStrategy will use one Heuristic. Assuming only binary strategy")  
            } else {
              message("[BinaryRealTypeStrategy][INFO] BinaryRealTypeStrategy can use only two Heuristic. Using the firsts heuristic on heuristic parameter")  
            }
          }
        }
      }
      if (!is.numeric(maxClusters)) {
        stop("[BinaryRealTypeStrategy][ERROR] maxClusters parameter must be defined as a 'numeric' type")
      } 
      super$initialize(
        name = "BinaryRealTypeStrategy",
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
        message("[", super$getName(), "][INFO] number of clusters must be greater than 1\n. Assuming default value")
      } 
    },
    execute = function(...) {
      private$all.distribution <- vector(mode = "list", length = 2)
      private$best.distribution <- vector(mode = "list", length = 1)
      private$best.distribution[[1]] <- data.frame(cluster = integer(), features = I(list()))
      private$not.distribution <- vector(mode = "list", length = 1)
      private$not.distribution[[1]] <- data.frame(cluster = integer(), features = I(list()))
      if (nrow(private$subset$removeUnnecesaryBinary()) > 0) {
        if (!is.null(private$heuristic[[1]])) {
          message("[", super$getName(), "][INFO] Using ", private$heuristic[[1]]$getName(), " heuristic to distribute binary features")
          clusterData <- data.frame(k = integer(), homogeneity = numeric(), dist = I(list()))
          class <- private$subset$getClass()
          class <- car::recode(class, 
                               paste0("'",
                                      private$subset$getPositiveClass(),
                                      "'='1'; '",
                                      names(table(class))[which(!names(table(class)) %in% private$subset$getPositiveClass())],
                                      "'='0'"),
                              as.numeric = TRUE,
                              as.factor = TRUE) #IMPROVED REMOVED LOOP
          class <- as.integer(as.integer(class) - 1)
          corpus <- private$subset$removeUnnecesaryBinary()
          tableH <- sapply(names(corpus), function(colName, class) {
            abs(private$heuristic[[1]]$heuristic(col1 = corpus[, colName], col2 = class, namesColums = c(colName, private$subset$getClassName())))
          }, class)
          table <- na.omit(tableH)
          notHeuristic <- setdiff(names(tableH), names(table))
          if (length(table) > 0) {
            for (k in 2:self$getMaxClusters()) {
              clustering <- rep(c(1:k, (k:1)), length(table)/(2 * k) + 1)[1:length(table)]
              cluster <- vector(mode = "list", length = length(table))
              names(cluster) <- names(table)
              sumGroup <- vector(mode = "list", length = k)
              for (i in 1:k) {
                sumGroup[[i]] <- table[order(table, decreasing = TRUE)[clustering == i]]
                for (j in order(table, decreasing = TRUE)[clustering == i]) {
                  cluster[[j]] <- list(i)
                }
              }
              groupMeasure <- lapply(sumGroup, sum)
              deltha <- max(unlist(groupMeasure)) - min(unlist(groupMeasure))
              clusterData <- rbind(clusterData, data.frame(k = k, homogeneity = deltha, dist = I(list(cluster))))
            }
            private$all.distribution[[1]] <- clusterData
            aux <- unlist(private$all.distribution[[1]][private$all.distribution[[1]]$k == private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1], ]$dist)
            for (i in 1:private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1]) {
              private$best.distribution[[1]] <- rbind(private$best.distribution[[1]], 
                                                      data.frame(cluster = i, dist = I(list(names(aux[aux == i])))))
            }
          }
          if (length(notHeuristic) > 0) {
            message("[", super$getName(), "][INFO] Adding binary features not distributed with ", private$heuristic[[1]]$getName(), " heuristic")
            private$not.distribution[[1]] <- rbind(private$not.distribution[[1]],
                                                   data.frame(cluster = (nrow(private$not.distribution[[1]]) + 1),
                                                              dist = I(list(notHeuristic))))
          }  
        } else {
          message("[", super$getName(), "][INFO] ", super$getName(), " has not heuristic to binary features. Creating only one cluster for them...")  
          clusterData <- data.frame(k = integer(), homogeneity = numeric(), dist = I(list()))
          clusterData <- rbind(clusterData, data.frame(k = 1, homogeneity = 0, dist = I(list(names(private$subset$getBinaryFeatures())))))
          private$all.distribution[[1]] <- clusterData
          private$best.distribution[[1]] <- rbind(private$best.distribution[[1]],
                                                  data.frame(cluster = (nrow(private$best.distribution[[1]]) + 1),
                                                             dist = I(list(names(private$subset$getBinaryFeatures())))))
        }
      } else {
        message("[", super$getName(), "][INFO] There are not binary features to clustering...")  
      }
      if (nrow(private$subset$removeUnnecesaryReal()) > 0) {
        if (length(private$heuristic) == 1 || is.null(private$heuristic[[2]])) {
          message("[", super$getName(), "][INFO] ", super$getName(), " has not heuristic to real features. Creating only one cluster for them...")  
          private$best.distribution[[1]] <- rbind(private$best.distribution[[1]],
                                                  data.frame(cluster = (nrow(private$best.distribution[[1]]) + 1),
                                                             dist = I(list(names(private$subset$getRealFeatures())))))
        } else {
          message("[", super$getName(), "][INFO] Using ", private$heuristic[[2]]$getName(), " heuristic to distribute real features")
          
          if (private$heuristic[[2]]$getName() == "KendallHeuristic") {
            class <- private$subset$getClass()
            class <- car::recode(class,
                                 paste0("'",
                                        private$subset$getPositiveClass(),
                                        "'='1'; '",
                                        names(table(class))[which(!names(table(class)) %in% private$subset$getPositiveClass())],
                                        "'='0'" ),
                                as.numeric = TRUE,
                                as.factor = TRUE) #IMPROVED REMOVED LOOP
            class <- as.integer(as.integer(class) - 1)
            corpus <- private$subset$removeUnnecesaryReal()
            tableH <- sapply(names(corpus), function(colName, class) {
              private$heuristic[[2]]$heuristic(col1 = corpus[, colName], col2 = class, namesColums = c(colName, private$subset$getClassName()))
            }, class)
            correlation.table <- na.omit(tableH)
            notHeuristic <- setdiff(names(tableH), names(correlation.table))
            
            if (length(table) > 0) {
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
            }
            if (length(notHeuristic) > 0) {
              message("[", super$getName(), "][INFO] Adding real features not distributed with ", private$heuristic[[2]]$getName(), " heuristic")
              private$not.distribution[[1]] <- rbind(private$not.distribution[[1]],
                                                     data.frame(cluster = (nrow(private$not.distribution[[1]]) + 1),
                                                                dist = I(list(notHeuristic))))
            }  
          } else {
            clusterData <- data.frame(k = integer(), homogeneity = numeric(), dist = I(list()))
            class <- private$subset$getClass()
            class <- car::recode(class,
                                 paste0("'",
                                        private$subset$getPositiveClass(),
                                        "'='1'; '",
                                        names(table(class))[which(!names(table(class)) %in% private$subset$getPositiveClass())],
                                        "'='0'"),
                                as.numeric = TRUE,
                                as.factor = TRUE) #IMPROVED REMOVED LOOP
            class <- as.integer(as.integer(class) - 1)
            corpus <- private$subset$removeUnnecesaryReal()
            tableH <- sapply(names(corpus), function(colName, class) {
              abs(private$heuristic[[2]]$heuristic(col1 = corpus[, colName], col2 = class, namesColums = c(colName, private$subset$getClassName())))
            }, class)
            table <- na.omit(tableH)
            notHeuristic <- setdiff(names(tableH), names(table))
            if (length(table) > 0) {
              for (k in 2:self$getMaxClusters()) {
                  clustering <- rep(c(1:k, (k:1)), length(table)/(2 * k) + 1)[1:length(table)] 
                  cluster <- vector(mode = "list", length = length(table))
                  names(cluster) <- names(table)
                  sumGroup <- vector(mode = "list", length = k)
                  for (i in 1:k) {
                    sumGroup[[i]] <- table[order(table, decreasing = TRUE)[clustering == i]]
                    for (j in order(table, decreasing = TRUE)[clustering == i]) {
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
            if (length(notHeuristic) > 0) {
              message("[", super$getName(), "][INFO] Adding real features not distributed with ", private$heuristic[[2]]$getName(), " heuristic")
              private$not.distribution[[1]] <- rbind(private$not.distribution[[1]],
                                                     data.frame(cluster = (nrow(private$not.distribution[[1]]) + 1),
                                                                dist = I(list(notHeuristic))))
            }  
          }
        }
      } else {
        message("[", super$getName(), "][INFO] There are not real features to clustering...")  
      }
    },
    getDistribution = function(cluster = NULL, group = NULL, includeClass = "NONE", ...) {
      if (is.null(private$best.distribution) || is.null(private$all.distribution)) {
        message("[", super$getName(), "][INFO] Function 'execute()' must be called first. Automatically run execute function")
        self$execute(...)
      }
      if (!toupper(includeClass) %in% c("NONE","BEGIN","END")) {
        message("[", super$getName(), "][INFO] Class parameter not included. Assuming class not included")
        includeClass <- "NONE"
      }else {
        includeClass <- toupper(includeClass)
      }
      if (length(private$heuristic) == 1 || (length(private$heuristic) == 2 && is.null(private$heuristic[[2]]))) {
        private$getDistributionBinary(cluster = cluster, group = group, includeClass = includeClass, ...)
      } else {
        private$getDistributionBoth(cluster = cluster, group = group, includeClass = includeClass, ...)
      }
    },
    createSubset = function(subset, cluster = NULL, ...) {
      if (is.null(private$all.distribution) || is.null(private$all.distribution)) {
        message("[", super$getName(), "][INFO] Function 'execute()' must be called first. Automatically run execute function.")
        self$execute(...)
      }
      if (missing(subset) || is.null(subset) || !"Subset" %in% class(subset)) {
        stop("[", super$getName(), "][ERROR] Subset parameter must be defined as 'Subset' object")
      }
      if (length(private$heuristic) == 1 || (length(private$heuristic) == 2 && is.null(private$heuristic[[2]]))) {
        private$createSubsetBinary(subset = subset, cluster = cluster, ...)
      } else {
        private$createSubsetBoth(subset = subset, cluster = cluster, ...)
      }
    },
    plot = function(dir.path = NULL, file.name = NULL, plotObject = list(), ...) {
      if (!is.list(plotObject)) {
        stop("[", super$getName(), "][ERROR] plotObject parameter must be defined as 'list' type")
      }
      if (length(plotObject) == 0) {
        stop("[", super$getName(), "][ERROR] plotObject parameter must be defined as a list of Plots")
      } else {
        if (length(plotObject) == 1) {
          if (is.null(plotObject)[[1]] || !"Plot" %in% class(plotObject[[1]])) {
            stop("[", super$getName(), "][ERROR] plotObject parameter must be defined as a list of Plot objects")
          } else {
            message("[", super$getName(), "][INFO] ", super$getName(), " will use one Plot Assuming only binary plot")  
          }
        } else {
          if (length(plotObject) == 2) {
            if (is.null(plotObject[[1]]) && is.null(plotObject[[2]])) {
              stop("[", super$getName(), "][ERROR] plotObject parameter must be defined as a list of Plots")
            } else {
              if (is.null(plotObject[[1]]) || !"Plot" %in% class(plotObject[[1]])) {
                message("[", super$getName(), "][INFO] ", super$getName(), " will use one Plot. Assuming only real plot")  
              } else {
                if (is.null(plotObject[[2]]) || !"Plot" %in% class(plotObject[[2]]) ) {
                  message("[", super$getName(), "][INFO] ", super$getName(), " will use one Plot. Assuming only binary plot")  
                } else {
                  message("[", super$getName(), "][INFO] ", super$getName(), " can use only two Plot. Using the firsts plots on plotObject parameter")  
                }
              }
            }
          }
        }
      }
      plot <- NULL
      plotBinary <- NULL
      if (!is.null(plotObject[[1]]) && !is.null(private$heuristic[[1]])) {
        summary <- data.frame(k = private$all.distribution[[1]][, 1],
                              dispersion = private$all.distribution[[1]][, 2], 
                              row.names = NULL, 
                              check.names = FALSE)
        plotBinary <- plotObject[[1]]$plot(summary)
      }
      plotReal <- NULL
      if (length(plotObject) == 2 && !is.null(plotObject[[2]]) && length(private$heuristic) >= 2 && !is.null(private$heuristic[[2]])) {
        if (length(private$heuristic) == 2 && !is.null(private$heuristic[[2]])) {
          if (private$heuristic[[2]]$getName() == "KendallHeuristic") {
            summaryReal <- data.frame(interval = c(private$subset$getPositiveClass(), 
                                                   names(table(private$subset$getClass()))[which(!names(table(private$subset$getClass())) %in% private$subset$getPositiveClass())]),
                                      value = round(c(private$meanPositiveTau, private$meanNegativeTau), 3))
            plotReal <- KendallPlot$new()$plot(summary = summaryReal)
          } else {
            summaryReal <- data.frame(k = private$all.distribution[[2]][, 1],
                                      dispersion = private$all.distribution[[2]][, 2], 
                                      row.names = NULL, check.names = FALSE)
            plotReal <- plotObject[[2]]$plot(summary = summaryReal, ...)
          }
          if (!is.null(plotBinary)) {
            plot <- grid.arrange(plotBinary, plotReal, nrow = 2, ncol = 1)
          }
        } else {
          message("[", super$getName(), "][INFO] There is not heuristic to real features. Imposible generates plot of real features")  
        }
      }
      if (is.null(plot)) {
        if (!is.null(plotBinary)) {
          plot <- plotBinary 
        } else {
          if (!is.null(plotReal)) {
            plot <- plotReal
          }
        }
      }
      if (!is.null(dir.path)) {
        if (!dir.exists(dir.path)) {
          dir.create(dir.path, recursive = TRUE) 
        }
        ggsave(paste0(file.path(dir.path, file.name), ".pdf"), device = "pdf", plot = plot, limitsize = FALSE)
        message("[", super$getName(), "][INFO] Plot has been succesfully saved at: ", paste0(file.path(dir.path, file.name), ".pdf"))
      } else {
        show(plot)
      }
    }
  ),
  private = list(
    maxClusters = NULL,
    meanPositiveTau = NULL,
    meanNegativeTau = NULL,
    getDistributionBinary = function(cluster = NULL, group = NULL, includeClass = "NONE", ...) {
      if (missing(cluster) ||
          is.null(cluster) ||
          (is.list(cluster) && length(cluster) == 0) ||
          (is.list(cluster) && is.null(cluster[[1]])) ||
          (is.list(cluster) && (is.numeric(cluster[[1]]) &&
                                (cluster[[1]] == private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1]))) ||
          (is.numeric(cluster) &&
           (cluster == private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1]))) {
        switch(includeClass,
               "NONE" = {
                 final.distr <- private$best.distribution[[1]][, 2]
               },
               "END" =  {
                 final.distr <-
                   lapply(private$best.distribution[[1]][, 2], function(x) {
                     append(x, private$subset$getClassName())
                   })
               },
               "BEGIN" = {
                 final.distr <-
                   lapply(private$best.distribution[[1]][, 2], function(x) {
                     append(x, private$subset$getClassName(), 0)
                   })
               })
        if (!missing(group) &&
            !is.null(group) &&
            (is.list(group) &&
             length(group) != 0 &&
             !is.null(cluster[[1]]) &&
             is.numeric(group[[1]])) &&
            group[[1]] <= length(final.distr) ||
            (is.numeric(group) && 0 < group && group <= length(final.distr))) {
          if (is.list(group)) {
            group <- group[[1]]
          }
          final.distr[[group]]
        } else {
          if (!(is.list(group) && length(group) != 0 && group[[1]] <= length(final.distr)) || !(is.numeric(group) && 0 < group && group <= length(final.distr))) {
            message("[", super$getName(), "][INFO] Group selected is not exist in the cluster. Assuming best distribution.")
          }
          final.distr
        }
      } else {
        if (is.list(cluster)) {
          cluster <- cluster[[1]]
        }
        distribution <- data.frame(cluster = integer(), features = I(list()))
        aux <- unlist(private$all.distribution[[1]][private$all.distribution[[1]]$k == cluster,]$dist)
        if (!is.null(aux)) {
          for (i in 1:cluster) {
            distribution <- rbind(distribution, 
                                  data.frame(cluster = i, 
                                             dist = I(list(names(aux[aux == i])))))
          }
          if (nrow(private$subset$getRealFeatures()) > 0) {
            distribution <- rbind(distribution, 
                                  data.frame(cluster = cluster + 1, 
                                             dist = I(list(names(private$subset$getRealFeatures())))))
          }
          switch(includeClass,
                 "NONE" = {
                   final.distr <- distribution[, 2]
                 },
                 "END" = {
                   final.distr <-
                     lapply(distribution[, 2], function(x) {
                       append(x, private$subset$getClassName())
                     })
                 },
                 "BEGIN" = {
                   final.distr <-
                     lapply(distribution[, 2], function(x) {
                       append(x, private$subset$getClassName(), 0)
                     })
                 })
          if (!missing(group) &&
              !is.null(group) &&
              (is.list(group) &&
               length(group) != 0 &&
               !is.null(cluster[[1]]) &&
               is.numeric(group[[1]])) &&
              group[[1]] <= length(final.distr) ||
              (is.numeric(group) && 0 < group && group <= length(final.distr))) {
            if (is.list(group)) {
              group <- group[[1]]
            }
            final.distr[[group]]
          } else {
            if (!(is.list(group) && length(group) != 0 && group[[1]] <= length(final.distr)) || !(is.numeric(group) && 0 < group && group <= length(final.distr))) {
              message("[", super$getName(), "][INFO] Group selected is not exist in the cluster. Assuming best distribution.")
            }
            final.distr
          } 
        } else {
          warning("[", super$getName(), "][WARNING] Cluster selected is not exist")
        }
      }
    },
    getDistributionBoth = function(cluster = NULL, group = NULL, includeClass = "NONE", ...) {
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
      switch(includeClass,
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
      if (!missing(group) && !is.null(group)) {
        if (is.numeric(group) && group <= length(final.distr)) {
          final.distr[[group]]
        } else {
          if (is.list(group) && length(group) == 1 && is.numeric(group[[1]]) && group[[1]] <= length(final.distr)) {
            final.distr[[group[[1]]]]
          } else {
            if (is.list(group) && length(group) == 2 && is.numeric(group[[1]]) && is.numeric(group[[2]]) && group[[1]] <= binaryK && group[[2]] <= realK) {
              pos <- binaryK + group[[2]] 
              final.distr[c(group[[1]], pos)]
            } else  {
              final.distr
            }
          }
        }
      } else {
        final.distr
      }
    },
    createSubsetBinary = function(subset, cluster = NULL, ...) {
      na.rm <- eval(substitute(alist(...))[["na.rm"]])
      if (!is.logical(na.rm)) {
        message("[", super$getName(), "][INFO] 'na.rm' parameter must be a logical value (TRUE or FALSE). Assuming na.rm = TRUE.")
        na.rm = TRUE
      }
      if (is.null(cluster) ||
          missing(cluster) ||
          !is.numeric(cluster) ||
          (is.numeric(cluster) &&
           !cluster %in% c(
             min(private$all.distribution[[1]]$k):max(private$all.distribution[[1]]$k)
           ))) {
        message("[", super$getName(), "][INFO] Incorrect cluster parameter. Should be between: ", min(private$all.distribution[[1]]$k), " <= cluster <= ", max(private$all.distribution[[1]]$k))
        message("[", super$getName(), "][INFO] Assuming best cluster configuration (", private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1], ")")
        cluster <- private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1]
      }
      distribution <- self$getDistribution(cluster = cluster, includeClass = "NONE")
      cluster.dist <- vector(mode = "list")
      if (na.rm) {
        cluster.dist <- lapply(distribution, function(group) {
          group.features <- subset$getInstances(features = c(unlist(group)))
          sd.result <- apply(group.features, 2, sd, na.rm = TRUE)
          sd.result <- sd.result[-which(sd.result == 0, arr.ind = TRUE)]
          if (length(sd.result != 0)) {
            Subset$new(dataset = subset$getInstances(features = c(subset$getClassName(), names(sd.result))), classIndex = 1, positive.class = subset$getPositiveClass())
          } else {
            Subset$new(dataset = subset$getInstances(features = c(subset$getClassName(), unlist(group))), classIndex = 1, positive.class = subset$getPositiveClass())
          }
        })
      } else {
        cluster.dist <- lapply(distribution, function(group) {
          Subset$new(dataset = subset$getInstances(features = c(subset$getClassName(), unlist(group))), classIndex = 1, positive.class = subset$getPositiveClass())
        })
      }
      cluster.dist
    },
    createSubsetBoth = function(subset, cluster = NULL, ...) {
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
          binaryK <- private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1]
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
    }
  )
)