library("R6")
SimpleStrategy <- R6Class(
  classname = "SimpleStrategy",
  inherit = StrategyGeneric,
  portable = TRUE,
  public = list(
    initialize = function(subset, heuristic, maxClusters = 50) {
      if (!"Subset" %in% class(subset)) {
        stop("[SimpleStrategy][ERROR] subset parameter must be defined as 'Subset' type")
      }
      if ("Heuristic" %in% class(heuristic)) {
        heuristic <- list(heuristic)
      } else {
        if (is.list(heuristic) && length(heuristic) >= 1) {
          if (length(heuristic) > 1) {
            message("[SimpleStrategy][INFO] SimpleStrategy only use one Heuristic. Assuming the first element on the heuristic list")
          }
          if (!"Heuristic" %in% class(heuristic[[1]])) {
            stop("[SimpleStrategy][ERROR] heuristic parameter must be defined as a 'Heuristic' type")
          } else {
            heuristic <- list(heuristic[[1]])
          }
        } else {
          stop("[SimpleStrategy][ERROR] heuristic parameter must be defined as a 'Heuristic' type or as a list of 'Heuristic' type")
        }  
      }  
      if (!is.numeric(maxClusters)) {
        stop("[SimpleStrategy][ERROR] maxClusters parameter must be defined as a 'numeric' type")
      } 
      super$initialize(
        name = "SimpleStrategy",
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
      private$all.distribution <- vector(mode = "list", length = 1)
      private$best.distribution <- vector(mode = "list", length = 1)
      private$best.distribution[[1]] <- data.frame(cluster = integer(), features = I(list()))
      private$not.distribution <- vector(mode = "list", length = 1)
      private$not.distribution[[1]] <- data.frame(cluster = integer(), features = I(list()))
      
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
      corpus <- private$subset$removeUnnecesary()
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
                                                  data.frame(cluster = i, 
                                                             dist = I(list(names(aux[aux == i])))))
        }
      }
      if (length(notHeuristic) > 0) {
        message("[", super$getName(), "][INFO] Adding features not distributed with ", private$heuristic[[1]]$getName(), " heuristic")
        private$not.distribution[[1]] <- rbind(private$not.distribution[[1]],
                                                data.frame(cluster = (nrow(private$not.distribution[[1]]) + 1),
                                                           dist = I(list(notHeuristic))))
      }      
    },
    getDistribution = function(cluster = NULL, group = NULL, includeClass = "NONE", ...) {
      if (is.null(private$best.distribution) || is.null(private$all.distribution)) {
        message("[", super$getName(), "][INFO] Function 'execute()' must be called first. Automatically run execute function")
        self$execute(...)
      }
      if (!toupper(includeClass) %in% c("NONE","BEGIN","END")) {
        message("[", super$getName(), "][INFO] Class parameter not included. Assuming class not included")
        class <- "NONE"
      } else { 
        class <- toupper(includeClass)
      }
      if (missing(cluster) || is.null(cluster) || (is.numeric(cluster) && (cluster == private$all.distribution[[1]][which.min(private$all.distribution[[1]][, 2]), 1]))) {
        switch(class,
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
            is.numeric(group) && group <= length(final.distr)) {
          final.distr[[group]]
        } else {
          if (!(is.numeric(group) && group <= length(final.distr))) {
            message("[", super$getName(), "][INFO] Group selected is not exist in the cluster. Assuming best distribution.")
          }
          final.distr
        }
      } else {
        distribution <- data.frame(cluster = integer(), features = I(list()))
        aux <- unlist(private$all.distribution[[1]][private$all.distribution[[1]]$k == cluster, ]$dist)
        if (!is.null(aux)) {
          for (i in 1:cluster) {
            distribution <-
              rbind(distribution, data.frame(cluster = i, dist = I(list(names(
                aux[aux == i]
              )))))
          }
          switch(class,
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
              is.numeric(group) && group <= length(final.distr)) {
            final.distr[[group]]
          } else {
            if (!(is.numeric(group) && group <= length(final.distr))) {
              message("[", super$getName(), "][INFO] Group selected is not exist in the cluster. Assuming best distribution.")
            }
            final.distr
          }
        } else {
          warning("[", super$getName(), "][WARNING] Cluster selected is not exist")
        }
      }
    },
    createSubset = function(subset, cluster = NULL, ...) {
      na.rm <- eval(substitute(alist(...))[["na.rm"]])
      if (!is.logical(na.rm)) {
        message("[", super$getName(), "][INFO] 'na.rm' parameter must be a logical value (TRUE or FALSE). Assuming na.rm = TRUE.")
        na.rm <- TRUE
      }
      if (is.null(private$all.distribution)) {
        message("[", super$getName(), "][INFO] Function 'execute()' must be called first. Automatically run execute function")
        self$execute(...)
      }
      if (!"Subset" %in% class(subset)) {
        stop("[", super$getName(), "][ERROR] Subset parameter must be defined as 'Subset' object")
      }
      if (is.null(cluster) || missing(cluster) || !is.numeric(cluster) || (is.numeric(cluster) && !cluster %in% c(min(private$all.distribution[[1]]$k):max(private$all.distribution[[1]]$k)))) {
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
    plot = function(dir.path = NULL, file.name = NULL, plotObject = list(BinaryPlot$new()), ...) {
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
      summary <- data.frame(k = private$all.distribution[[1]][, 1],
                            dispersion = private$all.distribution[[1]][, 2],
                            row.names = NULL)
      plot <- plotObject[[1]]$plot(summary)
      if (!is.null(dir.path)) {
        if (!dir.exists(dir.path)) {
          dir.create(dir.path, recursive = TRUE)
        }
        ggsave(paste0(file.path(dir.path, file.name), ".pdf"), device = "pdf", plot = plot, limitsize = FALSE)
        message("[", super$getName(), "][INFO] Plot has been succesfully saved at: ", paste0(file.path(dir.path, file.name), ".pdf"))
      } else { 
        invisible(show(plot))
      }
    }
  ),
  private = list(
    maxClusters = NULL
  )
)