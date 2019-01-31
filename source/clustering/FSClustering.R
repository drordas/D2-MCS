library("R6")
library("tools")

FSClustering <- R6Class(
  classname = "FSClustering",
  portable = TRUE,
  inherit = Cluster,
  public = list(
    initialize = function(dataset, maxClusters = 50) {
      if (!"Subset" %in% class(dataset))
        stop("[CLUSTER][ERROR] Dataset must be a Subset type\n")
      super$initialize(name = "FSCLUSTERING", dependences = "FSelector" , maxClusters)
      private$all.distribution <- NULL
      private$best.distribution <- NULL
      private$class <- dataset$getClass()
      private$className <- dataset$getClassName()
      private$dataset <- dataset$getInstances(ignore.class = FALSE)
      private$dataset.noclass <- dataset$getInstances(ignore.class = TRUE)
      private$heuristic <- super$defaultHeuristic
    },
    
    execute = function(method = NULL, heuristic = NULL) {
      if (is.null(method) ||
          !toupper(method) %in% c("IG", "CHI", "CC", "OR", "ORS", "SIG", "GR", "MI")) {
        cat("[",
            super$getName(),
            "][ERROR] Feature Selection Method not defined.\n")
        cat("\tFeature Selection methods available:\n")
        cat("\t\t-IG: Information Gain\n")
        cat("\t\t-CHI: Chi-square\n")
        cat("\t\t-CC: Correlation Coefficient\n")
        cat("\t\t-OR: Ods Ratio\n")
        cat("\t\t-ORS: Ods Ratio Squared\n")
        cat("\t\t-SIG: Signed Information Gain\n") #oneR
        cat("\t\t-GR: Gain Ratio\n")
        cat("\t\t-MI: Mutual Information\n")
        stop()
      }
      private$method <- method
      if (!is.null(heuristic) && is.function(heuristic))
        private$heuristic <- heuristic
      

      ### TODO: SWITCH ###
      if(private$method == "CHI")
        private$dataset <- private$removeUnnecesary(private$dataset.noclass)
      if(private$method == "OR" || private$method == "ORS" || private$method == "MI"){
        binaryIndex <- sapply( private$dataset, function(e){
          ( super$isBinary(e) || length( unique(e) ) == 2) 
        })
        if( dim(private$dataset[,!binaryIndex])[2] > 0 )
          private$dataset <- private$removeUnnecesary(private$dataset[,binaryIndex])
      }
      else
        private$dataset <- private$removeUnnecesary(private$dataset)
      
      
      private$all.distribution <- private$computeTest(private$dataset)
      private$best.distribution <- data.frame(cluster = integer(), features = I(list()))
      aux <- unlist(private$all.distribution$getClusterDist()[private$all.distribution$getClusterDist()$k == private$all.distribution$getBestK(),]$dist)
      for (i in 1:private$all.distribution$getBestK())
        private$best.distribution <-rbind(private$best.distribution, data.frame(cluster = i, dist = I(list(names(aux[aux == i])))))
      private$min <- min(private$all.distribution$getClusterDist()$k)
      private$max <- max(private$all.distribution$getClusterDist()$k)
    },
    
    
    plot = function(dir.path = NULL,file.name = NULL) {
      summary <-data.frame(
          k = private$all.distribution$getClusterDist()[, 1],
          dispersion = private$all.distribution$getClusterDist()[, 2],
          row.names = NULL
        )
      min <- data.frame(x = summary[which.min(summary[, 2]),][, 1], y = min(summary[, 2]))
      max <- data.frame(x = summary[which.max(summary[, 2]),][, 1], y = max(summary[, 2]))
      ggplot(summary, aes(k, dispersion)) + geom_line() + geom_point() +
        geom_point(
          aes(x, y),
          min,
          fill = "transparent",
          color = "blue",
          shape = 21,
          size = 3,
          stroke = 1
        ) +
        geom_text(aes(x, y, label = sprintf("%.10f", y)),
                  min,
                  hjust = -0.45,
                  color = 'blue') +
        geom_point(
          aes(x, y),
          max,
          fill = "transparent",
          color = "red",
          shape = 21,
          size = 3,
          stroke = 1
        ) +
        geom_text(aes(x, y, label = sprintf("%.3f", y)),
                  max,
                  hjust = -0.45,
                  color = 'red') +
        scale_x_continuous(breaks = seq(from = 2, to = nrow(summary) + 1), trans =
                             'log2') +
        scale_y_continuous(limits = c(min(summary$dispersion), max(summary$dispersion)), trans =
                             'sqrt') +
        labs(x = "Number of clusters", y = "Dispersion")
      
      if( !is.null(dir.path) ){
        if(!dir.exists(dir.path)) dir.create(dir.path,recursive = TRUE)
        ggsave(paste0(file.path(dir.path,file.name),".pdf"),device="pdf",plot=last_plot(), limitsize = FALSE)
        cat("[",super$getName(),"][INFO] Plot has been succesfully saved at: ",paste0(file.path(dir.path,file.name),".pdf"),"\n",sep="")
      }
    },
    getDistribution = function(cluster, group , includeClass = "NONE") {
      if (is.null(private$best.distribution) ||
          is.null(private$all.distribution)) {
        warning(
          "[",
          super$getName(),
          "][Warning] Function 'execute()' must be called first. Automatically run execute function\n",
          sep = ""
        )
        self$execute()
      }
      if (!toupper(includeClass) %in% c("NONE", "BEGIN", "END")) {
        cat(
          "[",
          super$getName(),
          "][INFO] Class parameter not included. Assuming class not included\n",
          sep = ""
        )
        class <- "NONE"
      } else
        class <- toupper(includeClass)
      if (missing(cluster) ||
          (is.numeric(cluster) &&
           (cluster == private$all.distribution$getBestK()))) {
        switch (class,
                "NONE" = {
                  final.distr <- private$best.distribution[, 2]
                },
                "END" =  {
                  final.distr <-
                    lapply(private$best.distribution[, 2], function(x) {
                      append(x, private$className)
                    })
                },
                "BEGIN" = {
                  final.distr <-
                    lapply(private$best.distribution[, 2], function(x) {
                      append(x, private$className, 0)
                    })
                })
        if (!missing(group) &&
            !is.null(group) &&
            is.numeric(group) && group <= length(final.distr))
          final.distr[[group]]
        else
          final.distr
      } else{
        distribution <- data.frame(cluster = integer(), features = I(list()))
        aux <-
          unlist(private$all.distribution$getClusterDist()[private$all.distribution$getClusterDist()$k ==
                                                             cluster,]$dist)
        for (i in 1:cluster)
          distribution <-
          rbind(distribution, data.frame(cluster = i, dist = I(list(names(
            aux[aux == i]
          )))))
        switch (class,
                "NONE" = {
                  final.distr <- distribution[, 2]
                },
                "END" = {
                  final.distr <-
                    lapply(distribution[, 2], function(x) {
                      append(x, private$className)
                    })
                },
                "BEGIN" = {
                  final.distr <-
                    lapply(distribution[, 2], function(x) {
                      append(x, private$className, 0)
                    })
                })
        if (!missing(group) &&
            !is.null(group) &&
            is.numeric(group) && group <= length(final.distr))
          final.distr[[group]]
        else
          final.distr
      }
    },
    createSubset = function(cluster = NULL, subset = NULL) {
      if (is.null(private$all.distribution)) {
        cat(
          "[",
          super$getName(),
          "][Warning] Function 'execute()' must be called first. Automatically run execute function\n",
          sep = ""
        )
        self$execute()
      }
      if (missing(subset) ||
          is.null(subset) || !"Subset" %in% class(subset))
        stop("[FSClustering][ERROR] Subset parameter must be defined as 'Subset' object\n")
      
      if (is.null(cluster) ||
          missing(cluster) || !is.numeric(cluster) ||
          (is.numeric(cluster) &&
           !cluster %in% c(private$min:private$max))) {
        cat(
          "[",
          super$getName(),
          "][WARNING] Incorrect cluster parameter. Should be between: ",
          private$min,
          " <= cluster <= ",
          private$max,
          "\n",
          sep = ""
        )
        cat(
          "                         Assuming best cluster configuration (",
          private$all.distribution$getBestK(),
          ")\n",
          sep = ""
        )
        cluster <- private$all.distribution$getBestK()
      }
      distribution <-
        self$getDistribution(cluster = cluster, includeClass = "NONE")
      cluster.dist <- ClusterDistribution$new()
      invisible(lapply(distribution, function(group) {
        cluster.dist$add(subset$getInstances(features = c(subset$getClassName(), unlist(group))), classIndex = 1)
      }))
      cluster.dist
    }
  ),
  
  
  private = list(
    computeTable = function(corpus) {
      switch (private$method,
              "IG" = {
                ig.test <- information.gain(as.formula(sprintf("`%s` ~.", private$className)),corpus)
                ig.values <- ig.test$attr_importance
                names(ig.values) <- row.names(ig.test)
                ig.zero <- ig.values[which(ig.values == 0, arr.ind = TRUE)]
                ig.nonzero <- ig.values[which(ig.values != 0, arr.ind = TRUE)]
                ig.values <- list("NonZero"=ig.nonzero,"Zero"=ig.zero)
                ig.values
              },
              "CHI" = { # WARNING! Chi-squared approximation may be incorrect
                chisq.result <- sapply(corpus,function(e){
                  chisq.test(private$class, e)$p.value
                  })
                names(chisq.result) <- names(corpus)
                chisq.result
              },
              "CC" = { # Preguntar a Tomás
                
              },
              "OR" = {
                oddsr.result <- sapply(corpus,function(e){
                  odds.ratio(private$class,e)$p
                })
                names(oddsr.result) <- names(corpus)
                oddsr.result
              },
              "ORS" = {
                oddsrs.result <- sapply(corpus,function(e){
                  odds.ratio(private$class,e)$p
                })
                names(oddsrs.result) <- names(corpus)
                sqrt(oddsrs.result)
              },
              "GR" = {
                gainr <- gain.ratio(as.formula(sprintf("`%s` ~.", private$className)),corpus)
                gainr.values <- gainr$attr_importance
                names(gainr.values) <- row.names(gainr)
                gainr.values
              },
              "MI" = {
                mutinf <- sapply(corpus,function(e){
                  mutinformation(private$class,e)
                })
                names(mutinf) <- names(corpus)
                mutinf
              })
    },
    
    computeTest = function(corpus) {
      clusteredData <- ClusterData$new()
      fs.table <- private$computeTable(corpus)
      if(private$method == "IG")
        fs.table <- unlist(unname(fs.table["NonZero"]))
      fs.index <- order(fs.table, decreasing = TRUE)
      fs.size <- length(fs.table)
      totalGroups <- 2:super$getMaxClusters()
      for (k in totalGroups) {
        clustering <- private$heuristic(fs.table, k)
        cluster <- integer(length = length(fs.table))
        names(cluster) <- names(fs.table)
        sumGroup <- vector(k, mode = "list")
        for (i in 1:k) {
          sumGroup[[i]] <- fs.table[fs.index[clustering == i]]
          cluster[fs.index[clustering == i]] <- i
        }
        groupMeasure <- lapply(sumGroup, sum)
        deltha <-
          max(unlist(groupMeasure)) - min(unlist(groupMeasure))
        clusteredData$addNewCluster(k, deltha, cluster)
      }
      clusteredData$setBestK(clusteredData$getClusterDist()[which.min(clusteredData$getClusterDist()[, 2]), 1])
      clusteredData
    },
    
    removeUnnecesary = function(corpus) {
      corpus[,sapply(corpus,function(c){length(unique(c)) >= 2})]
    },
    
    getUnnecesary = function(corpus) {
      names(corpus[, !sapply(corpus, function(c) {
        length(unique(c)) >= 2
      })])
    },
    
    data.unbinary = NULL,
    dataset = NULL,
    dataset.noclass = NULL,
    class = NULL,
    className = NULL,
    all.distribution = NULL,
    best.distribution = NULL,
    min = NULL,
    max = NULL,
    heuristic = NULL,
    method = NULL
  )
)