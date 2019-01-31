
defaultSortHeuristic <- function(corpus, k){
  sorted.corpus <- sort(corpus, decreasing = TRUE)
  cluster.index <- vector(length = length(corpus) )
  cluster.sum <- vector(length = k )
  cluster.sum[1:k] <- 0
  aux <- c()
  for(i in 1:length(sorted.corpus)){
    cat("METO: ",sorted.corpus[i],"\n")
    cat("BEGIN")
    print(cluster.sum)
    cat("##########\n")
    aux[1:k] <- sorted.corpus[i]
    cluster <- (which.min((cluster.sum+aux)-(max(cluster.sum))))
    cluster.index[i] <- cluster
    cluster.sum[cluster] <- cluster.sum[cluster]+sorted.corpus[i]
    cat("END\n")
    print(cluster.sum)
    cat("#########\n")
  }
  cluster.index
}

# ----------------------------------------------------------------------

completeSortHeuristic <- function(v,k){
  sorted.v <- sort(v, decreasing = TRUE)
  clusters <- list()
  for(i in 1:k){
    clusters[[i]] <- list()
  }
  names.clusters <- list()
  for(i in 1:k){
    names.clusters[[i]] <- list()
  }
  cluster.sum <- vector(length = k )
  cluster.sum[1:k] <- 0
  v.n <- c()
  for(i in 1:length(sorted.v)){
    v.n[1:k] <- sorted.v[i]
    cluster <- (which.min((cluster.sum+v.n)-(max(cluster.sum))))
    print(cluster)
    clusters[[cluster]][length(clusters[[cluster]])+1] <- sorted.v[i]
    names.clusters[[cluster]][length(names.clusters[[cluster]])+1] <- names(sorted.v[i])
    cluster.sum[cluster] <- cluster.sum[cluster]+sorted.v[i]
  }
  for(i in 1:k){
    names(clusters[[i]]) <- names.clusters[[i]]
  }
  clusters
}

# ----------------------------------------------------------------------
# ### TEST ###
# ----------------------------------------------------------------------
### Clustering completo ###
# ----------------------------------------------------------------------
# dataset <- c(6,4,5,4,7,2,3,3,1)
# names(dataset) <- c("A","B","C","D","E","F","G","H","I")
# k <- 3
# result <- completeSortHeuristic(dataset, k)
# str(result)
# str(unlist(result[[2]]))
# names(result[[2]])
# ----------------------------------------------------------------------
### Vector de índices de clustering ###
# ----------------------------------------------------------------------
# datasetCalculado <- c(6,4,5,4,7,2,3,3,1)
# names(dataset) <- c("A","B","C","D","E","F","G","H","I")
# k <- 4
# result <- defaultSortHeuristic(datasetCalculado, k)
# result
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------