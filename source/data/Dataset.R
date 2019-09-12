library("R6")
Dataset <- R6Class(
  classname = "Dataset",
  portable = TRUE,
  public = list(
    initialize = function(filepath, positive.class, header = TRUE, sep = ",", skip = 0, normalize.names = FALSE, classIndex) {
      if (is.null(classIndex)) {
        stop("[Dataset][ERROR] Class Index not defined")
      }
      if (!is.character(positive.class)) {
        stop("[Dataset][ERROR] positive.class not defined")
      } 
      if (!file.exists(filepath)) {
        stop("[Dataset][ERROR] Cannot initialize corpus")  
      }
      if (header) {
        private$positive.class <- positive.class
        private$corpus <- read.csv(filepath, header = header, skip = (skip + 1), sep = sep)
        self$setClassIndex(classIndex)
        columNames <- unlist(strsplit(scan(file = filepath, nlines = 1, what = "character"), split = sep))
        if (isTRUE(normalize.names)) {
          self$setColumNames(make.names(columNames, unique = TRUE))
        } else {
          self$setColumNames(columNames)
        }
      } else {
        private$positive.class <- positive.class
        private$corpus <- read.csv(filepath, header = header, skip = skip, sep = sep)
        self$setClassIndex(classIndex)
      }
    },
    setColumNames = function(names) {
      names(private$corpus) <- names
      self$setClassIndex(private$classIndex)
    },
    getColumNames = function() {
      names(private$corpus)
    },
    getDataset = function() {
      private$corpus
    },
    getClassName = function() {
      private$className
    },
    getClassIndex = function() {
      private$classIndex
    },
    getNcol = function() {
      ncol(private$corpus)
    },
    getNrow = function() {
      nrow(private$corpus)
    },
    getPositiveClass = function() {
      private$positive.class
    },
    setPositiveClass = function(positiveClass) {
      private$positiveClass <- positiveClass
    },
    setClassIndex = function(classIndex) {
      if (classIndex < ncol(private$corpus)) {
        private$classIndex <- classIndex
        private$className <- names(private$corpus)[classIndex]
      } 
    },
    setClassName = function(className) {
      private$classIndex <- which(names(private$corpus) == className)
      private$className <- className
    },
    executePartition = function(nfolds = 10){
      private$partitions = Partitioner$new(data = private$corpus, nfolds = nfolds)
      private$partitions$execute()
    },
    getPartitions = function() {
      if (is.null(private$partitions)) {
        message("[Dataset][WARNING] Dataset distribution is null. Executing default distribution process")
        self$executePartition()
      }
      private$partitions
    },
    getSubset = function(numFolds = NULL){
      if (is.null(private$partitions)) {
        cat("[Dataset][WARNING] Dataset distribution is null. Executing default distribution process\n")
        self$executePartition()
      }
      Subset$new(private$partitions$getInstances(numFolds), self$getClassIndex(), self$getPositiveClass())
    }
  ),
  private = list(
    corpus = NULL,
    classIndex = NULL,
    className = NULL,
    partitions = NULL,
    positive.class = NULL
  )
)

Partitioner <- R6Class(
  classname = "Partitioner",
  portable = TRUE,
  public = list(
    initialize = function(data, nfolds = 10) {
      if (is.null(data) || nrow(data) == 0) { 
        stop("[DATASET][ERROR] Dataset not defined or empty")
      }
      private$data <- data
      private$folds <- nfolds
    },
    execute = function() {
      private$distribution <- caret::createFolds(data$getDataset()[, data$getClassIndex()], k = private$folds)
    },
    getFolds = function(numFolds = NULL) {
      if (is.null(numFolds)) {
        private$distribution 
      } else {
        if (length(numFolds) == 1 ) {
          if (max(numFolds) <= length(private$distribution)) {
            private$distribution[[numFolds]]
          } else {
            message("[Partitioner][ERROR] Required fold exceeds maximum number of folds")
          }
        } else {
          sort(Reduce(union,private$distribution[numFolds]))
        }
      }
    },
    getInstances = function(numPartition) {
      if (is.null(numPartition) || numPartition > private$folds) {
        stop("[Partitioner][ERROR] Number of folds must be inside 1 <= numPartitions <= ", private$folds)
      }
      dist <- self$getFolds(numPartition)
      private$data[dist, ]
    },
    removeFolds = function() {
      private$distribution <- vector(mode = "list", length = private$folds)
    },
    getNumFolds = function() {
      private$folds
    }
  ),
  private = list(
    data = NULL,
    folds = 10,
    distribution = NULL
  )
)

Subset <- R6Class(
  classname = "Subset",
  portable = TRUE,
  public = list(
    initialize = function(dataset, classIndex, positive.class) {
      if (is.null(dataset) || is.null(classIndex)) { 
        stop("[Subset][ERROR] Dataset not defined or empty")
      }
      if (!is.character(positive.class)) {
        stop("[Subset][ERROR] positive.class not defined")
      } 
      private$data <- dataset
      private$classIndex <- classIndex
      private$positive.class <- positive.class
      private$instances <- c(1:nrow(private$data))
      private$features <- -(private$classIndex)
      private$className <- names(private$data)[private$classIndex]
    },
    getInstances = function(instances = private$instances, features = NULL, ignore.class = FALSE) {
      if (is.null(features) && isTRUE(ignore.class)) {
        return(private$data[instances, -private$classIndex])
      }
      if (is.null(features) && !isTRUE(ignore.class)) {
        return(private$data[instances, ])
      }
      if (!is.null(features) && (is.vector(features) || is.numeric(features))) {
        return(private$data[instances, features])
      }
    },
    getFeatures = function(features = private$features) {  
      names(private$data)[features]
    },
    getClass = function() {
      private$data[, private$classIndex]
    },
    getClassIndex = function() {
      private$classIndex
    },
    getClassName = function() {
      private$className
    },
    getNcol = function() {
      ncol(private$data)
    },
    getNrow = function() {
      nrow(private$data)
    },
    getPositiveClass = function() {
      private$positive.class
    },
    setPositiveClass = function(positiveClass) {
      private$positiveClass <- positiveClass
    },
    getBinaryFeatures = function(ignore.class = TRUE) {
      binaryIndex <- sapply(self$getInstances(ignore.class = ignore.class), function(e) {
        (private$isBinary(e) || length(unique(e)) == 2)
      })
      self$getInstances(ignore.class = ignore.class)[, binaryIndex]
    },
    getRealFeatures = function(ignore.class = TRUE) {
      binaryIndex <- sapply(self$getInstances(ignore.class = ignore.class), function(e) {
        (private$isBinary(e) || length(unique(e)) == 2)
      })
      self$getInstances(ignore.class = ignore.class)[, !binaryIndex]
    },
    removeUnnecesary = function(ignore.class = TRUE) {
      private$removeUnnecesaryGeneric(self$getInstances(ignore.class = ignore.class))
    },
    removeUnnecesaryBinary = function() {
      private$removeUnnecesaryGeneric(self$getBinaryFeatures())
    },
    removeUnnecesaryReal = function() {
      private$removeUnnecesaryGeneric(self$getRealFeatures())
    },
    getUnnecesary = function() {
      private$getUnnecesaryGeneric(self$getInstances(ignore.class = ignore.class))
    },
    getUnnecesaryBinary = function() {
      private$getUnnecesaryGeneric(self$getBinaryFeatures())
    },
    getUnnecesaryReal = function() {
      private$getUnnecesaryGeneric(self$getRealFeatures())
    }
  ),
  private = list(
    data = NULL,
    classIndex = NULL,
    className = NULL,
    instances = NULL,
    features = NULL,
    positive.class = NULL,
    isBinary = function(column) {
      unique <- unique(column)
      if (!is.numeric(column) | any(is.na(column))) {
        return(FALSE)
      } else {
        return(!(any(as.integer(unique) != unique) || length(unique) > 2 || min(column) != 0 || max(column) != 1))
      }
    },
    removeUnnecesaryGeneric = function(corpus) {
      corpus[, sapply(corpus, function(c) {length(unique(c)) >= 2})]
    },
    getUnnecesaryGeneric = function(corpus) {
      names(corpus[, !sapply(corpus, function(c) {length(unique(c)) >= 2})])
    }
  )
)