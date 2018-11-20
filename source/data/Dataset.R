library("R6")
Dataset <- R6Class(
  classname = "Dataset",
  portable = TRUE,
  public = list(
    initialize = function(filepath, header=TRUE, sep=",", skip=0, normalize.names = FALSE, classIndex=NULL){
      if( is.null(classIndex) ) stop("[Dataset][ERROR] Class Index not defined\n")
      if( file.exists(filepath) ){
        if(header){
          private$corpus <- read.csv(filepath, header=header, skip=(skip+1), sep=sep)
          self$setClassIndex(classIndex)
          columNames <- unlist(strsplit(scan(file=filepath,nlines=1, what="character"),split=sep))
          if(isTRUE(normalize.names)){
            #self$setColumNames(sprintf("`%s`",make.names(columNames, unique = TRUE) ) )
            self$setColumNames( make.names(columNames, unique = TRUE) )
          }else self$setColumNames(columNames)
        }else{
          private$corpus <- read.csv(filepath, header=header, skip=skip, sep=sep)
          self$setClassIndex(classIndex)
        }
      }else stop("[Dataset][ERROR] Cannot initialize corpus\n")
    },
    setColumNames = function(names){
      names(private$corpus) <- names
      self$setClassIndex(private$classIndex)
    },
    getColumNames = function(){
      names(private$corpus)
    },
    getDataset = function(){
      private$corpus
    },
    getClassName = function(){
      private$className
    },
    getClassIndex = function(){
      private$classIndex
    },
    getNcol = function(){
      ncol(private$corpus)
    },
    getNrow = function(){
      nrow(private$corpus)
    },
    setClassIndex = function(classIndex){
      if(classIndex < ncol(private$corpus)){
        private$classIndex <- classIndex
        private$className <- names(private$corpus)[classIndex]
      } 
    },
    setClassName = function(className){
      private$classIndex <- which( names(private$corpus) == className )
      private$className <- className
    },
    executePartition = function(nfolds=10){
      private$partitions=Partitioner$new(data = private$corpus, nfolds= nfolds )
      private$partitions$execute()
    },
    getPartitions = function(){
      if(is.null(private$partitions)){
        cat("[DATASET][WARNING] Dataset distribution is null. Executing default distribution process\n")
        self$executePartition()
      }
      private$partitions
    },
    getSubset = function(numFolds = NULL){
      if(is.null(private$partitions)){
        cat("[DATASET][WARNING] Dataset distribution is null. Executing default distribution process\n")
        self$executePartition()
      }
      Subset$new( private$partitions$getInstances(numFolds), self$getClassIndex() )
    }
  ),
  private = list(
    corpus = NULL,
    classIndex = NULL,
    className = NULL,
    partitions = NULL
  )
)

Partitioner <- R6Class(
  classname = "Partitioner",
  portable = TRUE,
  public = list(
    initialize = function(data=NULL,nfolds=10){
      if( is.null(data) || nrow(data) == 0 ) 
        stop("[DATASET][ERROR] Dataset not defined or empty\n")
      private$data <- data
      private$folds <- nfolds
    },
    execute = function(){
      private$distribution <- caret::createFolds(data$getDataset()[,data$getClassIndex()], k = private$folds)
    },
    getFolds = function(numFolds = NULL){
      if(is.null(numFolds)){
        private$distribution 
      }else{
        if( length(numFolds) == 1 ){
          if( max(numFolds) <= length(private$distribution) )
            private$distribution[[numFolds]]
          else cat("[PARTITIONER][ERROR] Required fold exceeds maximum number of folds\n")
        }else{
          sort(Reduce(union,private$distribution[numFolds]))
        }
      }
    },
    getInstances = function(numPartition = NULL){
      if(is.null(numPartition) || numPartition > private$folds)
        stop("[PARTITIONER][ERROR] Number of folds must be inside 1 <= numPartitions <= ", private$folds)
      dist <- self$getFolds(numPartition)
      private$data[dist, ]
    },
    getNumFold = function(numPartition){
      if( numPartition <= length(private$distribution) ){
        private$distribution[[numPartition]]
      }
      else stop("[PARTITIONER][ERROR] Number of fold must be inside 1 <= numPartition <= ", length(private$distribution) )
    },
    removeFolds = function(){
      private$distribution <- vector(mode="list",length = folds)
    },
    getNumFolds = function(){
      private$folds
    }
  ),
  private = list (
    data = NULL,
    folds = 10,
    #instances = NULL,
    distribution = NULL
  )
)

Subset <- R6Class(
  classname = "Subset",
  portable = TRUE,
  public = list(
    initialize = function(dataset=NULL,classIndex=NULL){
      if( is.null(dataset) || is.null(classIndex) ) 
        stop("[Subset][ERROR] Dataset not defined or empty\n")
      private$data <- dataset
      private$classIndex <- classIndex
      private$instances <- c(1:nrow(private$data))
      private$features <- -(private$classIndex)
      private$className <- names(private$data)[private$classIndex]
    },
    getInstances = function(instances = private$instances, features = NULL, ignore.class = FALSE){
      if(is.null(features) && isTRUE(ignore.class))
        return (private$data[instances, -private$classIndex])
      if(is.null(features) && !isTRUE(ignore.class) )
        return (private$data[instances, ])
      if(!is.null(features) && ( is.vector(features) || is.numeric(features) ) )
        return (private$data[instances, features])
    },
    getFeatures = function(features = private$features){
      names(private$data)[private$features]
    },
    getClass = function(){
      private$data[,private$classIndex]
    },
    getClassIndex = function(){
      private$classIndex
    },
    getClassName = function(){
      private$className
    },
    getNcol = function(){
      ncol(private$data)
    },
    getNrow = function(){
      nrow(private$data)
    }#,
    # save = function(path){
    #   if(missing(path) || is.null(path)){
    #     out <- as.environment(as.list.environment(self))
    #     classs(out) <- c("Subset","R6")
    #     saveRDS(out,path)
    #   }cat("[Subset][ERROR] Save path not specified\n")
    # }
  ),
  private = list(
    data = NULL,
    classIndex = NULL,
    className = NULL,
    instances = NULL,
    features = NULL
  )
)