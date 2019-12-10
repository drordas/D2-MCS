Dataset <- R6Class(
  classname = "Dataset",
  portable = TRUE,
  public = list(
    initialize = function( filepath, header = TRUE, sep = ",", skip = 0,
                           normalize.names = FALSE, ignore.columns = FALSE,
                           class.index, positive.class ){

      if (is.null(class.index) || is.null(positive.class)){
        stop("[",class(self)[1],"][ERROR] Positive class was not defined")
      }

      if( !is.numeric(class.index) || class.index < 0){
        stop("[",class(self)[1],"][ERROR] Class index is incorrect. ",
             "Must be an integer greater than 0")
      }

      if( is.numeric(ignore.columns) && class.index %in% ignore.columns ){
         message("[",class(self)[1],"][ERROR] Class cannot be ignored")
        ignore.columns <- setdiff(ignore.columns,class.index)
      }

      if (!file.exists(filepath)) {
        stop("[",class(self)[1],"][ERROR] Corpus cannot be found at defined location")
      }

      dt.size <- (file.info(filepath)$size / 2^30)

      message("[",class(self)[1],"][INFO] Dataset size: ",
              round(dt.size,digits = 4)," Gb.")

      if(dt.size > 1){
        stop("[",class(self)[1],"][ERROR] High Dimensional Dataset is not compatible with",
             "Dataset class loader")
      }

      message("[",class(self)[1],"][INFO] Loading Dataset...")

      if (header) {
        private$corpus <- read.csv( filepath, header= header,
                                    skip= (skip + 1), sep= sep )
        if(! (class.index %in% 1:ncol(private$corpus)) ){
          stop("[",class(self)[1],"][ERROR] Class index exceeds dataset limits.",
               " Must be between 1 and ",ncol(private$corpus)," Aborting...")
        }
        columnNames <- unlist(strsplit(scan(file = filepath, nlines = 1,
                                            what = "character"), split = sep))
        if (isTRUE(normalize.names)) {
          columnNames <- make.names(columnNames, unique = TRUE)
        }
        names(private$corpus) <- columnNames
      } else {
        private$corpus <- read.csv(filepath, header= header, skip= skip, sep= sep)
      }

      if(!isFALSE(ignore.columns) && is.numeric(ignore.columns)){
        self$removeColumns(ignore.columns)
      }

      private$class.index <- class.index
      private$class.name <- names(private$corpus)[private$class.index]

      if ( positive.class %in% private$corpus[,class.index] ){
        private$positive.class <- positive.class
        private$class.values <- as.character(unique(private$corpus[,class.index]))
      }else{
        stop("[",class(self)[1],"][ERROR] Positive class value not found.",
                      "Aborting dataset loading...")
      }

      message("[",class(self)[1],"][INFO] Finish. Total: ",
              nrow(private$corpus)," rows and ",
              ncol(private$corpus)," columns")
      message("[",class(self)[1],"][INFO] Class values: ",
              paste0(self$getClassValues(),collapse = ", "))

    },
    getColumnNames = function() { names(private$corpus) },
    getDataset = function() { private$corpus },
    getClassName = function() { private$class.name },
    getClassIndex = function() { private$class.index },
    getClassValues = function(){ private$class.values },
    getPositiveClass = function() { private$positive.class },
    getNcol = function() { ncol(private$corpus) },
    getNrow = function() { nrow(private$corpus) },
    setPositiveClass = function(positive.class) {
      if ( positive.class %in% private$class.values ){
        private$positive.class <- positive.class
      }else{ stop(red("[Dataset][ERROR] Positive class value not found. Task not done\n")) }
    },
    setClassIndex = function(class.index, positive.class) {
      if ( class.index %in% 1:ncol(private$corpus) ) {
        if( (positive.class %in% as.character(unique(private$corpus[,class.index])) ) ){
          private$class.values <- as.character(unique(private$corpus[,class.index]))
          private$class.index <- class.index
          private$class.name  <- names(private$corpus)[class.index]
          private$positive.class <- positive.class
        }else{ message( red("[Dataset][ERROR] Positive class value not found. Task not done.\n") ) }
      }else {
        message(red("[Dataset][ERROR] Class index exceeds dataset limits. Must be between 1 and ",ncol(private$corpus),". Task not done\n"))
      }
    },
    setClassName = function(class.name, positive.class) {
      if( (length(which(self$getColumnNames() == class.name )) == 0) ){
        message(red("[Dataset][ERROR] Class name not found. Task not done")) }
      else { self$setClassIndex(which(names(private$corpus) == class.name),positive.class) }
    },
    createPartitions = function( num.folds, percent.folds , class.balance = TRUE ){
      if( (missing(num.folds) && missing(percent.folds)) ||
          ((!is.numeric(num.folds) || length(num.folds) != 1) &&
           !is.numeric(percent.folds)) ) {
        message(yellow("[Dataset][WARNING] Parameters are invalid. Assuming division with default k=10 folds"))
        private$partitions <- createFolds( private$corpus[,private$class.index],
                                           k = 10, list = TRUE )
      }else{
        if ( is.numeric(num.folds) && length(num.folds) == 1 && missing(percent.folds) ) {
          message( "[Dataset][INFO] Perfoming dataset partitioning into ",
                   num.folds," groups" )
          private$partitions <- createFolds( private$corpus[,private$class.index],
                                             k = num.folds, list = TRUE )
        }else{
          if (!is.logical(class.balance)) { stop(red("[Dataset][ERROR] class.balance not defined")) }
          if ( is.numeric(num.folds) && length(num.folds) == 1 && is.vector(percent.folds) ) {
            if ( length(percent.folds) == num.folds && is.numeric(percent.folds) &&
                 ( sum(percent.folds) == 100 || sum(percent.folds) == 1 ) ) {
              if ( sum(percent.folds) == 100 ) {
                percent.folds <- percent.folds * 100
              }
              message("[Dataset][INFO] Perfoming dataset partitioning into ",length(percent.folds)," groups")
              remaining <- private$corpus

              for (index in 1:(num.folds - 1)) {
                message("===============================================================\n")
                message("[Dataset][INFO] Spliting ",index," group with ", percent.folds[index], "\n")
                message("===============================================================")
                if (class.balance) {
                  split <- createDataPartition(remaining[[self$getClassIndex()]], p = percent.folds[index],list = FALSE)
                } else {
                  split <- createDataPartition(remaining, p = percent.folds[index],list = FALSE)
                }
                private$partitions <- append( private$partitions, split)
                remaining <- remaining[-split,]
              }
              private$partitions <-  append( private$partitions, remaining)

              if( ( num.folds < 10 ) ) {
                names(private$partitions) <- paste0("Fold0",which(1:num.folds < 10))
              } else {
                names(private$partitions) <- paste0("Fold",which(1:num.folds >= 10))
              }
            }else{ message(red("[Dataset][ERROR] Fold partition and/or probability mismatch. Task not performed")) }
          }else{
            if( ( missing(num.folds) || !is.numeric(num.folds) || length(num.folds) != 1 )
                && is.numeric(percent.folds) && ( sum(percent.folds) == 100 ||
                                                  sum(percent.folds) == 1 ) ) {
              if (sum(percent.folds) == 1 ) {
                percent.folds <- percent.folds * 100
              }
              message("[Dataset][INFO] Perfoming dataset partitioning into ",
                      length(percent.folds)," groups")
              remaining <- private$corpus
              for (index in 1:(length(percent.folds) - 1)) {
                message("===============================================================\n")
                message("[Dataset][INFO] Spliting ",index," group with ",percent.folds[index],"\n")
                message("===============================================================")
                if (class.balance) {
                  split <- createDataPartition(remaining[[self$getClassIndex()]], p = percent.folds[index],list = FALSE)
                } else {
                  split <- createDataPartition(remaining, p = percent.folds[index],list = FALSE)
                }
                private$partitions <- append( private$partitions, split )
                remaining <- remaining[-split,]
              }
              private$partitions <- append( private$partitions, remaining )
              if( ( num.folds < 10 ) ) {
                names(private$partitions) <- paste0("Fold0",which(1:num.folds < 10))
              } else {
                names(private$partitions) <- paste0("Fold",which(1:num.folds >= 10))
              }
            }else{ message(red("[Dataset][ERROR] Cannot perform partition process. Aborted")) }
          }
        }
      }
    },
    createSubset = function( num.folds = NULL, column.id=NULL,
                             opts = list(remove.na=TRUE, remove.const=FALSE)){
      subset <- NULL
      if (is.null(private$partitions)) {
        message(red("[",class(self)[1],"][ERROR] Dataset distribution is null. Task not performed"))
        return(NULL)
      }
      if ( missing(num.folds) || is.null(num.folds) || !is.numeric(num.folds) ||
           !(max(num.folds) %in% c(1:length(private$partitions)) ) )
      {
        message("[",class(self)[1],"][ERROR] Incorrect number of folds. ",
                "Must be between 1 and ",length(private$partitions),
                ". Task not performed")
        return(NULL)
      }

      if ( !is.null(column.id) && !(column.id %in% 1:ncol(private$corpus)) ){
        message("[",class(self)[1],"][WARNING] Feature identifier is not correct. ",
                "Ignoring value")
        column.id <- NULL
      }

      subset <- private$corpus[ sort(Reduce(union,private$partitions[num.folds])), ]
      class.index <- private$class.index

      if( is.list(opts) ){
        na.remov <- 0
        const.remov <- 0
        filtered <- subset[,-private$class.index]

        if(exists("remove.na",opts) && isTRUE(opts$remove.na) ) {
          filtered <- Filter(function(col) !all(is.na(col)), filtered)
          na.remov <- ( (ncol(subset)-1) - ncol(filtered) )
          message("[Dataset][INFO] Removed columns containing NA values (total of ",na.remov,").")
        }

        if(exists("remove.const",opts) && isTRUE(opts$remove.const) ) {
          filtered <- Filter(function(col) sd(col, na.rm = TRUE) != 0, filtered)
          const.remov <- ( (ncol(subset)-1) - ncol(filtered) ) + na.remov
          message("[Dataset][INFO] Removed columns containing constant values (total of ",const.remov,").")
        }

        if( private$class.index >= ncol(filtered) ){
          subset <- cbind(filtered,subset[,private$class.index])
          class.index <- ncol(filtered)
        }else{
          if( private$class.index == 1 ){
            subset <- cbind( subset[,private$class.index], filtered )
          }else{
            subset <- cbind( filtered[1:private$class.index-1],
                             subset[,private$class.index],
                             filtered[private$class.index:ncol(filtered)] )
          }
        }
        names(subset)[class.index] <- private$class.name
      }

      Subset$new( dataset = subset,class.index = private$class.index,
                  class.values = self$getClassValues(),
                  positive.class = self$getPositiveClass() )
    },
    createTrain = function(num.folds = NULL,
                           opts = list(remove.na= TRUE, remove.const = FALSE)) {
      trainSet <- NULL
      if (is.null(private$partitions)) {
        message("[",class(self)[1],"][ERROR] Dataset distribution is null.",
                "Task not performed")
        return(NULL)
      }
      if ( missing(num.folds) || is.null(num.folds) || !is.numeric(num.folds) ||
           !(max(num.folds) %in% c(1:length(private$partitions)) ) )
      {
        message("[",class(self)[1],"][ERROR] Incorrect number of folds.",
                "Must be between 1 and ",length(private$partitions),
                ". Task not performed")
        return(NULL)
      }

      trainSet <- private$corpus[ sort(Reduce(union,private$partitions[num.folds])), ]
      class.index <- private$class.index
      if ( is.list(opts) ) {
        na.remov <- 0
        const.remov <- 0
        filtered <- trainSet[,-private$class.index]

        if ( exists("remove.na",opts) && isTRUE(opts$remove.na) ) {
          filtered <- Filter(function(col) !all(is.na(col)), filtered)
          na.remov <- ( (ncol(trainSet)-1) - ncol(filtered) )
          message("[",class(self)[1],"][INFO] Removed columns containing NA ",
                  "values (total of ",na.remov,").")
        }
        if( exists("remove.const",opts) && isTRUE(opts$remove.const) ) {
          filtered <- Filter(function(col) sd(col, na.rm = TRUE) != 0, filtered)
          const.remov <- ( (ncol(trainSet)-1) - ncol(filtered) ) + na.remov
          message("[",class(self)[1],"][INFO] Removed columns containing ",
                  "constant values (total of ",const.remov,").")
        }

        if ( private$class.index >= ncol(filtered) ) {
          trainSet <- cbind(filtered, trainSet[,private$class.index])
          class.index <- ncol(filtered)
        } else {
          if ( private$class.index == 1 ) {
            trainSet <- cbind( trainSet[,private$class.index], filtered )
          }else{
            trainSet <- cbind( filtered[1:private$class.index - 1],
                               trainSet[,private$class.index],
                               filtered[private$class.index:ncol(filtered)] )
          }
        }
        names(trainSet)[class.index] <- private$class.name
      }

      TrainSet$new( clusters = list(trainSet), class.name = self$getClassName(),
                    class.values = self$getClassValues(),
                    positive.class = self$getPositiveClass() )
    },
    removeColumns = function(index) {
      if(!is.null(index) && all(dplyr::between(index,1,ncol(private$corpus)))){
        private$corpus <- private$corpus[,-index]
        private$class.index <- which(private$corpus %in% private$class.name)
      }else{
        message("[",class(self)[1],"][ERROR] Class index out of bounds. ",
                "Must be between [1-",ncol(private$corpus),"]")
      }
    }
  ),
  private = list(
    positive.class = NULL,
    corpus = NULL,
    class.index = NULL,
    class.name = NULL,
    class.values = NULL,
    partitions = list()
  )
)