Dataset <- R6Class(
  classname = "Dataset",
  portable = TRUE,
  public = list(
    initialize = function( filepath, header = TRUE, sep = ",", skip = 0, 
                           normalize.names = FALSE, class.index, positive.class ) {
      if (missing(class.index) || is.null(class.index)) { stop(red("[Dataset][ERROR] Class Index not defined")) }
      if (missing(positive.class) || !is.character(positive.class)) { stop(red("[Dataset][ERROR] positive.class not defined")) } 
      if (!file.exists(filepath)) { stop(red("[Dataset][ERROR] Cannot initialize corpus")) }
      
      if (header) {
        private$corpus <- read.csv(filepath, header = header, skip = (skip + 1), sep = sep)
        if(! (class.index %in% 1:ncol(private$corpus)) ){ 
          stop("[Dataset][ERROR] Class index exceeds dataset limits. Must be between 1 and ",ncol(private$corpus)," Aborting...\n")
        }
        columnNames <- unlist(strsplit(scan(file = filepath, nlines = 1, what = "character"), split = sep))
        if (isTRUE(normalize.names)) { columnNames <- make.names(columnNames, unique = TRUE) }
        names(private$corpus) <- columnNames
      } else { private$corpus <- read.csv(filepath, header = header, skip = skip, sep = sep) }
      
      private$class.index <- class.index
      private$class.name <- names(private$corpus)[private$class.index]
      
      if ( positive.class %in% private$corpus[,class.index] ){
        private$positive.class <- positive.class
        private$class.values <- as.character(unique(private$corpus[,class.index]))
      }else{ stop(red("[Dataset][ERROR] Positive class value not found. Aborting dataset loading...\n")) }
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
    createPartitions = function( num.folds, percent.folds ){
      if( (missing(num.folds) && missing(percent.folds)) || 
          (!is.numeric(num.folds) && !is.numeric(percent.folds)) ){
        message(yellow("[Dataset][WARNING] Parameters are invalid. Assuming division with default k=10 folds"))
        private$partitions <- createFolds( private$corpus[,private$class.index],
                                           k = 10, list = TRUE )
      }else{
        if ( is.numeric(num.folds) && missing(percent.folds) ){
          message( "[Dataset][INFO] Perfoming dataset partitioning into ",
                   num.folds," groups" )
          private$partitions <- createFolds( private$corpus[,private$class.index],
                                             k = num.folds, list = TRUE )
        }else{
          if( is.numeric(num.folds) && is.vector(percent.folds) ){
            if( length(percent.folds) == num.folds && 
                ( is.integer(percent.folds) && sum(percent.folds) == 100 || 
                  is.double(percent.folds) && sum(percent.folds) == 1 ) )
            {
              message("[Dataset][INFO] Perfoming dataset partitioning into",length(percent.folds),"groups")
              remaining <- private$corpus
              for (index in 1:(num.folds)-1){
                message("===============================================================\n")
                message("[Dataset][INFO] Spliting ",index," group with ",percent.folds[index],"\n")
                message("===============================================================")
                split <- createDataPartition(remaining$Activity, p=percent.folds[index],list = FALSE)
                private$partitions <- list.append( private$partitions, split)
                remaining <- remaining[-split,]
              }
              private$partitions <-  list.append( private$partitions, remaining)
              
              ifelse( ( num.folds < 10),
                      ( name(private$partitions) <- paste0("Fold0",which(1:num.folds < 10)) )
                      ( name(private$partitions) <- c( paste0("Fold0",which(1:num.folds < 10)), 
                                                       paste0("Fold",which(1:num.folds >=10)))) )
            }else{ message(red("[Dataset][ERROR] Fold partition and/or probability mismatch. Task not performed")) }
          }else{
            if( ( missing(num.folds) || !is.integer(num.folds) ) && 
                is.vector(percent.folds) && 
                ( is.integer(percent.folds) && sum(percent.folds) == 100 || 
                  is.double(percent.folds) && sum(percent.folds) == 1 )  ){
              message("[Dataset][INFO] Perfoming dataset partitioning into ",
                      length(percent.folds),"groups\n")
              remaining <- private$corpus
              for (index in 1:length(percent.folds)-1){
                message("===============================================================\n")
                message("[Dataset][INFO] Spliting ",index," group with ",percent.folds[index],"\n")
                message("===============================================================")
                split <- createDataPartition( remaining$Activity, 
                                              p=percent.folds[index],list = FALSE)
                private$partitions <- list.append( private$partitions, split)
                remaining <- remaining[-split,]
              }
              private$partitions <-  list.append( private$partitions, remaining)
              ifelse( ( num.folds < 10),
                      ( name(private$partitions) <- paste0("Fold0",which(1:num.folds < 10)) )
                      ( name(private$partitions) <- c( paste0("Fold0",which(1:num.folds < 10)), 
                                                       paste0("Fold",which(1:num.folds >=10)))) )
              
            }else{ message(red("[Dataset][ERROR] Cannot perform partition process. Aborted\n")) }
          }
        }
      }
    },
    createSubset = function( num.folds = NULL, 
                             opts = list(remove.na=TRUE, remove.const=FALSE)){
      subset <- NULL
      if (is.null(private$partitions)) {
        message(red("[Dataset][ERROR] Dataset distribution is null. Task not performed"))
        return(NULL)
      }
      if ( missing(num.folds) || is.null(num.folds) || !is.numeric(num.folds) || 
           !(max(num.folds) %in% c(1:length(private$partitions)) ) )
      {
        message(red("[Dataset][ERROR] Incorrect number of folds. Must be between 1 and ",
                    length(private$partitions),". Task not performed")) 
        return(NULL)
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