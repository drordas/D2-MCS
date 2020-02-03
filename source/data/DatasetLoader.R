DatasetLoader <- R6::R6Class(
  classname = "DatasetLoader",
  portable = TRUE,
  public = list(
    initialize = function() { self },
    load = function(filepath, header = TRUE, sep = ",", skip.lines = 0,
                    target.class = NULL, positive.class= NULL,
                    normalize.names = FALSE, string.as.factor = FALSE,
                    ignore.columns = NULL ){

      if (is.null(filepath) || !file.exists(filepath)) {
        stop("[",class(self)[1],"][FATAL] Corpus cannot be found at defined ",
             "location. Aborting...")
      }

      dt.size <- (file.info(filepath)$size / 2^30)

      if ( dplyr::between(dt.size,0,1) ){

        if (is.null(positive.class)){
          stop("[",class(self)[1],"][FATAL] Positive class was not defined. ",
               "Aborting...")
        }

        if( !inherits(target.class, c("character","numeric") )){
          stop("[",class(self)[1],"][FATAL] Target class is incorrect. ",
               "Must contain a numerical or character value. Aborting...")
        }else{
          if ( is.character(target.class) && !isTRUE(header)) {
            stop("[",class(self)[1],"][FATAL] Cannot name target class ",
                 "without columns names.")
          }
        }

        # if( is.numeric(ignore.columns) && class.index %in% ignore.columns ){
        #   message("[",class(self)[1],"][ERROR] Class cannot be ignored. ",
        #           "Task not performed")
        #   ignore.columns <- setdiff(ignore.columns,class.index)
        # }

        dataset <- Dataset$new(filepath = filepath, header = header, sep = sep,
                               skip = skip.lines, normalize.names = normalize.names,
                               target.class = target.class,  positive.class = positive.class,
                               ignore.columns = ignore.columns)
      }else{
        dataset <- HDDataset$new(filepath = filepath, header = header, sep = sep,
                                 skip = skip.lines, normalize.names = normalize.names,
                                 ignore.columns = ignore.columns)
      }
      dataset
    }
  )
)