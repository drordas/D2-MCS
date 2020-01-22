DatasetLoader <- R6::R6Class(
  classname = "DatasetLoader",
  portable = TRUE,
  public = list(
    initialize = function() { self },
    load = function(filepath, header= TRUE, sep= ",", skip= 0,
                    normalize.names= FALSE, class.index= NULL,
                    positive.class= NULL, ignore.columns = NULL){

      if (is.null(filepath) || !file.exists(filepath)) {
        stop("[",class(self)[1],"][FATAL] Corpus cannot be found at defined ",
             "location. Aborting...")
      }

      dt.size <- (file.info(filepath)$size / 2^30)

      if ( dplyr::between(dt.size,0,1) ){

        if (is.null(class.index) || is.null(positive.class)){
          stop("[",class(self)[1],"][FATAL] Positive class was not defined. ",
               "Aborting...")
        }

        if( !is.numeric(class.index) || class.index < 0){
          stop("[",class(self)[1],"][FATAL] Class index is incorrect. ",
               "Must be an integer greater than 0. Aborting...")
        }

        if( is.numeric(ignore.columns) && class.index %in% ignore.columns ){
          message("[",class(self)[1],"][ERROR] Class cannot be ignored. ",
                  "Task not performed")
          ignore.columns <- setdiff(ignore.columns,class.index)
        }

        dataset <- Dataset$new(filepath= filepath, header= header, sep= sep,
                               skip= skip, normalize.names= normalize.names,
                               class.index= class.index,  positive.class = positive.class,
                               ignore.columns= ignore.columns)
      }else{
        dataset <- HDDataset$new(filepath = filepath,header = header,sep = sep,
                                 skip = skip,normalize.names = normalize.names,
                                 ignore.columns = ignore.columns)
      }
      dataset
    }
  )
)