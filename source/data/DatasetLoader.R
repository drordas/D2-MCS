DatasetLoader <- R6::R6Class(
  classname = "DatasetLoader",
  portable = TRUE,
  public = list(
    initialize = function() { self },
    load = function(filepath, header= TRUE, sep= ",", skip= 0,
                    normalize.names= FALSE, ignore.columns = FALSE,
                    class.index= NULL, positive.class= NULL){

      if (!file.exists(filepath)) {
        stop("[",class(self)[1],"][FATAL] Corpus cannot be found at defined location")
      }

      dt.size <- (file.info(filepath)$size / 2^30)

      if ( dplyr::between(dt.size,0,1) ){

        if (is.null(class.index) || is.null(positive.class)){
          stop("[",class(self)[1],"][FATAL] Positive class was not defined")
        }

        if( !is.numeric(class.index) || class.index < 0){
          stop("[",class(self)[1],"][FATAL] Class index is incorrect. ",
               "Must be an integer greater than 0")
        }

        if( is.numeric(ignore.columns) && class.index %in% ignore.columns ){
          message("[",class(self)[1],"][ERROR] Class cannot be ignored")
          ignore.columns <- setdiff(ignore.columns,class.index)
        }

        if (!file.exists(filepath)) {
          stop("[",class(self)[1],"][FATAL] Corpus cannot be found at defined location")
        }

        dataset <- Dataset$new(filepath= filepath, header= header, sep= sep,
                               skip= skip, normalize.names= normalize.names,
                               ignore.columns= ignore.columns, class.index= class.index,
                               positive.class = positive.class)
      }else{
        dataset <- HDDataset$new(filepath = filepath,header = header,sep = sep,
                                 skip = skip,normalize.names = normalize.names,
                                 ignore.columns = ignore.columns)
      }
      dataset
    }
  )
)