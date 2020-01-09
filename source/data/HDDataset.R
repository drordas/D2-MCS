HDDataset <- R6::R6Class(
  classname = "HDDataset",
  portable = TRUE,
  public = list(
    initialize = function( filepath, header = TRUE, sep = ",", skip = 0,
                           normalize.names = FALSE, ignore.columns=FALSE)
    {
      if (!file.exists(filepath)) {
        stop("[",class(self)[1],"][ERROR] Corpus cannot be found at defined location")
      }

      private$file.path <- filepath
      dt.size <- (file.info(filepath)$size / 2^30)

      message("[",class(self)[1],"][INFO] Dataset size: ",
              round(dt.size,digits = 4)," Gb.")

      if(dt.size < 1){
        stop("[",class(self)[1],"][ERROR] Low Dimensional Dataset is not compatible with",
             " HDDataset class loader")
      }

      message("[",class(self)[1],"][INFO] Loading High Dimensional Dataset...")

      if (header) {
        column.names <- unlist(strsplit(scan(file= filepath, nlines= 1,
                                             what= "character", quiet = TRUE),
                                        split = sep))
        num.columns <- length(column.names)

        if (isTRUE(normalize.names))
          column.names <- make.names(column.names, unique = TRUE)

        private$corpus <- setNames(data.frame(matrix(ncol = num.columns,
                                                     nrow = 0)),
                                   column.names)
      }else {
        num.columns <- length(unlist(strsplit(scan(file= filepath, nlines= 1,
                                                   what= "character", quiet = TRUE),
                                              split = sep)))
        private$corpus <- data.frame(matrix(ncol = num.columns, nrow = 0))
      }

      private$sep <- sep
      private$start.at <- skip

      message("[",class(self)[1],"][INFO] Finish!")
    },
    getFeatureNames = function() { names(private$corpus) },
    getNcol = function() { ncol(private$corpus) },
    createSubset = function(column.id=FALSE,chunk.size=100000){
      if(isFALSE(all.equal(chunk.size, as.integer(chunk.size)))){
        message("[",class(self)[1],"][WARNING] Chunk size is not valid. Must ",
                "be an integer higher than 0. Asumming 100000 as default value")
        chunk.size <- 100000
      }

      if(is.numeric(column.id) && !dplyr::between(column.id,0,self$getNcol())){
        message("[",class(self)[1],"][WARNING] Identifier cannot exceed number",
                "of columns [1-",self$getNcol(),"]. Assuming no identifier")
        column.id <- FALSE
      }

      HDSubset$new(file.path= private$file.path, feature.names= private$corpus,
                   feature.id = column.id, start.at= private$start.at,
                   sep = private$sep, chunk.size=chunk.size)
    }
  ),
  private = list(
    file.path = NULL,
    corpus = NULL,
    start.at = 0,
    sep = ","
  )
)