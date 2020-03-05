Subset <- R6::R6Class(
  classname = "Subset",
  portable = TRUE,
  public = list(
    initialize = function(dataset, class.index = NULL, class.values = NULL,
                          positive.class = NULL, feature.id = NULL) {
       if (any(is.null(dataset), nrow(dataset) == 0, !is.data.frame(dataset))) {
        stop("[", class(self)[1], "][FATAL] Dataset empty or incorrect ",
             "(must be a data.frame). Aborting...")
      }
      private$data <- dataset
      if (any(is.null(class.index), is.null(class.values), is.null(positive.class))) {
        message("[", class(self)[1], "][INFO] Subset created without an associated class")
        class.index <- NULL
        class.values <- NULL
        positive.class <- NULL
        private$class.name <- NULL
        private$positive.class <- NULL
        private$feature.names <- names(private$data)
      } else {
        if (!is.numeric(class.index) || !class.index %in% c(1:ncol(dataset))) {
          stop("[", class(self)[1], "][FATAL] Class index parameter is incorrect. ",
               "Must be between 1 and ", ncol(dataset), ". Aborting...")
        }

        if (!is.factor(class.values)) {
          stop("[", class(self)[1], "][FATAL] Class values parameter must be defined ",
               "as 'factor' type. Aborting...")
        }
        private$positive.class <- positive.class

        if (!private$positive.class %in% dataset[[class.index]]) {
          stop("[", class(self)[1], "][FATAL] Positive Class parameter is incorrect. ",
               "Must be '", paste(levels(class.values), collapse = "' '"), "'. Aborting...")
        }

        class.values <- relevel(x = factor(class.values,
                                           levels = unique(class.values)),
                                ref = as.character(private$positive.class))

        if (!all(class.values == relevel(x = factor(dataset[[class.index]],
                                                    levels = unique(dataset[[class.index]])),
                                         ref = as.character(private$positive.class)))) {
          stop("[", class(self)[1], "][FATAL] Class values parameter is incorrect. ",
               "Must match with the values in column ", class.index, " in the ",
               "dataset. Aborting...")
        }

        private$class.name <- names(private$data)[class.index]
        private$feature.names <- names(private$data[, -class.index])
      }

      private$class.index <- class.index
      private$feature.id <- feature.id
      private$class.values <- class.values
    },
    getFeatureNames = function() { private$feature.names },
    getFeatures = function(feature.names = NULL) {
      if (is.vector(feature.names) && length(feature.names) > 0) {
        if (is.null(private$class.index)) {
          private$data[, feature.names]
        } else {
          private$data[, intersect(names(private$data[, -private$class.index]), feature.names)]
        }
      } else {
        if (is.null(private$class.index)) {
          private$data
        } else {
          private$data[, -private$class.index]
        }
      }
    },
    getID = function(){
      if (!is.null(private$feature.id))
        private$feature.names[private$feature.id]
      else private$feature.id
    },
    getIterator = function(chunk.size = private$chunk.size, verbose = FALSE) {
      if(!is.numeric(chunk.size)){
        message("[",class(self)[1],"][WARNING] Chunk size is not valid. ",
                "Assuming default value")
        chunk.size <- private$chunk.size
      }

      if(!is.logical(verbose)){
        message("[",class(self)[1],"][WARNING] Verbose type is not valid. ",
                "Assuming 'FALSE' as default value")
        verbose <- FALSE
      }
      DIterator$new(data = private$data,chunk.size = chunk.size,
                    verbose = verbose)
    },
    getClassValues = function() { private$class.values },
    getClassBalance = function(target.value = NULL) {
      if (is.null(private$class.index)) {
        message("[", class(self)[1], "][WARNING] Subset has no associated class. ",
                "Task not performed")
      } else {
        if (is.null(target.value)) {
          target.value <- private$positive.class
        } else {
          if (!(target.value %in% private$class.values)) {
            message("[", class(self)[1], "][WARNING] Target class not found. ",
                    "Assuming default '", private$positive.class, "' value")
            target.value <- private$positive.class
          }
        }
        count <- as.data.frame(t(as.matrix(table(private$data[, private$class.index]))))
        round(count[, target.value] / sum(count[, which(names(count) != target.value)]), digits = 3)
      }
    },
    getClassIndex = function() { private$class.index },
    getClassName = function() { private$class.name },
    getNcol = function() { ncol(private$data) },
    getNrow = function() { nrow(private$data) },
    getPositiveClass = function() { private$positive.class },
    isBlinded = function() {FALSE}
  ),
  private = list(
    data = NULL,
    class.index = NULL,
    class.name = NULL,
    feature.names = NULL,
    class.values = NULL,
    positive.class = NULL,
    chunk.size = 10000,
    feature.id = NULL
  )
)
