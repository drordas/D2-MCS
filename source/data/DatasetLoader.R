DatasetLoader <- R6::R6Class(
  classname = "DatasetLoader",
  portable = TRUE,
  public = list(
    initialize = function() { self },
    load = function(filepath, header = TRUE, sep = ",", skip.lines = 0,
                    normalize.names = FALSE, string.as.factor = FALSE,
                    ignore.columns = NULL) {

      if (is.null(filepath) || !file.exists(filepath)) {
        stop("[", class(self)[1], "][FATAL] Corpus cannot be found at defined ",
             "location. Aborting...")
      }

      dt.size <- (file.info(filepath)$size / 2^30)

      if (dplyr::between(dt.size, 0, 1)) {

        dataset <- Dataset$new(filepath = filepath, header = header, sep = sep,
                               skip = skip.lines, normalize.names = normalize.names,
                               ignore.columns = ignore.columns)
      } else {
        dataset <- HDDataset$new(filepath = filepath, header = header, sep = sep,
                                 skip = skip.lines, normalize.names = normalize.names,
                                 ignore.columns = ignore.columns)
      }
      dataset
    }
  )
)