library("R6")
Heuristic <- R6Class(
  classname = "Heuristic",
  portable = TRUE,
  public = list(
    initialize = function(name = NULL) {
      private$name <- name
    },
    getName = function() {
      private$name
    },
    heuristic = function(col1, col2, namesColums = NULL, ...) {
      stop("[Heuristic][ERROR] Function 'heuristic must be implemented in inherited class'")
    }
  ),
  private = list(
    name = NULL,
    isBinary = function(column) {
      unique <- unique(column)
      if (!is.numeric(column) | any(is.na(column))) {
        return(FALSE)
      } else {
        return(!(any(as.integer(unique) != unique) || length(unique) > 2 || min(column) != 0 || max(column) != 1))
      }
    }
  )
)