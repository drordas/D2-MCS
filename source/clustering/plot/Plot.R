library("R6")
Plot <- R6Class(
  classname = "Plot",
  portable = TRUE,
  public = list(
    initialize = function(name = NULL) {
      private$name <- name
    },
    getName = function() {
      private$name
    },
    plot = function(summary, ...) {
      stop("[Plot][ERROR] Function 'plot must be implemented in inherited class'")
    }
  ),
  private = list(
    name = NULL
  )
)