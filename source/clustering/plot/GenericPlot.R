library("R6")
GenericPlot <- R6Class(
  classname = "GenericPlot",
  portable = TRUE,
  public = list(
    initialize = function() { },
    getName = function() { class(self)[1] },
    plot = function(summary, ...) {
      stop("[GenericPlot][ERROR] Function 'plot must be implemented in inherited class'")
    }
  )
)