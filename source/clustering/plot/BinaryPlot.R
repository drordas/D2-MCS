BinaryPlot <- R6::R6Class(
  classname = "BinaryPlot",
  inherit = GenericPlot,
  portable = TRUE,
  public = list(
    initialize = function() { },
    plot = function(summary, ...) {
      if (!is.data.frame(summary)) {
        stop("[", class(self)[1], "][FATAL] Summary parameter must be defined ",
             "as 'data.frame' type. Aborting...")
      }
      super$plot(summary) + labs(title = "Binary Data")  + theme_light() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5))

    }
  )
)