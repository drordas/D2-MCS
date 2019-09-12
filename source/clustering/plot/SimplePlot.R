library("R6")
SimplePlot <- R6Class(
  classname = "SimplePlot",
  inherit = Plot,
  portable = TRUE,
  public = list(
    initialize = function(name = "SimplePlot") {
      super$initialize(name = name)
    },
    plot = function(summary, ...) {
      if (!is.data.frame(summary)) {
        stop("[", super$getName(), "][ERROR] summary parameter must be defined as 'data.frame' type")
      }
      min <- data.frame(x = summary[which.min(summary[, 2]),][, 1], y = min(summary[, 2]))
      max <- data.frame(x = summary[which.max(summary[, 2]),][, 1], y = max(summary[, 2]))
      ggplot(summary, aes(k, dispersion)) + geom_line() + geom_point() +
        geom_point(aes(x, y), min, fill = "transparent", color = "blue", shape = 21, size = 3, stroke = 1 ) +
        geom_point(aes(x, y), max, fill = "transparent", color = "red", shape = 21, size = 3, stroke = 1 ) +
        geom_text(aes(x, y, label = sprintf("%.3f", y)), max, hjust = -0.45, color = 'red') +
        geom_text(aes(x, y, label = sprintf("%.10f", y)), min, hjust = -0.45, color = 'blue') +
        scale_x_continuous(name = "Number of clusters", breaks = seq(from = 2, to = nrow(summary) + 1)) + 
        scale_y_continuous(limits = c(min(summary$dispersion), max(summary$dispersion)), trans = 'sqrt') +
        #scale_y_continuous( name="Dispersion (represented as logaritmic scale)", trans = "log", breaks = c( min$y,max$y ) ) + 
        labs(x = "Number of clusters", y = "Dispersion")
    }
  )
)