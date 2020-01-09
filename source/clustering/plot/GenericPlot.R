GenericPlot <- R6::R6Class(
  classname = "GenericPlot",
  portable = TRUE,
  public = list(
    initialize = function() { },
    getName = function() { class(self)[1] },
    plot = function(summary, ...) {
      if (!is.data.frame(summary)) {
        stop("[", super$getName(), "][FATAL] summary parameter must be defined as 'data.frame' type")
      }
      min <- data.frame(x = summary[which.min(summary[, 2]), ][, 1],y = min(summary[, 2]))
      max <- data.frame(x = summary[which.max(summary[, 2]), ][, 1],y = max(summary[, 2]))
      ggplot(summary, aes(k, dispersion)) + geom_point(aes(color = dispersion), position = position_jitter()) + 
        scale_color_continuous(name = "",low = "blue", high = "red", guide = FALSE ) + 
        geom_text_repel(aes(x, y, label = sprintf("%s", format(min$y, digits = 2, scientific = TRUE))), 
                        min, hjust = 0.5, vjust = 0, point.padding = 0.25, color = 'blue', size = 3 ) +
        geom_text_repel(aes(x, y, label = sprintf("%s", format(max$y,digits = 2, scientific = TRUE))), 
                        max, hjust = 0.5, vjust = 1, point.padding = 0.25, color = 'red', size = 3) + 
        scale_y_continuous(name = "Dispersion (using logaritmic scale)", trans = "log2", breaks = c(min$y, max$y)) + 
        scale_x_continuous(name = "Number of clusters", breaks = seq(from = 2, to = nrow(summary) + 1))
    }
  )
)