library("R6")
KendallPlot <- R6Class(
  classname = "KendallPlot",
  inherit = GenericPlot,
  portable = TRUE,
  public = list(
    initialize = function() { },
    plot = function(summary, ...) {
      ggplot(data = summary, aes(x = interval, y = value)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        geom_text_repel(aes(label = summary$value), size = 4, hjust = 1.6, color = "white" ) +
        coord_flip() + 
        scale_y_continuous(expand = c(0, 0),
                           limits = c(0, max(summary$value) * 1.2)) +  
        geom_hline(yintercept = max(summary$value), linetype = "dashed", color = "black") + 
        labs(title = "Unbinary Data", x = "Class", y = "Mean Tau Value") + 
        theme_light()
    }
  )
)