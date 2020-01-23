TrainOutput <- R6::R6Class(
  classname = "TrainOutput",
  portable = TRUE,
  public = list(
    initialize = function(models, class.values, positive.class) {
      if ( is.null(models) || !is.list(models) ) {
        stop("[", class(self)[1], "][FATAL] Models parameter must be defined as ",
             "'list' type. Aborting...")
      }
      if ( is.null(class.values) || !is.character(class.values) && length(class.values) < 2 ) {
        stop("[", class(self)[1], "][FATAL] Class.values parameter must be defined as ",
             "'list' character. Aborting...")
      }
      if ( is.null(positive.class) || !is.character(positive.class) || !positive.class %in% class.values ) {
        stop("[", class(self)[1], "][FATAL] Positive.class parameter must be defined as ",
             "'list' character. Aborting...")
      }
      private$models <- models
      private$class.values <- class.values
      private$positive.class <- positive.class
    },
    getModels = function(metric) {
      if ( is.null(metric) || is.list(metric) || !metric %in% self$getMetrics() ) {
        stop("[",class(self)[1],"][FATAL] Metric not defined of invalid. Aborting...")
      }
      private$models[[metric]]
    },
    getPerformance = function(metrics = NULL){
      if ( is.null(metrics) && !is.character(metrics) &&
           !any(metrics %in% self$getMetrics()) ) {
        message("[", class(self)[1], "][INFO] Metrics not defined or invalid. ",
                "Asuming all available metrics '",
                paste0(self$getMetrics(),collapse = ", "),"'")
        metrics <- self$getMetrics()
      }

      performance <- list()
      for(metric in metrics) {
        models.performance <- data.frame(matrix(ncol = 0, nrow = 1))
        models.name <- c()
        for(model in self$getModels(metric)){
          models.performance <- cbind(models.performance,model$model.performance)
          models.name <- c(models.name,model$model.name)
        }
        names(models.performance) <- models.name
        performance[[metric]] <- models.performance
      }
      performance
    },
    savePerformance = function(dir.path, metrics = NULL){
      if (is.null(dir.path))
        stop( "[",class(self)[1],"][FATAL] Save folder not set. Aborting...")

      dir.path <- gsub("\\/$", "", dir.path)

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if (dir.exists(dir.path)) {
          message("[",class(self)[1],"][INFO] Folder '", dir.path,
                  "' has been succesfully created")
        } else { stop("[",class(self)[1],"][FATAL] Cannot create directory '",
                      dir.path,"'. Aborting... ") }
      } else { message("[",class(self)[1],"][INFO] Folder already exists") }

      if ( is.null(metrics) && !is.character(metrics) &&
           !any(metrics %in% self$getMetrics()) ) {
        message("[", class(self)[1], "][INFO] Metrics not defined or invalid. ",
                "Asuming all available metrics '",
                paste0(self$getMetrics(),collapse = ", "),"'")
        metrics <- self$getMetrics()
      }

      output <- data.frame(matrix(ncol = 4,nrow=0))
      for(metric in metrics){
        for(num.model in seq_len(length(self$getModels(metric)))){
          model <- self$getModels(metric)[[num.model]]
          row <- c(metric,paste0("[Cluster_",num.model,"]"), model$model.name,
                   model$model.performance )
          output <- rbind(output,row,stringsAsFactors=FALSE)
        }
      }

      names(output) <- c("Measure","Cluster","Model","Performance")
      path <- file.path(dir.path,"Performance_Train_Measures.csv")
      write.table(output, file = path, sep = ";", dec = ".", row.names = FALSE)
      message("[",class(self)[1],"][INFO] Performances successfully saved at: ",
              path)
    },
    plot = function(dir.path, metrics = NULL) {
      if (is.null(dir.path))
        stop( "[",class(self)[1],"][FATAL] Save folder not set. Aborting...")

      dir.path <- gsub("\\/$", "", dir.path)

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if (dir.exists(dir.path)) {
          message("[",class(self)[1],"][INFO] Folder '", dir.path,
                  "' has been succesfully created")
        } else { stop("[",class(self)[1],"][FATAL] Cannot create directory '",
                      dir.path,"'. Aborting... ") }
      } else { message("[",class(self)[1],"][INFO] Folder already exists") }


      if ( is.null(metrics) &&
           !is.character(metrics) &&
           !any(metrics %in% self$getMetrics()) ) {
        message("[", class(self)[1], "][WARNING] Metrics are invalid. ",
                "Asuming all available metrics", self$getMetrics())
        metrics <- self$getMetrics()
      }

      invisible(sapply(metrics, function(metric) {
        summary <- do.call(rbind, lapply(self$getModels(metric), function(model) {
          data.frame(model$model.name, model$model.performance,
                     stringsAsFactors = FALSE)
        } ))

        summary <- cbind(data.frame(sprintf("[Cluster %s]", seq(1,nrow(summary)))),
                         summary)
        names(summary) <- c("clusters", "models", "measure")

        min.pos <- which.min(summary$measure)
        min <- data.frame( x = summary[min.pos, ]$clusters, y = min(summary[,3]) )
        max.pos <- which.max(summary$measure)
        max <- data.frame( x = summary[max.pos, ]$clusters, y= max(summary[,3]) )
        avg <- round(mean(summary$measure), digits = 2)
        remainning <- data.frame( x = summary[-c(min.pos,max.pos),]$clusters,
                                  y = summary[-c(min.pos,max.pos),]$measure )
        measure <- metric

        ggplot(summary, aes(clusters, measure, group = 1)) + geom_line() +
          geom_point() +
          geom_text(aes(x, y, label = sprintf("%.3f", y)), remainning,
                    size = 3, hjust = -.4, vjust = 1.5, color = 'black' ) +
          geom_point(aes(x,y), min, fill = "transparent", color = "red",
                     shape = 21, size = 3, stroke = 1) +
          geom_text(aes(x, y, label = sprintf("%.3f", y)), min, size = 3,
                    hjust = -.4, vjust = 1.5, color = 'red' ) +
          geom_text(aes(x, y, label = sprintf("%.3f",y)), max, size = 3,
                    hjust = -.4, vjust = 1.5, color = 'blue' ) +
          geom_point(aes(x, y), max, fill = "transparent", color = "blue",
                     shape = 21, size = 3,stroke = 1) +
          geom_hline(aes(yintercept = avg), linetype = "twodash",
                     color = "#696969", show.legend = TRUE) +
          geom_text(aes(0, avg, label = "Average"), hjust = -.2, vjust = -1) +
          geom_text(aes(label = models), hjust = -.2, vjust = 0) +
          labs(x = "Model name", y = paste0(measure," value"),
               title = paste0("Performance benchmarking plot during training")) +
          theme(axis.text.x = element_text(angle = 75, hjust = 1),
                plot.title = element_text(hjust = 0.5))

        save.path <- file.path(dir.path, paste0("Performance_Train_Plot_", metric,".pdf") )
        message("[D2MCS][INFO] Plot saved has been succesfully saved at : '",
                save.path, "'")
        ggsave(filename = save.path, device = "pdf")
      }))
    },
    getMetrics = function() { names(private$models) },
    getClassValues = function() { private$class.values },
    getPositiveClass = function() { private$positive.class },
    getSize = function() { length(names(private$models)) }
  ),
  private = list(
    models = NULL,
    class.values = NULL,
    positive.class = NULL
  )
)