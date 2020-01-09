ClassificationOutput <- R6::R6Class(
  classname = "ClassificationOutput",
  portable = TRUE,
  public = list(
    initialize = function(voting.schemes, models) {
      if (missing(voting.schemes) || length(Filter( function(x) inherits(x, "SimpleVoting") ||
                                                    inherits(x, "CombinedVoting"), voting.schemes) ) == 0) {
        stop("[", class(self)[1], "][ERROR] Voting Schemes missing or invalid. ",
             "Must inherit from 'SimpleVoting' or 'CombinedVoting' class.")
      }
      if (missing(models) && !is.list(models)) {
        stop("[", class(self)[1], "][ERROR] Models are incorrect. Must be a 'list' type. Aborting...")
      }

      private$voting.schemes <- voting.schemes
      private$trained.models <- models
      private$positive.class <- voting.schemes[[1]]$getPositiveClass()
      private$negative.class <- setdiff(voting.schemes[[1]]$getClassValues(),
                                        private$positive.class)
    },
    getPerformances = function(test.set, measures, voting.schemes = NULL, metrics = NULL, cutoffs = NULL){
      if (!inherits(test.set, "Subset"))
        stop("[",class(self)[1],"][ERROR] Test set invalid.",
             " Must be a Subset object. Aborting...")

      if (test.set$getNrow() == 0)
        stop("[",class(self)[1],"][ERROR] Test set is empty. Aborting...")

      if (!is.list(measures) || !all(sapply(measures, inherits,"MeasureFunction"))) {
        stop("[",class(self)[1],"][ERROR] Measures should be a list comprised of ",
             "'MeasureFunction' objects. Aborting...")
      }

      if (private$positive.class != test.set$getPositiveClass()) {
        stop("[", class(self)[1], "][ERROR] Positive class values missmatch. ['",
             test.set$getPositiveClass(), "' vs '",
             private$positive.class, "'] used in classification",
             "and test respectively. Aborting... ")
      }
      final.voting.schemes <- private$voting.schemes
      if (all(missing(voting.schemes), missing(metrics), missing(cutoffs),
              !is.character(voting.schemes), !is.character(metrics), !is.numeric(cutoffs))) {
        message("[", class(self)[1], "][WARNING] Voting.schemes, metric and cutoff are not defined or invalid. ",
                "Asuming all voting schemes to get its performance.")
      } else {
        if (!missing(voting.schemes) && is.character(voting.schemes)) {
          message("[", class(self)[1], "][INFO] Selecting voting.schemes which",
                  " inherit from :", paste(voting.schemes, collapse = " "))
          final.voting.schemes <- Filter(function(vot) all(inherits(vot, voting.schemes)),
                                         final.voting.schemes)
        }
        if (!missing(metrics) && is.character(metrics)) {
          message("[", class(self)[1], "][INFO] Selecting voting.schemes which",
                  " have this metrics: ", paste(metrics, collapse = " "))
          final.voting.schemes <- Filter(function(vot)
                                           ifelse(inherits(vot, "SimpleVoting"),
                                                  vot$getMetric() %in% metrics,
                                                  all(vot$getMetrics() == metrics)),
                                         final.voting.schemes)
        }
        if (!missing(cutoffs) && is.numeric(cutoffs)) {
          message("[", class(self)[1], "][INFO] Selecting voting.schemes which",
                  " have this cutoffs: ", paste(cutoffs, collapse = " "))
          final.voting.schemes <- Filter(function(vot) vot$getCutoff() %in% cutoffs,
                                         final.voting.schemes)
        }
      }
      performances <- list()

      if (length(final.voting.schemes) == 0) {
        message("[", class(self)[1], "][WARNING] There are no voting schemes ",
                "that have metrics as '", paste(metrics, collapse = " "),
                "' and as cutoff '", paste(cutoffs, collapse = " "), "'")
        return(performances)
      }

      for (voting.scheme in final.voting.schemes) {

        if (!(test.set$getPositiveClass() %in% voting.scheme$getClassValues())) {
          stop("[", class(self)[1], "][ERROR] Positive class '",
               test.set$getPositiveClass(), "' in test set does not match",
               " with [", paste0(voting.scheme$getClassValues(), collapse = ", "), "]")
        }

        real.values <- test.set$getClassValues()
        pred.values <- voting.scheme$getPrediction(type = "raw")

        if (length(levels(real.values)) != length(unique(pred.values[,1])) ||
            !(levels(real.values) %in% unique(pred.values[,1]))) {
          stop("[",class(self)[1],"][ERROR] Class values missmatch. Aborting...")
        }
        real.values <- relevel(x = real.values,
                               ref = private$positive.class)
        pred.values <- factor(pred.values[,1],
                              levels = c(private$positive.class,
                                         setdiff(voting.scheme$getClassValues(),
                                                 private$positive.class)))

        performance <- do.call(rbind, lapply(measures, function(entry, cf) {
          result <- entry$compute(cf)
          df <- data.frame(entry$getName(), result)
          rownames(df) <- NULL
          names(df) <- c("Measure","Value")
          df
        }, cf = ConFMatrix$new(caret::confusionMatrix(data = pred.values,
                                                      reference = real.values,
                                                      positive = private$positive.class))))
        performances <- append(performances, list(performance))
        if (inherits(voting.scheme, "SimpleVoting")) {
          names(performances)[length(performances)] <- paste(voting.scheme$getName(),
                                                             voting.scheme$getMetric(),
                                                             voting.scheme$getCutoff(),
                                                             sep = "_")
        } else {
          names(performances)[length(performances)] <- paste(voting.scheme$getName(),
                                                             paste0(voting.scheme$getMetrics(),
                                                                   collapse = "-"),
                                                             voting.scheme$getCutoff(),
                                                             sep = "_")
        }
      }
      performances
    },
    plot = function(dir.path, test.set, measures, voting.schemes = NULL, metrics = NULL, cutoffs = NULL){
      if (missing(dir.path))
        stop("[", class(self)[1],"][INFO] Path not defined. Aborting.")

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if (dir.exists(dir.path)) {
          message("[", class(self)[1], "][INFO] Folder '", dir.path,
                  "' has been succesfully created")
        } else { stop("[", class(self)[1], "][ERROR] Cannot create directory '",
                      dir.path, "'. Aborting... ") }
      } else { message("[", class(self)[1], "][INFO] Folder already exists") }

      performances <- self$getPerformances(test.set = test.set,
                                           measures = measures,
                                           voting.schemes = voting.schemes,
                                           metrics = metrics,
                                           cutoffs = cutoffs)

      for (peformanceName in names(performances)) {
        performance <- performances[[peformanceName]]
        plot <- ggplot(performance, aes(x = Measure,y = Value) ) + geom_bar(stat = "identity") +
          geom_point(aes(shape = 15,stroke = 1)) + scale_shape_identity() +
          ggtitle("Classifier performance Benchmarking") + guides(fill = FALSE) +
          theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "none" )
        plotly::ggplotly(plot)

        ggsave( paste0(file.path(dir.path, peformanceName), ".pdf"), device = "pdf",
                plot = plot, limitsize = FALSE )
        message("[", class(self)[1],"][INFO] Plot has been succesfully saved at: ",
                paste0(file.path(dir.path, peformanceName),".pdf"))
      }
    },
    getPredictions = function(voting.schemes = NULL, metrics = NULL, cutoffs = NULL, type = NULL, target = NULL, filter = FALSE){
      final.voting.schemes <- private$voting.schemes
      if (all(missing(voting.schemes), missing(metrics), missing(cutoffs),
              !is.character(voting.schemes), !is.character(metrics), !is.numeric(cutoffs))) {
        message("[", class(self)[1], "][WARNING] Voting.schemes, metric and cutoff are not defined or invalid. ",
                "Asuming all voting schemes to get its performance.")
      } else {
        if (!missing(voting.schemes) && is.character(voting.schemes)) {
          message("[", class(self)[1], "][INFO] Selecting voting.schemes which'",
                  " inherit from :", paste(voting.schemes, collapse = " "))
          final.voting.schemes <- Filter(function(vot) all(inherits(vot, voting.schemes)),
                                         final.voting.schemes)
        }
        if (!missing(metrics) && is.character(metrics)) {
          message("[", class(self)[1], "][INFO] Selecting voting.schemes which'",
                  " have this metrics: ", paste(metrics, collapse = " "))
          final.voting.schemes <- Filter(function(vot)
            ifelse(inherits(vot, "SimpleVoting"),
                   vot$getMetric() %in% metrics,
                   all(vot$getMetrics() == metrics)),
            final.voting.schemes)
        }
        if (!missing(cutoffs) && is.numeric(cutoffs)) {
          message("[", class(self)[1], "][INFO] Selecting cutoffs which'",
                  " have this cutoffs: ", paste(cutoffs, collapse = " "))
          final.voting.schemes <- Filter(function(vot) vot$getCutoff() %in% cutoffs,
                                         final.voting.schemes)
        }
      }
      if (length(final.voting.schemes) == 0) {
        message("[", class(self)[1], "][WARNING] There are no voting schemes ",
                "that have metrics as '", paste(metrics, collapse = " "), "' and as cutoff '", paste(cutoffs, collapse = " "), "'")
        return(NULL)
      }
      names <- list()
      predictions <- lapply(final.voting.schemes, function(voting.scheme, type, target)  {
        voting.scheme$getPrediction(type = type, target = target, filter = filter)
      }, type, target)
      names(predictions) <- lapply(final.voting.schemes, function(voting.scheme){
        paste(voting.scheme$getName(),
              ifelse(inherits(voting.scheme, "SimpleVoting"),
                     voting.scheme$getMetric(),
                     paste(voting.scheme$getMetrics(), collapse = "-")),
              voting.scheme$getCutoff())
      })
      PredictionOutput$new(predictions = predictions,
                           type = type,
                           target = target)
    },
    getMetrics = function() { names(private$trained.models) },
    getPositiveClass = function() { private$positive.class },
    getModelInfo = function(metrics = NULL) {
      if (missing(metrics) ||
          !is.character(metrics) ||
          !all(metrics %in% self$getMetrics())) {
        message("[", class(self)[1], "][WARNING] Metrics are not defined or invalid. ",
                "Asuming all metrics of clasification.output", self$getMetrics())
        metrics <- self$getMetrics()
      }
      models.info <- lapply(metrics, function(metric) {
        do.call(rbind, lapply(private$trained.models[[metric]], function(model) {
          aux <- data.frame(model$model.name, model$model.performance)
          rownames(aux) <- NULL
          names(aux) <- c("Model Name", "Performance")
          aux
        }))
      })
      names(models.info) <- metrics
      models.info
    },
    savePredictions = function(dir.path, type = NULL, target = NULL, voting.schemes = NULL, metrics = NULL, cutoffs = NULL){
      final.voting.schemes <- private$voting.schemes
      if (all(missing(voting.schemes), missing(metrics), missing(cutoffs),
              !is.character(voting.schemes), !is.character(metrics), !is.numeric(cutoffs))) {
        message("[", class(self)[1], "][WARNING] Voting.schemes, metric and cutoff are not defined or invalid. ",
                "Asuming all voting schemes to get its performance.")
      } else {
        if (!missing(voting.schemes) && is.character(voting.schemes)) {
          message("[", class(self)[1], "][INFO] Selecting voting.schemes which",
                  " inherit from: ", paste(voting.schemes, collapse = " "))
          final.voting.schemes <- Filter(function(vot) all(inherits(vot, voting.schemes)),
                                         final.voting.schemes)
        }
        if (!missing(metrics) && is.character(metrics)) {
          message("[", class(self)[1], "][INFO] Selecting voting.schemes which",
                  " have this metrics: ", paste(metrics, collapse = " "))
          final.voting.schemes <- Filter(function(vot)
            ifelse(inherits(vot, "SimpleVoting"),
                   vot$getMetric() %in% metrics,
                   all(vot$getMetrics() == metrics)),
            final.voting.schemes)
        }
        if (!missing(cutoffs) && is.numeric(cutoffs)) {
          message("[", class(self)[1], "][INFO] Selecting voting.schemes which",
                  " have this cutoffs: ", paste(cutoffs, collapse = " "))
          final.voting.schemes <- Filter(function(vot) vot$getCutoff() %in% cutoffs,
                                         final.voting.schemes)
        }
      }
      if (length(final.voting.schemes) == 0) {
        message("[", class(self)[1], "][WARNING] There are no voting schemes ",
                "that have metrics as '", paste(metrics, collapse = " "), "' and as cutoff '", paste(cutoffs, collapse = " "), "'")
        return(NULL)
      }

      if (missing(dir.path))
        stop( "[",class(self)[1],"][ERROR] Save folder not set. Aborting...")

      dir.path <- gsub("\\/$", "", dir.path)

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if (dir.exists(dir.path)) {
          message("[", class(self)[1], "][INFO] Folder '", dir.path,
                  "' has been succesfully created")
        } else { stop("[", class(self)[1], "][ERROR] Cannot create directory '",
                     dir.path, "'. Aborting... ") }
      } else { message("[", class(self)[1], "][INFO] Folder already exists") }

      for (voting.scheme in final.voting.schemes) {
        if (is.null(type) || (!type %in% c("prob","raw"))) {
          message("[", class(self)[1], "][INFO] Prediction type not set or invalid.")
          if (is.null(target) || !(target %in% c(private$positive.class,
                                                 private$negative.class))) {
            message("[", class(self)[1], "][INFO] Target class not set or invalid. ",
                    "Saving all predictions with all target values")
            #SAVING PROB
            path <- file.path(dir.path, paste0(voting.scheme$getName(), "_",
                                               ifelse(inherits(voting.scheme, "SimpleVoting"),
                                                      voting.scheme$getMetric(),
                                                      paste(voting.scheme$getMetrics(),
                                                            collapse = "-")),
                                               "_",
                                               voting.scheme$getCutoff(), "_",
                                               "prob_", private$positive.class,
                                               ".csv"))
            df <- data.frame(voting.scheme$getPrediction("prob", private$positive.class),
                             voting.scheme$getPrediction("prob", private$negative.class))
            rownames <- rownames(df)
            df <- cbind(rownames,df)
            names(df) <- c("ID", private$positive.class,private$negative.class)
            write.table(df, file = path, sep = ";", dec = ".", row.names = FALSE)

            #SAVING RAW
            path <- file.path(dir.path, paste0(voting.scheme$getName(), "_",
                                               ifelse(inherits(voting.scheme, "SimpleVoting"),
                                                      voting.scheme$getMetric(),
                                                      paste(voting.scheme$getMetrics(),
                                                            collapse = "-")), "_",
                                               voting.scheme$getCutoff(), "_",
                                               "raw_Predictions.csv"))
            df <- data.frame(voting.scheme$getPrediction("raw"))
            rownames <- rownames(df)
            df <- cbind(rownames,df)
            names(df) <- c("ID","Predictions")
            write.table(df, file = path, sep = ";", dec = ".", row.names = FALSE)
            #SAVING COMBINED
            path <- file.path(dir.path, paste0(voting.scheme$getName(), "_",
                                               ifelse(inherits(voting.scheme, "SimpleVoting"),
                                                      voting.scheme$getMetric(),
                                                      paste(voting.scheme$getMetrics(),
                                                            collapse = "-")), "_",
                                               voting.scheme$getCutoff(), "_",
                                               "comb_Predictions",
                                               ".csv"))
            prob <- data.frame(voting.scheme$getPrediction("prob", private$positive.class),
                               voting.scheme$getPrediction("prob", private$negative.class))
            colnames(prob) <- c(private$positive.class, private$negative.class)
            rownames <- rownames(prob)
            raw <- as.vector(t(voting.scheme$getPrediction("raw")))
            df <- data.frame(matrix(ncol = 2, nrow = 0))
            for (row in 1:length(raw)) {
              df <- rbind(df, data.frame(raw[row], prob[row, raw[row]]))
            }
            df <- cbind(rownames, df)
            names(df) <- c("ID", "Prediction", "Probability")
            write.table( df, file = path, sep = ";", dec = ".", row.names = FALSE)
          } else {
            message("[",class(self)[1],"][INFO] Saving all predictions for target",
                    " value '",target,"'")
            for (i in c("prob","raw","bin")) {
              path <- file.path(dir.path, paste0(voting.scheme$getName(), "_",
                                                 ifelse(inherits(voting.scheme, "SimpleVoting"),
                                                        voting.scheme$getMetric(),
                                                        paste(voting.scheme$getMetrics(),
                                                              collapse = "-")),
                                                 voting.scheme$getCutoff(), "_",
                                                 i, "_", private$positive.class,
                                                 ".csv"))
              df <- data.frame(voting.scheme$getPrediction(i, target))
              names(df) <- target
              write.table(df, file = path, sep = ";", dec = ".", row.names = FALSE)
            }
          }
        } else {
          message("[", class(self)[1],"][INFO] Prediction type set as '", type,"'.")
          if (is.null(target) || !(target %in% c( private$positive.class,
                                                  private$negative.class))) {
            message("[", class(self)[1], "][INFO] Target class not set or invalid. ",
                    "Saving '", type, "' predictions for all target values")
            path <- file.path(dir.path, paste0(voting.scheme$getName(), "_",
                                               ifelse(inherits(voting.scheme, "SimpleVoting"),
                                                      voting.scheme$getMetric(),
                                                      paste(voting.scheme$getMetrics(),
                                                            collapse = "-")),
                                               voting.scheme$getCutoff(), "_",
                                               type, "_", private$positive.class,
                                               ".csv"))
            df <- data.frame(voting.scheme$getPrediction(type, private$positive.class),
                             voting.scheme$getPrediction(type, private$negative.class))
            names(df) <- c(private$positive.class, private$negative.class)
            write.table(df, file = path, sep = ";", dec = ".", row.names = FALSE)
          }else{
            message("[", class(self)[1], "][INFO] Saving '", type, "' predictions ",
                    "for '", target, "'target values")
            path <- file.path(dir.path, paste0(voting.scheme$getName(), "_",
                                               ifelse(inherits(voting.scheme, "SimpleVoting"),
                                                      voting.scheme$getMetric(),
                                                      paste(voting.scheme$getMetrics(),
                                                            collapse = "-")),
                                               voting.scheme$getCutoff(), "_",
                                               type, "_", target,
                                               ".csv"))
            df <- data.frame(voting.scheme$getPrediction(type, target))
            names(df) <- target
            write.table(df, file = path, sep = ";", dec = ".", row.names = FALSE)
          }
        }
      }
    }
  ),
  private = list(
    positive.class = NULL,
    negative.class = NULL,
    voting.schemes = NULL,
    trained.models = NULL
  )
)