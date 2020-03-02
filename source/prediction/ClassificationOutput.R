ClassificationOutput <- R6::R6Class(
  classname = "ClassificationOutput",
  portable = TRUE,
  public = list(
    initialize = function(voting.schemes, models) {
      if(length(voting.schemes) == 0){
        stop("[",class(self)[1],"][FATAL] Voting Schemes not executed. ",
             "Aborting...")
      }

      if (!all(names(voting.schemes) %in% c("SingleVoting", "CombinedVoting") )) {
        stop("[", class(self)[1], "][FATAL] Voting Schemes must inherit from ",
             "'SimpleVoting' or 'CombinedVoting' classes. Aborting...")
      }

      if (!is.list(models)) {
        stop("[", class(self)[1], "][FATAL] Models parameter must be defined ",
             "as 'list' type. Aborting...")
      }

      positive.class <- unique( as.vector(unlist(sapply(voting.schemes, function(metrics) {
                                sapply(metrics, function(cutoffs) {
                                    sapply(cutoffs, function(voting.names) {
                                      sapply(voting.names, function(votings) {
                                        votings$getFinalPred()$getPositiveClass() } )
                                      } )
                                  } ) }
                                )) ))

      class.values <- unique( as.vector(unlist(sapply(voting.schemes, function(metrics) {
                              sapply(metrics, function(cutoffs) {
                                sapply(cutoffs, function(voting.names) {
                                  sapply(voting.names, function(votings) {
                                    votings$getFinalPred()$getClassValues() } )
                                } )
                              } ) }
                            )) ) )

      if (length(positive.class) != 1 ){
        stop("[",class(self)[1],"][FATAL] Defined positive class does ",
             "not match. Aborting...")
      }

      private$voting.schemes <- voting.schemes
      private$trained.models <- models
      private$positive.class <- positive.class
      private$negative.class <- setdiff(class.values,positive.class)
      private$available$cutoffs <- unique( as.vector(unlist(sapply(private$voting.schemes,
                                           function(metrics) {
                                             sapply(metrics, function(cutoff) {
                                               names(cutoff) })
                                           }))))

      private$available$metrics <- unique( as.vector(unlist(sapply(private$voting.schemes,
                                           function(metrics) { names(metrics) }))))

      private$available$votings <- unique( as.vector(unlist(sapply(private$voting.schemes,
                                           function(metrics) {
                                              sapply(metrics, function(cutoff) {
                                                sapply(cutoff, function(voting) {
                                                  names(voting) } ) } )
                                            })) ))
    },
    getMetrics = function() { unique(names(private$trained.models)) },
    getPositiveClass = function() { private$positive.class },
    getModelInfo = function(metrics = NULL) {
      if (missing(metrics) ||
          !is.character(metrics) ||
          !all(metrics %in% self$getMetrics())) {
        message("[", class(self)[1], "][WARNING] Metrics are not defined or invalid. ",
                "Asuming all metrics of clasification.output (",
                paste(self$getMetrics(), collapse = ","), ")")
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
    getPerformances = function(test.set, measures, voting.names = NULL,
                               metric.names = NULL, cutoff.values = NULL){
      if (!inherits(test.set, "Subset"))
        stop("[",class(self)[1],"][FATAL] Testset parameter must be defined ",
             "as 'Subset' type. Aborting...")

      if (test.set$getNrow() == 0)
        stop("[",class(self)[1],"][FATAL] Test set is empty. Aborting...")

      if (!is.list(measures) || !all(sapply(measures, inherits,"MeasureFunction"))) {
        stop("[",class(self)[1],"][FATAL] Measures should be a list comprised of ",
             "'MeasureFunction' objects. Aborting...")
      }

      if (private$positive.class != test.set$getPositiveClass()) {
        stop("[", class(self)[1], "][FATAL] Positive class values missmatch. ['",
             test.set$getPositiveClass(), "' vs '",
             private$positive.class, "'] used in classification ",
             "and test respectively. Aborting...")
      }

      real.values <- test.set$getClassValues()

      if(!all(unique(real.values) %in% union(private$positive.class,
                                             private$negative.class) )){
        stop("[",class(self)[1],"][FATAL] Predicted values and Real values ",
             "missmatch, Aborting... ")
      }

      if(!is.character(cutoff.values)) cutoff.values <- as.character(cutoff.values)

      if(length(cutoff.values) != 0){
        if( any(cutoff.values %in% private$available$cutoffs) ){
          aval.cutoffs <- intersect(private$available$cutoffs,cutoff.values)
        }else{
          message("[",class(self)[1],"][WARNING] Defined cutoffs are not ",
                  "available. Using all cutoffs")
          aval.cutoffs <- private$available$cutoffs
        }
      }else{
        message("[",class(self)[1],"][INFO] Cutoff values not defined or invalid. ",
                "Using all cutoffs")
        aval.cutoffs <- private$available$cutoffs
      }

      if(!is.null(metric.names)){
        if(any(metric.names %in% private$available$metrics) ){
          aval.metrics <- intersect(metric.names,private$available$metrics)
        }else {
          message("[",class(self)[1],"][WARNING] Defined metrics are not available. ",
                  "Using all metrics")
          aval.metrics <- private$available$metrics
        }
      }else{
        message("[",class(self)[1],"][INFO] Metrics are not defined. ",
                "Using all metrics")
        aval.metrics <- private$available$metrics
      }

      if(!is.null(voting.names)){
        if(any(voting.names %in% private$available$votings) ){
          aval.votings <- intersect(voting.names,private$available$votings)
        }else {
          message("[",class(self)[1],"][WARNING] Defined votings are not available. ",
                  "Using all votings")
          aval.votings <- private$available$votings
        }
      }else{
        message("[",class(self)[1],"][INFO] Votings are not defined. ",
                "Using all votings")
        aval.votings <- private$available$votings
      }

      valid.votings <- private$getVotings(aval.metrics,aval.cutoffs,aval.votings)

      performances <- list()
      if(length(valid.votings) == 0){
        message("[",class(self)[1],"][ERROR] There are no voting schemes ",
                "with '", paste0(metric.names, collapse = ", ")," metrics and ",
                "'", paste0(aval.cutoffs, collapse = ", "), "' cutoffs.")
        return(performances)
      }

      if(!is.factor(real.values)){
        real.values <- relevel(x = factor(real.values,
                                          levels=c(private$positive.class,
                                                   private$negative.class)),
                               ref = private$positive.class)
      }

      for (voting.name in names(valid.votings)) {
        voting <- valid.votings[[voting.name]]
        if ( !(test.set$getPositiveClass() %in% voting$getFinalPred()$getClassValues() )) {
          stop("[", class(self)[1], "][FATAL] Positive class mismatch '",
               test.set$getPositiveClass(), "' vs [",
               paste0(voting$getFinalPred()$getClassValues(),
                      collapse = ", "), "]")
        }

        pred.values <- voting$getFinalPred()$getRaw()
        voting.positive <- voting$getFinalPred()$getPositiveClass()
        voting.negative <- voting$getFinalPred()$getNegativeClass()

        if ( !all(levels(real.values) %in% unique(pred.values)) ) {
          stop("[",class(self)[1],"][FATAL] Real target values and predicted ",
               "target values missmatch. Aborting...")
        }

        if(!is.factor(pred.values)){
          pred.values <- relevel(x = factor(pred.values,
                                            levels = c(voting.positive,
                                                       voting.negative)),
                                 ref = voting.positive)
        }

        performance <- do.call(rbind, lapply(measures, function(entry, cf) {
          result <- entry$compute(cf)
          df <- data.frame(class(entry)[1], result)
          rownames(df) <- NULL
          names(df) <- c("Measure","Value")
          df
        }, cf = ConfMatrix$new(caret::confusionMatrix(data = pred.values,
                                                      reference = real.values,
                                                      positive = private$positive.class))))
        performances[[voting.name]] <- performance
      }
      performances
    },
    savePerformances = function(dir.path, test.set, measures, voting.names = NULL,
                                metric.names = NULL, cutoff.values = NULL){
      if (is.null(dir.path))
        stop( "[",class(self)[1],"][FATAL] Save folder not set. Aborting...")

      dir.path <- gsub("\\/$", "", dir.path)

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if (dir.exists(dir.path)) {
          message("[", class(self)[1], "][INFO] Folder '", dir.path,
                  "' has been succesfully created")
        } else { stop("[", class(self)[1], "][FATAL] Cannot create directory '",
                      dir.path, "'. Aborting... ") }
      } else { message("[", class(self)[1], "][INFO] Folder already exists") }

      performances <- self$getPerformances(test.set, measures, voting.names = NULL,
                                           metric.names = NULL, cutoff.values = NULL)

      summary <- data.frame()
      for(pos in seq_len(length(performances))){
        path <- names(performances)[pos]
        split <- unlist(strsplit(path,"_"))
        df <- data.frame("Voting"= split[3],"Cutoff"=split[2],performances[[pos]])
        summary <- rbind(summary,df)
      }
      save.path <- file.path(dir.path,"Classification_Performances.csv")
      write.table( summary, file = save.path, sep = ";",
                   dec = ".", row.names = FALSE )
      message("[", class(self)[1], "][INFO] Classification performance saved at: ",save.path)

    },
    plotPerformances = function(dir.path, test.set, measures, voting.names = NULL,
                                metric.names = NULL, cutoff.values = NULL){
      if (missing(dir.path))
        stop("[", class(self)[1],"][FATAL] Path not defined. Aborting...")

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if (dir.exists(dir.path)) {
          message("[", class(self)[1], "][INFO] Folder '", dir.path,
                  "' has been succesfully created")
        } else { stop("[", class(self)[1], "][FATAL] Cannot create directory '",
                      dir.path, "'. Aborting...") }
      } else { message("[", class(self)[1], "][INFO] Folder already exists") }

      performances <- self$getPerformances(test.set = test.set,
                                           measures = measures,
                                           voting.names = voting.names,
                                           metric.names = metric.names,
                                           cutoff.values = cutoff.values)

      for (peformance.name in names(performances)) {
        performance <- performances[[peformance.name]]
        plot <- ggplot(performance, aes(x = Measure,y = Value) ) + geom_bar(stat = "identity") +
          geom_point(aes(shape = 15,stroke = 1)) + scale_shape_identity() +
          ggtitle("Classifier performance Benchmarking") + guides(fill = FALSE) +
          theme( plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
                 legend.position = "none" )
        plotly::ggplotly(plot)

        ggsave( paste0(file.path(dir.path, peformance.name), ".pdf"), device = "pdf",
                plot = plot, limitsize = FALSE )
        message("[", class(self)[1],"][INFO] Plot has been succesfully saved at: ",
                paste0(file.path(dir.path, peformance.name),".pdf"))
      }
    },
    getPredictions = function(voting.names = NULL, metric.names = NULL,
                              cutoff.values = NULL, type = NULL, target = NULL,
                              filter = FALSE ){

      if(is.null(type) || !type %in% c("raw","prob") ){
        message("[",class(self)[1],"][WARNING] Type value is invalid. ",
                "Must be 'raw' of 'prob'. Assuming 'raw'")
        type <- "raw"
      }

      if( !isTRUE(all.equal(type,"raw")) &&
          (is.null(target) || !(target %in% private$positive.class )))
      {
        message("[",class(self)[1],"][WARNING] Target value does not match with",
                " actual target values: '",paste(private$positive.class,
                                                 private$negative.class,
                                                 sep = ", "),"'. Assuming '",
                private$positive.class,"' as default value")
        target <- private$positive.class
      }

      if(!is.logical(filter)){
        message("[",class(self)[1],"][WARNING] Filter parameter must be defined ",
                "as 'logical' type. Assuming 'FALSE' as default value")
        filter <- FALSE
      }

      if(!is.character(cutoff.values)) cutoff.values <- as.character(cutoff.values)

      if(length(cutoff.values) != 0){
        if(any(cutoff.values %in% private$available$cutoffs ) ){
          aval.cutoffs <- intersect(cutoff.values,private$available$cutoffs)
        }else{
          message("[",class(self)[1],"][WARNING] Defined Cutoffs are not ",
                  "available. Using all available cutoffs")
          aval.cutoffs <- private$available$cutoffs
        }
      }else{
        message("[",class(self)[1],"][INFO] Cutoffs are not defined. ",
                "Using all available cutoffs")
        aval.cutoffs <- private$available$cutoffs
      }

      if(!is.null(metric.names)){
        if(any(metric.names %in% private$available$metrics) ){
          aval.metrics <- intersect(metric.names,private$available$metrics)
        }else {
          message("[",class(self)[1],"][WARNING] Defined Metrics are not ",
                  "available. Using all available metrics")
          aval.metrics <- private$available$metrics
        }
      }else{
        message("[",class(self)[1],"][INFO] Metrics are not defined. ",
                "Using all available metrics")
        aval.metrics <- private$available$metrics
      }

      if(!is.null(voting.names)){
        if(any(voting.names %in% private$available$votings) ){
          aval.votings <- intersect(voting.names,private$available$votings)
        }else {
          message("[",class(self)[1],"][WARNING] Defined Votings are not ",
                  "available. Using all available votings")
          aval.votings <- private$available$votings
        }
      }else{
        message("[",class(self)[1],"][INFO] Votings are not defined. ",
                "Using all available votings")
        aval.votings <- private$available$votings
      }

      valid.votings <- private$getVotings(aval.metrics,aval.cutoffs,aval.votings)

      if( length(valid.votings) == 0 ){
        message("[",class(self)[1],"][ERROR] There are no voting schemes ",
                "with '", paste(metric.names, collapse = ", ")," metrics and ",
                "'", paste(cutoff.values, collapse = " "), "' cutoffs.")
        return(NULL)
      }

      predictions <- list()

      for (voting.name in names(valid.votings)) {
        voting <- valid.votings[[voting.name]]
        prediction <- voting$getFinalPred( type = type, target = target,
                                           filter = filter )
        predictions[[voting.name]] <- prediction
      }

      PredictionOutput$new(predictions = predictions, type = type, target = target)
    },
    savePredictions = function(dir.path, voting.names = NULL, type = NULL,
                               target = NULL, metric.names = NULL,
                               cutoff.values = NULL ) {
      if (is.null(dir.path))
        stop( "[",class(self)[1],"][FATAL] Save folder not set. Aborting...")

      dir.path <- gsub("\\/$", "", dir.path)

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if (dir.exists(dir.path)) {
          message("[", class(self)[1], "][INFO] Folder '", dir.path,
                  "' has been succesfully created")
        } else { stop("[", class(self)[1], "][FATAL] Cannot create directory '",
                     dir.path, "'. Aborting... ") }
      } else { message("[", class(self)[1], "][INFO] Folder already exists") }

      if(!is.character(cutoff.values)) cutoff.values <- as.character(cutoff.values)

      if(length(cutoff.values) != 0){
        if(any(cutoff.values %in% private$available$cutoffs ) ){
          aval.cutoffs <- intersect(cutoff.values, private$available$cutoffs)
        }else{
          message("[",class(self)[1],"][WARNING] Defined cutoffs are not ",
                  "available. Using all cutoffs")
          aval.cutoffs <- private$available$cutoffs
        }
      }else{
        message("[",class(self)[1],"][INFO] Cutoff values not defined or invalid. ",
                " Using all cutoffs")
        aval.cutoffs <- private$available$cutoffs
      }

      if(!is.null(metric.names)){
        if(any(metric.names %in% private$available$metrics) ){
          aval.metrics <- intersect(metric.names,private$available$metrics)
        }else {
          message("[",class(self)[1],"][WARNING] Defined metrics are not available. ",
                  "Using all metrics")
          aval.metrics <- private$available$metrics
        }
      }else{
        message("[",class(self)[1],"][INFO] Metrics are not defined. ",
                "Using all metrics")
        aval.metrics <- private$available$metrics
      }

      if(!is.null(voting.names)){
        if(any(voting.names %in% private$available$votings) ){
          aval.votings <- intersect(voting.names,private$available$votings)
        }else {
          message("[",class(self)[1],"][WARNING] Defined votings are not available. ",
                  "Using all votings")
          aval.votings <- private$available$votings
        }
      }else{
        message("[",class(self)[1],"][INFO] Votings are not defined. ",
                "Using all votings")
        aval.votings <- private$available$votings
      }

      valid.votings <- private$getVotings(aval.metrics,aval.cutoffs,aval.votings)

      if( length(valid.votings)==0 ){
        stop("[",class(self)[1],"][ERROR] There are no voting schemes ",
                "with '", paste(metric.names, collapse = ", ")," metrics and ",
                "'", paste(cutoff.values, collapse = " "), "' cutoffs. Aborting...")
      }

      predictions <- list()
      for (voting.name in names(valid.votings)) {
        voting <- valid.votings[[voting.name]]
        prediction <- voting$getFinalPred(type = type, target = target,
                                          filter = filter)
        predictions[[voting.name]] <- prediction
      }

      for (voting.name in names(valid.votings)) {
        voting.scheme <- valid.votings[[voting.name]]

        if (is.null(type) || (!type %in% c("prob","raw"))) {
          message("[", class(self)[1], "][INFO] Prediction type not set or invalid.")
          if (is.null(target) || !(target %in% c(private$positive.class,
                                                 private$negative.class))) {
            message("[", class(self)[1], "][INFO] Target class not set or invalid. ",
                    "Saving all predictions with all target values")
            #SAVING PROB
            path <- file.path(dir.path, paste0(voting.name, "_prob_",
                                               private$positive.class,".csv"))
            df <- data.frame(voting.scheme$getFinalPred("prob", private$positive.class),
                             voting.scheme$getFinalPred("prob", private$negative.class))
            rownames <- rownames(df)
            df <- cbind(rownames,df)
            names(df) <- c("ID", private$positive.class,private$negative.class)
            write.table(df, file = path, sep = ";", dec = ".", row.names = FALSE)

            #SAVING RAW
            path <- file.path(dir.path, paste0(voting.name, "_raw_Predictions.csv"))
            df <- data.frame(voting.scheme$getFinalPred("raw"))
            rownames <- rownames(df)
            df <- cbind(rownames,df)
            names(df) <- c("ID","Predictions")
            write.table(df, file = path, sep = ";", dec = ".", row.names = FALSE)

            #SAVING COMBINED
            path <- file.path(dir.path, paste0(voting.name, "_comb_Predictions.csv"))
            prob <- data.frame(voting.scheme$getFinalPred("prob", private$positive.class),
                               voting.scheme$getFinalPred("prob", private$negative.class))
            colnames(prob) <- c(private$positive.class, private$negative.class)
            rownames <- rownames(prob)
            raw <- as.vector(t(voting.scheme$getFinalPred("raw")))
            df <- data.frame(matrix(ncol = 2, nrow = 0))
            for (row in 1:length(raw)) {
              df <- rbind(df, data.frame(raw[row], prob[row, raw[row]]))
            }
            df <- cbind(rownames, df)
            names(df) <- c("ID", "Prediction", "Probability")
            write.table( df, file = path, sep = ";", dec = ".", row.names = FALSE)
          } else {
            message("[",class(self)[1],"][INFO] Saving all predictions for target ",
                    "value '",target,"'")
            for (i in c("prob","raw","bin")) {
              path <- file.path(dir.path, paste0(voting.name, "_",
                                                 i, "_", target,
                                                 ".csv"))
              df <- data.frame(voting.scheme$getFinalPred(i, target))
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
            path <- file.path(dir.path, paste0(voting.name,"_",private$positive.class,
                                               ".csv"))
            df <- data.frame(voting.scheme$getFinalPred(type, private$positive.class),
                             voting.scheme$getFinalPred(type, private$negative.class))
            names(df) <- c(private$positive.class, private$negative.class)
            write.table(df, file = path, sep = ";", dec = ".", row.names = FALSE)
          }else{
            message("[", class(self)[1], "][INFO] Saving '", type, "' predictions ",
                    "for '", target, "'target values")
            path <- file.path(dir.path, paste0(voting.name,"_",target,".csv"))
            df <- data.frame(voting.scheme$getFinalPred(type, target))
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
    trained.models = NULL,
    available = list(),
    getVotings = function(metrics,cutoffs,votings) {
      valid.votings <- list()
      for(voting.type in private$voting.schemes){
        voting.metrics <- intersect(names(voting.type),metrics)
        for(metric in voting.metrics){
          voting.metric <- voting.type[[metric]]
          voting.cutoffs <- intersect(names(voting.metric),cutoffs)
          for(cutoff in voting.cutoffs){
            voting.cutoff <- voting.metric[[cutoff]]
            voting.names <- intersect(names(voting.cutoff),votings)
            for(voting.name in voting.names){
              voting.scheme <- voting.cutoff[[voting.name]]
              entry.name <- paste(metric,cutoff,voting.name,sep = "_")
              valid.votings[[entry.name]] <- voting.scheme
            }
          }
        }
      }
      valid.votings
    }
  )
)