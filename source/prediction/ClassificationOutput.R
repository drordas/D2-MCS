ClassificationOutput <- R6Class(
  classname = "ClassificationOutput",
  portable = TRUE,
  public = list(
    initialize = function (voting.scheme, models, metric) {
      if(!inherits(voting.scheme,"VotingScheme")){
        stop("[",class(self)[1],"][ERROR] Voting parameter not valid.",
             " Must inherit from 'VotingScheme' class. Aborting...")
      }

      private$voting <- voting.scheme
      private$trained.models <- models
      private$metric <- metric
      private$performance <- NULL
      private$real.values <- NULL
      private$pred.values <- NULL
      private$positive.class <- voting.scheme$getPositiveClass()
      private$negative.class <- setdiff(voting.scheme$getClassValues(),
                                        private$positive.class)
    },
    getPerformance = function(test.set, measures){
      if(!inherits(test.set,"Subset"))
        stop("[",class(self)[1],"][ERROR] Test set invalid.",
             " Must be a Subset object. Aborting...")

      if( test.set$getNrow() == 0 )
        stop("[",class(self)[1],"][ERROR] Test set is empty. Aborting...")

      if(private$voting$getPositiveClass() != test.set$getPositiveClass()){
        stop("[",class(self)[1],"][ERROR] Positive class values missmatch. ['",
             test.set$getPositiveClass(),"' vs '",
             private$voting$getPositiveClass(),"'] used in classification",
             "and test respectively. Aborting... ")
      }

      if(!(test.set$getPositiveClass() %in% private$voting$getClassValues()) ){
        stop("[",class(self)[1],"][ERROR] Positive class '",
             test.set$getPositiveClass(),"' in test set does not match",
             " with [",paste0(private$voting$getClassValues(),collapse = ", "),"]")
      }

      if(!is.list(measures) || !all(sapply(measures,inherits,"MeasureFunction"))){
        stop("[",class(self)[1],"][ERROR] Measures should be a list comprised of ",
             "'MeasureFunction' objects. Aborting...")
      }

      if(is.null(private$real.values) || is.null(pred.values)){
        private$uniformize(test.set)
      }

      private$performance <- do.call(rbind,lapply( measures, function(entry,cf,perf){
        result <- entry$compute(cf)
        if(entry$getName() %in% private$trained.models$metric){
          df <- data.frame( entry$getName(), result)
        }else{
          df <- data.frame( entry$getName(), result)#,
                            #"    ",train.perf.string)
        }
        rownames(df) <- NULL
        names(df) <- c("Measure","Value")
        df
      }, cf= ConFMatrix$new( caret::confusionMatrix(private$pred.values,
                             private$real.values,
                             positive=private$voting$getPositiveClass())))
      )
      private$performance
    },
    plot = function(test.set=NULL, measures=NULL){
      if ( (!inherits(test.set,"Subset") || is.null(measures)) &&
           is.null(private$performance) )
        stop("[D2MCSOutput][ERROR] Method 'computedPerformance' should be ",
             "previously executed or arguments must be defined")

      if(is.null(private$performance)) self$computePerformance(test.set,measures)

      plot <- ggplot(private$performance, aes(x=Measure,y=Value) ) + geom_bar(stat="identity") +
        geom_point(aes(shape=15,stroke=1)) + scale_shape_identity() +
        ggtitle("Classifier performance Benchmarking") + guides(fill=FALSE) +
        theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "none" )
      plotly::ggplotly(plot)

    },
    getWeights = function() { private$voting$getWeights() },
    getPredictions = function(type=NULL, target=NULL){
      private$voting$getPrediction(type,target)
    },
    getMetric = function() { private$metric },
    getPositiveClass = function() { private$positive.class },
    getModelInfo = function() {
      model.info <- do.call(rbind,lapply(private$trained.models, function(model){
        aux <- data.frame(model$model.name,model$model.performance)
        rownames(aux) <- NULL
        names(aux) <- c("Model Name","Performance")
        aux
      }))
      model.info
    },
    savePredictions = function(dir.path, type = NULL, target = NULL){
      if(missing(dir.path))
        stop( "[",class(self)[1],"][ERROR] Save folder not set. Aborting...")

      dir.path <- gsub("\\/$","",dir.path)

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if(dir.exists(dir.path)) {
          message("[",class(self)[1],"][INFO] Folder '",dir.path,
                  "' has been succesfully created")
        }else { stop("[",class(self)[1],"][ERROR] Cannot create directory '",
                     dir.path,"'. Aborting... ") }
      }else { message("[",class(self)[1],"][INFO] Folder already exists") }

      if(is.null(type) || (!type %in% c("prob","raw")) ){
        message("[",class(self)[1],"][INFO] Prediction type not set or invalid.")
        if( is.null(target) || !(target %in% c( private$positive.class,
                                                private$negative.class ) )){
          message("[",class(self)[1],"][INFO] Target class not set or invalid. ",
                  "Saving all predictions with all target values")
          #SAVING PROB
          path <- file.path(dir.path,paste0("prob_",private$positive.class,".csv"))
          df <- data.frame(private$voting$getPrediction("prob",private$positive.class),
                           private$voting$getPrediction("prob",private$negative.class))
          rownames <- rownames(df)
          df <- cbind(rownames,df)
          names(df) <- c("ID",private$positive.class,private$negative.class)
          write.table( df, file= path, sep= ";", dec= ".", row.names = FALSE)

          #SAVING RAW
          path <- file.path(dir.path,paste0("raw_Predictions.csv"))
          df <- data.frame(private$voting$getPrediction("raw"))
          rownames <- rownames(df)
          df <- cbind(rownames,df)
          names(df) <- c("ID","Predictions")
          write.table( df, file= path, sep= ";", dec= ".", row.names = FALSE)
          #SAVING COMBINED
          path <- file.path(dir.path,paste0("comb_Predictions.csv"))
          prob <- data.frame(private$voting$getPrediction("prob",private$positive.class),
                             private$voting$getPrediction("prob",private$negative.class))
          rownames <- rownames(prob)
          raw <- as.vector(t(private$voting$getPrediction("raw")))
          df <- data.frame(matrix(ncol = 2, nrow = 0))
          for(row in 1:length(raw)) {
            df <- rbind(df,data.frame(raw[row],prob[row,raw[row]]))
          }
          df <- cbind(rownames,df)
          names(df) <- c("ID","Prediction","Probability")
          write.table( df, file= path, sep= ";", dec= ".", row.names = FALSE)
        }else{
          message("[",class(self)[1],"][INFO] Saving all predictions for target",
                  " value '",target,"'")
          for (i in c("prob","raw","bin") ){
            path <- file.path(dir.path,paste0(i,"_",private$positive.class,".csv"))
            df <- data.frame(private$voting$getPrediction(i,target))
            names(df) <- target
            write.table(df, file= path, sep= ";", dec= ".", row.names = FALSE)
          }
        }
      }else{
        message("[",class(self)[1],"][INFO] Prediction type set as '",type,"'.")
        if( is.null(target) || !(target %in% c( private$positive.class,
                                                private$negative.class ) ) ){
          message("[",class(self)[1],"][INFO] Target class not set or invalid. ",
                  "Saving '",type,"' predictions for all target values")
          path <- file.path(dir.path,paste0(type,"_",private$positive.class,".csv"))
          df <- dataframe(private$voting$getPrediction(type,private$positive.class),
                          private$voting$getPrediction(type,private$negative.class))
          names(df) <- c(private$positive.class,private$negative.class)
          write.table( df, file= path, sep= ";", dec= ".", row.names = FALSE)
        }else{
          message("[",class(self)[1],"][INFO] Saving '",type,"' predictions ",
                  "for '",target,"'target values")
          path <- file.path(dir.path,paste0(type,"_",target,".csv"))
          df <- data.frame(private$voting$getPrediction(type,target))
          names(df) <- target
          write.table( df, file= path, sep= ";", dec= ".", row.names = FALSE)
        }
      }
    }
  ),
  private = list(
    uniformize = function(test.set){
      private$real.values <- test.set$getClassValues()

      if( is.factor(private$real.values) ){
        private$pred.values <- private$voting$getPrediction("raw",private$voting$getPositiveClass())
        if( length(levels(private$real.values)) != length(levels(private$pred.values)) ||
            !(levels(private$real.values) %in% levels(private$pred.values)) ){
          stop("[",class(self)[1],"][ERROR] Class values missmatch. Aborting...")
        }
      }else{
        if (all(sapply(unique(private$real.values),
                       function(digit){
                         !length(grep("[^[:digit:]]",
                                      format(digit[1], scientific= FALSE)))
                       }))) {
          private$pred.values <- private$voting$getPrediction("bin",private$voting$getPositiveClass() )
          if( length(unique(private$real.values)) != length(unique(private$pred.values)) ||
              !(unique(private$real.values) %in% unique(private$pred.values)) ){
            stop("[",class(self)[1],"][ERROR] Class values missmatch. Aborting...")
          }
        }
        private$real.values <- factor(private$real.values,
                                      levels=c(private$voting$getPositive(),
                                               setdiff(private$voting$getClassValues(),
                                                       private$voting$getPositive())))
        private$pred.values <- factor(private$pred.values,
                                      levels=c(private$voting$getPositive(),
                                               setdiff(private$voting$getClassValues(),
                                                       private$voting$getPositive())))
      }
    },
    pred.values = NULL,
    real.values = NULL,
    positive.class = NULL,
    negative.class = NULL,
    voting = NULL,
    trained.models = NULL,
    performance = NULL,
    metric = NULL
  )
)