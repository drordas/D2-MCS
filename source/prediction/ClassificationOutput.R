ClassificationOutput <- R6Class(
  classname = "ClassificationOutput",
  portable = TRUE,
  public = list(
    initialize = function (voting.scheme, models) {
      if(!inherits(voting.scheme,"VotingScheme")){
        stop("[",class(self)[1],"][ERROR] Voting parameter not valid.",
             " Must inherit from 'VotingScheme' class. Aborting...")
      }
      
      private$voting <- voting.scheme
      private$trained.models <- models
      private$performance <- NULL
      private$real.values <- NULL
      private$pred.values <- NULL
    },
    computePerformance = function(test.set, measures){
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
      
      # train.perf.digits <- round( sapply(private$trained.models$models, 
      #                                    function(model){model$model.performance}),
      #                             digits = 4 )
      #train.perf.string <- paste0(train.perf.digits,collapse = ", ")
      
      private$performance <- do.call(rbind,lapply( measures, function(entry,cf,perf){
        result <- entry$compute(cf)
        sapply(train.perf.digits,function(x){result-x})
        if(entry$getName() %in% private$trained.models$metric){
          df <- data.frame( entry$getName(), result)#, 
                            #"<==>",train.perf.string)
        }else{
          df <- data.frame( entry$getName(), result)#,
                            #"    ",train.perf.string)
        }
        rownames(df) <- NULL
        names(df) <- c("Measure","Value")#,"Reference","Train Performance,Gain Ratio") 
        df 
      }, cf= ConFMatrix$new( caret::confusionMatrix(private$pred.values,
                             private$real.values,
                             positive=private$voting$getPositiveClass())))#,
         #perf=train.perf.digits )
      )
      #private$performance <- cbind(private$performance,train.performance)
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
    #getPredictions = function() { private$voting },
    getMetric = function() { private$trained.models$metric },
    getPositiveClass = function() { private$positive.class },
    getNegativeClass = function() { private$negative.class },
    getModelInfo = function() { 
      model.info <- do.call(rbind,lapply(private$model, function(model){ 
        aux <- data.frame(model$model.name,model$model.performance)
        rownames(aux) <- NULL
        names(aux) <- c("Model Name","Performance") 
        aux
      }))
      model.info
    }
  ),
  private = list(
    uniformize = function(test.set){
      private$real.values <- test.set$getClassValues()
      
      if( is.factor(private$real.values) ){
        private$pred.values <- private$voting$getPrediction("raw",private$voting$getPositiveClass())
        if( length(levels(real.values)) != length(levels(private$pred.values)) || 
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
    performance = NULL
  )
)