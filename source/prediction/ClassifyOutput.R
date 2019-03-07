ClassifyOutput <- R6Class(
  classname = "ClassifyOutput",
  portable = TRUE,
  public = list(
    initialize = function (preds, models, metric, weights, positive.class, negative.class){
      
      if(length(levels(preds)) != 2 )
        stop("[D2MCSOutput][ERROR] Class values incorrect or missmatch. Aborting...\n")
      
      if( all(levels(preds) %in% c("0","1")) || all(levels(preds) %in% c(0,1))  ){
        preds.levels <- as.numeric( levels(preds) )
        preds <- factor(preds, levels=c(0,1), labels=c(negative.class,positive.class))
      }else preds <- relevel(preds,ref=negative.class)
      
      ifelse( is.data.frame(weights), private$weights <- as.numeric(weights), private$weights <- weights  ) 
      private$positive.class <- positive.class
      private$negative.class <- negative.class
      private$metric <- metric
      private$preds <- preds
      private$model <- models
      private$performance <- NULL
    },
    computePerformance = function( obs, measures ){
      if(length(levels(obs)) != 2 || length(levels(private$preds)) != length(levels(obs)) )
        stop("[D2MCSOutput][ERROR] Class values incorrect or missmatch. Aborting...\n")

      if ( is.null(measures) || !is.list(measures) || !all(sapply(measures, inherits,"MeasureFunction")) )
        stop("[D2MCSOutput][ERROR] Measures should be a list comprised of MeasureFunction objects\n")
      
      if( all(levels(obs) %in% c("0","1")) ){
         obs.levels <- as.numeric( levels(obs) )
         obs <- factor(obs, levels=c(0,1), labels=c(private$negative.class,private$positive.class))
      }else obs <- relevel(obs,ref=private$negative.class)
      
      private$performance <- do.call(rbind,lapply( measures, function(entry,y) { 
        df <- data.frame(entry$getName(), entry$compute(y))
        rownames(df) <- NULL
        names(df) <- c("Measure","Value") 
        df 
      }, y= ConFMatrix$new(caret::confusionMatrix(private$preds,obs, positive=private$positive.class)) )) 
      
      private$performance
    },
    plotPerformance = function(obs=NULL, measures=NULL){
      if ( (is.null(obs) || is.null(measures)) && is.null(private$performance) )
        stop("[D2MCSOutput][ERROR] Method 'computedPerformance' should be previously executed or arguments must be defined\n")
      
      if(is.null(private$performance)) self$computePerformance(obs,measures)
      
      plot <- ggplot(private$performance, aes(x=Measure,y=Value) ) + geom_bar(stat="identity") + 
        geom_point(aes(shape=15,stroke=1)) + scale_shape_identity() + 
        ggtitle("Classifier performance Benchmarking") + guides(fill=FALSE) + 
        theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "none" )
      ggplotly(plot)
        
    },
    getWeights = function() { private$weights },
    getPredictions = function() { private$preds },
    getMetric = function() { private$metric },
    getPositiveClass = function() { private$positive.class }, 
    getNegativeClass = function() { private$negative.class },
    getModels = function() { private$model }
  ),
  private = list(
    positive.class = NULL,
    negative.class = NULL,
    weights = NULL,
    preds = NULL,
    model = NULL,
    metric = NULL,
    performance = NULL
  )
)