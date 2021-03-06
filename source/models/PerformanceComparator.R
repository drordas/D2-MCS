PerformanceComparator <- R6Class(
  classname = "PerformanceComparator",
  portable = TRUE,
  public = list(
    initialize = function (test.set, d2mcs.models, op.results, pareto.optimal = NULL, voting.scheme = NULL, measures){
      cat("[PerformanceComparator][INFO] -------------------------------------------------------\n",sep="")
      cat("[PerformanceComparator][INFO] Performance comparator started! \n")
      cat("[PerformanceComparator][INFO] -------------------------------------------------------\n",sep="")
      if( !is.null(test.set) && !inherits(test.set,"Subset")  )
        stop("[PerformanceComparator][ERROR] Test dataset missing or incorrect. Should inherit from 'Subset class'. Aborting...\n")
      
      if( is.null(test.set$getClass()) )
        stop("[PerformanceComparator][ERROR] Undefined class in dataset. Aborting...\n")

      if ( !inherits(d2mcs.models,"TrainOutput" ) )
        stop("[PerformanceComparator][ERROR] D2MCS models are not correct. Must be a 'TrainOutput' class. Aborting...\n")

      if ( !inherits(op.results,"Optimizers") )
        stop("[PerformanceComparator][ERROR] Optimizers must inherit from 'Optimizers' class. Aborting...\n")

      if ( is.null(measures) || !is.list(measures) || !all(sapply(measures, inherits,"MeasureFunction")) )
        stop("[PerformanceComparator][ERROR] Measures should be a list comprised of MeasureFunction objects\n")
      
      if ( !inherits(voting.scheme,"VotingScheme") ){
        cat("[PerformanceComparator][INFO] VotingScheme invalid or unasigned. Using '",op.results$getVotingMethod()$getName(),"'\n", sep="") 
        voting <- op.results$getVotingMethod()
      }else voting <- voting.scheme
      
      if(!is.null(pareto.optimal) && !inherits(pareto.optimal,"ParetoDistance")){
        cat("[PerformanceComparator][ERROR] Pareto distance function should inherit from 'ParetoDistance class'. Assuming default method\n")
        private$distance <- EuclideanDistance$new()
      }else private$distance <- pareto.optimal

      ## COMPUTING PREDICTIONS
      cat("[PerformanceComparator][INFO] -------------------------------------------------------\n",sep="")
      prediction.cluster <- PredictionList$new( d2mcs.models$getMetric() )
      invisible(sapply ( 1:op.results$getModels()$size(), function(pos, models, pred.cluster, set, instances){
        cat("[PerformanceComparator][INFO] Computing predictions for cluster '",pos,"' of '",models$size(),"'\n",sep="")
        cat("[PerformanceComparator][INFO] -------------------------------------------------------\n",sep="")
        pred <- Prediction$new( model = models$getAt(pos)$getObject(), class.values = levels(set$getClass()), 
                                positive.class = op.results$getPositiveClass() )
        pred$execute(instances)
        pred.cluster$addPrediction(pred)
      }, models= op.results$getModels(), pred.cluster = prediction.cluster, set = test.set, 
         instances = test.set$getInstances(ignore.class = TRUE) ) )
      
      real.class <- test.set$getClass()
      negative.class <- levels(real.class)[ which(levels(real.class) != op.results$getPositiveClass()) ]

      opt.models <- do.call(c,lapply(op.results$getOptimizers(), function(opt, predictions, pareto.distance, real, pos.class, neg.class){
        ifelse( inherits(opt,"MOOData"), weights <- as.numeric(opt$getValues(pareto.distance)[1,]), weights <- as.numeric(opt$getValues()) )
        preds <- voting$execute( predictions, weights )
        ModelPerformance$new( model.name = opt$getName(), 
                              ConFMatrix$new( caret::confusionMatrix( factor(preds,levels=c(0,1), 
                                                                             labels=c(neg.class,pos.class)), 
                                                                      relevel(real,ref=neg.class), positive=pos.class) ), weights)
      }, pareto.distance = private$distance, predictions= prediction.cluster, real = real.class, 
         pos.class = op.results$getPositiveClass(), neg.class= negative.class ) )

      preds <- voting$execute(prediction.cluster, weights=d2mcs.models$getWeights() )
      
      cl <- ModelPerformance$new( model.name = "Classifier", 
                                  conf.mat = ConFMatrix$new(caret::confusionMatrix( factor(preds,levels=c(0,1), labels=c(negative.class,op.results$getPositiveClass())), 
                                                                                    relevel(real.class,ref=negative.class), 
                                                                                    positive=op.results$getPositiveClass())), 
                                  weights = d2mcs.models$getWeights() )
      private$results <- c( opt.models, cl )
      
      df <- as.data.frame( do.call(rbind,lapply(private$results, function(model, measures){
        lapply(measures, function(measure,model){
          measure$compute(model)
        }, model=model)
      }, measures = measures )) )
      
      private$performance <- cbind( df[!sapply(df, is.list)], (t(apply( df[sapply( df, is.list)], 1, unlist))) )
      private$performance <- cbind( sapply(results, function(x) {x$getName() } ), private$performance )
      colnames(private$performance) <- c("Models",sapply(measures, function(m) {m$getName() } ) )
    },
    plotResults = function(){
      plot <- reshape2::melt(private$performance,id="Models")
      ggplotly(ggplot(plot, aes(x=variable,y=value, fill=Models)) + 
               geom_bar(position = "dodge", stat="identity") + theme(legend.position = "top") )
    },
    showResults = function(){  private$performance }
  ),
  private = list(
    results = NULL,
    distance = NULL,
    performance = NULL
  )
)