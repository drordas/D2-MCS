D2MCS <- R6Class(
  classname = "D2MCS",
  portable = TRUE,                   
  public = list(
    initialize = function(dir.path, num.cores = NULL, socket.type="PSOCK", outfile,
                          serialize=FALSE, trainFunction = NULL ){ 
      
      if(missing(dir.path)) { 
        stop( "[",class(self)[1],"][ERROR] Path to store ML models should be defined" ) 
      }
      dir.path <- gsub("\\/$","",dir.path)
      
      if(missing(outfile)) { 
        message("[",class(self)[1],"][INFO] Path for Log file not defined") 
        outfile <- "/dev/null"
      } 
      else{
        if (!file.exists(outfile)){
          dir.create(outfile, recursive = TRUE)
          message("[",class(self)[1],"][INFO] Logs path not definded '",
                  outfile, "' does not exist. Creating ..." )
        }
      }
      
      if (!dir.exists(dir.path)) { 
        dir.create(dir.path, recursive = TRUE) 
        if(dir.exists(dir.path)) {
          message("[",class(self)[1],"][INFO] Directory '",dir.path,
                  "' has been succesfully created")
        }else { stop("[",class(self)[1],"][ERROR] Cannot create directory '",
                     dir.path,"'") }
      } 
      else { message("[",class(self)[1],"][INFO] Directory already exists") }

      if( any(is.null(num.cores), (num.cores >= detectCores()), num.cores > 10)  ){
        if(detectCores() > 10){
          message(yellow(paste0("[",class(self)[1],"][WARNING] Invalid number of cores",
                         " (1>= num.cores <= 10)")))
        }else{
          message(yellow(paste0("[",class(self)[1],"][WARNING] Invalid number of cores",
                         " (1>= num.cores < ",detectCores(),")")))
        }
        cores <- min(max(1,detectCores()-2),10)
        message("[",class(self)[1],"][INFO] Using default number of cores (",
                cores,"/",detectCores(),")")
      } 
      else {cores <- nCores}
      
      if( !socket.type %in% c("PSOCK","FORK") ){
        warning(yellow("[",class(self)[1],"][WARNING] Invalid socket type.",
                       "Assuming 'PSOCK' cluster\n"))
        socket <- "PSOCK"
      }
      else { socket <- socket.type }
      
      if(!is.logical(serialize)){
        message(yellow("[",class(self)[1],"][WARNING] Invalid serialization",
                       "option. Assuming not serialization\n"))
        xdr <- FALSE
      }
      else xdr <- serialize
      
      if ( any( missing(trainFunction), is.null(trainFunction), 
                (!"TrainFunction" %in% class(trainFunction)) ) ){
        stop( "[",class(self)[1],"][ERROR] TrainFunction not defined or",
              "incorrect (should inherit from TrainFunction abstract class)" )
      }
      else { private$trainFunction <- trainFunction }

      private$availableModels <- private$loadAvailableModels()
      private$path <- dir.path

      private$cluster.conf <- list( cores=cores, socket=socket, 
                                    outfile=outfile, xdr=xdr )
      private$cluster.obj <- NULL
      #private$executed.models <- NULL
      private$classify.output <- NULL
      private$cluster.models <- list(models=list(),metric=NULL)
      private$values.real <- NULL
    },
    train = function(train.set= NULL, num.clusters= NULL, 
                     ex.classifiers= c(), ig.classifiers=c(), 
                     metric= NULL, saveAllModels= FALSE ) {
      
      #CHECK IF TRAIN.SET IS VALID
      if(!"TrainSet" %in% class(train.set) ){
        stop("[",class(self)[1],"][ERROR] Train set not defined of",
             "incorrect (must be of type 'Cluster')\n")
      }

      #CHECK IF NUM.CLUSTER IS VALID    
      if (missing(num.clusters)){
        message("[",class(self)[1],"][INFO] Number of clusters not set.",
                "Using all clusters..." )
      }
      if (any( is.null(num.clusters),!is.numeric(num.clusters), 
               !is.vector(num.clusters) )) {
        message(yellow(paste0("[",class(self)[1],"][WARNING] Number of clusters not set",
                       " (must be numeric or vector). Using all clusters")))
        num.clusters <- c(1:train.set$getNumClusters())
      }
      else{
        if (all(is.numeric(num.clusters), num.clusters > train.set$getNumClusters()) ){
          message(yellow(paste0("[",class(self)[1],"][WARNING] Number of clusters ",
                         "is higher than number of existing clusters.",
                         "Using all clusters")))
          num.clusters <- c(1:train.set$getNumClusters())
        }
        else num.clusters <- c(1:num.clusters)
      }

      #VERIFY IF EX.CLASSIFIERS PARAMETER IS DEFINED (AND VALID)
      if(all(is.character(ex.classifiers),length(ex.classifiers) > 0) ) {
        usedModels <- ex.classifiers
      }
      else { usedModels <- private$availableModels$name }
      
      #VERIFY IF IG.CLASSIFIERS PARAMETER IS DEFINED (AND VALID)
      if( all(is.character(ig.classifiers), length(ig.classifiers) > 0) ){
        message("[",class(self)[1],"][INFO] Ignoring '",
                length(ig.classifiers),"' M.L models")
        usedModels <- setdiff(usedModels,ig.classifiers)
        message("[",class(self)[1],"][INFO] Still '",length(usedModels),
                "' M.L models available")
      }
      
      message("[",class(self)[1],"][INFO] Making parallel socket cluster with ",
              private$cluster.conf$cores," cores")
      
      private$cluster.obj <- makeCluster( private$cluster.conf$cores, 
                                          type= private$cluster.conf$socket, 
                                          outfile= private$cluster.conf$outfile, 
                                          useXDR=private$cluster.conf$xdr )
      private$metric <- metric
      available.models <- private$availableModels[(private$availableModels$name %in% usedModels), ]
      num.available <- nrow(available.models)
      private$cluster.models <- list(models=list(), metric=private$metric)
      
      #START TRAINING PROCESS  
      for (i in num.clusters ){
        message("[",class(self)[1],"][INFO] ---------------------------------------")
        message("[",class(self)[1],"][INFO] Training models for cluster '",i,
                "' of '", max(num.clusters),"'")
        message("[",class(self)[1],"][INFO] ---------------------------------------")
        model.path <- file.path( private$path,private$metric,
                                 paste0("C[",i,"-",train.set$getNumClusters(),"]") )
        
        executed.models <- ExecutedModels$new(model.path)
        
        message("[",class(self)[1],"][INFO] Total '",
                executed.models$size(),"' models were previously executed")
        
        #DELETE IMCOMPATIBLE MODELS
        if (abs(mean(cor(train.set$getFeatureValues(i), 
                        as.numeric(train.set$getClassValues()) ), 
                    na.rm=TRUE) ) < 0.3 ) ##CORRELATION
        { 
          message(yellow(paste0("[",class(self)[1],"][WARNING] High-Correlated Data (< 0.3).",
                         " Ignoring Linear-based and Discriminant-based models")))
          pending.models <- subset(available.models,
                                     !grepl("Linea[l|r]|Discriminant",
                                            paste(available.models$description,
                                                  available.models$family,sep=" ")) 
                                     )
          message("[",class(self)[1],"][INFO] Removing ",
                  (nrow(available.models) - nrow(pending.models)),
                  " incompatible M.L. models")
        }else pending.models <- available.models
        
        #UPDATE MODELS
        pending.models <- pending.models[!(pending.models$name %in% 
                                          executed.models$getNames()), ]
        num.pending <- nrow(pending.models)
        num.executed <- executed.models$size()
        
        #COMPUTE IF ML MODELS HAVE NOT BEEN EXECUTED
        if(num.pending == 0){
          if( any(is.null(num.executed),num.executed==0) ) {
            stop("[",class(self)[1],"][ERROR] Models were not executed for",
                 "cluster ",i,"/",max(num.clusters),". Aborting...")
          }

          private$cluster.models$models <- append( private$cluster.models$models,
                                            list(executed.models$getBest()$train) )
          if(i < max(num.clusters))
            message( "[",class(self)[1],"][INFO] Remaining ",num.pending,
                     " M.L. models has been already executed for cluster ",
                     i,"/",max(num.clusters),". Executing next cluster...")
          else {
            message( "[",class(self)[1],"][INFO] Remaining ",num.pending,
                     " M.L. models has been already executed for cluster ",
                     i,"/",max(num.clusters),".")
            message("[",class(self)[1],"][INFO] Finish !")
            if(!is.null(private$cluster.obj)){ 
              stopCluster(private$cluster.obj) 
              private$cluster.obj <- NULL 
            }
          }
          next
        }else{ 
          message("[",class(self)[1],"][INFO] ",
                   num.executed,"/",num.available,
                   " M.L. models has been already executed for cluster ",
                   i,"/",max(num.clusters),"") 
          message("[",class(self)[1],"][INFO] Executing remaining ",
                  num.pending," M.L. model(s)" )
        }

        apply(pending.models, 1, function(model, executedModels){
          ifelse(isTRUE(model$prob), 
                 private$trainFunction$create(UseProbability$new(), 
                                              search.method= "random", 
                                              class.probs = TRUE ),
                 private$trainFunction$create(NoProbability$new(), 
                                              search.method= "random", 
                                              class.probs = FALSE)
          )
          
          if( executedModels$exist(model$name) ){
            message("[",class(self)[1],"][INFO] Model '",
                    model$name,"' has been previously trained. Skipping...")
          }else{
            model.instances <- train.set$getInstances(i)
            model.recipe <- DefaultModelFit$new(model.instances, 
                                                train.set$getClassName())$createRecipe()
            model.type <- Model$new(dir= model.path, model= model)
            model.type$train(train.set= model.instances, fitting= model.recipe,
                             trFunction= private$trainFunction, metric= private$metric)
            if(model.type$isTrained()){
              message("[",class(self)[1],"][INFO] Model '",
                      model.type$getName(),"' has been succesfully trained")
              executedModels$add(model.type,keep.best=!isTRUE(saveAllModels))
              executedModels$save()
            }else{
              message("[",class(self)[1],"][WARNING] Unable to train model '",
                      model$name,"'. Skipping...")
            }
          }
        }, executedModel= executed.models )
        
        private$cluster.models$models <- append( private$cluster.models$models, 
                                          list( executed.models$getBest()$train) )
        message("[",class(self)[1],"][INFO] Finish!")
      }

      if(!is.null(private$cluster.obj)) { 
        stopCluster(private$cluster.obj)
        private$cluster.obj <- NULL 
      }
    },
    classify = function(test.set, voting.scheme, positive.class=NULL){
      if( !inherits(test.set,"Subset")  )
        stop("[",class(self)[1],"][ERROR] Test dataset missing or invalid. ",
             "Must be a Subset object\n")
      
      if (missing(voting.scheme) || !inherits(voting.scheme,"VotingScheme") )
        stop("[",class(self)[1],"][ERROR] Voting Scheme missing or invalid. ",
             "Must inherit from VotingScheme abstract class.")
      
      class.values <- unique(test.set$getClassValues())
      if(is.factor(class.values)) class.values <- levels(class.values)
      
      if (is.null(positive.class)){
        positive.class <- test.set$getPositiveClass()
      }else{
        if ( !(positive.class %in% class.values) ){
          message(yellow(paste0("[",class(self)[1],"][WARNING] Positive class ",
                                "value is invalid. Must be [",
                                paste0(class.values,collapse = ", "),"].",
                                "Assuming default value (",test.set$getPositiveClass(),")")))
          positive.class <- test.set$getPositiveClass()
        }
      }
      if ( any( is.null(private$cluster.models$models),
                !is.list(private$cluster.models$models),
               length(private$cluster.models$models)==0) ){
        stop("[",class(self)[1],"][ERROR] Models were not trained. Aborting...")
      }
      
      if( any(is.null(test.set$getClassValues()),
              length(test.set$getClassValues())!=nrow(test.set$getFeatures() )) ){
        stop("[",class(self)[1],"][ERROR] Target values missing or invalid. Aborting...")
      }

      private$values.real <- test.set$getClassValues()

      message("[",class(self)[1],"][INFO] ------------------------------------",
              "-------------------")
      message("[",class(self)[1],"][INFO] Starting classification operation")
      message("[",class(self)[1],"][INFO] ------------------------------------",
              "-------------------")
      
      instances <- test.set$getFeatures()
      predictions <- ClusterPredictions$new(class.values= class.values, 
                                            positive.class= positive.class)
      num.clusters <- length(private$cluster.models$models)
      
      for ( cluster in 1:num.clusters ){
        message("[",class(self)[1],"][INFO] Computing predictions for cluster '",
                cluster,"' of '",num.clusters,"'")
        message("[",class(self)[1],"][INFO] ----------------------------------",
                "---------------------")
        pred <- Prediction$new( model= private$cluster.models$models[[cluster]], 
                                class.values= class.values, 
                                positive.class= positive.class )
        pred$execute(instances)
        predictions$add(pred)
      }
      
      message("[D2MCS][INFO] Computing final prediction values using '",
              voting.scheme$getName(),"'")
      voting.scheme$execute(predictions)

      private$classify.output <- ClassificationOutput$new(voting.scheme= voting.scheme, 
                                                          models= private$cluster.models)
      message("[D2MCS][INFO] -------------------------------------------------------")
      message("[D2MCS][INFO] Classification operation finished")
      message("[D2MCS][INFO] -------------------------------------------------------")
      
      private$classify.output
    },
    optimize = function(opt.set, voting.scheme, opt.algorithm, weights=NULL, positive.class=NULL){
      if( !is.null(opt.set) && !inherits(opt.set,"Subset")  )
        stop("[D2MCS][ERROR] Test dataset missing or incorrect. Should inherit",
             " from 'Subset class'. Aborting...")

      if ( !inherits(voting.scheme,"VotingScheme") )
        stop("[D2MCS][ERROR] Voting Scheme missing or invalid. Aborting...")

      if ( !is.list(opt.algorithm) && !inherits(opt.algorithm,"WeightsOptimizer") )
        stop("[D2MCS][ERROR] Optimization algorithm is invalid. Must inherit",
             " from 'WeightedOptimizer' Aborting...")
      
      if(!is.list(opt.algorithm)) opt.algorithm <- list(opt.algorithm)
      
      if( is.list(opt.algorithm) && !all(sapply(opt.algorithm,inherits,"WeightsOptimizer")) )
        stop("[D2MCS][ERROR] Optimization algorithms is invalid. List elements",
             " must inherit from 'WeightedOptimizer' object. Aborting...")

      if( is.factor(opt.set$getClassValues()))
        class.values <- levels(unique(opt.set$getClassValues()))
      else class.values <- unique(opt.set$getClassValues())
      
      if ( is.null(positive.class) )
        positive.class <- opt.set$getPositiveClass()
      else{
        if( !positive.class %in% class.values ){
          message("[D2MCS][WARNING] Positive class missing or invalid. ",
                "Must be: [",paste0(class.values,collapse=", "),"]. ",
                "Assuming default positive class: ",opt.set$getPositiveClass())
          positive.class <- opt.set$getPositiveClass()
        }
      }
      
      if ( any( is.null(private$cluster.models$models),
                !is.list(private$cluster.models$models),
                length(private$cluster.models$models)==0) ){
        stop("[",class(self)[1],"][ERROR] Models were not trained. Aborting...")
      }
      
      if( any(is.null(weights),length(weights) < 
              length(private$cluster.models$models),!is.numeric(weights))){
        message("[",class(self)[1],"][WARNING] Weights not defined.",
                "Assuming default weigths: [",paste0(round(self$getBestPerformanceByCluster(),
                                                           digits = 3),
                                                     collapse= ", "),"].")
        weights <- self$getBestPerformanceByCluster()
      }
      
      weights <- weights[1:length(private$cluster.models$models)]
      
      if( any(is.null(opt.set$getClassValues()),
              length(opt.set$getClassValues())!=nrow(opt.set$getFeatures() )) ){
        stop("[",class(self)[1],"][ERROR] Number of target values and instances",
             " missmatch. Aborting...")
      }
      
      real.values <- opt.set$getClassValues()

      message("[D2MCS][INFO] -------------------------------------------------------")
      message("[D2MCS][INFO] D2MCS Optimization stage")
      message("[D2MCS][INFO] -------------------------------------------------------")

      instances <- opt.set$getFeatures()
      negative.class <- setdiff(class.values,opt.set$getPositiveClass())
      
      predictions <- ClusterPredictions$new(class.values= class.values, 
                                            positive.class= positive.class)
      
      num.clusters <- length(private$cluster.models$models)
      
      for ( cluster in 1:num.clusters ){
        message("[",class(self)[1],"][INFO] Computing predictions for cluster '",
                cluster,"' of '",num.clusters,"'")
        message("[",class(self)[1],"][INFO] ----------------------------------",
                "---------------------")
        pred <- Prediction$new( model= private$cluster.models$models[[cluster]], 
                                class.values= class.values, 
                                positive.class= positive.class )
        pred$execute(instances)
        predictions$add(pred)
      }
      
      message("[D2MCS][INFO] Computing prediction values using '",
              voting.scheme$getName(), "voting scheme")

      
      compute.fitness <- function(weights,min.function) {
        voting.scheme$execute(predictions=predictions,weights=weights)
        pred.values <- voting.scheme$getPrediction("raw",positive.class)
        mf <- min.function(caret::confusionMatrix(pred.values,real.values, 
                                                  positive=positive.class, 
                                                  mode="everything"))
        return(mf)
      }

      
      message("[D2MCS][INFO] Starting optimization process using ",
              paste0(sapply(opt.algorithm, function(x) x$getName() ),
                     collapse = ", ")," Optimization Algorithm(s)")
      
      freq <- table(real.values)

      opt.data <- sapply( opt.algorithm, function(alg, wg, freq) { 
        alg$execute(wg, compute.fitness)
        alg$getResult(n.positive = as.numeric(freq[positive.class]), 
                      n.negative= as.numeric(freq[negative.class]) )
      }, wg = weights, freq= freq)
      
      return (Optimizers$new( voting.scheme= voting.scheme, 
                              cluster.models= private$cluster.models$models, 
                              metric= private$metric, 
                              optimizers= opt.data, 
                              positive.class= positive.class, 
                              negative.class= negative.class))
    },
    getBestPerformanceByCluster = function(){
      if ( any( is.null(private$cluster.models$models),
                !is.list(private$cluster.models$models),
                length(private$cluster.models$models)==0) ){
        stop("[",class(self)[1],"][ERROR] Models were not trained. Aborting...")
      }
      
      c(sapply(private$cluster.models$models, function(model) model$model.performance))
    },
    getTrainedModels = function(){
      if( is.null(private$models.weights) || is.null(private$bestModels) || 
          (private$bestModels$size() < 1) )
        stop("[D2MCS][ERROR] Parameters not assigned.",
             "Please execute Train method first")
      TrainOutput$new(models= private$bestModels, 
                      weights= private$models.weights, metric= private$metric)
    },
    plotTrain = function(){
      if ( any( is.null(private$cluster.models$models),
                !is.list(private$cluster.models$models),
                length(private$cluster.models$models)==0) ){
        stop("[",class(self)[1],"][ERROR] Models were not trained. Aborting...")
      }
      
      plotPath <- file.path(private$path,private$metric,"train_plots")
      if (!dir.exists(plotPath)) dir.create(plotPath,recursive = TRUE)
      
      #exec.models <- length(private$cluster.models$models)
      
      summary <- do.call(rbind,lapply(private$cluster.models$models, function(x) { 
        df <- data.frame(x$model.name,x$model.performance, 
                         stringsAsFactors = FALSE) 
      } ))
      summary<- cbind(data.frame(sprintf("[Cluster %s]",seq(1,nrow(summ)))),
                      summary)
      names(summary) <- c("clusters","models","measure")
      
      min.pos <- which.min(summ$measure)
      min <- data.frame( x= summ[min.pos, ]$clusters, y= min(summ[,3]) )
      max.pos <- which.max(summ$measure)
      max <- data.frame( x=summ[max.pos, ]$clusters, y= max(summ[,3]) )
      avg <- round(mean(summ$measure), digits = 2)
      measure <- private$metric
      
      ggplot(summary, aes(clusters,measure, group=1)) + geom_line() + geom_point() +
        geom_point(aes(x,y), min, fill="transparent", color="red", 
                   shape=21, size=3,stroke=1) +
        geom_text(aes(x,y,label=sprintf("%.3f",y)), min, size=3, 
                  hjust=-.4, vjust=1.5, color='red' ) +
        geom_text(aes(x,y,label=sprintf("%.3f",y)), max, size=3, 
                  hjust=-.4, vjust=1.5, color='blue' ) +
        geom_point(aes(x,y), max, fill="transparent", color="blue", 
                   shape=21, size=3,stroke=1) + 
        geom_hline(aes(yintercept=avg), linetype="twodash", 
                   color= "#696969", show.legend = TRUE) + 
        geom_text(aes(0,avg,label="Average"), hjust=-.2, vjust=-1) +
        geom_text(aes(label=models), hjust=-.2, vjust=0) +
        labs(x = "Model name", y = paste0(measure," value"),
             title = paste0("Performance benchmarking plot during training")) +
        theme (axis.text.x = element_text(angle = 75, hjust = 1),
               plot.title = element_text(hjust = 0.5))
      
      save.path <- file.path(plotPath,paste0("Performance_Train_Plot.pdf") )
      message("[D2MCS][INFO] Plot saved has been succesfully saved at : '",
              save.path,"'")
      ggsave(filename = save.path,device="pdf")
    },
    # getPredictions = function(type=NULL, target=NULL){
    #   if( is.null(private$classify.output) || 
    #       is.null(private$classify.output$getPredictions(type,target) ) )
    #     stop("[D2MCS][ERROR] Prediction not computed. Execute 'classify' function first\n")
    #   private$classify.output$getPredictions(type,target)
    # },
    # comparePerformance = function(test.set, opt.alg, measures){
    #   if( !inherits(test.set,"Subset") )
    #     stop("[D2MCS][ERROR] Test set must be a Subset class\n")
    #   
    #   if ( is.null(test.set$getClass()) )
    #     stop("[D2MCS][ERROR] Class values should be provided to compute performance\n")
    #   
    #   if( !inherits(opt.alg,"Optimizers") )
    #     stop("[D2MCS][ERROR] Optimization algorithms are incorrect. Must be a Optimizers class\n")
    #   
    #   if ( !is.list(measures) || !all(sapply(measures, inherits,"MeasureFunction")) )
    #     stop("[D2MCS][ERROR] Measures should be a list comprised of MeasureFunction objects\n")
    #   
    #   private$classify.output
    #   
    # },
    getAvailableModels = function(){ private$availableModels[,c(1,2)] }#,
    # savePredictions = function(dir.path, target){
    #   if(missing(filename) || is.null(filename) )
    #     stop("[D2MCS][ERROR] Store filename must be selected. Aborting...\n")
    #   path <- file.path(getwd(),"results",filename)
    #   
    #   if( is.null(private$classify.output) )
    #     stop("[D2MCS][ERROR] Classification is not executed. Execute 'classify' function first. Aborting... \n")
    #   
    #   write.csv(private$classify.output$preds, file=path, row.names = FALSE)
    #   cat("[D2MCS][INFO] Classification results succesfully saved at: ",path,"\n")
    # },
    # removeAll = function(){
    #   if(!is.null(private$path) && dir.exists(private$path)){
    #     if(!unlink(private$path,recursive = TRUE,force = TRUE))
    #       message("[D2MCS][INFO] Path '",private$path,"' succesfully removed")
    #     else message("[D2MCS][ERROR] Path '",private$path,"' could not be removed")
    #   }
    # }
  ),
  private = list(
    loadAvailableModels = function(){
      model.list <- caret::getModelInfo()
      
      if(is.null(model.list)){
        stop("[",class(self)[1],"][ERROR] Models not found in caret library.",
             " Aborting...")
      }
      model.names <- names(model.list)
      
      models <- do.call(rbind, apply( t(model.names),2, function(name, modelList) { 
        if( !name %in% c("null") && 
            modelList[[name]]$type %in% "Classification" ){ 
          data.frame( name=name,description=modelList[[name]]$label, 
                      family=base::trimws(modelList[[name]]$tags[1]), 
                      library=I(list(modelList[[name]]$library)),
                      prob=(!is.null(modelList[[name]]$prob) && 
                            length(grep("response",deparse(modelList[[name]]$prob))) == 0 ), 
                      stringsAsFactors=FALSE )
        } 
      }, modelList=model.list ) )
      
      message("[",class(self)[1],"][INFO] ",nrow(models),
              " classifiers has been succesfully loaded" )
      models <- with(models,models[order(models$family,models$name),])
      models
    },
    getName = function(){ class(self)[1] },
    cluster.conf = NULL,
    cluster.obj = NULL,
    availableModels = NULL,
    trainFunction = NULL,
    path = NULL,
    classify.output = NULL,
    executed.models = NULL,
    metric = NULL,
    best.model = NULL,
    cluster.models = NULL,
    values.real = NULL
  )
)