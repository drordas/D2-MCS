library("R6")

D2MCS <- R6Class(
  classname = "D2MCS",
  portable = TRUE,                   
  public = list(
    initialize = function(path, num.cores = NULL, socket.type="PSOCK", outfile,
                          serialize=FALSE, trainFunction = NULL ){ 
      
      if(missing(path)) { stop("[D2MCS][ERROR] Path not defined. Path should be given to store the models\n") }
      
      if (!file.exists(path)){
        dir.create(path, recursive = TRUE)
        message("[D2MCS][INFO] Path '",path, "' does not exist. Creating ...\n")
      }
      
      if(missing(outfile)) { stop("[D2MCS][ERROR] Cluster log file not defined\n") }
      
      if (!file.exists(outfile)){
        dir.create(outfile, recursive = TRUE)
        message("[D2MCS][INFO] Cluster log  file '",path, "' does not exist. Creating ...\n")
      }
      
      if(is.null(num.cores) || num.cores >=  detectCores() || num.cores > 10  ){
        ifelse( (detectCores() > 10), 
                (warning(yellow("[D2MCS][WARNING] Invalid number of cores (1>= num.cores <= 10)\n"))),
                (warning(yellow("[D2MCS][WARNING] Invalid number of cores (1>= num.cores < ",detectCores(),")"))) )
        cores <- min(max(1,detectCores()-2),10)
        message("[D2MCS][INFO] Using default number of cores (",cores,"/",detectCores(),") \n")
      }else cores <- nCores
      
      if( !socket.type %in% c("PSOCK","FORK") ){
        warning(yellow("[D2MCS][WARNING] Invalid socket type. Assuming 'PSOCK' cluster\n"))
        socket <- "PSOCK"
      }else socket <- socket.type
      
      if(!is.logical(serialize)){
        warning(yellow("[D2MCS][WARNING] Invalid serialization option. Assuming not serialization\n"))
        xdr <- FALSE
      }else xdr <- serialize
      
      if (missing(trainFunction) || is.null(trainFunction) || !"TrainFunction" %in% class(trainFunction) )
        stop("[D2MCS][ERROR] TrainFunction not defined or incorrect (should inherit from TrainFunction abstract class)\n")
      else private$trainFunction <- trainFunction
      
      private$availableModels <- private$loadAvailableModels()
      private$path <- file.path(getwd(),path)
      dir.create( private$path, recursive = TRUE, showWarnings = FALSE )
      
      private$cluster.conf <- list( cores=cores, socket=socket, outfile=outfile, xdr=xdr )
      private$cluster.obj <- NULL
      private$executedModels <- NULL
      private$classify.output <- NULL
      private$performance <- NULL
      private$models.weights <- c()
      private$real.values <- NULL
    },
    train = function( train.set = NULL, num.clusters = NULL, ex.classifiers = c(), 
                      ig.classifiers=c(), metric = NULL, saveAllModels = FALSE ){
      if(missing(train.set) || is.null(train.set) || !"ClusterDistribution" %in% class(train.set) )
        stop(red("[D2MCS][ERROR] Train set not defined of incorrect (must be of type 'Cluster')\n"))
    
      if (missing(num.clusters)) message("[D2MCS][INFO] Number of clusters not set. Using all clusters... \n")
      if (is.null(num.clusters) || !is.numeric(num.clusters) || !is.vector(num.clusters) ){
        warning(yellow("[D2MCS][WARNING] Number of clusters not set (must be numeric or vector). Using all clusters\n"))
        num.clusters <- c(1:train.set$getNumClusters())
      }else{
        if (is.numeric(num.clusters) && num.clusters > train.set$getNumClusters() ){
          warning(yellow("[D2MCS][WARNING] num.clusters is higher than number of existing clusters. Using all clusters\n"))
          num.clusters <- c(1:train.set$getNumClusters())
        }else num.clusters <- c(1:num.clusters)
      }

      if(missing(ex.classifiers) || is.null(ex.classifiers) || length(ex.classifiers) == 0){
        usedModels <- private$availableModels$name
      }else usedModels <- ex.classifiers
      
      if(!missing(ig.classifiers) && !is.null(ig.classifiers) && length(ig.classifiers) > 0){
        message("[D2MCS][INFO] Ignoring '",length(ig.classifiers),"' M.L models\n")
        usedModels <- setdiff(usedModels,ig.classifiers)
        message("[D2MCS][INFO] Still '",length(usedModels),"' M.L models available\n")
      }
      
      message("[D2MCS][INFO] Making parallel Socket Cluster with",private$cluster.conf$cores,"cores\n")
      private$cluster.obj <- makeCluster( private$cluster.conf$cores, type= private$cluster.conf$socket, 
                                          outfile= private$cluster.conf$outfile, useXDR=private$cluster.conf$xdr )
      
      private$bestModels <- ModelsList$new( size = length(num.clusters) )
      private$executedModels <- ExecutedModelsList$new( size = length(num.clusters) )
      private$metric <- metric
      used.models <- private$availableModels[ (private$availableModels$name %in% usedModels), ]
        
      for (i in num.clusters ){
        message("[D2MCS][INFO] ---------------------------------------\n")
        message("[D2MCS][INFO] Training models for cluster '",i,"' of '", max(num.clusters),"'\n")
        message("[D2MCS][INFO] ---------------------------------------\n")
        model.subset <- train.set$getAt(i)
        model.fitClass <- DefaultModelFit$new(model.subset)

        model.savePath <- file.path( paste0(private$path,private$metric),
                                     paste0("C[",i,"-",train.set$getNumClusters(),"]") )
        
        private$executedModels$loadFrom(file.path(model.savePath,".executed"),i)
        private$bestClusterModel <- NULL

        if(private$executedModels$size(i) > 0 ){
          bestModel <- private$executedModels$getBestModel(i)
          
          private$bestClusterModel <- ModelEntry$new( name= bestModel$method, object= NULL,
                                                      performance= bestModel$performance,
                                                      path= file.path(model.savePath,paste0(bestModel$method,".rds")) )
        }else private$bestClusterModel <- ModelEntry$new( name= "NULL", performance= 0.0 )
        
        if(abs(mean(cor(model.subset$getInstances(ignore.class = TRUE), as.numeric(model.subset$getClass())), na.rm=TRUE) ) < 0.3 ){ ##CORRELATION
          warning(yellow("[D2MCS][WARNING] High-Correlated Data (< 0.3). Ignoring Linear-based and Discriminant-based models\n"))
          remaining.models <- subset(used.models,!grepl("Linea[l|r]|Discriminant",paste(used.models$description,used.models$family,sep=" ")) )
          message("[D2MCS][INFO] Removing ",nrow(used.models) - nrow(remaining.models)," incompatible M.L. models\n")
        }else remaining.models <- used.models
        
        num.remaining <- nrow(remaining.models)
        cluster.models <- list()
        remaining.models <- remaining.models[ !(remaining.models$name %in% private$executedModels$getAt(i)$getNames()), ]
        
        if(nrow(remaining.models) == 0){
          private$bestModels$insertAt(i,private$bestClusterModel)
          private$models.weights <- c( private$models.weights,private$bestClusterModel$getPerformance() )
          if(i < max(num.clusters))
            message( "[D2MCS][INFO] Remaining ",num.remaining,
                     " M.L. models has been already executed for cluster ",i,"/",max(num.clusters),". Executing next cluster...\n")
          else {
            message( "[D2MCS][INFO] Remaining ",num.remaining,
                     " M.L. models has been already executed for cluster ",i,"/",max(num.clusters),".\n")
            message("[D2MCS][INFO] Finish !")
            if(!is.null(private$cluster.obj)){ stopCluster(private$cluster.obj); private$cluster.obj <- NULL }
          }
          next
          
        }else{ 
          message( "[D2MCS][INFO]",(num.remaining-nrow(remaining.models)),"/",num.remaining,
                   " M.L. models has been already executed for cluster ",i,"/",max(num.clusters),"\n") 
          message( "[D2MCS][INFO] Executing remaining",nrow(remaining.models),"M.L. model(s)\n" )
        }
        
        apply(remaining.models, 1, function(model, executedModel){
          ifelse( isTRUE(model$prob), private$trainFunction$create( UseProbability$new(), search.method= "random", class.probs = TRUE ),
                                      private$trainFunction$create( NoProbability$new(), search.method= "random", class.probs = FALSE ) )

          model.fit <- model.fitClass$createRecipe()
          model.type <- Model$new( dir = model.savePath, method = model$name,
                                   family = model$family, description = model$description,
                                   pkgName = as.vector( unlist(model$library) ), 
                                   trFunction = private$trainFunction, metric = metric )
          if( !executedModel$isTrained(i,model$name) ){
            model.type$train(dataset = model.subset$getInstances(), fitting = model.fit )
            if( isTRUE(saveAllModels) ) model.type$saveModel() 
            executedModel$insertModeltAt(i,model.type)
            if( private$bestClusterModel$getPerformance() < model.type$getPerformance() ){
              private$bestClusterModel$removeModel()
              private$bestClusterModel <- ModelEntry$new( name= model.type$getName(), object= model.type, 
                                           performance= model.type$getPerformance(), path= model.type$getPath() )
              private$bestClusterModel$save()
            }
            executedModel$saveAt(paste0(model.savePath,"/.executed"),i)
          }else message("[D2MCS][INFO] Model '",model$name,"' has been previously trained\n")
        }, executedModel = private$executedModels )
        
        private$bestModels$insertAt(i,private$bestClusterModel)
        private$models.weights <- as.numeric(c( private$models.weights,private$bestClusterModel$getPerformance() ))
        message("[D2MCS][INFO] Finish! \n")
      }
      if(!is.null(private$cluster.obj)) { stopCluster(private$cluster.obj); private$cluster.obj <- NULL }
    },
    classify = function( test.set = NULL, voting.scheme, positive.class){
      if( !"Subset" %in% class(test.set)  )
        stop("[D2MCS][ERROR] Test dataset missing or incorrect. Must be a Subset object\n")
      
      if (missing(voting.scheme) || !"VotingScheme" %in% class(voting.scheme) )
        stop("[D2MCS][ERROR] Voting Scheme missing or invalid\n")
      
      if ( is.null(positive.class) || !positive.class %in% levels(test.set$getClass()) )
        stop("[D2MCS][ERROR] Positive class missing or invalid (must be: ",paste0(levels(test.set$getClass()),collapse=", "),"). Aborting...\n")
      
      if( is.null(private$bestModels) || private$bestModels$size() < 1 )
        stop("[D2MCS][ERROR] Models were not trained. Please run 'Train' method first\n")

      message("[D2MCS][INFO] -------------------------------------------------------\n")
      message("[D2MCS][INFO] Starting classification operation\n")
      message("[D2MCS][INFO] -------------------------------------------------------\n")
      
      if( test.set$getClassIndex() > 0 && !is.null(test.set$getClass()) )
        private$real.values <- test.set$getClass()
      
      instances <- test.set$getInstances(ignore.class = TRUE)
      negative.class <- levels(test.set$getClass())[which(levels(test.set$getClass())!=positive.class)]
      prediction.cluster <- PredictionList$new( private$metric )

      for ( cluster in 1:private$bestModels$size() ){
        message("[D2MCS][INFO] Computing predictions for cluster '",cluster,"' of '",private$bestModels$size(),"'\n")
        message("[D2MCS][INFO] -------------------------------------------------------\n")
        pred <- Prediction$new( model = private$bestModels$getAt(cluster)$getObject(), 
                                class.values = levels( test.set$getClass() ), positive.class = positive.class )
        pred$execute(instances)
        prediction.cluster$addPrediction(pred)
      }
      
      message("[D2MCS][INFO] Computing final prediction values using '",voting.scheme$getName(),"'\n")

      private$classify.output <- ClassifyOutput$new( preds = voting.scheme$execute(prediction.cluster), models = private$bestModels,
                     metric = private$metric, weights = as.numeric(private$models.weights),
                     positive.class = positive.class, negative.class = negative.class )
       
      message("[D2MCS][INFO] -------------------------------------------------------\n")
      message("[D2MCS][INFO] Classification operation finished \n")
      message("[D2MCS][INFO] -------------------------------------------------------\n")
      
      private$classify.output
    },
    optimize = function(opt.set, voting.scheme, opt.algorithm, positive.class, metric=private$metric){
      if( !is.null(opt.set) && !inherits(opt.set,"Subset")  )
        stop("[D2MCS][ERROR] Test dataset missing or incorrect. Should inherit from 'Subset class'. Aborting...\n")

      if ( !inherits(voting.scheme,"VotingScheme") )
        stop("[D2MCS][ERROR] Voting Scheme missing or invalid. Aborting...\n")

      if ( !is.list(opt.algorithm) && !inherits(opt.algorithm,"WeightsOptimizer") )
        stop("[D2MCS][ERROR] Optimization algorithm is invalid. Must inherit from 'WeightedOptimizer' Aborting...\n")
      
      if(!is.list(opt.algorithm)) opt.algorithm <- list(opt.algorithm)
      
      if( is.list(opt.algorithm) && !all(sapply(opt.algorithm,inherits,"WeightsOptimizer")) )
        stop("[D2MCS][ERROR] Optimization algorithms is invalid. List elements must inherit from 'WeightedOptimizer' object. Aborting...\n")

      if ( is.null(positive.class) || !positive.class %in% levels(opt.set$getClass()) )
        stop("[D2MCS][ERROR] Positive class missing or invalid (must be: ",paste0(levels(opt.set$getClass()),collapse=", "),"). Aborting...\n")

      if( is.null(private$bestModels) || private$bestModels$size() < 1 )
        stop("[D2MCS][ERROR] Models were not trained. Please run 'Train' method first\n")
      
      if ( is.null(private$models.weights) || length(private$models.weights) < 1 ){
        cat("[D2MCS][ERROR] Weigths not defined. Initializing weights values\n")
        private$models.weights <- rep.int(1,times = private$bestModels$size() )
      }else private$models.weights <- as.numeric(private$models.weights)
      
      message("[D2MCS][INFO] -------------------------------------------------------\n")
      message("[D2MCS][INFO] D2MCS Optimization stage\n")
      message("[D2MCS][INFO] -------------------------------------------------------\n")
      
      if( opt.set$getClassIndex() > 0 && !is.null(opt.set$getClass()) )
        private$real.values <- opt.set$getClass()
      
      instances <- opt.set$getInstances(ignore.class = TRUE)
      negative.class <- levels(opt.set$getClass())[which(levels(opt.set$getClass())!=positive.class)]
      
      real.values <- factor( opt.set$getClass(), levels= c(negative.class, positive.class),  labels = c(0, 1) )
      
      prediction.cluster <- PredictionList$new( private$metric )
      
      for ( cluster in 1:private$bestModels$size() ){
        message("[D2MCS][INFO] Computing predictions for cluster '",cluster,"' of '", private$bestModels$size(),"'\n")
        message("[D2MCS][INFO] -------------------------------------------------------\n")
        pred <- Prediction$new( model = private$bestModels$getAt(cluster)$getObject(), 
                                class.values = levels(opt.set$getClass()), positive.class = positive.class )
        pred$execute(instances)
        prediction.cluster$addPrediction(pred)
      }
      
      compute.fitness <- function(weights,min.function) {
        pred.values <- voting.scheme$execute(prediction.cluster,weights)
        mf <- min.function(caret::confusionMatrix(pred.values,real.values, positive="1", mode="everything" ))
        return(mf)
      }
      
      message("[D2MCS][INFO] Starting optimization process: \n")
      freq <- table(real.values)
      
      opt.data <- sapply( opt.algorithm, function(alg, wg, freq) { 
        alg$execute(wg, compute.fitness)
        alg$getResult(n.positive = as.numeric(freq["1"]), n.negative = as.numeric(freq["0"]) )
      }, wg = private$models.weights, freq= freq)
      
      message("[D2MCS][INFO] Finish optimmization process!'\n")
      
      return (Optimizers$new( voting.scheme = voting.scheme, cluster.models = private$bestModels, metric = private$metric, 
                              optimizers = opt.data, positive.class = positive.class, negative.class = negative.class))
    },
    getBestPerformanceByCluster = function(){
      if ( is.null(private$models.weights) || length(private$models.weights) < 1 )
        stop("[D2MCS][ERROR] Train stage should be executed first\n")
      else as.numeric(private$models.weights)
    },
    getTrainedModels = function(){
      if( is.null(private$models.weights) || is.null(private$bestModels) || (private$bestModels$size() < 1) )
        stop("[D2MCS][ERROR] Parameters not assigned. Please execute Train method first\n")
      TrainOutput$new(models = private$bestModels, weights = private$models.weights, metric = private$metric)
    },
    plotTrain = function(){
      if ( is.null(private$executedModels ) || private$executedModels$size() < 1 )
        stop("[D2MCS][ERROR] Models were not trained. Please run 'executeTrain' method first\n")
      else{
        plotPath <- paste0(private$path,"plots")
        if (!dir.exists(plotPath)) dir.create(plotPath,recursive = TRUE)
        
        for ( i in 1:private$executedModels$size() ){
          models.cluster <- private$executedModels$getAt(i)
          summary <- data.frame( model=models.cluster$getNames(), measure= as.numeric(models.cluster$getPerformances()), 
                                 stringsAsFactors = FALSE )
          min <- data.frame( x=summary[which.min(summary[,2]), ][, 1],y= min(summary[,2]) )
          max <- data.frame( x=summary[which.max(summary[,2]), ][, 1],y= max(summary[,2]) )
          avg <- round(mean(summary$measure ), digits = 2)
          measure <- private$metric
          
          ggplot(summary, aes(model,measure, group=1)) + geom_line() + geom_point() +
            geom_point(aes(x,y), min, fill="transparent", color="red", shape=21, size=3,stroke=1) +
            geom_text(aes(x,y,label=sprintf("%.3f",y)), min, hjust=-0.45, color='red' ) +
            geom_point(aes(x,y), max, fill="transparent", color="blue", shape=21, size=3,stroke=1) +
            geom_text(aes(x,y,label=sprintf("%.3f",y)), max, hjust=-0.45, color='blue' ) +
            geom_hline(aes(yintercept=avg), linetype="twodash", color = "#696969", show.legend = TRUE) +
            labs(x = "Model name", y = paste0(measure," value"),
                 title = paste0("Performance benchmarking plot for cluster=",i)) +
            theme (axis.text.x = element_text(angle = 75, hjust = 1),
                   plot.title = element_text(hjust = 0.5))
          save.path <- file.path(plotPath,paste0("TRAIN_",toupper(measure),"_C[",i,"-",private$executedModels$size(),"].pdf") )
          message("[D2MCS][INFO] Plot saved from cluster ",i," of ",private$executedModels$size()," at: '",save.path,"'\n")
          ggsave(filename = save.path,device="pdf")
        }
      }
    },
    getPredictions = function(){
      if( is.null(private$classify.output) )
        stop("[D2MCS][ERROR] Prediction not computed. Execute 'classify' function first\n")
      #private$classify.output$preds
      private$classify.output$getPredictions()
    },
    comparePerformance = function(test.set, opt.alg, measures){
      if( !inherits(test.set,"Subset") )
        stop("[D2MCS][ERROR] Test set must be a Subset class\n")
      
      if ( is.null(test.set$getClass()) )
        stop("[D2MCS][ERROR] Class values should be provided to compute performance\n")
      
      if( !inherits(opt.alg,"Optimizers") )
        stop("[D2MCS][ERROR] Optimization algorithms are incorrect. Must be a Optimizers class\n")
      
      if ( !is.list(measures) || !all(sapply(measures, inherits,"MeasureFunction")) )
        stop("[D2MCS][ERROR] Measures should be a list comprised of MeasureFunction objects\n")
      
      private$classify.output
      
    },
    getAvailableModels = function(){ private$availableModels[,c(1,2)] },
    savePredictions = function(filename){
      if(missing(filename) || is.null(filename) )
        stop("[D2MCS][ERROR] Store filename must be selected. Aborting...\n")
      path <- file.path(getwd(),"results",filename)
      
      if( is.null(private$classify.output) )
        stop("[D2MCS][ERROR] Classification is not executed. Execute 'classify' function first. Aborting... \n")
      
      write.csv(private$classify.output$preds, file=path, row.names = FALSE)
      cat("[D2MCS][INFO] Classification results succesfully saved at: ",path,"\n")
    },
    removeAll = function(){
      if(!is.null(private$path) && dir.exists(private$path)){
        if(!unlink(private$path,recursive = TRUE,force = TRUE))
          message("[D2MCS][INFO] Path '",private$path,"' succesfully removed\n")
        else message("[D2MCS][ERROR] Path '",private$path,"' could not be removed\n")
      }
    }
  ),
  private = list(
    loadAvailableModels = function(){
      model.list <- getModelInfo()
      model.names <- names(model.list)
      
      models <- do.call(rbind, apply( t(model.names),2, function(name, modelList) { 
        if( !name %in% c("null") && 
            modelList[[name]]$type %in% "Classification" ){ 
          data.frame( name=name,description=modelList[[name]]$label, 
                      family=base::trimws(modelList[[name]]$tags[1]), library=I(list(modelList[[name]]$library)),
                      prob=(!is.null(modelList[[name]]$prob) && length(grep("response",deparse(modelList[[name]]$prob))) == 0 ), stringsAsFactors=FALSE )
        } 
      }, modelList=model.list ) )
      
      message("[D2MCS][INFO] ",nrow(models)," classifiers has been succesfully loaded\n")
      models <- with(models,models[order(models$family,models$name),])
      models
    },
    cluster.conf = NULL,
    cluster.obj = NULL,
    availableModels = NULL,
    trainFunction = NULL,
    path = NULL,
    bestClusterModel = NULL,
    classify.output = NULL,
    executedModels = NULL,
    metric = NULL,
    bestModels = NULL,
    performance = NULL,
    models.weights = NULL,
    real.values = NULL
  )
)