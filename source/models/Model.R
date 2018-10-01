library("R6")
library("tictoc")
Model <- R6Class(
  classname = "Model",
  portable = TRUE,                   
  public = list(
    initialize = function(dir = NULL, method = NULL, family = NULL, 
                          description = NULL, pkgName = NULL, trFunction = NULL,
                          metric = NULL){
      
      if ( is.null(method) )
        stop("[Model][ERROR] Name should be defined\n")
      if ( is.null(family) )
        stop("[Model][ERROR] Family should be defined. Aborting execution\n")
      if( is.null(dir))
        stop("[Model][ERROR] Save directory should be defined. Aborting execution\n")
      if( is.null(trFunction) || !("TrainFunction" %in% class(trFunction)) )
        stop("[Model][ERROR] TrainFunction should be 'TrainFunction' class. Aborting execution\n")
      if( is.null(metric))
        stop("[Model][ERROR] Metric should be defined. Aborting execution\n")
      if( !metric %in% trFunction$getMeasures())
        stop("[Model][ERROR] Metric not available for '",method,"' classifier. Aborting execution\n")
      if( is.null(method) )
        stop("[Model][ERROR] Method parameter must be defined as 'formula' or 'recipe'\n")
      if(!dir.exists(dir)){
        cat("[Model][INFO] Save directory not exist. Creating...\n")
        dir.create(dir,showWarnings = FALSE,recursive = TRUE)
      }
      if(!is.null(pkgName)){
        private$loadPackages(pkgName)
        private$packages <- pkgName
      }
      
      private$dir <- dir
      private$metric <- metric
      private$method <- method
      private$description <- description
      private$family <- family
      private$trainFunction <- trFunction
      private$RDSpath <- paste0(private$dir,"/",private$method,".rds")
      private$CSVpath <- paste0(private$dir,"/",private$method,".csv")

      if( !dir.exists(private$dir) ){
        cat("[",private$method,"][INFO] '",private$dir,"' does not exists. Creating....\n", sep="")
        dir.create(private$dir,recursive = TRUE)
        if(!dir.exists(private$dir))
          stop("[",private$method,"][ERROR] '",private$dir,"' Cannot be created. Aborting execution.\n")
      }

      if( file.exists( private$RDSpath ) ){
        cat("[",private$method,"][INFO] '",private$family,"' '",private$description,"' already exists. Loading!\n", sep="")
        private$modelInfo <- readRDS( private$RDSpath )
        private$isTrained <- TRUE
      }else{
        private$modelInfo <- ModelData$new( dir= dir, method = method, family = family, 
                                            description = description, metric = metric )
        private$isTrained <- FALSE
        cat("[",private$method,"][INFO] '",family,"' '",description,"' has been succesfully created !\n", sep="")
      }
    },
    getDir = function(){
      private$dir
    },
    exists = function(){
      private$isTrained
    },
    getPath = function(){
      private$RDSpath
    },
    getName = function(){
      private$method
    },
    getFamily = function(){
      private$family
    },
    getDescription = function(){
      private$description
    },
    getValidMetrics = function(){
      private$trainFunction$getMeasures()
    },
    getMetric = function(){
      private$metric 
    },
    train = function(dataset=NULL, fitting = NULL){
      data <- dataset
      
      if( missing(dataset) || is.null(dataset) || ( !"Subset" %in% class(dataset) && !is.data.frame(dataset) )  )
        stop("[",private$method,"][ERROR] Dataset not defined, null, or invalid type (data.frame of Subset object)\n")

      if( !class(fitting) %in% c("recipe", "formula") )
        stop("[",private$method,"][ERROR] Model should be 'formula' or 'recipe' type. Aborting execution\n")
      
      if( class(dataset) %in% "Subset" )
        data <- dataset$getInstances()
      
      if( !private$isTrained ){
        cat("[",private$method,"][INFO] Performing model training and hyperparameter optimization stage...\n", sep="")
        tic(quiet = TRUE)
        trainedModel <- caret::train(fitting,data=data, method=private$method,
                                      trControl=private$trainFunction$getTrFunction(), metric=private$metric)
        time <- toc(quiet=TRUE)
        private$modelInfo$setTrainInfo(trainedModel, (time$toc - time$tic) )
        private$isTrained <- TRUE
        #private$performance <- private$modelInfo$getBestModelPerformance(private$metric)
        cat("[",private$method,"][INFO] Finished [",(time$toc - time$tic),"s]\n", sep="")
      }else cat("[",private$method,"][WARNING] Retraining not implemented yet. Avoid execution\n", sep="")
    },
    getTrainedModel = function(){
      if ( is.null( private$modelInfo$getTrainModel() ) )
        cat("[",private$method,"][WARNING] Model '",private$method,"' is not trained\n")
      private$modelInfo$getTrainModel()
    },
    getElapsedTime = function(){
      private$modelInfo$getExectime()
    },
    getPerformance = function(metric = private$metric){
      if( metric %in% trFunction$getMeasures() )
        private$modelInfo$getBestModelPerformance(private$metric)
      else {
        cat("[",private$method,"][WARNING] Metric not available\n")
        return(NULL)
      }
    },
    getModelConfiguration = function(){
      private$modelInfo$getBestConfiguration()
    },
    saveModel = function(replace=TRUE){
      if ( is.null( private$modelInfo$getTrainModel() ) )
        cat("[",private$method,"][ERROR] Cannot save untrained model. Aborting execution\n", sep="")
      else{
        if( file.exists( private$RDSpath ) ){
          if (replace){
            cat("[",private$method,"][INFO] Model '",private$method,"' already exists. Replacing previous model\n", sep="")
            saveRDS (object = private$modelInfo, file=private$RDSpath )
            cat("[",private$method,"][INFO] Model '",private$method,"' succesfully saved at:", private$RDSpath,"\n", sep="")
            #write.table(self$getModelConfiguration(),file = private$CSVpath, append = FALSE)
          }else cat("[",private$method,"][INFO] Model '",private$method,"' already exists. Model not saved\n", sep="")
        }else{
          saveRDS (object = private$modelInfo, file=private$RDSpath )
          cat("[",private$method,"][INFO] Model '",private$method,"' succesfully saved at: ", private$RDSpath,"\n", sep="")
          #write.table(self$getModelConfiguration(),file = private$CSVpath, append = FALSE)
        }
      }
    },
    removeModel = function(){
      if(file.exists( private$RDSpath ) )
        file.remove(private$RDSpath)
    }
  ),
  private = list(
    loadPackages = function(pkgName){
      new.packages <- pkgName[!(pkgName %in% installed.packages()[,"Package"])]
      if(length(new.packages)){ 
        cat("[Model][INFO]",length(new.packages),"packages needed to execute aplication\n Installing packages ...")
        suppressMessages(install.packages(new.packages,repos="https://ftp.cixug.es/CRAN/"))
      }
      lapply(pkgName,require,character.only=TRUE)
    },
    saveSummary = function (){
      path <- paste0(private$path,"/",private$metric,"_summary.txt")
      if (file.exists(path)){
        
      }
    },
    dir = NULL,
    packages = NULL,
    method = NULL,
    family = NULL,
    description = NULL,
    fit = NULL,
    metric = NULL,
    trainFunction = NULL,
    trainedModel = NULL,
    isTrained = FALSE,
    modelInfo = NULL,
    RDSpath = NULL,
    CSVpath = NULL
  )
)

ModelData <- R6Class(
  classname = "ModelData",
  portable = TRUE,                   
  public = list(
    initialize = function(dir,method,family,description,metric){
      private$dir <- dir
      private$method <- method
      private$family <- family
      private$description <- description
      private$metric <- metric
      private$execTime <- 0
    },
    getModelDir = function(){
      private$dir
    },
    getModelMethod = function(){
      private$method
    },
    getModelFamily = function(){
      private$family
    },
    getModelDescription = function(){
      private$description
    },
    getModelFitting = function(){
      private$fit
    },
    getTrainModel = function(){
      private$trainModel
    },
    setTrainInfo = function(trModel, execTime){
      private$trainModel <- trModel
      private$execTime <- execTime
    },
    getBestModel = function(){
      private$trainModel
    },
    getBestConfiguration = function(){
      private$trainModel$bestTune
    },
    getBestModelPerformance = function (metric){
      if(missing(metric)) 
        mtr <- private$metric
      else mtr <- metric
      
      private$trainModel$results[ best(private$trainModel$results, metric=mtr,  maximize = TRUE), ][[mtr]]
    },
    getModelType = function(){
      private$trainModel$modelType
    },
    getModelSummary = function(){
      private$trainModel$finalModel
    },
    getExectime = function(){
      private$execTime
    },
    setFitting = function(fit){
      private$fit <- fit
    }
  ),
  private = list(
    dir = NULL,
    method = NULL,
    family = NULL,
    description = NULL,
    fit = NULL,
    metric = NULL,
    execTime = 0,
    trainModel = NULL
  )
)