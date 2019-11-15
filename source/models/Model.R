Model <- R6Class(
  classname = "Model",
  portable = TRUE,                   
  public = list(
    initialize = function(dir.path, model){
      private$dir.path <- gsub("\\/$","",dir.path)
      if(!dir.exists(private$dir.path)){
        message("[",class(self)[1],"][INFO] Save directory not exist. Creating...")
        dir.create(private$dir.path, showWarnings= FALSE, recursive = TRUE)
        if(!dir.exists(private$dir.path))
          stop("[",class(self)[1],"][ERROR] Path '",private$dir.path,
               "' cannot be created. Aborting execution...")
      }
      
      if( missing(model) ){
        stop("[",class(self)[1],"][ERROR] Model is missing or incorrect.",
             "Aborting...")
      }
      
      private$RDS.path <- file.path(private$dir.path, paste0(model$name,".rds"))
      private$model.info <- model
      private$model.train <- list( model.name=model$name, exec.time=NULL,
                                   model.performance= NULL, model.data=NULL, 
                                   model.libs = model$library)
      private$metric <- NULL
      
      if( file.exists( private$RDS.path ) ){
        message("[",class(self)[1],"][INFO] Model '",private$model.info$name,
                "' already exists. Loading...")
        private$model.train <- readRDS(private$RDS.path)
        
        if( is.null(private$model.train) ||
            any(sapply(private$model.train,is.null)) ){
          message("[",class(self)[1],"][ERROR] Unable to load trained model.")
        }else{
          message("[",class(self)[1],"][INFO] '",
                  paste(private$model.info[1:3], collapse="', "),
                  "' has been succesfully loaded!")
        }
      }
    },
    isTrained = function(){
      ifelse(is.null(private$model.train$model.data),
             FALSE,TRUE)
    },
    getDir = function(){ private$dir.path },
    #getRDS = function(){ private$RDS.path },
    getName = function(){ private$model.info$name },
    getFamily = function(){ private$model.info$family },
    getDescription = function(){ private$model.info$description },
    #getValidMetrics = function(){ private$train.function$getMeasures() },
    #getMetric = function(){ private$metric  },
    train = function(train.set, fitting, trFunction, metric){
      if( is.null(private$model.train) || 
          any(sapply(private$model.train,is.null)) ) {
        message("[",class(self)[1],"][INFO] Model '",private$model.info$name,
                "' has not been trained. Starting training process...")
        
        if( !inherits(train.set,"data.frame") ){
          stop("[",class(self)[1],"][ERROR][",self$getName(),"] ",
               "Cannot perform trainning stage.",
               "Train set must be a data.frame type")
        }
        
        if( nrow(train.set) == 0 ){
          stop("[",class(self)[1],"][ERROR][",self$getName(),"] ", 
               "Cannot perform trainning stage. Train set is empty")
        }
        
        if( !inherits(trFunction,"TrainFunction") ){
          stop("[",class(self)[1],"][ERROR][",self$getName(),"] ",
               "TrainFunction invalid. Should inherit from TrainFunction class")
        }
        
        valid.metrics <- trFunction$getMeasures()
        if(any(is.null(metric),!(metric %in% valid.metrics))){
          stop("[",class(self)[1],"][ERROR][",self$getName(),"] ", 
               "Metric is not defined or unavailable ",
               "Must be a [",paste(valid.metrics,collapse=", "),"] type")
        }
        
        if( !is.null(private$model.train$model.libs) && 
            !is.na(private$model.train$model.libs) && 
            !private$model.train$model.libs %in% "NA" ){
          message("[",class(self)[1],"][INFO][",self$getName(),"] ",
                  "Loading required packages...")
          private$loadPackages(private$model.train$model.libs)
        }
        
        message("[",class(self)[1],"][INFO][",self$getName(),"] ",
                "Performing training and hyperparameter optimization stage...")
        
        private$metric <- metric
        tictoc::tic(quiet = TRUE)
        
        private$model.train$model.data <- caret::train( x=fitting, data=train.set, 
                                                        method= private$model.info$name,
                                                        trControl= trFunction$getTrFunction(), 
                                                        metric= metric )
        time <- tictoc::toc(quiet=TRUE)
        private$model.train$model.performance <- self$getPerformance()
        if (!is.null(private$model.train$model.data) ){
          message("[",class(self)[1],"][INFO][",self$getName(),"] ", 
                  "Finished in [",(time$toc - time$tic)," segs]")
          private$model.train$exec.time <- (time$toc - time$tic)
        }else{ 
          message("[",class(self)[1],"][ERROR][",self$getName(),"] ",  
                  "Unable to train model. Skipping...")
        }

        if( !is.null(private$model.train$model.libs) && 
            !is.na(private$model.train$model.libs) && 
            !private$model.train$model.libs %in% "NA" ){
          message("[",class(self)[1],"][INFO][",self$getName(),"] ",
                  "Detaching required packages...")
          private$unloadPackages(private$model.train$model.libs)
        }
      }else{
        message("[",class(self)[1],"][INFO][",self$getName(),"]",
                "Model has already been trained")
      }
    },
    getTrainedModel = function(){
      if ( !self$isTrained() ){
        message("[",class(self)[1],"][WARNING] Model '",private$model.info$name,
            "' is not trained")
        NULL  
      }else { private$model.train }
    },
    getExecutionTime = function(){
      if ( !self$isTrained() )
        message("[",class(self)[1],"][WARNING] Model '",private$model.info$name,
                "' is not trained")
      private$model.train$exec.time
    },
    getPerformance = function(metric=private$metric){
      if( !is.null(private$model.train$model.data) && 
          !is.null(private$metric) )
      {
        model.result <- private$model.train$model.data
        if( metric %in% model.result$perfNames ){
          model.result <- private$model.train$model.data$results
          model.result[best(model.result, metric=metric, maximize= TRUE), ][[metric]]
        }else {
          stop("[",class(self)[1],"][ERROR] Metric is not defined or unavailable ",
               "Must be a [",paste(self$getValidMetrics(),collapse=", "),"] type.")
          NULL
        } 
      }else{
        if (is.null(private$model.train$model.data))
          message("[",class(self)[1],"][WARNING] Model '",
                  private$model.info$name,"' is not trained")
        if (is.null(private$metric)) 
          message("[",class(self)[1],"][WARNING] Metric is NULL")
        NULL
      }
    },
    getConfiguration = function(){
      if(!is.null(private$model.train$model.data)){
        private$model.train$model.data$bestTune
      }else{
        message("[",class(self)[1],"][WARNING] Model '",private$model.info$name,
                "' is not trained")
        NULL
      }
    },
    save = function(replace=TRUE){
      if ( is.null(private$model.train$model.data) )
        message("[",class(self)[1],"][ERROR] Cannot save untrained model. Aborting...")
      else{
        if( file.exists( private$RDS.path ) ){
          if (replace){
            message("[",class(self)[1],"][INFO] Model '",private$method,
                    "' already exists. Replacing previous model")
            saveRDS (object = private$model.train, file=private$RDS.path )
            message("[",class(self)[1],"][INFO] Model '",private$model.info$name,
                    "' succesfully saved at:", private$RDS.path)
          }else{
            message("[",class(self)[1],"][INFO] Model '",private$model.info$name,
                    "' already exists. Model not saved") 
          }
        }else{
          saveRDS(object = private$model.train, file=private$RDS.path )
          message("[",class(self)[1],"][INFO] Model '",private$model.info$name,
                  "' succesfully saved at: ",private$RDS.path)
        }
      }
    },
    remove = function(){
      if(file.exists(private$RDS.path) ){ 
        file.remove(private$RDS.path) 
      }else{ 
        message("[",class(self)[1],"][WARNING] Cannot remove unsaved model") 
      }
    }
  ),
  private = list(
    loadPackages = function(pkgName){
      new.packages <- pkgName[!(pkgName %in% installed.packages()[,"Package"])]
      if(length(new.packages)){ 
        message("[Model][INFO][",self$getName(),"]",length(new.packages),
            "packages needed to execute aplication\n Installing packages ...")
        suppressMessages(install.packages( new.packages,
                                           repos="https://ftp.cixug.es/CRAN/", 
                                           dependencies = TRUE, 
                                           quiet= TRUE, verbose= FALSE))
      }
      lapply(pkgName, function(pkg){
        if (! pkg %in% loaded_packages() ){
          library(pkg,character.only = TRUE,warn.conflicts = FALSE,quietly = TRUE)
          #packrat::snapshot()
        }
      })
    },
    unloadPackages = function(pkgName){
      lapply(pkgName, function(pkg){
        if( (pkg %in% loaded_packages()$package) && (!pkg %in% c("dplyr","plyr")) ){
          pck.name <- paste0("package:",pkg)
          try({
            detach(pck.name, character.only = TRUE)
            pd.file <- attr(packageDescription(pkg), "file")
            library.dynam.unload(pkg, libpath = sub("/Meta.*", '', pd.file))
          }, silent = TRUE)
        }
      })
    },
    dir.path = NULL,
    model.data = NULL,
    model.train = NULL,
    model.info = NULL,
    metric = NULL,
    RDS.path = NULL
  )
)