library("R6")
library("tictoc")
Model <- R6Class(
  classname = "Model",
  portable = TRUE,                   
  public = list(
    initialize = function(dir = NULL, method = NULL, family = NULL, 
                          description = NULL, pkgName = NULL, trFunction = NULL, fit = NULL,
                          metric = NULL)
    {
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
        stop("[Ranger][ERROR] Metric not available for '",method,"' classifier. Aborting execution\n")
      if( !class(fit) %in% c("formula","recipe" ) )
        stop("[Model][ERROR] Fit should be a 'formula' or 'recipe' type. Aborting execution\n")
      if( is.null(method) )
        stop("[Model][ERROR] Method parameter must be defined as 'formula' or 'recipe'\n")
      if(!dir.exists(dir)){
        cat("[Model][INFO] Save directory not exist. Creating...\n")
        dir.create(dir,showWarnings = FALSE,recursive = TRUE)
      }
      private$dir <- dir
      if(!is.null(pkgName)){
        private$loadPackages(pkgName)
        private$packages <- pkgName
      }
      
      private$method <- method
      private$family <- family
      private$trainFunction <- trFunction
      private$description <- description
      private$fit <- fit
      private$metric <- metric
      private$isTrained <- FALSE
    },
    getDir = function(){
      private$dir
    },
    getLocation = function(){
      paste0(private$dir,"/",private$method)
    },
    getName = function(){
      private$method
    },
    getFamily = function (){
      private$family
    },
    getDescription = function (){
      private$description
    },
    getValidMetrics = function(){
      private$trainFunction$getMeasures()
    },
    train = function(dataset=NULL){
      stop("[Model][ERROR] Class is abstract. Function 'train' must be implemented in inherited class\n")
    },
    getModel = function(){
      stop("[Model][ERROR] Class is abstract. Function 'getModel' must be implemented in inherited class\n")
    },
    saveModel = function(){
      stop("[Model][ERROR] Class is abstract. Function 'saveModel' must be implemented in inherited class\n")
    },
    loadModel = function(){
      stop("[Model][ERROR] Class is abstract. Function 'saveModel' must be implemented in inherited class\n")
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
    getPackages = function(){
      private$packages
    },
    getTrainFunction = function(){
      private$trainFunction
    },
    getFormula = function (){
      private$fit
    },
    getMetric = function(){
      private$metric
    },
    isTrained = function(){
      private$isTrained
    },
    setTrained = function(trained){
      private$isTrained=trained
    },
    dir = NULL,
    packages = NULL,
    method = NULL,
    family = NULL,
    description = NULL,
    fit = NULL,
    metric = NULL,
    trainFunction = NULL,
    trainModel = NULL,
    isTrained = FALSE
  )
)