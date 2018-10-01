library("R6")
ModelFit <- R6Class(
  classname = "ModelFit",
  portable = TRUE,                   
  public = list(
    initialize = function(dataset){
      if(is.null(dataset) || !"Subset" %in% class(dataset) ){
        stop("[ModelFit][ERROR] Dataset must be a Subset object\n")
      }
      
      if(!"recipes" %in% rownames(installed.packages()))
        install.packages("recipes",repos="https://ftp.cixug.es/CRAN/")
      library("recipes")
      private$className <- dataset$getClassName()
      private$features <- dataset$getFeatures()
      private$instances <- dataset$getInstances()
    },
    createFormula = function(simplify = TRUE){
      stop("[ModelFit][ERROR] createFormula method is abstract\n")
    },
    createRecipe = function(){
      stop("[ModelFit][ERROR] createRecipe method is abstract\n")
    }
  ),
  private = list(
    className = NULL,
    features = NULL,
    instances = NULL
  )
)