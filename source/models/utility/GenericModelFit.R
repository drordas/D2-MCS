library("R6")
GenericModelFit <- R6Class(
  classname = "GenericModelFit",
  portable = TRUE,                   
  public = list(
    initialize = function(instances, class.name){
      if( any(!"data.frame" %in% class(instances), nrow(instances) == 0) ){
        stop("[",class(self)[1],"][ERROR] Instances must be a non-empty data.frame")
      }

      if ( !(class.name %in% names(instances)) ){
        stop("[",class(self)[1],"][ERROR] Class name not included in instances data.frame")
      }
      
      if(!"recipes" %in% rownames(installed.packages()))
        install.packages( "recipes", repos="https://ftp.cixug.es/CRAN/" )
      library("recipes")
      
      private$class.name <- class.name
      private$feature.names <- names(instances)
      private$instances <- instances
    },
    createFormula = function(simplify = TRUE){
      stop("[ModelFit][ERROR] createFormula method is abstract\n")
    },
    createRecipe = function(){
      stop("[ModelFit][ERROR] createRecipe method is abstract\n")
    }
  ),
  private = list(
    class.name = NULL,
    feature.names = NULL,
    instances = NULL
  )
)