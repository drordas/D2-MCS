GenericModelFit <- R6::R6Class(
  classname = "GenericModelFit",
  portable = TRUE,                   
  public = list(
    initialize = function(instances, class.name){
      if( any(!"data.frame" %in% class(instances), nrow(instances) == 0) ){
        stop("[",class(self)[1],"][FATAL] Instances must be a non-empty data.frame")
      }

      if ( !(class.name %in% names(instances)) ){
        stop("[",class(self)[1],"][FATAL] Class name not included in instances data.frame")
      }
      
      if(!"recipes" %in% rownames(installed.packages()))
        install.packages( "recipes", repos="https://ftp.cixug.es/CRAN/" )
      library("recipes")
      
      private$class.name <- class.name
      private$feature.names <- names(instances)
      private$instances <- instances
    },
    createFormula = function(simplify = TRUE){
      stop("[",class(self)[1],"][FATAL] createFormula method is abstract")
    },
    createRecipe = function(){
      stop("[",class(self)[1],"][FATAL] createRecipe method is abstract")
    }
  ),
  private = list(
    class.name = NULL,
    feature.names = NULL,
    instances = NULL
  )
)