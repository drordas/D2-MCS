library("R6")
source("ModelFormula.R")
ModelRecipes <- R6Class(
  classname = "ModelRecipes",
  portable = TRUE,                   
  public = list(
    initialize = function (dataset){
      if(!"recipes" %in% rownames(installed.packages()))
        install.packages("recipes",repos="https://ftp.cixug.es/CRAN/")
      library("recipes")
      
      if( class(dataset)[1] != "Subset" )
        stop("[ModelRecipes][ERROR] Parameter should be a Subset object\n")
      
      private$model.formula <- ModelFormula$new(dataset)$createFormula(simplify=TRUE)
      private$model.recipe <- recipe(private$model.formula, data=dataset$getInstances())
    },
    createRecipe = function(){
      stop("[ModelRecipes][ERROR] Abstract method. Should be implemented in inherited class\n")
    },
    getRecipe = function(){
      private$model.recipe
    }
  ),
  private = list(
    model.formula = NULL,
    model.recipe = NULL
  )
)