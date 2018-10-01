library("R6")
ModelFormula <- R6Class(
  classname = "ModelFormula",
  portable = TRUE,                   
  public = list(
    initialize = function(dataset){
      if(is.null(dataset) || class(dataset)[1] !="Subset" ){
        stop("[ModelUtility][ERROR] Dataset must be a Subset object\n")
      }
      private$className <- dataset$getClassName()
      private$features <- dataset$getFeatures()
    },
    createFormula = function(simplify = TRUE){
      if(isTRUE(simplify))
        as.formula( paste0(sprintf("`%s`", private$className )," ~ .") )
      else as.formula(paste0( paste0(sprintf("`%s`", private$className )," ~ "), 
                              paste0(sprintf("`%s`",private$features ), collapse = "+" )) )
    }
  ),
  private = list(
    className = NULL,
    features = NULL
  )
)