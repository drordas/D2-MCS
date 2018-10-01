library("R6")
source("ModelFit.R")
DefaultModelFit <- R6Class(
  classname = "DefaultModelFit",
  inherit = ModelFit,
  portable = TRUE,                   
  public = list(
    initialize = function(dataset){ 
      super$initialize(dataset)
    },
    createFormula = function(simplify = TRUE){
      if(isTRUE(simplify))
        as.formula( paste0(sprintf("`%s`", private$className )," ~ .") )
      else as.formula(paste0( paste0(sprintf("`%s`", private$className )," ~ "), 
                              paste0(sprintf("`%s`",private$features ), collapse = "+" )) )
    },
    createRecipe = function(){
      recipe <- recipe( self$createFormula(simplify=TRUE), data=private$instances )
      recipe %>%
        step_zv(all_predictors()) %>% step_nzv(all_predictors()) %>% step_corr(all_predictors()) %>% 
        step_center(all_predictors()) %>% step_scale(all_predictors())
    }
  )
)