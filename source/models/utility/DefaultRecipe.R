library("R6")
DefaultRecipe <- R6Class(
  classname = "DefaultRecipe",
  inherit = ModelRecipes,
  portable = TRUE,                   
  public = list(
    initialize = function(dataset){ super$initialize(dataset) },
    createRecipe = function(){
      super$getRecipe() %>%
        step_zv(all_predictors()) %>% step_nzv(all_predictors()) %>% step_corr(all_predictors()) %>% 
        step_center(all_predictors()) %>% step_scale(all_predictors())
    }
  )
)