DefaultModelFit <- R6::R6Class(
  classname = "DefaultModelFit",
  inherit = GenericModelFit,
  portable = TRUE,
  public = list(
    initialize = function(instances, class.name){
      super$initialize(instances, class.name)
    },
    createFormula = function(simplify = FALSE){
      if(isTRUE(simplify))
        as.formula( paste0(sprintf("`%s`", private$class.name )," ~ .") )
      else as.formula(paste0( paste0(sprintf("`%s`", private$class.name )," ~ "),
                              paste0(sprintf("`%s`",private$feature.names ), collapse = "+" )) )
    },
    createRecipe = function(){
      recipe <- recipe( self$createFormula(simplify=TRUE), data=private$instances )
      recipe %>% step_zv( all_predictors()) %>%  step_nzv(all_predictors()) %>%
        step_corr(all_predictors()) %>% step_center(all_predictors()) %>%
        step_scale(all_predictors() )
    }
  )
)