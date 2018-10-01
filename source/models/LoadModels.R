library("R6")
LoadModels <- R6Class(
  classname = "LoadModels",
  portable = TRUE,                   
  public = list(
    initialize = function(directory= NULL){
      if(is.null(directory) || !dir.exists(directory)){
        stop("Directory not found on path: '",directory,"'\n")
      }
      private$directory <- directory
      allModelsNames <- list.files(directory,"*.R", full.names = FALSE)
      private$modelNames <- gsub( "*.R","",allModelsNames[!allModelsNames %in% paste0(private$directory,"/Model.R")] )
      allModelsPaths <- list.files(directory,"*.R", full.names = TRUE)
      private$modelPaths <- allModelsPaths[!allModelsPaths %in% "Model.R"]
    },
    getModelNames = function(){
      allModelsNames <- list.files(private$directory,"*.R", full.names = FALSE)
      private$modelNames <- gsub( "*.R","",allModelsNames[!allModelsNames %in% "Model.R"] )
      private$modelNames
    },
    getModelPaths = function(){
      allModelsPaths <- list.files(private$directory,"*.R", full.names = TRUE)
      private$modelPaths <- allModelsPaths[!allModelsPaths %in% paste0(private$directory,"/Model.R")]
      private$modelPaths
    }
  ),
  private = list(
    directory = NULL,
    modelPaths = NULL,
    modelNames = NULL,
    listModels = NULL
  )
)