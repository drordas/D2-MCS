library("R6")
ExecutedModelsList <- R6Class(
  classname = "ExecutedModelsList",
  portable = TRUE,                   
  public = list(
    initialize = function(size = 0){
      private$listOfModels <- vector( mode="list", length = size )
      for(i in 1:size) private$listOfModels[[i]] <- ModelInfo$new()
    },
    insertModeltAt = function(position, model){
      if ( position > 0 && position <= length(private$listOfModels) && "Model" %in% class(model) ){
        private$listOfModels[[position]]$insert(model$getName(), model$getPerformance(), model$getElapsedTime() )
      }else stop("[ExecutedModelsList][WARNING] Position exceeds length of list or model element is invalid. Aborting execution")
    },
    insertAt = function(position, modelName, modelPerformance, modelElapsedTime){
      if ( position > 0 && position <= length(private$listOfModels) && "Model" %in% class(model) ){
        private$listOfModels[[position]]$insert(modelName, modelPerformance, modelElapsedTime )
      }else stop("[ExecutedModelsList][WARNING] Position exceeds length of list or model element is invalid. Aborting execution")
    },
    loadFrom = function(path, position){
      if( file.exists(path) ){
        models <- read.csv(file=path, header=TRUE, stringsAsFactors = FALSE, sep=",")
        if ( position > 0 && position <= length(private$listOfModels) ){
          if(ncol(models) == 3){
            for(i in 1:nrow(models) )
              private$listOfModels[[position]]$insert(models[i,1], models[i,2], models[i,3] )
          }else cat("[ExecutedModelsList][WARNING] Error loading models from file '",path,"'\n", sep="")
        }else stop("[ExecutedModelsList][WARNING] Position exceeds length of list. Aborting execution")
      }#else cat("[ExecutedModelsList][WARNING] Error path '",path,"' does not exists\n", sep="")
    },
    exists = function(position, modelName){
      if( position > 0 && position <= length(private$listOfModels) ){
        ifelse( "ModelInfo" %in% class(private$listOfModels[[position]]) && 
                 modelName %in% private$listOfModels[[position]]$getNames(), TRUE, FALSE )
      }else return(FALSE)
    },
    getAt = function(position){
      if ( position > 0 && position <= length(private$listOfModels) ){
        private$listOfModels[[position]]
      }else stop("[ExecutedModelsList][WARNING] Position exceeds length of list. Aborting...")
    },
    size = function(){
      length(private$listOfModels)
    },
    removeAt = function(position){
      if ( position > 0 && position <= length(private$listOfModels) ){
        private$listOfModels[[-position]]
      }else stop("[ExecutedModelsList][WARNING] Position exceeds length of list. Aborting...")
    },
    saveAt = function(path, position){
      write.table(private$listOfModels[[position]]$getAll(),file=path,append = FALSE, sep = ",", row.names = FALSE)
    }
  ),
  private = list(
    listOfModels = NULL
  )
)