library("R6")
ModelsList <- R6Class(
  classname = "ModelsList",
  portable = TRUE,                   
  public = list(
    initialize = function(size = 0){
      private$listOfModels <- vector( mode="list", length = size )
    },
    insertAt = function(position, entry){
      if ( position > 0 && position <= length(private$listOfModels) && 
           "ModelEntry" %in% class(entry) ){
        if( length(private$listOfModels[[position]]) == 0 || 
            private$listOfModels[[position]]$getPerformance() < entry$getPerformance() )
          private$listOfModels[[position]] <- entry$clone()
      }else stop("[ModelsList][WARNING] Position exceeds length of list or entry element is invalid. Aborting execution")
    },
    getAt = function(position){
      if ( position > 0 && position <= length(private$listOfModels) ){
        private$listOfModels[[position]]
      }else stop("[ModelsList][WARNING] Position exceeds length of list. Aborting...")
    },
    size = function(){
      length(private$listOfModels)
    },
    removeAt = function(position){
      if ( position > 0 && position <= length(private$listOfModels) ){
        private$listOfModels[[-position]]
      }else stop("[ModelsList][WARNING] Position exceeds length of list. Aborting...")
    },
    saveAll = function(){
      for (i in c(1:length(private$listOfModels) ) )  
        private$listOfModels[[i]]$getObject()$saveModel()
    }
  ),
  private = list(
    listOfModels = NULL
  )
)