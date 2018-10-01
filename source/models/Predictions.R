library("R6")
Predictions <- R6Class(
  classname = "Predictions",
  portable = TRUE,                   
  public = list(
    initialize = function(size = 0, classValues){
      private$predsList <- vector(mode="list", length = size)
      private$classValues <- classValues
    },
    addPredictionAt = function(position, element){
      if( position > 0 && position <= length(private$predsList) ){
        private$predsList[[position]] <- element
      }else stop("[Predictions][ERROR] Position exceeds list bounds\n")
    },
    getPredictionAt = function(position){
      if( position > 0 && position <= length(private$predsList) ){
        private$predsList[[position]]
      }else stop("[Predictions][ERROR] Position exceeds list bounds\n")
    },
    size = function(){ length(private$predsList) },
    print = function(){
      cat("------- PREDICED VALUES -------\n")
      for( i in c(1:length(private$predsList) ) ){
        cat("------- CLUSTER: ",i," -------\n", sep="")
        print(private$predsList[[i]])
      }
      cat("-------------------------------\n")
      cat("--------- REAL VALUES ---------\n")
      print(private$classValues)
    }
  ),
  private = list(
    predsList = NULL,
    classValues = NULL
  )
)