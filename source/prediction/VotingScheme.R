VotingScheme <- R6::R6Class(
  classname = "VotingScheme",
  portable = TRUE,
  public = list(
    initialize = function(){
      private$name <- class(self)[1]
    },
    execute = function(predictions){
      stop("[",class(self)[1],"][ERROR] Class is abstract.",
           " Method should be defined in inherited class. Aborting...")
    },
    getPrediction = function(type=NULL, target=NULL){
      stop("[",class(self)[1],"][ERROR] Class is abstract.",
           " Method should be defined in inherited class. Aborting...")
    },
    getName = function(){ private$name },
    setWeights = function(weights){
      stop("[",class(self)[1],"][ERROR] Class is abstract.",
           " Method should be defined in inherited class. Aborting...")
    },
    getWeights = function(){
      stop("[",class(self)[1],"][ERROR] Class is abstract.",
            " Method should be defined in inherited class. Aborting...")
    },
    getPositiveClass = function(){
      stop("[",class(self)[1],"][ERROR] Class is abstract.",
           " Method should be defined in inherited class. Aborting...")
    },
    getClassValues = function(){
      stop("[",class(self)[1],"][ERROR] Class is abstract.",
           " Method should be defined in inherited class. Aborting...")
    },
    getCutoff = function(){
      stop("[",class(self)[1],"][ERROR] Class is abstract.",
           " Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list( name = NULL )
)