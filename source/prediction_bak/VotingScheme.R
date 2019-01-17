library("R6")
VotingScheme <- R6Class(
  classname = "VotingScheme",
  portable = TRUE,                   
  public = list(
    initialize = function(name){
      if( missing(name) )
        stop("[VotingScheme][ERROR] VotingScheme name should be defined. Aborting execution.")
      
      private$name <- name
    },
    execute = function(predictions){
      stop("[VotingScheme][ERROR] Class is abstract. Method should be defined in inherited class\n.")
    },
    getName = function(){ private$name }
  ),
  private = list(
    name = NULL
  )
)