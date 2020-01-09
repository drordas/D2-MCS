SPPV <- R6::R6Class(
  classname = "SPPV",
  portable = TRUE,
  inherit = MinFunction,
  public = list(
    initialize = function (){
      super$initialize(name= "PPV", objective.names= "PPV" )
    },
    computeMeasure = function(conf.matrix){
      if( !"confusionMatrix" %in% class(conf.matrix) )
        stop("[",super$name,"][FATAL] Argument must be a valid caret confusionMatrix object")

      return( conf.matrix$byClass["Pos Pred Value"] )
    },
    pack = function(opt.values ,n.positive, n.negative){
      SPPVData$new(optimx= opt.values, n.positive= n.positive, n.negative= n.negative)
    }
  )
)