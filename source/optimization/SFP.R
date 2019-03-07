SFP <- R6Class(
  classname = "SFP",
  portable = TRUE,
  inherit = MinFunction,
  public = list(
    initialize = function (){
      super$initialize(name= "FP", objective.names= "FP" )
    },
    computeMeasure = function(conf.matrix){
      if( !"confusionMatrix" %in% class(conf.matrix) )
        stop("[",super$name,"][ERROR] Argument must be a valid caret confusionMatrix object\n")
      #print(conf.matrix$table)
      
      #print(as.double(conf.matrix$table[1,2]))
      print(conf.matrix$table[2,1])
      return( conf.matrix$table[2,1] )
    },
    pack = function(alg.name,pareto.front, n.positive, n.negative){
      #FPFNdata$new(alg.name = alg.name, pareto.front, n.positive, n.negative)
    }
  )
)