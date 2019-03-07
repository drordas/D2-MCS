FPFN <- R6Class(
  classname = "FPFN",
  portable = TRUE,
  inherit = MinFunction,
  public = list(
    initialize = function (){
      super$initialize(name= "FP & FN", objective.names= c("FP","FN") )
    },
    computeMeasure = function(conf.matrix){
      if( !"confusionMatrix" %in% class(conf.matrix) )
        stop("[",super$name,"][ERROR] Argument must be a valid caret confusionMatrix object\n")

      fn <- as.double(conf.matrix$table[1,2])
      fp <- as.double(conf.matrix$table[2,1])

      c(fp,fn)
    },
    pack = function(alg.name,pareto.front, population, n.positive, n.negative){
      FPFNdata$new( alg.name = alg.name, pareto.front = pareto.front, population= population, 
                    n.positive = n.positive, n.negative = n.negative )
    }
  )
)