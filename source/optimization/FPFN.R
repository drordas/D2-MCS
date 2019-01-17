FPFN <- R6Class(
  classname = "FPFN",
  portable = TRUE,
  inherit = MinFunction,
  public = list(
    initialize = function (){
      super$initialize(name= "FP & FN", n.objectives= 2)
    },
    computeMeasure = function(conf.matrix){
      if( !"confusionMatrix" %in% class(conf.matrix) )
        stop("[",super$name,"][ERROR] Argument must be a caret confusionMatrix object\n")

      fn <- as.double(conf.matrix$table[1,2])
      fp <- as.double(conf.matrix$table[2,1])

      c(fp,fn)
    },
    xlabel= "False Positives (FP)",
    ylabel= "False Negatives (FN)",
    objectives = c("FP","FN")
  )
)