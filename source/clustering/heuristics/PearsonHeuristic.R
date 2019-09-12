library("R6")
PearsonHeuristic <- R6Class(
  classname = "PearsonHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "PearsonHeuristic")
    },
    heuristic = function(subset, ...) {
      corpus <- subset$removeUnnecesaryReal()
      class <- subset$getClass()
      positive.class <- subset$getPositiveClass()
      negative.class <- names(table(class))[which(!names(table(class)) %in% positive.class)]
      #RECODE FACTOR VALUES TO NUMERIC ONES (POSITIVE -> 1 & NEGATIVE -> 0)
      binaryClass <-
        car::recode(
          class,
          paste0(
            "'",
            positive.class,
            "'='1'; '",
            negative.class,
            "'='0'"
          ),
          as.numeric = TRUE,
          as.factor = FALSE
        ) #IMPROVED REMOVED LOOP
      
      sapply(corpus, function(c) {
        cor.test(c, binaryClass, method = "spearman", exact = FALSE)$p.value
      })  
    }
  )
)