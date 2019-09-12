library("R6")
KendallHeuristic <- R6Class(
  classname = "KendallHeuristic",
  inherit = Heuristic,
  portable = TRUE,
  public = list(
    initialize = function() {
      super$initialize(name = "KendallHeuristic")
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
        estimateValue <- cor.test(c, binaryClass, method = "kendall")$estimate
        estimateValue <- unname(estimateValue, force = TRUE) #REMOVE TAU NAME.
        estimateValue
      })  
    }
  )
)