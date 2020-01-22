FinalPred <- R6::R6Class(
  classname = "FinalPred",
  portable = TRUE,
  public = list(
    initialize = function() {
      private$prob <- NULL
      private$raw <- NULL
      private$positive.class <- NULL
      private$negative.class <- NULL
    },
    set = function(prob, raw, class.values, positive.class) {
      if (length(positive.class) != 1 || !(positive.class %in% class.values)) {
        stop("[",class(self)[1],"][FATAL] Positive class is invalid. ",
             "Must be one of (",paste0(class.values,collapse = ", "),
             "). Aborting...")
      }

      if(any(is.null(prob), is.null(raw), nrow(prob)==0,
             ncol(row)==0, length(raw)==0) ){
        stop("[",class(self)[1],"][FATAL] Predictions were not computed. ",
             "Aborting...")
      }

      private$negative.class <- setdiff(class.values,positive.class)
      private$positive.class <- positive.class

      private$prob <- prob

      if (!is.factor(raw) ){
        private$raw <- factor(raw,levels = union( private$positive.class,
                                                  private$negative.class) )
        private$raw <- relevel(private$raw, ref = private$positive.class)
      }else{
        private$raw <- raw
        private$prob <- prob
      }

      if( any(is.na(private$raw)) ){
        stop("[",class(self)[1],"][FATAL] Class values contains NA's. ",
             "Aborting...")
      }

      names(private$prob) <- self$getClassValues()

    },
    getProb = function() { private$prob },
    getRaw = function() { private$raw },
    getClassValues = function() {
      union(private$positive.class, private$negative.class)
    },
    getPositiveClass = function() { private$positive.class },
    getNegativeClass = function() { private$negative.class }
  ),
  private = list(
    prob = NULL,
    raw = NULL,
    positive.class = NULL,
    negative.class = NULL
  )
)