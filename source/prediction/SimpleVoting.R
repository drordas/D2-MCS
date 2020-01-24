SimpleVoting <- R6::R6Class(
  classname = "SimpleVoting",
  portable = TRUE,
  public = list(
    initialize = function(cutoff=NULL) {
      if (!is.null(cutoff) && !is.numeric(cutoff)) {
        stop("[", class(self)[1], "][FATAL] Invalid values of cutoff. Aborting...")
      }

      if(any(is.null(cutoff),!is.numeric(cutoff),!(dplyr::between(cutoff,0,1)))){
        private$cutoff <- 0.5
      }else private$cutoff <- cutoff

      private$final.pred <- FinalPred$new()
    },
    getCutoff = function() { private$cutoff },
    getFinalPred = function(type= NULL, target = NULL, filter = NULL) {
      if( any(is.null(type), !(type %in% c("raw","prob")) )){
        private$final.pred
      }else{
        if(!is.logical(filter)){
          message("[", class(self)[1], "][WARNING] Filter parameter must be ",
                  "defined as 'logical' type. Aborting...")
          filter <- FALSE
        }
        class.values <- private$final.pred$getClassValues()

        switch(type,
           "prob" = {
             if (is.null(target) || !(target %in% class.values )) {
               message("[", class(self)[1], "][WARNING] Target not ",
                       "specified or invalid. Using '",
                       paste0( private$final.pred$getClassValues(),
                               collapse = ", " ),"'")
               target <- private$final.pred$getClassValues()
             }
             if (filter) {
               private$final.pred$getProb()[private$final.pred$getRaw() == target,
                                            target, drop = FALSE]
             } else {
               private$final.pred$getProb()[, target, drop = FALSE]
             }
           },
           "raw" = {
             if (filter) {
               private$final.pred$getRaw()[private$final.pred$getRaw() == target,
                                           ,drop = FALSE]
             } else { private$final.pred$getRaw() }
           }
        )
      }
    },
    execute = function(predictions, metric = NULL) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    cutoff = NULL,
    final.pred = NULL
  )
)