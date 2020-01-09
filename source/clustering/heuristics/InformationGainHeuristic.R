InformationGainHeuristic <- R6::R6Class(
  classname = "InformationGainHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    initialize = function() { },
    # heuristic = function(subset, ...) {
    #   # corpusDiscretes <- 
    #   # corpusContinues   <- 
    #   #si son discretas
    #   # ig.test.discretes <- private$information_gain(className, corpusDiscretes)
    #   #Si son continuas
    #   ig.test <- information.gain(as.formula(sprintf("`%s` ~.", className)), corpusContinues)
    #   
    #   # ig.test <- rbind(ig.test.discretes, ig.test.continues)
    #   
    #   ig.values <- ig.test$attr_importance
    #   names(ig.values) <- row.names(ig.test)
    #   ig.zero <- ig.values[which(ig.values == 0, arr.ind = TRUE)]
    #   ig.nonzero <- ig.values[which(ig.values != 0, arr.ind = TRUE)]
    #   ig.values <- list("NonZero" = ig.nonzero, "Zero" = ig.zero)
    #   ig.values <- unlist(unname(ig.values["NonZero"]))
    # },
    heuristic = function(col1, col2, column.names = NULL) {
      col1 <- as.integer(col1[,1]) - 1
      if (!private$isBinary(col1)) {
        warning("[", super$getName(), "][WARNING] Columns must to be binary. Return NA")
        NA
      } else {
        if (private$isBinary(col1)) {
          #if features are binary
          # ig.test.discretes <- private$information_gain(className, corpusDiscretes)
        } else {
          ig.test <- information.gain(as.formula(sprintf("`%s` ~.", names(col1))), col2)$attr_importance
        }
        ig.values <- ig.test$attr_importance
        names(ig.values) <- row.names(ig.test)
        ig.zero <- ig.values[which(ig.values == 0, arr.ind = TRUE)]
        ig.nonzero <- ig.values[which(ig.values != 0, arr.ind = TRUE)]
        ig.values <- list("NonZero" = ig.nonzero, "Zero" = ig.zero)
        ig.values <- unlist(unname(ig.values["NonZero"]))
      }
    }
  ),
  private = list(
    information_gain = function(target, data) {
      entropy <- function(freqs, method = "ML") {
        freqs = freqs/sum(freqs)
        if (method == "ML")  H = -sum(ifelse(freqs > 0, freqs * log(freqs), 0))
      }
      entropyHelper <- function(x) {
        return(entropy(table(x, useNA = "always")))
      }
      i <- which(names(data) == target)
      attr_entropies = sapply(data, entropyHelper)
      class_entropy = attr_entropies[i]
      attr_entropies = attr_entropies[-i]
      joint_entropies = sapply(data[-i], function(t) {
        entropyHelper(data.frame(cbind(data[[i]], t)))
      })
      results = class_entropy + attr_entropies - joint_entropies
      attr_names = dimnames(data)[[2]][-1]
      return(data.frame(attr_importance = results, row.names = attr_names))
    }
  )
)