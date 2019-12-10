Prediction <- R6Class(
  classname = "Prediction",
  portable = TRUE,
  public = list(
    initialize = function(model, feature.id=NULL){
      if ( !inherits(model,"list") || length(model) != 5 )
        stop("[",class(self)[1],"][ERROR] Model must be defined as a list of four ",
             "elements. Aborting...")
      private$model <- model
      private$feature.id <- feature.id
      private$results <- list(id=c(),raw=data.frame(),prob=data.frame())
      private$loaded.resources <- list(len.init.packages= length(.packages()),
                                       len.init.DLLs = length(.dynLibs()) )

      private$loadPackages(private$model$model.libs)
    },
    execute = function(pred.values,class.values, positive.class){
      if (!inherits(pred.values,"data.frame")){
        stop("[",class(self)[1],"][ERROR] Prediction values are not correct. ",
             "Must be a data.frame. Aborting..")
      }

      if(all(!is.null(private$feature.id),length(private$feature.id)>0)){

        private$results$id <- c(private$results$id,
                                as.character(pred.values[,private$feature.id]))
        pred.values[,-which(names(pred.values)==private$feature.id)]
      }
      private$results$raw <- private$fbind(private$results$raw,
                                           predict(object = private$model$model.data,
                                                   newdata=pred.values, type="raw" ),
                                           lvls=class.values, pclass=positive.class )
      private$results$prob <- rbind(private$results$prob,
                                    predict(object = private$model$model.data,
                                            newdata=pred.values, type="prob" ) )
    },
    getPrediction = function(type=NULL, target=NULL){
      if( is.null(type) || !type %in% c("raw","prob") ){
        message(yellow(paste0("[",class(self)[1],"][WARNING] Probability type ",
                              "missing or incorrect. Should be 'raw' or 'prob' ",
                              ". Assuming 'raw' by default")))
        type <- "raw"
      }
      switch (type,
              "prob"= {
                class.names <- names(private$results$prob)
                if(is.null(target) || !(target %in% class.names ) ){
                  message(yellow(paste0("[",class(self)[1],"][WARNING] Target not ",
                                        "specified or invalid. Using '",
                                        class.names[1],"' as default value")))
                  target <- class.names[1]
                }
                ret <- private$results$prob[,target, drop=FALSE]
              },
              "raw" = { ret <- as.data.frame(private$results$raw) }
      )

      if(length(private$results$id)!=nrow(ret)){
        private$results$id <- as.integer(seq(from=1,to=nrow(ret),by=1))
      }

      ret <- as.data.frame(ret,row.names=private$results$id)
      names(ret) <- ifelse(is.null(target),"Predictions",target)
      ret
    },
    getModelName = function(){ private$model$model.name },
    getModelPerformance = function(){ private$model$model.performance },
    finalize = function(){
      private$unloadPackages(private$loaded.resources$len.init.packages,
                             private$loaded.resources$len.init.DLLs)
    }
  ),
  private = list(
    results = NULL,
    model = NULL,
    loaded.resources = NULL,
    feature.id = NULL,
    loadPackages = function(pkgName){

      if (is.list(pkgName)) {
        pkgName <- unlist(pkgName)
      }

      new.packages <- pkgName[!(pkgName %in% installed.packages()[,"Package"])]
      if ( length(new.packages) ) {
        message("[", class(self)[1], "][INFO][", private$model$model.name, "]", length(new.packages),
                "packages needed to execute aplication\n Installing packages ...")
        suppressMessages(install.packages( new.packages,
                                           repos ="https://ftp.cixug.es/CRAN/",
                                           dependencies = TRUE,
                                           quiet = TRUE, verbose = FALSE))
      }
      lapply(pkgName, function(pkg) {
        if ( !pkg %in% loaded_packages() ) {
          library(pkg, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)
        }
      })
    },
    unloadPackages = function(len.init.packages, len.init.DLLs) {
      pkgs <- paste0("package:", head(x = .packages(), n = length(.packages()) - len.init.packages))
      if ( length(head(x = .packages(), n = length(.packages()) - len.init.packages)) > 0) {
        message("[", class(self)[1], "][INFO] Package to detach: ", paste(pkgs, collapse = " "))
        for (p in pkgs) {
          detach(p, unload = T, character.only = TRUE, force = F)
        }
      } else {
        # message("[", class(self)[1], "][INFO] There are not packages to detach")
      }

      pkglibs <- tail(x = .dynLibs(), n = length(.dynLibs()) - len.init.DLLs)
      if ( length(pkglibs) > 0) {
        # message("[", class(self)[1], "][INFO] Dlls to detach: ", paste(pkglibs, collapse = " "))
        for (lib in pkglibs) {
          dyn.unload(lib[["path"]])
        }
        libs <- .dynLibs()
        .dynLibs(libs[!(libs %in% pkglibs)])
      } else {
        # message("[", class(self)[1], "][INFO] There are not DLLs to unload\n")
      }
    },
    fbind = function(...,lvls,pclass){
      fact <- factor(do.call(c, lapply(list(...), as.character)),levels=lvls)
      relevel(fact,ref= pclass)
    }
  )
)