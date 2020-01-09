ExecutedModels <- R6::R6Class(
  classname = "ExecutedModels",
  portable = TRUE,                   
  public = list(
    initialize = function(dir.path){
      private$dir.path <- gsub("\\/$","",dir.path)
      if( !file.exists(private$dir.path) ){
        dir.create(private$dir.path,recursive = TRUE)
        private$models <- NULL
        private$best.model <- NULL
      }
      
      if (!file.exists(file.path(private$dir.path,".executed")) || 
          file.info(file.path(private$dir.path,".executed"))$size <= 0){
        file.create(file.path(private$dir.path,".executed"))
        private$models <- NULL
        private$best.model <- NULL
      }else{
        private$models <- read.csv( file=file.path(private$dir.path,".executed"), 
                                    header=TRUE,stringsAsFactors= FALSE, sep="," )
        best.perf <- private$models[which.max(private$models$performance),]
        best.path <- file.path(private$dir.path,paste0(best.perf$model,".rds"))
        if(file.exists(best.path)){
          
          private$best.model <- list(model=best.perf$model,
                                     performance= best.perf$performance,
                                     exec.time= best.perf$exec.time,
                                     train= readRDS(best.path)
                                )
        }else{ 
          message("[",class(self)[1],"][WARNING] Best model cannot be loaded")
          private$best.model <- NULL
        }
      }
    },
    getNames = function(){
      if ( !is.null(private$best.model) ){
        private$models[,"model"]
      }else{ NULL }
    },
    getBest = function(){
      if ( !is.null(private$best.model) ){
        private$best.model
      }else {
        message("[",class(self)[1],"][WARNING] Best model not found.")
        NULL
      }
    },
    add = function(model, keep.best=TRUE){
      if(!inherits(model,"Model")){
        message("[",class(self)[1],"][WARNING] ML model invalid. ",
                "Must be an instance of Model class. ",
                "Model not inserted")
      }else{
        
        private$models <- rbind(private$models,
                                data.frame(model= model$getName(),
                                           performance= model$getPerformance(),
                                           exec.time= model$getExecutionTime()))

        if (isTRUE(keep.best)){ #SAVE ONLY BEST MODELS. REMOVE WORST
          
          if(any( is.null(private$best.model), #IS BEST MODEL
                  model$getPerformance() > private$best.model$performance)){
            if(!is.null(private$best.model)){
              message("[",class(self)[1],"][INFO] Best model found. Replacing '",
                      private$best.model$model,"' with '",
                      model$getName(),"'")
              self$delete(private$best.model$model)
            }
            private$best.model <- list( model=model$getName(),
                                        performance= model$getPerformance(),
                                        exec.time= model$getExecutionTime(),
                                        train= model$getTrainedModel() )
            model$save()
          }
        }else{ model$save() }
      }
    },
    exist = function(model.name){
      if( !is.character(model.name) || is.null(private$models$model) ){
        FALSE
      }else { model.name %in% (private$models$model) }
    },
    size = function(){
      ifelse(is.null(private$models),0,nrow(private$models))
    },
    save = function(){
      if(nrow(private$models) > 0){
        write.table(private$models, file=file.path(private$dir.path,".executed"), 
                    append= FALSE, sep= ",", row.names = FALSE)
      }else{
        message("[",class(self)[1],"][WARNING] File is empty. ",
                "Operation not done")
      }
    },
    delete = function(model.name){
      if(self$exist(model.name)){
        object.path <- file.path(private$dir.path,paste0(model.name,".rds"))
        if(file.exists(object.path)){
          file.remove(object.path)
        }else {
          message("[",class(self)[1],"][WARNING] Cannot delete model. ",
                  "Path for model '",model.name,"' not found")
        }
      }else{
        message("[",class(self)[1],"][WARNING] Cannot delete model. ",
                "Model '",model.name,"' has not been executed")
      }
    }
  ),
  private = list(
    models = NULL,
    best.model = NULL,
    dir.path = NULL
  )
)