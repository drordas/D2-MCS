library("R6")

D2MCS <- R6Class(
  classname = "D2MCS",
  portable = TRUE,                   
  public = list(
    initialize = function(path="models", nCores = NULL, socket.type="PSOCK", outfile="d2mcs.log",
                          serialize=FALSE, trainFunction = NULL ){ 
      if(is.null(nCores) || nCores >=  detectCores() || nCores > 10  ){
        cat("[D2MCS][WARNING] Invalid number of cores ")
        if(detectCores() > 10)
          cat(" (1>= nCores <= 10)", sep="")
        else
          cat(" (1>= nCores < ",detectCores(),")", sep="")
        cores <- min(max(1,detectCores()-2),10)
        cat(" Using default configuration (",cores,"/",detectCores(),") \n", sep="")
      }else cores <- nCores
      
      if( !socket.type %in% c("PSOCK","FORK") ){
        cat("[D2MCS][WARNING] Invalid socket type. Assuming 'PSOCK' cluster\n")
        socket <- "PSOCK"
      }else socket <- socket.type
      
      if(!is.logical(serialize)){
        cat("[D2MCS][WARNING] Invalid serialization option. Assuming not serialization\n")
        xdr <- FALSE
      }else xdr <- serialize
      
      if (missing(trainFunction) || is.null(trainFunction) )
        stop("[D2MCS][ERROR] TrainFunction not defined\n")
      else private$trainFunction <- trainFunction
      
      private$loadModules()
      private$path <- file.path(getwd(),path)
      dir.create( private$path, recursive = TRUE, showWarnings = FALSE )
      
      private$cluster <- makeCluster(cores, type=socket, outfile=outfile, useXDR=xdr)
      private$executedModels <- NULL
      private$prediction.voted <- NULL
      private$performance <- NULL
      private$models.weights <- c()
    },
    train = function( cluster.dist = NULL, num.clusters = NULL, classifiers = c(), metric = NULL, saveAllModels = FALSE ){
      if(missing(cluster.dist) || is.null(cluster.dist) || !"ClusterDistribution" %in% class(cluster.dist) )
        stop("[D2MCS][ERROR] Subset must be of type 'Cluster'\n")
    
      if(missing(num.clusters) || is.null(num.clusters) || !is.numeric(num.clusters) || !is.vector(num.clusters) ){
        cat("[D2MCS][WARNING] Number of clusters missing or incorrect (must be numeric or vector). Using all clusters\n")
        num.clusters <- c(1:cluster.dist$getNumClusters())
      }else{
        if (is.numeric(num.clusters) && num.clusters > cluster.dist$getNumClusters() ){
          cat("[D2MCS][WARNING] num.clusters is higher than number of existing clusters. Using all clusters\n")
          num.clusters <- c(1:cluster.dist$getNumClusters())
        }else num.clusters <- c(1:num.clusters)
      }

      if(missing(classifiers) || is.null(classifiers) || length(classifiers) == 0){
        cat("[D2MCS][INFO] Classification models not defined. Executing all available classifiers\n")
        usedModels <- private$availableModels$METHOD
      }else usedModels <- classifiers

      private$bestModels <- ModelsList$new(size = length(num.clusters) )
      private$executedModels <- ExecutedModelsList$new(size = length(num.clusters))
      private$metric <- metric
      
      for (i in num.clusters ){
        cat("[D2MCS][INFO] ---------------------------------------\n",sep="")
        cat("[D2MCS][INFO] Training models for cluster '",i,"' of '", max(num.clusters),"'\n",sep="")
        cat("[D2MCS][INFO] ---------------------------------------\n",sep="")
        model.subset <- cluster.dist$getAt(i)
        model.fitClass <- DefaultModelFit$new(model.subset)
        model.savePath <- file.path(private$path,metric,paste0("C[",i,"-",cluster.dist$getNumClusters(),"]") )

        private$bestClusterModel <- NULL
        private$executedModels$loadFrom(paste0(model.savePath,"/.executed"),i)
        
        cluster.models <- list()
        
        apply(private$availableModels, 1, function(model){

          if( model$METHOD %in% usedModels ){
            switch (toupper(model$CONFIGURATION$probabilistic),
                    "TRUE" = {trFunction$create(UseProbability$new(),"grid")},
                    "FALSE" = {trFunction$create(NoProbability$new(),"grid")}
            )

            switch ( toupper(model$CONFIGURATION$fitting) ,
                     "FORMULA" = { model.fit <- model.fitClass$createFormula() },
                     "RECIPE" = { model.fit <- model.fitClass$createRecipe() }
            )
            model.type <- Model$new(dir = model.savePath, method = model$METHOD,
                                    family = model$FAMILY, description = model$DESCRIPTION,
                                    pkgName = as.vector( unlist(model$DEPENDENCES) ), trFunction = private$trainFunction,
                                    metric = metric )
            
            if( !private$executedModels$exists(i,model$METHOD) ){
              model.type$train(dataset = model.subset$getInstances(), fitting = model.fit )
              if( isTRUE(saveAllModels) ) model.type$saveModel() 
              private$executedModels$insertModeltAt(i,model.type)
            }else cat("[D2MCS][INFO] Model '",model$METHOD,"' has been executed previously. Ignoring...\n", sep="")

            if( model.type$exists() && (is.null(private$bestClusterModel) || 
                !"ModelEntry" %in% class(private$bestClusterModel) || 
                private$bestClusterModel$getPerformance() < model.type$getPerformance() ) )
            {
              if("ModelEntry" %in% class(private$bestClusterModel)) private$bestClusterModel$removeModel()
              private$bestClusterModel <- ModelEntry$new( name= model$METHOD, object= model.type,
                                                          performance= model.type$getPerformance(),
                                                          path= model.type$getPath() )
            }
            private$executedModels$saveAt(paste0(model.savePath,"/.executed"),i)
          }
        }) ##APPLY
        private$bestClusterModel$save()
        private$bestModels$insertAt(i,private$bestClusterModel)
        private$models.weights <- c( private$models.weights,private$bestClusterModel$getPerformance() )
      }
      #private$bestModels$saveAll()
    },
    classify = function( test.set = NULL, voting.scheme){
      if( !"Subset" %in% class(test.set)  )
        stop("[D2MCS][ERROR] Test dataset missing or incorrect. Must be a Subset object\n")
      
      if (missing(voting.scheme) || !"VotingScheme" %in% class(voting.scheme) )
        stop("[D2MCS][ERROR] Voting Scheme missing or invalid\n")
      
      if( is.null(private$bestModels) || private$bestModels$size() < 1 )
        cat("[D2MCS][ERROR] Models were not trained. Please run 'executeTrain' method first\n")
      else{
        cat("[D2MCS][INFO] -------------------------------------------------------\n",sep="")
        cat("[D2MCS][INFO] Starting prediction operation\n")
        cat("[D2MCS][INFO] -------------------------------------------------------\n",sep="")

        instances <- test.set$getInstances(ignore.class = TRUE)
        prediction.cluster <- PredictionList$new( private$metric )

        for ( cluster in 1:private$bestModels$size() ){
          cat("[D2MCS][INFO] Computing predictions for Cluster ",cluster,"\n",sep="")
          pred <- Prediction$new( model = private$bestModels$getAt(cluster)$getObject() )
          pred$execute(instances)
          prediction.cluster$addPrediction(pred)
        }
        cat("[D2MCS][INFO] -------------------------------------------------------\n",sep="")
        cat("[D2MCS][INFO] Computing final prediction values using '",voting.scheme$getName(),"'\n", sep="")
        private$prediction.voted <- voting.scheme$execute(prediction.cluster)
        cat("[D2MCS][INFO] -------------------------------------------------------\n",sep="")
      }
    },
    getBestPerformanceByCluster = function(){
      if ( is.null(private$models.weights) || length(private$models.weights) < 1 )
        stop("[D2MCS][ERROR] Train stage should be executed first\n")
      else private$models.weights
    },
    computeFinalPerformance = function(new.data, positive.class){
      if( is.null(private$prediction.voted) || !is.data.frame(private$prediction.voted) )
        stop("[D2MCS][ERROR] Voting scheme should be executed first to obtain predicion results\n")
      
      if (missing(new.data) || is.null(new.data) && (!"Subset" %in% class(new.data) || !is.data.frame(new.data)) ) 
        stop("[D2MCS][ERROR] Test data missing or incorrect object type (Subset class or data.set type)\n")
      
      if ("Subset" %in% class(new.data)){
        if(new.data$getNrow() != nrow(private$prediction.voted) )
          stop("[D2MCS][ERROR] Distinct dimension of predicted and real values\n")
        real.values <- new.data$getClass()
      }else real.values <- new.data
      
      pred.values <- private$prediction.voted[,ncol(private$prediction.voted)]
      
      if( !"factor" %in% class(pred.values) ) pred.values <- as.factor(pred.values)
      
      if ( length(levels(real.values)) != length(levels(pred.values)) || 
           ( length(setdiff(levels(pred.values), levels(real.values))) != length(setdiff(levels(real.values), levels(pred.values))) )  )
        stop("[D2MCS][ERROR] Different type of predicted and real class values\n")
      
      if (!positive.class %in% levels(pred.values) && !positive.class %in% levels(real.values) )
        stop("[D2MCS][ERROR] Positive class values not valid (",paste0(levels(real.values),collapse = ","),")\n")
      
      cf <- caret::confusionMatrix(real.values,pred.values, positive=positive.class, mode="everything")
      private$performance <- PerformanceMeasures$new( cf= cf, mcc= mltools::mcc(FP= cf$table[1,2], TP = cf$table[1,1], TN = cf$table[2,2], FN = cf$table[2,1]  ) )
    },
    getFinalPerformance = function(measure){
      if(is.null(private$performance) )
        stop("[D2MCS][ERROR] Performance not computed yet. ComputeFinalPerformance method should be executed first\n")
      private$performance
    },
    plotTestPerformance = function(){
      df <- data.frame(cluster = as.character(), name = as.character(), performance = as.numeric(), stringsAsFactors = FALSE )
      for( i in 1:private$executedModels$size() ){
        bestModel.name <- private$executedModels$getAt(i)$getNames()[which.max(private$executedModels$getAt(1)$getPerformances())]
        bestModel.value <- round( max(as.numeric(private$executedModels$getAt(i)$getPerformances())), digits = 4 )
        df <- rbind(df, data.frame( cluster=paste0("CLUSTER ",i), name=bestModel.name, performance = bestModel.value), stringsAsFactors = FALSE )
      }
      
      measure <- private$performance$getMeasure(private$metric)
      ggplot(df, aes(x=cluster, y=performance ) ) + geom_point( aes(color="By Cluster"),shape=18, size=3, show.legend = TRUE) + 
                   geom_label_repel(aes(label=paste0(name,"\n",df$performance) ), size=3 ) +
                   geom_point( aes(cluster,measure, color="Combined"), shape=15, size=3, show.legend = TRUE ) + 
                   theme( legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank() ) +
                   geom_segment(aes(x=df$cluster,y=measure, xend=df$cluster[length(df$cluster)], yend=measure ), color="blue", alpha=0.25  ) + 
                   annotate("text", x=(1 + nrow(df))/2, y=round(measure,digits = 2), label=round(measure,digits=2), color="blue" ) + 
                   xlab("Performance") + ylab( paste0("Performance (",private$metric,")") ) + 
                   labs( color= "Achieved performance\n" ) + 
                   scale_color_manual(values = c("black","blue")) + 
                   scale_y_continuous( limits=  c(min(df$performance-0.05), 1) )
      ggsave(filename = file.path(getwd(),"plots",paste0("TEST_",toupper(private$metric),"_Performance.pdf")), 
             plot=last_plot(),device="pdf", limitsize = FALSE)
    },
    plotTrainPerformances = function(){
      if ( is.null(private$executedModels ) || private$executedModels$size() < 1 )
        cat("[D2MCS][ERROR] Models were not trained. Please run 'executeTrain' method first\n")
      else{
        for ( i in 1:private$executedModels$size() ){
          models.cluster <- private$executedModels$getAt(i)
          summary <- data.frame( model=models.cluster$getNames(), measure= as.numeric(models.cluster$getPerformances()), 
                                 stringsAsFactors = FALSE )
          min <- data.frame( x=summary[which.min(summary[,2]), ][, 1],y= min(summary[,2]) )
          max <- data.frame( x=summary[which.max(summary[,2]), ][, 1],y= max(summary[,2]) )
          avg <- round(mean(summary$measure ), digits = 2)
          measure <- private$metric
          plotPath <- here::here("plots",paste0("TRAIN_",toupper(measure),"_C[",i,"-",private$executedModels$size(),"].pdf"))
          plot <- ggplot(summary, aes(model,measure, group=1)) + geom_line() + geom_point() +
            geom_point(aes(x,y), min, fill="transparent", color="red", shape=21, size=3,stroke=1) +
            geom_text(aes(x,y,label=sprintf("%.3f",y)), min, hjust=-0.45, color='red' ) +
            geom_point(aes(x,y), max, fill="transparent", color="blue", shape=21, size=3,stroke=1) +
            geom_text(aes(x,y,label=sprintf("%.3f",y)), max, hjust=-0.45, color='blue' ) +
            geom_hline(aes(yintercept=avg), linetype="twodash", color = "#696969", show.legend = TRUE) +
            labs(x = "Model name", y = paste0(measure," value"),
                 title = paste0("Performance benchmarking plot for cluster=",i)) +
            theme (axis.text.x = element_text(angle = 75, hjust = 1),
                   plot.title = element_text(hjust = 0.5))
          plot
          cat("[D2MCS][INFO] Plot saved at: '",plotPath,"'\n")
          ggsave(filename = plotPath, plot=last_plot(),device=file_ext(plotPath), limitsize = FALSE)
        }
      }
    },
    getPredictions = function(option = "all"){
      if( is.null(private$prediction.voted) || length(private$prediction.voted)  <= 0 )
        cat("[D2MCS][ERROR] Method 'executePrediction' should be executed first. Aborting execution\n")
      else{
        
        if(!option %in% c("cluster", "final", "all") )
          stop("[D2MCS][ERROR] Incorrect option should be 'cluster', 'final' or 'all'\n")
        
        switch (tolower(option) ,
          "cluster" = { private$prediction.voted[,-ncol(private$prediction.voted)] },
          "final" = { private$prediction.voted[,ncol(private$prediction.voted)] },
          "all" = { private$prediction.voted }
        )
      }
    },
    savePredictions = function(option = "all", filename){
      if(missing(filename) || is.null(filename) )
        stop("[D2MCS][ERROR] Store path must be selected\n")
      path <- file.path(getwd(),"results",filename)
      if( is.null(private$prediction.voted) || length(private$prediction.voted)  <= 0 )
        cat("[D2MCS][ERROR] Method 'executePrediction' should be executed first. Aborting execution\n")
      else{
        if(!option %in% c("cluster", "final", "all") )
          stop("[D2MCS][ERROR] Incorrect option should be 'cluster', 'final' or 'all'\n")
        
        switch (tolower(option) ,
                "cluster" = { write.csv(private$prediction.voted[,-ncol(private$prediction.voted)], file=path, row.names = FALSE) },
                "final" = { write.csv(private$prediction.voted[,ncol(private$prediction.voted)], file=path, row.names = FALSE) },
                "all" = { write.csv(private$prediction.voted, file=path, row.names = FALSE) }
        )
      }
      cat("[D2MCS][INFO] Classification results succesfully saved at: ",path,"\n")
    },
    removeAll = function(){
      if(!is.null(private$path) && dir.exists(private$path)){
        if(!unlink(private$path,recursive = TRUE,force = TRUE))
          cat("[D2MCS][INFO] Path '",private$path,"' succesfully removed\n")
        else cat("[D2MCS][ERROR] Path '",private$path,"' could not be removed\n")
      }
    }
  ),
  private = list(
    loadModules = function(){
      xml <- xmlInternalTreeParse("config/models.xml", isSchema = FALSE, isHTML = FALSE)
      xsd <- xmlParse("config/models.xsd", isSchema = TRUE)
      if( xmlSchemaValidate(xsd,xml)$status == 0 ){
        cat("[D2MCS][INFO] XML format of 'config/models.xml' matches XSD schema. Loading classifiers\n", sep="")
      }else stop("[D2MCS][INFO] XML format of 'config/models.xml' does not match XSD schema. Aborting execution\n")
      
      private$availableModels <- do.call(rbind,xpathApply(xml, "/methods/method", function(node) {
        model <- xmlValue(node[["name"]])
        description <- xmlValue(node[["description"]])
        family <- xmlValue(node[["family"]])
        if(is.null(description)) description <- NA
        if(is.null(family)) family <- NA
        
        if( is.null(node[["dependences"]]) ) 
          data.frame("METHOD" = model, "DESCRIPTION" = description, "FAMILY"=family,
                     "DEPENDENCES" = NA, "CONFIGURATION"=I(list(xmlToList(node[["configuration"]]))), stringsAsFactors = FALSE)
        else data.frame("METHOD" = model, "DESCRIPTION" = description, "FAMILY"=family,
                        "DEPENDENCES" = I(list(xmlToList(node[["dependences"]]))), 
                        "CONFIGURATION" = I(list(xmlToList(node[["configuration"]]))) ,stringsAsFactors = FALSE)
      }) )
      cat("[D2MCS][INFO] ",nrow(private$availableModels)," classifiers has been succesfully loaded\n", sep="")
      private$availableModels[order(private$availableModels$FAMILY),]
    },
    cluster = NULL,
    availableModels = NULL,
    modelFormula = NULL,
    modelRecipe = NULL,
    trainFunction = NULL,
    path = NULL,
    bestClusterModel = NULL,
    prediction.voted = NULL,
    executedModels = NULL,
    metric = NULL,
    bestModels = NULL,
    performance = NULL,
    models.weights = NULL
  )
)