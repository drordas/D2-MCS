toNumeric <- function(corpus, className){
  aux <- corpus
  indx <- sapply(aux, is.factor)
  aux[indx] <- lapply(aux[indx], function(x) as.numeric(x)-1)
  return(aux)
}

twoClassSummary <- function (data, lev = NULL, model = NULL){
  
  lvls <- levels(data$obs)
  if(length(lvls) > 2)
    stop(paste("Your outcome has", length(lvls),
               "levels. The customizeTwoClassSummary() function isn't appropriate."))
  
  if (!all(levels(data[, "pred"]) == lvls))
    stop("levels of observed and predicted data do not match")
  
  data$y = as.numeric(data$obs == lvls[2])
  data$z = as.numeric(data$pred == lvls[2])
  rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0, 1), data[, lvls[1]])
  confMat <- caret::confusionMatrix(table(data$z, data$y),positive="1")
  mcc <- mltools::mcc(TP=confMat$table[1,1],FP=confMat$table[1,2],TN=confMat$table[2,2],FN=confMat$table[2,1])
  ppv <- (confMat$table[1,1] / (confMat$table[1,1] + confMat$table[1,2]) )
  fn_tcr_9 <- ( 9*confMat$table[1,2] + confMat$table[2,1]) / (9 * (confMat$table[1,2] + confMat$table[2,2]) + 
                                                                confMat$table[2,1] + confMat$table[1,1] )
  out <- c(rocAUC,
           sensitivity(data[, "pred"], data[, "obs"], lev[1]),
           specificity(data[, "pred"], data[, "obs"], lev[2]),
           confMat$overall['Kappa'],
           confMat$overall['Accuracy'],
           fn_tcr_9, mcc, ppv)
  names(out) <- c("ROC", "Sens", "Spec","Kappa","Accuracy","TCR_9","MCC","PPV")
  out
}

getModel <- function(name, dir, path){
  fullPath <- paste0(dir,"/",path,"/")
  return ( readRDS(paste0(fullPath,paste0(name,".rds"))) )
}

noProbsSummary <- function (data, lev = NULL, model = NULL){
  
  lvls <- levels(data$obs)
  if(length(lvls) > 2)
    stop(paste("Your outcome has", length(lvls),
               "levels. The defaultSummary() function isn't appropriate."))
  
  if (!all(levels(data[, "pred"]) == lvls))
    stop("classSummary:: levels of observed and predicted data do not match")
  
  data$y = as.numeric(data$obs == lvls[2])
  data$z = as.numeric(data$pred == lvls[2])
  
  confMat <- caret::confusionMatrix(table(data$z, data$y),positive="1")
  fn_tcr_9 <- (9*confMat$table[1,2] + confMat$table[2,1]) / (9 * (confMat$table[1,2] + confMat$table[2,2]) + confMat$table[2,1] + confMat$table[1,1] )
  mcc <- mltools::mcc(TP=confMat$table[1,1],FP=confMat$table[1,2],TN=confMat$table[2,2],FN=confMat$table[2,1])
  ppv <- (confMat$table[1,1] / (confMat$table[1,1] + confMat$table[1,2]) )
  out <- c(confMat$overall['Kappa'],confMat$overall['Accuracy'],fn_tcr_9, mcc, ppv)
  names(out) <- c("Kappa","Accuracy","TCR_9", "MCC", "PPV")
  out
}

addEntry <- function(modelName, modelDesc, metric, parameters){
  tunnedModels <<- rbind(tunnedModels,data.frame(model=modelName,description=modelDesc,
                                                 metric=metric,values=I(list(parameters))))
}

getModelbyMetric <- function(model, metric){
  return( model$results[best(model$results,metric=metric, maximize=TRUE), ][[metric]] )
}

saveModel <- function(model,modelName,metric, path) {
  saveRDS(model,paste0(path, paste0(modelName,".rds")) )
  cat(metric,names(unlist(lapply(model$bestTune,paste))), sep=",", 
      file=paste0(path,paste0(modelName,".csv") ), 
      fill=TRUE, append=FALSE)
  cat(getModelbyMetric(model,metric), 
      unlist(lapply(model$bestTune,paste, sep=",")), sep=",", 
      file=paste0(path,paste0(modelName,".csv")), 
      fill=TRUE, append=TRUE)
}

saveModelObject <- function(model,modelName,metric,path){
  cat(metric,names(unlist(lapply(model$bestTune,paste))), sep=",", 
      file=paste0(path, modelName,".csv",sep=""), 
      fill=TRUE, append=FALSE )
  cat(getModelbyMetric(model,metric),
      unlist(lapply(model$bestTune,paste, sep=",")), sep=",", 
      file=paste0(path,modelName,".csv",sep=""), 
      fill=TRUE, append=TRUE )
}

tunnedModels <- NA

benchmarck <- function(env, metric, dataset, classIndex, folds, dir, path){
  tunnedModels <<- data.frame(model=character(), description=character(), 
                              metric=numeric(), values=I(list()), 
                              stringsAsFactors=FALSE)
  
  cat("\tChecking directory to store data: \n")
  if(file.exists(paste(dir,path,"/",sep="/", collapse="/"))){
    cat("\tFolder ",path," exists in ",dir," directory\n")
  }else{
    cat("\tFolder ",path," does not exist in ", 
        dir, "directory - Creating\n")
    dir.create(file.path(dir,path))
  }
  
  classNames <- names(dataset[,classIndex])
  
  fullPath <- paste0(dir,"/",path,"/")
  
  cat("\tBuilding best-performance models for classifiers...\n")  
  if (!dir.exists(file.path( fullPath ))){ 
    dir.create(file.path( fullPath ), showWarnings = FALSE)
    cat("\tPath for storing classifiers information not exists. Creating path at: ", fullPath,"\n", sep="")
  }else cat("\tPath for storing classifiers already exists. Accesing at: ",fullPath,"\n", sep="")
  
  tcTwoClass.grid <- trainControl(method="cv", number=folds, savePredictions="final", 
                                  classProbs=TRUE, summaryFunction=twoClassSummary, search="grid", 
                                  allowParallel=TRUE, verboseIter=FALSE)
  tcTwoClass.random <- trainControl(method="cv", number=folds, savePredictions="final", classProbs=TRUE, 
                                    summaryFunction=twoClassSummary, search="random", allowParallel=TRUE, 
                                    verboseIter=FALSE)
  noProbs.grid <- trainControl(method="cv", number=folds, savePredictions="final", 
                               summaryFunction=noProbsSummary, search="grid", 
                               allowParallel=TRUE, verboseIter=FALSE)
  
  model.formula <- as.formula(paste(sprintf("`%s`",names(dataset[classIndex]))," ~ ."))
  model.recipe <- recipe(model.formula, data=dataset) %>%
    step_zv(all_predictors()) %>% step_nzv(all_predictors()) %>% step_corr(all_predictors()) %>% 
    step_center(all_predictors()) %>% step_scale(all_predictors())
  
  ##BEGIN: Random Forest Models
  cat("\t[1] Random Forest Models\n")
  cat("\t\t[1.1] Random Forest\n")
  if(!file.exists( paste0( fullPath ,"ranger.rds")) ){
    cat("\t\t\tModel 'ranger' not exists. Training ...")
    start <-Sys.time()
    model.fit <- train(model.formula,data=dataset, method="ranger", 
                       trControl=tcTwoClass.grid, metric=metric) 
    stop <- Sys.time()
    cat(paste0("ranger,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    cat("... OK\n\t\t\tSuccesfully trained: Model 'ranger' saved at:",
        paste0(fullPath,"ranger.rds"),"\n")
    saveModel(model.fit,"ranger",metric,fullPath)
    addEntry("ranger","Random Forest for High Dimensional Data",
             getModelbyMetric(model.fit,metric),model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'ranger' already exists. Loading ...")
    if( !file.exists(paste0(fullPath, "ranger.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"ranger.rds"))
      
      saveModelObject(model.fit,"ranger",metric,fullPath)
      addEntry("ranger","Random Forest for High Dimensional Data", 
               getModelbyMetric(model.fit,metric), model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'ranger' loaded succesfully at:",
          paste0(fullPath,"ranger.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath, "ranger.csv"),header=TRUE, sep=",", dec=".")
      addEntry("ranger","Random Forest for High Dimensional Data",
               model.info[[metric]],list(model.info[,2:ncol(model.info)]))
      cat("... OK\n\t\t\tModel 'ranger' loaded succesfully at:",
          paste0(fullPath,"ranger.csv"),"\n")
    }
  }
  
  # cat("\t\t[1.2] Conditional Inference Random Forest\n")
  # if(!file.exists( paste0( fullPath ,"cforest.rds")) ){
  #   cat("\t\t\tModel 'cforest' not exists. Training ...")
  #   model.fit <- train(model.recipe,data=dataset, method="cforest",
  #                      trControl=tcTwoClass.random, metric=metric) 
  #   cat("... OK\n\t\t\tSuccesfully trained: Model 'cforest' saved at:",
  #       paste0(fullPath,"cforest.rds"),"\n")
  #   saveModel(model.fit,"cforest",metric, fullPath)
  #   addEntry("cforest","Conditional Inference Random Forest",
  #            getModelbyMetric(model.fit,metric), model.fit$bestTune)
  # }else{
  #   cat("\t\t\tModel 'cforest' already exists. Loading ...")
  #   if( !file.exists(paste0(fullPath, "cforest.csv")) ){
  #     model.fit <- readRDS(paste0(fullPath,"cforest.rds"))
  #     saveModelObject(model.fit,"cforest",metric,fullPath)
  #     addEntry("cforest","Conditional Inference Random Forest", 
  #              getModelbyMetric(model.fit,metric), model.fit$bestTune)
  #     cat("... OK\n\t\t\tModel 'cforest' loaded succesfully at:",
  #         paste0(fullPath,"cforest.rds"),"\n")
  #   }else{
  #     model.info <- read.csv(paste0(fullPath, "cforest.csv"),header=TRUE, sep=",", dec=".")
  #     addEntry("cforest","Conditional Inference Random Forest", 
  #              model.info[[metric]],list(model.info[,2:ncol(model.info)]))
  #     cat("... OK\n\t\t\tModel 'cforest' loaded succesfully at:",
  #         paste0(fullPath,"cforest.csv"),"\n")
  #   }
  # }
  ##END: Random Forest Models
  
  ##BEGIN: Clustering methods
  cat("\t[2] Clustering Models\n") 
  cat("\t\t[2.1] K-Nearest Neighbours\n")
  if(!file.exists( paste0(fullPath,"knn.rds")) ){
    cat("\t\t\tModel 'knn' not exists. Training ...")
    start <- Sys.time()
    model.fit <- train(model.recipe,data=dataset, method="knn",
                       trControl=tcTwoClass.grid, metric=metric) 
    stop <- Sys.time()
    cat(paste0("knn,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    cat("... OK\n\t\t\tSuccesfully trained. Model 'knn' saved at:",
        paste0(fullPath,"knn.rds"),"\n")
    saveModel(model.fit,"knn",metric,fullPath)
    addEntry("knn","K-Nearest Neighbours", getModelbyMetric(model.fit,metric), 
             model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'knn' already exists. Loading ...")
    if( !file.exists(paste0(fullPath,"knn.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"knn.rds"))
      saveModelObject(model.fit,"knn",metric,fullPath)
      addEntry("knn","K-Nearest Neighbours", getModelbyMetric(model.fit,metric), 
               model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'knn' loaded succesfully at:",paste0(fullPath,"knn.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"knn.csv"),header=TRUE, sep=",", dec=".")
      addEntry("knn","K-Nearest Neighbours", model.info[[metric]], 
               list(model.info[,2:ncol(model.info)]))
      cat("... OK\n\t\t\tModel 'knn' loaded succesfully at:",paste0(fullPath,"knn.csv"),"\n")
    }
  }
  
  cat("\t\t[2.2] Latent Dirichlet Allocation\n")
  if(!file.exists( paste0(fullPath,"lda.rds")) ){
    cat("\t\t\tModel 'lda' not exists. Training ...")
    start <- Sys.time()
    model.fit <- train(model.recipe, data=dataset, method="lda", 
                       trControl=tcTwoClass.grid, metric=metric)
    stop <- Sys.time()
    cat(paste0("lda,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    cat("... OK\n\t\t\tSuccesfully trained: Model 'lda' saved at:", paste0(fullPath,"lda.rds"),"\n")
    saveModel(model.fit,"lda",metric,fullPath)
    addEntry("lda","Latent Dirichlet Allocation", getModelbyMetric(model.fit,metric), 
             model.fit$bestTune)	
  }else{
    cat("\t\t\tModel 'lda' already exists. Loading ...")
    if ( !file.exists(paste0(fullPath,"lda.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"lda.rds"))
      saveModelObject(model.fit,"lda",metric,fullPath)
      addEntry("lda","Latent Dirichlet Allocation", getModelbyMetric(model.fit,metric),NA)
      cat("... OK\n\t\t\tModel 'lda' loaded succesfully at:",paste0(fullPath,"lda.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"lda.csv"),header=TRUE, sep=",", dec=".")
      addEntry("lda","Latent Dirichlet Allocation",model.info[[metric]], NA)
      cat("... OK\n\t\t\tModel 'lda' loaded succesfully at:",paste0(fullPath,"lda.csv"),"\n")
    }
  }
  ##END: Clustering methods
  
  ##BEGIN: Linear Methods
  cat("\t[3] Linear Models\n")   
  cat("\t\t[3.1] Regularized Generalized Linear Models\n")
  if(!file.exists( paste0(fullPath,"glmnet.rds")) ){
    cat("\t\t\tModel 'glmnet' not exists. Training ...")
    train <- Sys.time()
    model.fit <- train(model.recipe,data=dataset, method="glmnet",
                       trControl=tcTwoClass.grid,family="binomial", metric=metric) 
    stop <- Sys.time()
    cat(paste0("glmnet,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained. Model 'glmnet' saved at:",
        paste0(fullPath,"glmnet.rds"),"\n")
    saveModel(model.fit,"glmnet",metric,fullPath)
    addEntry("glmnet","Regularized Generalized Linear Models",
             getModelbyMetric(model.fit,metric),model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'glmnet' already exists. Loading ...")
    if( !file.exists(paste0(fullPath,"glmnet.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"glmnet.rds"))
      saveModelObject(model.fit,"glmnet",metric,fullPath)
      addEntry("glmnet","Regularized Generalized Linear Models",
               getModelbyMetric(model.fit,metric),model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'glmnet' loaded succesfully at:",paste0(fullPath,"glmnet.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"glmnet.csv"),header=TRUE, sep=",", dec=".")
      addEntry("glmnet","Regularized Generalized Linear Models",
               model.info[[metric]], list(model.info[,2:ncol(model.info)]))
      cat("... OK\n\t\t\tModel 'glmnet' loaded succesfully at:",paste0(fullPath,"glmnet.csv"),"\n")
    }
  }
  
  cat("\t\t[3.2] Bayesian Generalized Linear Model\n")
  if(!file.exists( paste0(fullPath,"bayesglm.rds")) ){
    cat("\t\t\tModel 'bayesglm' not exists. Training ...")
    
    start <- Sys.time()
    model.fit <- train(model.recipe, data=dataset, method="bayesglm", 
                       trControl=tcTwoClass.random, metric=metric)
    stop <- Sys.time()
    cat(paste0("bayesglm,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'bayesglm' saved at:",
        paste0(fullPath,"bayesglm.rds"),"\n")
    saveModel(model.fit,"bayesglm",metric,fullPath)
    addEntry("bayesglm","Bayesian Generalized Linear Model", 
             getModelbyMetric(model.fit,metric),NA)
  }else{
    cat("\t\t\tModel 'bayesglm' already exists. Loading ...")
    if( !file.exists(paste0(fullPath,"bayesglm.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"bayesglm.rds"))
      saveModelObject(model.fit,"bayesglm",metric,fullPath)
      addEntry("bayesglm","Bayesian Generalized Linear Model",
               getModelbyMetric(model.fit,metric),NA)	  
      cat("... OK\n\t\t\tModel 'bayesglm' loaded succesfully at:",paste0(fullPath,"bayesglm.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"bayesglm.csv"),header=TRUE, sep=",", dec=".")
      addEntry("bayesglm","Bayesian Generalized Linear Model",model.info[[metric]],NA)
      cat("... OK\n\t\t\tModel 'bayesglm' loaded succesfully at:",paste0(fullPath,"bayesglm.csv"),"\n")
    }
  }
  
  cat("\t\t[3.3] Penalized Multinomial Regression\n")
  if(!file.exists( paste0(fullPath,"multinom.rds")) ){
    cat("\t\t\tModel 'multinom' not exists. Training ...")
    start <- Sys.time()
    model.fit <- train(model.recipe,data=dataset, method="multinom",trControl=tcTwoClass.random, 
                       trace=FALSE, na.action="na.omit", MaxNWts=3000, metric=metric) 
    
    stop <- Sys.time()
    cat(paste0("multinom,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'multinom' saved at:",
        paste0(fullPath,"multinom.rds"),"\n")
    saveModel(model.fit,"multinom",metric, fullPath)
    addEntry("multinom","Penalized Multinomial Regression",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)	
  }else{
    cat("\t\t\tModel 'multinom' already exists. Loading ...")
    if ( !file.exists(paste0(fullPath,"multinom.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"multinom.rds"))
      # cat(metric, names(unlist(lapply(model.fit$bestTune,paste))), sep=",", 
      #     file=paste0(fullPath,"multinom.csv"), fill=TRUE, append=FALSE)
      # cat(getModelbyMetric(model.fit,metric),
      #     unlist(lapply(model.fit$bestTune,paste, sep=",")), sep=",", 
      #     file=paste0(fullPath,"multinom.csv"), fill=TRUE, append=TRUE)
      saveModelObject(model.fit,"multinom",metric,fullPath)
      addEntry("multinom","Penalized Multinomial Regression", 
               getModelbyMetric(model.fit,metric), model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'multinom' loaded succesfully at:",paste0(fullPath,"multinom.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"multinom.csv"),header=TRUE, sep=",", dec=".")
      addEntry("multinom","Penalized Multinomial Regression",
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'multinom' loaded succesfully at:",paste0(fullPath,"multinom.csv"),"\n")
    }
  }
  ##END: Linear Models  
  
  ##BEGIN: SVM Machines
  cat("\t[4] Suppor Vector Machines\n")   
  cat("\t\t[4.1] SVM with Radial Kernel\n")
  if(!file.exists( paste0(fullPath,"svmRadial.rds")) ){
    cat("\t\t\tModel 'svmRadial' not exists. Training ...")
    start <- Sys.time()
    model.fit <- train(model.recipe,data=dataset, method="svmRadial",trControl=tcTwoClass.random, 
                       verbose=FALSE, metric=metric) 
    
    stop <- Sys.time()
    cat(paste0("svmRadial,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'svmRadial' saved at:",paste0(fullPath,"svmRadial.rds"),"\n")
    saveModel(model.fit,"svmRadial",metric,fullPath)
    addEntry("svmRadial","Support Vector Machines with Radial Kernel",
             getModelbyMetric(model.fit,metric),model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'svmRadial' already exists. Loading ...")
    if (!file.exists( paste0(fullPath,"svmRadial.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"svmRadial.rds"))
      saveModelObject(model.fit,"svmRadial",metric,fullPath)
      addEntry("svmRadial","Support Vector Machines with Radial Kernel", 
               getModelbyMetric(model.fit,metric), model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'svmRadial' loaded succesfully at:",paste0(fullPath,"svmRadial.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"svmRadial.csv"),header=TRUE, sep=",", dec=".")
      addEntry("svmRadial","Support Vector Machines with Radial Kernel", 
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'svmRadial' loaded succesfully at:",paste0(fullPath,"svmRadial.csv"),"\n")
    }
  }
  
  cat("\t\t[4.2] SVM with Linear Kernel\n")
  if(!file.exists( paste0(fullPath,"svmLinear.rds")) ){
    cat("\t\t\tModel 'svmLinear' not exists. Training ...")
    
    start <- Sys.time()
    
    model.fit <- train(model.recipe,data=dataset, method="svmLinear",trControl=tcTwoClass.grid, 
                       verbose=FALSE, metric=metric) 
    stop <- Sys.time()
    cat(paste0("svmLineal,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'svmLinear' saved at:",
        paste0(fullPath,"svmLinear.rds"),"\n")
    saveModel(model.fit,"svmLinear",metric,fullPath)
    addEntry("svmLinear","SVM Linear Kernel",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)	
  }else{
    cat("\t\t\tModel 'svmLinear' already exists. Loading ...")
    if( !file.exists( paste0(fullPath, "svmLinear.rds")) ){
      model.fit <- readRDS(paste0(fullPath, "svmLinear.rds"))
      saveModelObject(model.fit,"svmLinear",metric,fullPath)
      addEntry("svmLinear","SVM Linear Kernel", 
               getModelbyMetric(model.fit,metric), model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'svmLinear' loaded succesfully at:", 
          paste0(fullPath,"svmLinear.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"svmLinear.csv"),header=TRUE, sep=",", dec=".")
      addEntry("svmLinear","SVM Linear Kernel",model.info[[metric]],
               model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'svmLinear' loaded succesfully at:",paste0(fullPath,"svmLinear.csv"),"\n")
    }
  }
  
  cat("\t\t[4.3] SVM with Class Weights\n")
  if(!file.exists( paste0(fullPath,"svmRadialWeights.rds")) ){
    
    start <- Sys.time()
    cat("\t\t\tModel 'svmRadialWeights' not exists. Training ...")
    model.fit <- train(model.recipe,data=dataset, method="svmRadialWeights", 
                       trControl=tcTwoClass.random,verbose=FALSE, metric=metric) 
    
    stop <- Sys.time()
    cat(paste0("svmRadialWeights,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'svmRadialWeights' saved at: ",
        paste0(fullPath,"svmRadialWeights.rds"),"\n")
    saveModel(model.fit,"svmRadialWeights",metric,fullPath)
    addEntry("svmRadialWeights","Support Vector Machines with Class Weights", 
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'svmRadialWeights' already exists. Loading ...")
    if ( !file.exists( paste0(fullPath,"svmRadialWeights.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"svmRadialWeights.rds"))
      saveModelObject(model.fit,"svmRadialWeights",metric,fullPath)
      addEntry("svmRadialWeights","Support Vector Machines with Class Weights", 
               getModelbyMetric(model.fit,metric), model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'svmRadialWeights' loaded succesfully at:",
          paste0(fullPath,"svmRadialWeights.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"svmRadialWeights.csv"),header=TRUE, sep=",", dec=".")
      addEntry("svmRadialWeights","Support Vector Machines with Class Weights", 
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'svmRadialWeights' loaded succesfully at:",
          paste0(fullPath,"svmRadialWeights.csv"),"\n")
    }
  }
  
  cat("\t\t[4.4] SVM with Polynomial Kernel\n")
  if(!file.exists( paste0(fullPath,"svmPoly.rds")) ){
    cat("\t\t\tModel 'svmPoly' not exists. Training ...")
    
    start <- Sys.time()
    model.fit <- train(model.recipe,data=dataset, method="svmPoly", 
                       trControl=tcTwoClass.grid, verbose=FALSE, metric=metric) 
    stop <- Sys.time()
    cat(paste0("svmPoly,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'svmPoly' saved at: ",
        paste0(fullPath,"svmPoly.rds"),"\n")
    saveModel(model.fit,"svmPoly",metric,fullPath)
    addEntry("svmPoly","Support Vector Machines with Polynomial Kernel",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'svmPoly' already exists. Loading ...")
    if ( !file.exists( paste0(fullPath,"svmPoly.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"svmPoly.rds"))
      saveModelObject(model.fit,"svmPoly",metric,fullPath)
      addEntry("svmPoly","Support Vector Machines with Polynomial Kernel", 
               getModelbyMetric(model.fit,metric), model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'svmPoly' loaded succesfully at:",
          paste0(fullPath,"svmPoly.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"svmPoly.csv"),header=TRUE, sep=",", dec=".")
      addEntry("svmPoly","Support Vector Machines with Polynomial Kernel",
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'svmPoly' loaded succesfully at:",
          paste0(fullPath,"svmPoly.csv"),"\n")
    }
  }
  ##END: SVM Machines
  
  ##BEGIN: Boosting/Bagging Models
  cat("\t[5] Boosting and Bagging Models\n")   
  cat("\t\t[5.1] Adaboost\n")
  if(!file.exists( paste0(fullPath,"adaboost.rds")) ){
    cat("\t\t\tModel 'adaboost' not exists. Training ...")
    
    start <- Sys.time()
    model.fit <- train(model.recipe, data=dataset, method="adaboost", 
                       trControl=tcTwoClass.grid, metric=metric)
    stop <- Sys.time()
    cat(paste0("adaboost,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    cat("... OK\n\t\t\tSuccesfully trained: Model 'adaboost' saved at:", 
        paste0(fullPath,"adaboost.rds"),"\n")
    saveModel(model.fit,"adaboost",metric,fullPath)
    addEntry("adaboost","Fast Adaboost",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'adaboost' already exists. Loading ...")
    if( !file.exists(paste0(fullPath,"adaboost.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"adaboost.rds"))
      saveModelObject(model.fit,"adaboost",metric,fullPath)
      addEntry("adaboost","Fast Adaboost", 
               getModelbyMetric(model.fit,metric), model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'adaboost' loaded succesfully at:",paste0(fullPath,"adaboost.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"adaboost.csv"),header=TRUE, sep=",", dec=".")
      addEntry("adaboost","Fast Adaboost", 
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'adaboost' loaded succesfully at:",paste0(fullPath,"adaboost.csv"),"\n")
    }
  }
  
  cat("\t\t[5.2] Asymetric Adaboost\n")
  if(!file.exists( paste0(fullPath,"ada.rds")) ){
    cat("\t\t\tModel 'ada' not exists. Training ...")
    
    start <- Sys.time()
    model.fit <- train(model.recipe, data=dataset, method="ada", 
                       trControl=tcTwoClass.grid, metric=metric)
    
    stop <- Sys.time()
    cat(paste0("ada,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'ada' saved at: ",
        paste0(fullPath,"ada.rds"),"\n")
    saveModel(model.fit,"ada",metric,fullPath)
    addEntry("ada","Asymetric Adaboost",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'ada' already exists. Loading ...")
    if( !file.exists( paste0(fullPath,"ada.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"ada.rds"))
      saveModelObject(model.fit,"ada",metric,fullPath)
      addEntry("ada","Asymetric Adaboost",
               getModelbyMetric(model.fit,metric), model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'ada' loaded succesfully at:",paste0(fullPath,"ada.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"ada.csv"),header=TRUE, sep=",", dec=".")
      addEntry("ada","Asymetric Adaboost", 
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'ada' loaded succesfully at:",paste0(fullPath,"ada.csv"),"\n")
    }
  }
  
  cat("\t\t[5.3] Bagged AdaBoost\n")
  if(!file.exists( paste0(fullPath,"AdaBag.rds")) ){
    cat("\t\t\tModel 'AdaBag' not exists. Training ...")
    
    start <- Sys.time()
    model.fit <- train(model.recipe, data=dataset, method="AdaBag", 
                       trControl=tcTwoClass.grid, metric=metric)
    stop <- Sys.time()
    cat(paste0("AdaBag,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'AdaBag' saved at: ",
        paste0(fullPath,"AdaBag.rds"),"\n")
    saveModel(model.fit,"AdaBag",metric,fullPath)
    addEntry("AdaBag","Bagged AdaBoost",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'AdaBag' already exists. Loading ...")
    if ( !file.exists( paste0(fullPath,"AdaBag.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"AdaBag.rds"))
      saveModelObject(model.fit,"AdaBag",metric,fullPath)
      addEntry("AdaBag","Bagged AdaBoost",
               getModelbyMetric(model.fit,metric), model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'AdaBag' loaded succesfully at:",
          paste0(fullPath,"AdaBag.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"AdaBag.csv"),
                             header=TRUE, sep=",", dec=".")
      addEntry("AdaBag","Bagged AdaBoost",
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'AdaBag' loaded succesfully at:",
          paste0(fullPath,"AdaBag.csv"),"\n")
    }
  }
  
  cat("\t\t[5.4] Bagged CART\n")
  if(!file.exists( paste0(fullPath,"treebag.rds")) ){
    cat("\t\t\tModel 'treebag' not exists. Training ...")
    start <- Sys.time()
    model.fit <- train(model.recipe, data=dataset, method="treebag", 
                       trControl=tcTwoClass.grid, metric=metric)
    stop <- Sys.time()
    cat(paste0("treebag,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'treebag' saved at: ",
        paste0(fullPath,"treebag.rds"),"\n")
    
    saveModel(model.fit,"treebag",metric,fullPath)
    addEntry("treebag","Bagged CART", getModelbyMetric(model.fit,metric),NA)
  }else{
    cat("\t\t\tModel 'treebag' already exists. Loading ...")
    if( !file.exists( paste0(fullPath,"treebag.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"treebag.rds"))
      saveModelObject(model.fit,"treebag",metric,fullPath)
      addEntry("treebag","Bagged CART", getModelbyMetric(model.fit,metric),NA)	  
      cat("... OK\n\t\t\tModel 'treebag' loaded succesfully at:",
          paste0(fullPath,"treebag.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"treebag.csv"),header=TRUE, sep=",", dec=".")
      addEntry("treebag","Bagged CART",model.info[[metric]],NA)      
      cat("... OK\n\t\t\tModel 'treebag' loaded succesfully at:",paste0(fullPath,"treebag.csv"),"\n")
    }
  }
  
  cat("\t\t[5.5] Gradient Boosting Machines\n")
  if(!file.exists( paste0(fullPath,"gbm.rds")) ){
    cat("\t\t\tModel 'gbm' not exists. Training ...")
    
    start <- Sys.time()
    model.fit <- train(model.recipe,data=dataset, method="gbm",trControl=tcTwoClass.grid, 
                       verbose=FALSE, metric=metric)
    
    stop <- Sys.time()
    cat(paste0("gbm,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained. Model 'gbm' saved at:",paste0(fullPath,"gbm.rds"),"\n")
    saveModel(model.fit,"gbm",metric,fullPath)
    addEntry("gbm","Gradient Bossting Machines",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel already exists. Loading ...")
    if( !file.exists( paste0(fullPath,"gbm.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"gbm.rds"))
      saveModelObject(model.fit,"gbm",metric,fullPath)
      addEntry("gbm","Gradient Bossting Machines", 
               getModelbyMetric(model.fit,metric), model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'gbm' loaded succesfully at:",paste0(fullPath,"gbm.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"gbm.csv"),header=TRUE, sep=",", dec=".")
      addEntry("gbm","Gradient Bossting Machines",
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'gbm' loaded succesfully at:",paste0(fullPath,"gbm.csv"),"\n")
    }
  }
  
  cat("\t\t[5.6] Extreme Gradient Boosting\n")
  if(!file.exists( paste0(fullPath,"xgbTree.rds")) ){
    cat("\t\t\tModel 'xgbTree' not exists. Training ...")
    start <- Sys.time()
    model.fit <- train(model.recipe,data=dataset, method="xgbTree",trControl=tcTwoClass.grid, 
                       verbose=FALSE, metric=metric)
    
    stop <- Sys.time()
    cat(paste0("xgbTree,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained. Model 'xgbTree' saved at: ",paste0(fullPath,"xgbTree.rds"),"\n")
    saveModel(model.fit,"xgbTree",metric,fullPath)
    addEntry("xgbTree","Extreme Gradient Boosting Trees",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel already exists. Loading ...")
    if( !file.exists( paste0(fullPath,"xgbTree.csv")) ){ 
      model.fit <- readRDS(paste0(fullPath,"xgbTree.rds"))
      saveModelObject(model.fit,"xgbTree",metric,fullPath)
      addEntry("xgbTree","Extreme Gradient Boosting Trees", 
               getModelbyMetric(model.fit,metric), model.fit$bestTune)	  
      cat("... OK\n\t\t\tModel 'xgbTree' loaded succesfully at:",
          paste0(fullPath,"xgbTree.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"xgbTree.csv"),header=TRUE, sep=",", dec=".")
      addEntry("xgbTree","Extreme Gradient Boosting Trees",
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'xgbTree' loaded succesfully at:",
          paste0(fullPath,"xgbTree.csv"),"\n")
    }
  }
  
  cat("\t\t[5.7] Gradient Boosting With Component-Wise Linear Models\n")
  if(!file.exists( paste0(fullPath,"glmboost.rds")) ){
    cat("\t\t\tModel 'glmboost' not exists. Training ...")
    
    start <- Sys.time()
    model.fit <- train(model.recipe,data=dataset, method="glmboost",
                       trControl=tcTwoClass.grid, metric=metric)
    
    stop <- Sys.time()
    cat(paste0("glmboost,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained. Model 'glmboost' saved at: ",
        paste0(fullPath,"glmboost.rds"),"\n")
    saveModel(model.fit,"glmboost",metric,fullPath)
    addEntry("glmboost","Gradient Boosting With Component-Wise Linear Models",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)	
  }else{
    cat("\t\t\tModel already exists. Loading ...")
    if( !file.exists( paste0(fullPath,"glmboost.csv")) ){ 
      model.fit <- readRDS(paste0(fullPath,"glmboost.rds"))
      saveModelObject(model.fit,"glmboost",metric,fullPath)
      addEntry("glmboost","Gradient Boosting With Component-Wise Linear Models",
               getModelbyMetric(model.fit,metric), model.fit$bestTune)	
      cat("... OK\n\t\t\tModel 'glmboost' loaded succesfully at:",
          paste0(fullPath,"glmboost.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"glmboost.csv"),header=TRUE, sep=",", dec=".")
      addEntry("glmboost","Gradient Boosting With Component-Wise Linear Models",
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'glmboost' loaded succesfully at:",
          paste0(fullPath,"glmboost.csv"),"\n")
    }
  }
  ##END: Boosting/Bagging Models
  
  ##BEGIN: TREE Models
  cat("\t[6] Tree Models\n")   
  cat("\t\t[6.1] J48 Trees\n") ##TESTED
  if(!file.exists( paste0(fullPath,"J48.rds")) ){
    cat("\t\t\tModel 'J48' not exists. Training ...")
    
    start <- Sys.time()
    model.fit <- train(model.recipe,data=dataset, method="J48",
                       trControl=tcTwoClass.grid,
                       na.action=na.pass, metric=metric) 
    stop <- Sys.time()
    cat(paste0("J48,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'J48' saved at: ",
        paste0(fullPath,"J48.rds"),"\n")
    #.jcache(model.fit$finalModel)
    saveModel(model.fit,"J48",metric,fullPath)
    addEntry("J48","J48 Trees",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)		
  }else{
    cat("\t\t\tModel 'J48' already exists. Loading ...")
    if( !file.exists( paste0(fullPath,"J48.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"J48.rds"))
      saveModelObject(model.fit,"J48",metric,fullPath)
      addEntry("J48","J48 Trees", getModelbyMetric(model.fit,metric), model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'J48' loaded succesfully at:",paste0(fullPath,"J48.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"J48.csv"),header=TRUE, sep=",", dec=".")
      addEntry("J48","J48 Trees",model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'J48' loaded succesfully at:",paste0(fullPath,"J48.csv"),"\n")
    }
  }
  
  cat("\t\t[6.2] CART - RPART1SE\n")
  if(!file.exists( paste0(fullPath,"rpart1SE.rds")) ){
    cat("\t\t\tModel 'rpart1SE' not exists. Training ...")
    
    start <- Sys.time()
    cart.fit <- train(model.recipe,data=dataset, method="rpart1SE", trControl=tcTwoClass.grid, metric=metric)
    
    stop <- Sys.time()
    cat(paste0("rpart1SE,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'rpart1SE' saved at: ",
        paste0(fullPath,"rpart1SE.rds"),"\n")
    saveModel(cart.fit,"rpart1SE",metric,fullPath)
    addEntry("rpart1SE","CART - RPART1SE' Trees",
             getModelbyMetric(cart.fit,metric),cart.fit$bestTune)
  }else{
    cat("\t\t\tModel 'CART - RPART1SE' already exists. Loading ...")
    if( !file.exists( paste0(fullPath,"rpart1SE.csv")) ){
      cart.fit <- readRDS(paste0(fullPath,"rpart1SE.rds"))
      saveModelObject(cart.fit,"rpart1SE",metric,fullPath)
      addEntry("rpart1SE","CART Trees",
               getModelbyMetric(cart.fit,metric),cart.fit$bestTune)
      cat("... OK\n\t\t\tModel 'CART - RPART1SE' loaded succesfully at:",
          paste0(fullPath,"rpart1SE.rds"),"\n")
    }else{
      cart.fit <- read.csv(paste0(fullPath,"rpart1SE.csv"), header=TRUE, sep=",", dec=".")
      addEntry("rpart1SE","CART - RPART1SE Trees",
               cart.fit[[metric]],model.info[,2:ncol(cart.fit)])
      cat("... OK\n\t\t\tModel 'CART - RPART1SE' loaded succesfully at:", 
          paste0(fullPath,"rpart1SE.csv"),"\n")
    }
  }
  
  cat("\t\t[6.3] CART - RPART2\n")
  if(!file.exists( paste0(fullPath,"rpart2.rds")) ){
    cat("\t\t\tModel 'rpart2' not exists. Training ...")
    
    start <- Sys.time()
    cart.fit <- train(model.recipe,data=dataset, method="rpart2", 
                      trControl=tcTwoClass.grid, metric=metric)
    stop <- Sys.time()
    cat(paste0("rpart2,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'rpart2' saved at: ",
        paste0(fullPath,"rpart2.rds"),"\n")
    saveModel(cart.fit,"rpart2",metric,fullPath)
    addEntry("rpart2","CART - RPART2 Trees",
             getModelbyMetric(cart.fit,metric),cart.fit$bestTune)
  }else{
    cat("\t\t\tModel 'CART - RPART2' already exists. Loading ...")
    if( !file.exists( paste0(fullPath,"rpart2.csv")) ){
      cart.fit <- readRDS(paste0(fullPath,"rpart2.rds"))
      saveModelObject(model.fit,"rpart2",metric,fullPath)
      addEntry("rpart2","CART - RPART2 Trees", 
               getModelbyMetric(cart.fit,metric),cart.fit$bestTune)
      cat("... OK\n\t\t\tModel 'CART - RPART2' loaded succesfully at:",
          paste0(fullPath,"rpart2.rds"),"\n")
    }else{
      cart.fit <- read.csv(paste0(fullPath,"rpart2.csv"), header=TRUE, sep=",", dec=".")
      addEntry("rpart2","CART Trees",cart.fit[[metric]],
               model.info[,2:ncol(cart.fit)])
      cat("... OK\n\t\t\tModel 'CART - RPART2' loaded succesfully at:",paste0(fullPath,"rpart2.csv"),"\n")
    }
  }
  ##END: TREE Models
  
  ##BEGIN: High Dimensional Data Models
  cat("\t[7] High Dimensional Data Models\n") 
  cat("\t\t[7.1] High Dimensional Discriminant Analysis\n")
  if( !file.exists( paste0(fullPath,"hdda.rds")) ){
    cat("\t\t\tModel 'hdda' not exists. Training ...")
    start <- Sys.time()
    model.fit <- train(model.formula,data=dataset, method="hdda",
                       trControl=tcTwoClass.random, show=FALSE, metric=metric) 
    
    stop <- Sys.time()
    cat(paste0("hdda,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'hdda' saved at: ",paste0(fullPath,"hdda.rds"),"\n")
    
    saveModel(model.fit,"hdda",metric,fullPath)
    addEntry("hdda","High Dimensional Discriminant Analysis",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'hdda' already exists. Loading ...")      
    if( !file.exists( paste0(fullPath,"hdda.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"hdda.rds"))
      saveModelObject(model.fit,"hdda",metric,fullPath)
      addEntry("hdda","High Dimensional Discriminant Analysis",
               getModelbyMetric(model.fit,metric), model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'hdda' loaded succesfully at:",paste0(fullPath,"hdda.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"hdda.csv"),header=TRUE, sep=",", dec=".")
      addEntry("hdda","High Dimensional Discriminant Analysis", 
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'hdda' loaded succesfully at:",paste0(fullPath,"hdda.csv"),"\n")
    }
  }
  
  cat("\t\t[7.2] High Dimensional Regularized Discriminat Analysis\n")
  if( !file.exists( paste0(fullPath,"hdrda.rds")) ){
    cat("\t\t\tModel 'hdrda' not exists. Training ...")
    start <- Sys.time()
    model.fit <- train(model.formula, data=dataset, method="hdrda", trControl=tcTwoClass.random, metric=metric)
    
    stop <- Sys.time()
    cat(paste0("hdrda,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'hdrda' saved at: ",paste0(fullPath,"hdrda.rds"),"\n")
    saveModel(model.fit,"hdrda",metric,fullPath)
    addEntry("hdrda","High Dimensional Regularized Discriminat Analysis",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'hdrda' already exists. Loading ...")
    if( !file.exists( paste0(fullPath,"hdrda.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"hdrda.rds"))
      saveModelObject(model.fit,"hdrda",metric,fullPath)
      addEntry("hdrda","High Dimensional Regularized Discriminat Analysis",
               getModelbyMetric(model.fit,metric), model.fit$bestTune)	  
      cat("... OK\n\t\t\tModel 'hdrda' loaded succesfully at:",paste0(fullPath,"hdrda.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"hdrda.csv"),header=TRUE, sep=",", dec=".")
      addEntry("hdrda","High Dimensional Regularized Discriminat Analysis",
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'hdrda' loaded succesfully at:",paste0(fullPath,"hdrda.csv"),"\n")
    }
  }
  
  cat("\t\t[7.3] Regularized Discriminat Analysis\n")
  if( !file.exists( paste0(fullPath,"rda.rds")) ){
    cat("\t\t\tModel 'rda' not exists. Training ...")
    
    start <- Sys.time()
    
    model.fit <- train(model.recipe, data=dataset, method="rda", 
                       trControl=tcTwoClass.random, metric=metric)
    
    stop <- Sys.time()
    cat(paste0("rda,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'rda' saved at: ",paste0(fullPath,"rda.rds"),"\n")
    saveModel(model.fit,"rda",metric,fullPath)
    addEntry("rda","Regularized Discriminat Analysis", 
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'rda' already exists. Loading ...")
    if( !file.exists( paste0(fullPath,"rda.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"rda.rds"))
      saveModelObject(model.fit,"rda",metric,fullPath)
      addEntry("rda","Regularized Discriminat Analysis",
               getModelbyMetric(model.fit,metric), model.fit$bestTune)	  
      cat("... OK\n\t\t\tModel 'rda' loaded succesfully at:",
          paste0(fullPath,"rda.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"rda.csv"),header=TRUE, sep=",", dec=".")
      addEntry("rda","Regularized Discriminat Analysis",
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'rda' loaded succesfully at:",paste0(fullPath,"rda.csv"),"\n")
    }
  }
  ##END: High Dimensional Data Models
  
  ##BEGIN: Neural Networks
  cat("\t[8] Neural Networks Models\n")   
  cat("\t\t[8.1] Neural Networks\n")
  if(!file.exists( paste0(fullPath,"nnet.rds")) ){
    cat("\t\t\tModel 'nnet' not exists. Training ...")
    
    start <- Sys.time()
    
    model.fit <- train(model.recipe, data=dataset, method="nnet", 
                       MaxNWts=(ncol(dataset)*getDoParWorkers()*10),
                       trControl=tcTwoClass.grid, trace=FALSE, metric=metric)
    
    stop <- Sys.time()
    cat(paste0("nnet,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'nnet' saved at: ",paste0(fullPath,"nnet.rds"),"\n")
    saveModel(model.fit,"nnet",metric,fullPath)
    addEntry("nnet","Neural Networks", 
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'nnet' already exists. Loading ...")
    if( !file.exists( paste0(fullPath,"nnet.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"nnet.rds"))
      saveModelObject(model.fit,"nnet",metric,fullPath)
      addEntry("nnet","Neural Networks",
               getModelbyMetric(model.fit,metric), model.fit$bestTune)		  
      cat("... OK\n\t\t\tModel 'nnet' loaded succesfully at:",
          paste0(fullPath,"nnet.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"nnet.csv"),header=TRUE, sep=",", dec=".")
      addEntry("nnet","Neural Networks",model.info[[metric]],
               model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'nnet' loaded succesfully at:",paste0(fullPath,"nnet.csv"),"\n")
    }
  }
  
  cat("\t\t[8.2] Neural Networks with Feature Extraction\n")
  if(!file.exists( paste0(fullPath,"pcaNNet.rds")) ){
    cat("\t\t\tModel 'pcaNNet' not exists. Training ...")
    
    start <- Sys.time()
    
    model.fit <- train(model.recipe, data=dataset, method="pcaNNet", 
                       MaxNWts=(ncol(dataset)*getDoParWorkers()*10), trControl=tcTwoClass.grid, 
                       trace=FALSE, metric=metric)
    stop <- Sys.time()
    cat(paste0("pcaNNet,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'pcaNNet' saved at: ",paste0(fullPath,"pcaNNet.rds"),"\n")
    saveModel(model.fit,"pcaNNet",metric,fullPath)
    addEntry("pcaNNet","Neural Networks with Feature Extraction",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'pcaNNet' already exists. Loading ...")
    if( !file.exists( paste0(fullPath,"pcaNNet.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"pcaNNet.rds"))
      saveModelObject(model.fit,"pcaNNet",metric,fullPath)
      addEntry("pcaNNet","Neural Networks with Feature Extraction",
               getModelbyMetric(model.fit,metric), model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'pcaNNet' loaded succesfully at:",paste0(fullPath,"pcaNNet.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"pcaNNet.csv"),header=TRUE, sep=",", dec=".")
      addEntry("pcaNNet","Neural Networks with Feature Extraction",model.info[[metric]],
               model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'pcaNNet' loaded succesfully at:",paste0(fullPath,"pcaNNet.csv"),"\n")
    }
  }
  ##END: Neural Networks
  
  ##BEGIN: Probabilistic Models
  cat("\t[9] Probabilistic Models\n")   
  cat("\t\t[9.1] klaR Naive Bayes \n") 
  if( !file.exists( paste0(fullPath,"nb.rds")) ){
    cat("\t\t\tModel 'nb' not exists. Training ...")
    
    start <- Sys.time()
    
    model.fit <- train(model.recipe, data=dataset, method="nb", 
                       trControl=tcTwoClass.grid, metric= metric)
    
    stop <- Sys.time()
    cat(paste0("nb,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'nb' saved at: ",paste0(fullPath,"nb.rds"),"\n")
    saveModel(model.fit,"nb",metric,fullPath)
    addEntry("nb","klaR Naive Bayes Classifier",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'nb' already exists. Loading ...")
    if ( !file.exists( paste0(fullPath,"nb.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"nb.rds"))
      saveModelObject(model.fit,"nb",metric,fullPath)
      addEntry("nb","klaR Naive Bayes Classifier", 
               getModelbyMetric(model.fit,metric),model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'nb' loaded succesfully at:",paste0(fullPath,"nb.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"nb.csv"),header=TRUE, sep=",", dec=".")
      addEntry("nb","klaR Naive Bayes Classifier",
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'nb' loaded succesfully at:",paste0(fullPath,"nb.csv"),"\n")
    }
  }
  
  cat("\t\t[9.2] Naive Bayes \n") 
  if( !file.exists( paste0(fullPath,"naive_bayes.rds")) ){
    cat("\t\t\tModel 'naive_bayes' not exists. Training ...")
    
    start <- Sys.time()
    model.fit <- train(model.recipe, data=dataset, method="naive_bayes", 
                       trControl=tcTwoClass.grid, metric=metric)
    
    stop <- Sys.time()
    cat(paste0("naive_bayes,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'naive_bayes' saved at: ",
        paste0(fullPath,"naive_bayes.rds"),"\n")
    saveModel(model.fit,"naive_bayes",metric,fullPath)
    addEntry("naive_bayes","Naive Bayes Classifier",
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'naive_bayes' already exists. Loading ...")
    if ( !file.exists( paste0(fullPath,"naive_bayes.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"naive_bayes.rds"))
      saveModelObject(model.fit,"naive_bayes",metric,fullPath)
      addEntry("naive_bayes","Naive Bayes Classifier",
               getModelbyMetric(model.fit,metric), model.fit$bestTune)	  
      cat("... OK\n\t\t\tModel 'naive_bayes' loaded succesfully at:",
          paste0(fullPath,"naive_bayes.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"naive_bayes.csv"),header=TRUE, sep=",", dec=".")
      addEntry("naive_bayes","Naive Bayes Classifier",model.info[[metric]],
               model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'naive_bayes' loaded succesfully at:",
          paste0(fullPath,"nb.csv"),"\n")
    }
  }
  ##END: Probabilistic Models
  
  ##BEGIN: Distance Discrimination Models
  cat("\t[10] Distance Discrimintation Models (L1 Regularization)\n")     
  cat("\t\t[10.1] Sparse Distance Weighted Discrimination\n")
  if(!file.exists( paste0(fullPath,"sdwd.rds")) ){
    cat("\t\t\tModel 'sdwd' not exists. Training ...")
    
    start <- Sys.time()
    
    model.fit <- train(model.recipe, data=dataset, method="sdwd", 
                       trControl=tcTwoClass.grid, metric=metric)
    
    stop <- Sys.time()
    cat(paste0("sdwd,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
    
    cat("... OK\n\t\t\tSuccesfully trained: Model 'sdwd' saved at: ",
        paste0(fullPath,"sdwd.rds"),"\n")
    saveModel(model.fit,"sdwd",metric,fullPath)
    addEntry("sdwd","Sparse Distance Weighted Discrimination", 
             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  }else{
    cat("\t\t\tModel 'sdwd' already exists. Loading ...")
    if ( !file.exists( paste0(fullPath,"sdwd.csv")) ){
      model.fit <- readRDS(paste0(fullPath,"sdwd.rds"))
      saveModelObject(model.fit,"sdwd",metric,fullPath)
      addEntry("sdwd","Sparse Distance Weighted Discrimination",
               getModelbyMetric(model.fit,metric), model.fit$bestTune)
      cat("... OK\n\t\t\tModel 'sdwd' loaded succesfully at:",
          paste0(fullPath,"sdwd.rds"),"\n")
    }else{
      model.info <- read.csv(paste0(fullPath,"sdwd.csv"),header=TRUE, sep=",", dec=".")
      addEntry("sdwd","Sparse Distance Weighted Discrimination",
               model.info[[metric]],model.info[,2:ncol(model.info)])
      cat("... OK\n\t\t\tModel 'sdwd' loaded succesfully at:",
          paste0(fullPath,"sdwd.csv"),"\n")
    }
  }
  
  #  cat("\t\t[10.2] Distance Weighted Discrimination with Lineal Basis Function Kernel\n")
  #  if(!file.exists( paste0(fullPath,"dwdLinear.rds")) ){
  #    cat("\t\t\tModel 'dwdLinear' not exists. Training ...")
  #    model.fit <- train(model.recipe, data=dataset, method="dwdLinear", 
  #                       trControl=tcTwoClass.grid, metric=metric)
  #    cat("... OK\n\t\t\tSuccesfully trained: Model 'dwdLineal' saved at: ",
  #        paste0(fullPath,"dwdLinear.rds"),"\n")
  #     saveRDS(model.fit,paste0(fullPath,"dwdLinear.rds"))
  #     cat(metric,names(unlist(lapply(model.fit$bestTune,paste))), sep=",", 
  #         file=paste0(fullPath,"dwdLinear.csv"), fill=TRUE, append=FALSE)
  #     cat(getModelbyMetric(model.fit,metric), unlist(lapply(model.fit$bestTune,paste, sep=",")), sep=",", 
  #         file=paste0(fullPath,"dwdLinear.csv"), fill=TRUE, append=TRUE)
  #    saveModel(model.fit,"dwdLinear",metric,fullPath)
  #    addEntry("dwdLinear","Distance Weighted Discrimination with Lineal Basis Function Kernel",
  #             getModelbyMetric(model.fit,metric), model.fit$bestTune)
  #  }else{
  #    cat("\t\t\tModel 'dwdLinear' already exists. Loading ...")
  #    if ( !file.exists( paste0(fullPath,"dwdLinear.csv")) ){
  #      model.fit <- readRDS(paste0(fullPath,"dwdLinear.rds"))
  #       cat(metric,names(unlist(lapply(model.fit$bestTune,paste))), sep=",", 
  #           file=paste0(fullPath,"dwdLinear.csv"), fill=TRUE, append=FALSE)
  #       cat(getModelbyMetric(model.fit,metric), unlist(lapply(model.fit$bestTune,paste, sep=",")), sep=",", 
  #           file=paste0(fullPath,"dwdLinear.csv"), fill=TRUE, append=TRUE)
  #      saveModelObject(model.fit,"dwdLinear",metric,fullPath)
  #      addEntry("dwdLinear","Distance Weighted Discrimination with Lineal Basis Function Kernel",
  #               getModelbyMetric(model.fit,metric), model.fit$bestTune)
  #      cat("... OK\n\t\t\tModel 'dwdLinear' loaded succesfully at:",paste0(fullPath,"dwdLinear.rds"),"\n")
  #    }else{
  #      model.info <- read.csv(paste0(fullPath,"dwdLinear.csv"),header=TRUE, sep=",", dec=".")
  #      addEntry("dwdLinear","Distance Weighted Discrimination with Lineal Basis Function Kernel",
  #               model.info[[metric]],model.info[,2:ncol(model.info)])
  #      cat("... OK\n\t\t\tModel 'dwdLinear' loaded succesfully at:",paste0(fullPath,"dwdLinear.csv"),"\n")
  #    }
  #  }
  
  ##BEGIN: Rule-Based Models
  cat("\t[11] Rule-Based Models\n") 
  # cat("\t\t[11.1] Random Forest Rule-Based Model\n")
  # if(!file.exists( paste0(fullPath,"rfRules.rds")) ){
  #   cat("\t\t\tModel 'rfRules' not exists. Training ...")
  #   
  #   start <- Sys.time()
  #   
  #   model.fit <- train(model.formula, data=dataset, method="rfRules", trace=FALSE, 
  #                      trControl=noProbs.grid, metric=metric)
  #   
  #   stop <- Sys.time()
  #   cat(paste0("rfRules,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
  #   
  #   cat("... OK\n\t\t\tSuccesfully trained: Model 'rfRules' saved at: ",paste0(fullPath,"rfRules.rds"),"\n")
  #   saveModel(model.fit,"rfRules",metric,fullPath)
  #   addEntry("rfRules","Random Forest Rule-Based Model",
  #            getModelbyMetric(model.fit,metric), model.fit$bestTune)
  # }else{
  #   cat("\t\t\tModel 'rfRules' already exists. Loading ...")
  #   if ( !file.exists( paste0(fullPath,"rfRules.csv")) ){
  #     model.fit <- readRDS(paste0(fullPath,"rfRules.rds"))
  #     saveModelObject(model.fit,"rfRules",metric,fullPath)
  #     addEntry("rfRules","Random Forest Rule-Based Model",
  #              getModelbyMetric(model.fit,metric), model.fit$bestTune)
  #     cat("... OK\n\t\t\tModel 'rfRules' loaded succesfully at:",
  #         paste0(fullPath,"rfRules.rds"),"\n")
  #   }else{
  #     model.info <- read.csv(paste0(fullPath,"rfRules.csv"),header=TRUE, sep=",", dec=".")
  #     addEntry("rfRules","Random Forest Rule-Based Model",model.info[[metric]],
  #              model.info[,2:ncol(model.info)])
  #     cat("... OK\n\t\t\tModel 'rfRules' loaded succesfully at:",
  #         paste0(fullPath,"rfRules.csv"),"\n")
  #   }
  # }  
  # 
  # cat("\t\t[11.2] Rule-Based Classifier\n")
  # if(!file.exists( paste0(fullPath,"JRip.rds")) ){
  #   cat("\t\t\tModel 'JRip' not exists. Training ...")
  #   
  #   start <- Sys.time()
  #   
  #   model.fit <- train(model.formula, data=dataset, method="JRip", trControl=noProbs.grid, metric=metric)
  #   
  #   stop <- Sys.time()
  #   cat(paste0("JRip,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
  #   
  #   cat("... OK\n\t\t\tSuccesfully trained: Model 'JRip' saved at: ",paste0(fullPath,"JRip.rds"),"\n")
  #   #.jcache(model.fit$finalModel)
  #   saveModel(model.fit,"JRip",metric,fullPath)
  #   addEntry("JRip","Rule-Based Classifier",
  #            getModelbyMetric(model.fit,metric), model.fit$bestTune)
  # }else{
  #   cat("\t\t\tModel 'JRip' already exists. Loading ...")
  #   if ( !file.exists( paste0(fullPath,"JRip.csv")) ){
  #     model.fit <- readRDS(paste0(fullPath,"JRip.rds"))
  #     saveModelObject(model.fit,"JRip",metric,fullPath)
  #     addEntry("JRip","JRip Rule-Based Classifier",
  #              getModelbyMetric(model.fit,metric), model.fit$bestTune)
  #     cat("... OK\n\t\t\tModel 'JRip' loaded succesfully at:",
  #         paste0(fullPath,"JRip.rds"),"\n")
  #   }else{
  #     model.info <- read.csv(paste0(fullPath,"JRip.csv"),header=TRUE, sep=",", dec=".")
  #     addEntry("JRip","JRip Rule-Based Classifier",
  #              model.info[[metric]],model.info[,2:ncol(model.info)])
  #     cat("... OK\n\t\t\tModel 'JRip' loaded succesfully at:",
  #         paste0(fullPath,"JRip.csv"),"\n")
  #   }
  # } 
  
  # cat("\t\t[11.3] Rule-Based Classifier\n")
  # if(!file.exists( paste0(fullPath,"PART.rds")) ){
  #   cat("\t\t\tModel 'PART' not exists. Training ...")
  #   
  #   start <- Sys.time()
  #   
  #   model.fit <- train(model.formula, data=dataset, method="PART", 
  #                      trControl=noProbs.grid, metric=metric)
  #   
  #   stop <- Sys.time()
  #   cat(paste0("PART,",stop-start,"\n"),file = paste0(fullPath,"/exec.time"), append = TRUE)
  #   
  #   cat("... OK\n\t\t\tSuccesfully trained: Model 'PART' saved at: ",
  #       paste0(fullPath,"PART.rds"),"\n")
  #   #.jcache(model.fit$finalModel)
  #   saveModel(model.fit,"PART",metric,fullPath)
  #   addEntry("PART","PART Rule-Based Classifier",
  #            getModelbyMetric(model.fit,metric),model.fit$bestTune)
  # }else{
  #   cat("\t\t\tModel 'PART' already exists. Loading ...")
  #   if ( !file.exists( paste0(fullPath,"PART.csv")) ){
  #     model.fit <- readRDS(paste0(fullPath,"PART.rds"))
  #     saveModelObject(model.fit,"PART",metric,fullPath)
  #     addEntry("PART","PART Rule-Based Classifier",
  #              getModelbyMetric(model.fit,metric), model.fit$bestTune)
  #     cat("... OK\n\t\t\tModel 'PART' loaded succesfully at:",
  #         paste0(fullPath,"PART.rds"),"\n")
  #   }else{
  #     model.info <- read.csv(paste0(fullPath,"PART.csv"),header=TRUE, sep=",", dec=".")
  #     addEntry("PART","PART Rule-Based Classifier",
  #              model.info[[metric]],model.info[,2:ncol(model.info)])
  #     cat("... OK\n\t\t\tModel 'PART' loaded succesfully at:",
  #         paste0(fullPath,"PART.csv"),"\n")
  #   }
  # }
  ##END: Rule-Based Models
  names(tunnedModels) <- c("model","description",metric,"values")
  
  
  cat("\t...FINISH\n")
  return(tunnedModels)
}

bestPerformanceClassifiers <- function(classifiers, measure, top){
  cat("\tClassifiers ranking by performance\n")
  if(nrow(classifiers) == 0 ) 
    cat("\t\tClassifiers not loaded. Execute benchmarking first")
  else{
    if(nrow(classifiers) < as.numeric(top) ){ 
      cat("\t\tLess available classifiers than required: '",nrow(classifiers),"'/",top,". Showing all available classifiers\n",sep="")
      top <- nrow(classifiers)
    }else cat("\t\tAvailable/required classifiers: ",nrow(classifiers),"/",top,"\n",sep="")
    
    cat("\t\t","#",measure," Measure","Model Name","Description\n",sep="\t")
    sorted <- classifiers[ order(classifiers[[measure]],decreasing=TRUE), ]
    for (i in 1:nrow(sorted)){
      cat("\t\t",i,sorted[i,3],sorted[i,1],sorted[i,2],"\n",sep="\t")
      if(i == top) cat("\t\t[------------------------------- [X] -------------------------------]\n")
    }
  }
}

plotClassifiers <- function(classifiers, measure, cluster){
  
  summary <- data.frame(model=classifiers$model, measure=classifiers[[measure]])
  min <- data.frame(x=summary[which.min(summary[,2]), ][, 1],y= min(summary[,2]))
  max <- data.frame(x=summary[which.max(summary[,2]), ][, 1],y= max(summary[,2]))
  
  plot <- ggplot(summary, aes(model,measure, group=1)) + geom_line() + geom_point() + 
    geom_point(aes(x,y), min, fill="transparent", color="red", shape=21, size=3,stroke=1) + 
    geom_text(aes(x,y,label=sprintf("%.3f",y)), min, hjust=-0.45, color='red' ) +
    geom_point(aes(x,y), max, fill="transparent", color="blue", shape=21, size=3,stroke=1) + 
    geom_text(aes(x,y,label=sprintf("%.3f",y)), max, hjust=-0.45, color='blue' ) + 
    #scale_y_continuous(limits=c(0, max( summary$measure) )) + 
    #scale_x_continuous(breaks=seq(from=2,to=nrow(summary) + 1)) + 
    labs(x = "Model name", y = paste0(measure," value"), 
         title = paste0("Performance benchmarking plot for cluster=",cluster)) + 
    theme (axis.text.x = element_text(angle = 75, hjust = 1),
           plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0("plot_",measure,"_",cluster,".png"),plot,device="png")
}
