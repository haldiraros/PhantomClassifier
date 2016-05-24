calculatePhantom <- function(a,object,alpha){
  res <- a*alpha+as.numeric(object)*(1-alpha)
  return(res)
}

##calculate phantom function
# x - misclassified object
# Y - closest objects of same class for object x
# alpha - how close to object should phantoms be located in [0;1]
createPhantoms <- function(x,Y,alpha=0.5){
  x_classless <- x[,-ncol(x)]
  Y_classless <- Y[,-ncol(Y)]
  #base version - just numeric
  phantoms <- t(apply(Y_classless,1,calculatePhantom,x_classless,alpha))
  phantoms <-data.frame(phantoms)
  phantoms <- cbind(phantoms, rep.int(x[,ncol(x)],dim(Y)[1]))
  names(phantoms)[[ncol(phantoms)]] <- names(x)[[ncol(x)]]
  return(phantoms)
}


metaPhantom <-function(data,errors,k,alpha){
  err_split <- split(errors,errors[,ncol(errors)])
  
  #init phantoms
  phantoms <- data[0,]
  
  for(cl in names(err_split)){
    err_sub <- err_split[[cl]]
    if(dim(err_sub)[1]>0){
      data_sub<-data[data[,ncol(data)]==cl,]
      #basic knn - works just for numeric data
      closeObj <- knnx.index(data_sub[,-ncol(data_sub)],
                             err_sub[,-ncol(err_sub)],k=k+1)
      closeObj <- closeObj[,-1] #remove error object that is always closest
      
      for(x in 1:dim(err_sub)[1]){
        closeObj_ <- if(is.null(dim(closeObj))){ closeObj }else{ closeObj[x,] }
        pha <- createPhantoms(err_sub[x,],data_sub[closeObj_,],alpha)
        phantoms <- rbind(phantoms,pha)
      }
    }
  }
  #rename phantoms
  rownames(phantoms) <- NULL
  rownames(phantoms) <-paste0('pha_',rownames(phantoms))
  return(phantoms)
}
## test functiion for all - 'til I get on with a reporting script
phantomsTest <- function(k=3,alpha=0.5){
  library(caret)
  library(datasets)
  library(FNN)
  library(dplyr)
  
  #read data
  # exclude the separable class for now
  data <- iris %>% filter(Species!='setosa') 
  data$Species <- factor(data$Species)
  
  inTraining<-createDataPartition(y=data[,ncol(data)],
                                  p=0.7,
                                  list=FALSE)
  
  data_train <- data[ inTraining,]
  data_test <- data[-inTraining,]
  print(dim(data_train))
  ##classify 1st time
  #modFit <- train(x=data_train[,-ncol(data_train)],
  #                y=data_train[,ncol(data_train)],, method="rf", prox=TRUE)
  modFit <-  train(x=data_train[,-ncol(data_train)],
                   y=data_train[,ncol(data_train)],
                   method = "knn", tuneLength = 3,preProc = c("center", "scale"))
  
  pred <- predict(modFit, data_test[,-ncol(data_test)])
  cm <- confusionMatrix(data = pred, data_test[,ncol(data_test)])
  print(cm)
  
  #find object that were wrongly classified
  errors<- data_test
  errors$predRight <- pred==data_test[,ncol(data_test)]
  errors <- filter(errors,predRight==FALSE) %>% select(-predRight)
  
  #call phantom generalization
  phantoms <- metaPhantom(data,errors,k,alpha)
  
  print(phantoms)

  
  #add phantoms just to train data (first)
  data_train2 <- rbind(data_train,phantoms)
  
  print(dim(data_train2))
  
  ##classify 2nd time
  #modFit2 <- train(x=data_train[,-ncol(data_train)],
  #                y=data_train[,ncol(data_train)], method="rf", prox=TRUE)
  modFit2 <-  train(x=data_train2[,-ncol(data_train2)],
                    y=data_train2[,ncol(data_train2)],
                    method = "knn", tuneLength = 3,preProc = c("center", "scale"))
  
  pred2 <- predict(modFit2, data_test[,-ncol(data_test)])
  cm2 <- confusionMatrix(data = pred2, data_test[,ncol(data_test)])
  print(cm2)
  
}
