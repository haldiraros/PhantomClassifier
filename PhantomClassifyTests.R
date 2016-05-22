calculatePhantom <- function(a,object,alpha){
  res <- a*alpha+as.numeric(object)*(1-alpha)
  return(res)
}

##calculate phantom function
# x - misclassified object
# Y - closest objects of same class for object x
# alpha - how close to object should phantoms be located in [0;1]
createPhantoms <- function(x,Y,alpha=0.5,classN){
  x_classless <- x[,-which(names(x) %in% c(classN))]
  Y_classless <- Y[,-which(names(Y) %in% c(classN))]
  #base version - just numeric
  phantoms <- t(apply(Y_classless,1,calculatePhantom,x_classless,alpha))
  phantoms <-data.frame(phantoms)
  phantoms <- cbind(phantoms, rep.int(x[,which(names(x)%in% c(classN))],dim(Y)[1]))
  names(phantoms)[[ncol(phantoms)]] <- classN
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
  
  inTraining<-createDataPartition(y=data$Species,
                                  p=0.7,
                                  list=FALSE)
  
  data_train <- data[ inTraining,]
  data_test <- data[-inTraining,]
  print(dim(data_train))
  ##classify 1st time
  #modFit <- train(Species ~ ., data=data_train, method="rf", prox=TRUE)
  modFit <-  train(Species ~ ., data=data_train, method = "knn", tuneLength = 1,preProc = c("center", "scale"))
  
  pred <- predict(modFit, data_test)
  cm <- confusionMatrix(data = pred, data_test$Species)
  print(cm)
  
  #find object that were wrongly classified
  data_test$predRight <- pred==data_test$Species
  errors <- filter(data_test,predRight==FALSE) %>% select(-predRight)
  
  err_split <- split(errors,errors$Species)
  
  
  phantoms <- data[0,]
  
  for(cl in names(err_split)){
    err_sub <- err_split[[cl]]
    if(dim(err_sub)[1]>0){
      data_sub <- data %>% filter(Species == cl)
      closeObj <- knnx.index(data_sub[,-5],err_sub[,-5],k=k+1)
      closeObj <- closeObj[,-1] #remove error object
      
      for(x in 1:dim(err_sub)[1]){
        closeObj_ <- if(is.null(dim(closeObj))){ closeObj }else{ closeObj[x,] }
        pha <- createPhantoms(err_sub[x,],data_sub[closeObj_,],alpha,'Species')
        phantoms <- rbind(phantoms,pha)
      }
    }
  }
  
  print(phantoms)
  
  #add phantoms just to train data (first)
  data_train2 <- rbind(data_train,phantoms)
  
  print(dim(data_train2))
  
  ##classify 2nd time
  #modFit2 <- train(Species ~ ., data=data_train2, method="rf", prox=TRUE)
  modFit2 <-  train(Species ~ ., data=data_train2, method = "knn", tuneLength = 1,preProc = c("center", "scale"))
  
  pred2 <- predict(modFit2, data_train2)
  cm2 <- confusionMatrix(data = pred, data_test$Species)
  print(cm2)
  
}
