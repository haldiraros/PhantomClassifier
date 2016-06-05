
## example test script
phantomsTest <- function(k=3,alpha=0.5){
  source("PhantomClassify.R")
  
  library(caret)
  library(datasets)
  library(dplyr)
  
  #read data
  # exclude the separable class for now
  #### iris
  #data <- iris %>% filter(Species!='setosa') 
  #data$Species <- factor(data$Species)
  
  #### Sonar
  library(mlbench)
  data <- Sonar
  
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
  phantoms <- metaPhantom(Class ~ .,data,errors,k,alpha)
  
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