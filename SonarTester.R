  source("PhantomClassify.R")
  source("DataReading.R")

  library(caret)
  library(dplyr)
  library(DMwR)

  ###Init
  k <- 3
  alpha <- 0.5
  testAlg <- "KNN" # tree, lr, nb, KNN
  testForm <- Class ~ .
  set.seed(4228)
  iterations<-40
  resultDirName<-"testResults"
  #set up result dataframe
  results <- data.frame(dataset=character(0),
                        algorithm=character(0),
                        technique=character(0),
                        testNumber=numeric(0),
                        precision=numeric(0),
                        recall=numeric(0),
                        accuracy=numeric(0),
                        Fmeasure=numeric(0),
                        kappa=numeric(0),
                        AUC=numeric(0))
  #############################################################
  ###READ DATA###
  dataName<-"Spambase"
  data<-readTestData(dataName)


  ####helpers####
  calcStatistics <- function (prediction,oracle,probs) {
    cm2 <- confusionMatrix(data=prediction,oracle)
    print(cm2)
    vPrecision <- cm2$byClass[['Pos Pred Value']]
    vRecall <- cm2$byClass[['Sensitivity']]
    vAcc <- cm2$overall[['Accuracy']]
    vF_measure <- 2 * ((vPrecision * vRecall) / (vPrecision + vRecall))
    vKappa<- cm2$overall[['Kappa']]
    #library(irr) #if weights needed
    #Sol <-cbind(prediction,oracle)
    #vKappa<-kappa2(Sol,"squared")

    library(pROC)
    vRoc<-roc(oracle,probs)$auc[1]
    #vRoc<-auc(prediction,oracle)[1]
    return( list(precision=vPrecision,
             recall=vRecall,
             accuracy=vAcc,
             Fmeasure=vF_measure,
             kappa=vKappa,
             AUC=vRoc) )

  }

  recordStatistics <- function(statRegistry,datasetName,algorithm,technique,testNumber,
                               statisticsToAdd){
    return(rbind(statRegistry,data.frame(datasetName,algorithm,technique,testNumber,statisticsToAdd)))

  }

  prepModFit <- function (data_mod,method="knn"){

    if(method=="tree"){
      ###Random forest###

      modFitRet <- train(x=data_mod[,-ncol(data_mod)],
                      y=data_mod[,ncol(data_mod)],
                      method="J48")
    }
    else if(method=="nb"){
    #### naive bayes  ####
      modFitRet <- train(x=data_mod[,-ncol(data_mod)],
                      y=data_mod[,ncol(data_mod)],
                      method="nb")
    }
    else if(method=="lr"){
    #### log reg ####
    modFitRet <- train(x=data_mod[,-ncol(data_mod)],
                    y=data_mod[,ncol(data_mod)],
                    method="glm", family="binomial",maxit = 100)
    }else{
    #### KNN ####
    modFitRet <-  train(x=data_mod[,-ncol(data_mod)],
                     y=data_mod[,ncol(data_mod)],
                     method = "knn", tuneLength = 3,preProc = c("center", "scale"))
    }

    return(modFitRet)

  }


  ####START TEST LOOP####
  for(i in 1:iterations){
  ######
  print(paste("######### iteration ",i," ##############"))
  inTraining<-createDataPartition(y=data[,ncol(data)],
                                  p=0.8,
                                  list=FALSE)

  data_train <- data[ inTraining,]
  data_test <- data[-inTraining,]
  print(dim(data_train))
  ###############################################################
  ####basic classification####
  modFit_original <- prepModFit(data_train,testAlg)
  pred_original <- predict(modFit_original, data_test[,-ncol(data_test)])
  pred_original_prob <- predict(modFit_original, data_test[,-ncol(data_test)],type = "prob")

  results<-recordStatistics(results,dataName,testAlg,"basic",i,
  calcStatistics(pred_original,data_test[,ncol(data_test)],pred_original_prob[,1])
  )
  ################################################################
  ####Use SMOTE to enrich training set####
  data_train_smote <- DMwR::SMOTE(testForm,data=data_train)
  print(dim(data_train_smote))
  modFit_SMOTE <- prepModFit(data_train_smote,testAlg)
  pred_SMOTE <- predict(modFit_SMOTE, data_test[,-ncol(data_test)])
  pred_SMOTE_prob <- predict(modFit_SMOTE, data_test[,-ncol(data_test)],type = "prob")

  results<-recordStatistics(results,dataName,testAlg,"SMOTE",i,
  calcStatistics(pred_SMOTE,data_test[,ncol(data_test)],pred_SMOTE_prob[,1])
  )
  ################################################################
  #### Phantoms ####
  #### Divide training set again #####
  forPhantomTraining<-createDataPartition(y=data_train[,ncol(data_train)],
                                  p=0.8,
                                  list=FALSE)

  phantom_base_train <- data_train[ forPhantomTraining,]
  phantom_base_test <- data_train[-forPhantomTraining,]

  ##### Learn base classifier later used for phantoms #######
  modFit_BeforePhantom <- prepModFit(phantom_base_train,testAlg)
  pred_BeforePhantom <- predict(modFit_BeforePhantom, phantom_base_test[,-ncol(phantom_base_test)])

  #### find object that were wrongly classified ####
  errors<- phantom_base_test
  errors$predRight <- pred_BeforePhantom==phantom_base_test[,ncol(phantom_base_test)]
  errors <- filter(errors,predRight==FALSE) %>% dplyr::select(-predRight)

  #call phantom generalization
  phantoms <- metaPhantom(testForm,data_train,errors,k,alpha)

  #add phantoms just to train data (first)
  data_train_phantom <- rbind(data_train,phantoms)
  print(dim(data_train_phantom))
  modFit_phantom <- prepModFit(data_train_phantom,testAlg)

  pred_phantom <- predict(modFit_phantom, data_test[,-ncol(data_test)])
  pred_phantom_prob <- predict(modFit_phantom, data_test[,-ncol(data_test)],type = "prob")
  results<-recordStatistics(results,dataName,testAlg,"Phantom",i,
  calcStatistics(pred_phantom,data_test[,ncol(data_test)],pred_phantom_prob[,1])
  )
  ####END TEST LOOP####
  }
  ####
  ####### save results

  if(!dir.exists(resultDirName)) dir.create(resultDirName)
  filename<- paste(dataName,testAlg,sep = "_")
  write.csv(results,file=paste0(resultDirName,"/",filename,".csv"))


  #Other usefull things
  #ggplot(tsss,aes(x = 1-testROC$specificities,y=testROC$sensitivities))+geom_point()
