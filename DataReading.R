library(dplyr)
readTestData <- function(datasetName){
  data<-data.frame()

  #### iris
  if(datasetName=="iris"){
    library(datasets)
    ##collapse dataset
    data <- iris %>% filter(Species!='setosa')
    data$Species <- factor(data$Species)
    colnames(data)[ncol(data)] <- "Class"

  }

  #### Sonar
  if(datasetName=="Sonar"){
    library(mlbench)
    data(Sonar)
    data <- Sonar
  }

  #### PID
  if(datasetName=="PID"){
    data<-read.csv("data/PID/pima-indians-diabetes.data",header=FALSE,fill=FALSE,strip.white=T)
    colnames(data)[ncol(data)] <- "Class"
    data[['Class']]<-factor(data[['Class']])
  }

  #### Phoneme
  if(datasetName=="Phoneme"){
    data<-read.csv("data/Phoneme/phoneme.dat",sep="",header=FALSE,fill=FALSE,strip.white=T)
    colnames(data)[ncol(data)] <- "Class"
    data[['Class']]<-factor(data[['Class']])

  }
  #### Satimage
  if(datasetName=="Satimage"){
    data1<-read.csv("data/Satimage/sat.trn",header=FALSE,sep="",fill=FALSE,strip.white=T)
    data2<-read.csv("data/Satimage/sat.tst",header=FALSE,sep="",fill=FALSE,strip.white=T)
    data<-rbind(data1,data2)
    colnames(data)[ncol(data)] <- "Class"
    ##collapse dataset
    data[['Class']]<-(data[['Class']]!=4)*1
    data[['Class']]<-factor(data[['Class']])
  }
  #### Adult
  if(datasetName=="Adult"){
    data1<-read.csv("data/Adult/adult.data",header=FALSE,col.names=c("age", "type_employer", "fnlwgt", "education",
                                                              "education_num","marital", "occupation", "relationship", "race","sex",
                                                              "capital_gain", "capital_loss", "hr_per_week","country", "income"),fill=FALSE,strip.white=T)
    data2<-read.csv("data/Adult/adult.test",header=FALSE,col.names=c("age", "type_employer", "fnlwgt", "education",
                                                                     "education_num","marital", "occupation", "relationship", "race","sex",
                                                                     "capital_gain", "capital_loss", "hr_per_week","country", "income"),fill=FALSE,strip.white=T)
    data_pre<-rbind(data1,data2)

    data<-dplyr::filter(data_pre[,!names(data_pre) %in% c("education_num")],country!='Holand-Netherlands')

    colnames(data)[ncol(data)] <- "Class"
  }
  #### mammography
  if(datasetName=="Mammography"){
    library(R.matlab)
    readDataMat<- readMat("data/Mammography/mammography.mat")
    data<-unique(data.frame(readDataMat[[1]],Class=factor(readDataMat[[2]])))
  }
  #### Wine
  if(datasetName=="Wine"){
    data<-read.csv("data/Wine/wine.data",header=FALSE,fill=FALSE,strip.white=T)
    cols <- 1:ncol(data)
    cols[c(1,ncol(data))] <- cols[c(ncol(data),1)]
    data <-data[,cols]
    colnames(data)[ncol(data)] <- "Class"
    ##collapse dataset
    data[['Class']]<-cut(data[['Class']],breaks=c(0,1,3),labels=c('class1','class2or3'))
  }
  #### Ionosphere
  if(datasetName=="Ionosphere"){
    data<-read.csv("data/Ionosphere/ionosphere.data",header=FALSE,fill=FALSE,strip.white=T)
    colnames(data)[ncol(data)] <- "Class"
    data[['Class']]<-factor(data[['Class']])
  }
  #### Spambase
  if(datasetName=="Spambase"){
    data<-read.csv("data/Spambase/spambase.data",header=FALSE,fill=FALSE,strip.white=T)
    colnames(data)[ncol(data)] <- "Class"
    data[['Class']]<-factor(data[['Class']])
  }
  #### Abalone
  if(datasetName=="Abalone"){
    data1<-read.csv("data/Abalone/abalone.data",header=FALSE,fill=FALSE,strip.white=T)
    colnames(data1)[ncol(data1)] <- "Class"
    ##collapse dataset
    data <- data1 %>% filter(Class %in% c(9,18))
    data[['Class']]<-(data[['Class']]!=18)*1
    data[['Class']]<-factor(data[['Class']])
  }

  #### Oil
  if(datasetName=="Oil"){
    data<-read.arff("data/Oil/oil.arff")
    cols <- 1:ncol(data)
    cols[c(1,ncol(data))] <- cols[c(ncol(data),1)]
    data <-data[,cols]
    colnames(data)[ncol(data)] <- "Class"
  }

  return(data)
}
