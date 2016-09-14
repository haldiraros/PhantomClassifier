library(dplyr)
library(stats)
library(xtable)

path = "testResults/"
alg <- "lr"
file.names <- dir(path, pattern =".csv")
file.filter <- paste0(".*",alg,"\\.csv$")

table.caption <- paste0("Wyniki dla algorytmu ",alg,".")
table.label<- paste0("tbl_",alg)

measure_list<-c("precision","recall","accuracy","Fmeasure","kappa","AUC")

filtered.files <-file.names[grepl(file.filter,file.names)]
data.collected <- data.frame()
format.collected<-data.frame()
for(file in filtered.files){
  print(file)
  data<-read.csv(paste0(path,file))
  #prepare for summary
  data.collected<-rbind(data.collected,data)
  #per file summary
  data_sum<-tbl_df(data) %>% group_by(datasetName,technique) %>% summarise_each(funs(mean),-c(algorithm,testNumber,X))
  data_format <- NULL
  #do significance testing
  for(measure in measure_list){
    #Testy
    res<-tryCatch({
    res_PS<-t.test(data[[measure]][data$technique=='Phantom'],
                   data[[measure]][data$technique=='SMOTE'], var.equal = FALSE, paired
           = FALSE)
    res_Pb<-t.test(data[[measure]][data$technique=='Phantom'],
                   data[[measure]][data$technique=='basic'], var.equal = FALSE, paired
                   = FALSE)
    res_Sb<-t.test(data[[measure]][data$technique=='SMOTE'],
                   data[[measure]][data$technique=='basic'], var.equal = FALSE, paired
                   = FALSE)
    #Find maximum value for measure
    flags<- switch(which.max(data_sum[[measure]]),
           c(TRUE,!(res_Pb$p.value<0.05),!(res_Sb$p.value<0.05)), #base is max
           c(!(res_Pb$p.value<0.05),TRUE,!(res_PS$p.value<0.05)), #Phantom is max
           c(!(res_Sb$p.value<0.05),!(res_PS$p.value<0.05),TRUE)  #SMOTE is max
           )
    },warning =function(war){
      print(war)
    },error = function(err){
      print(err)
      return(c(FALSE,FALSE,FALSE))
    },finally={}
  )
    data_format<-cbind(data_format,res)
#     print(res_PS)
#     print(res_Pb)
#     print(res_Sb)
  }
  format.collected<-rbind(format.collected,data_format)
}

collected_sum<-tbl_df(data.collected) %>% group_by(datasetName,technique) %>% summarise_each(funs(mean),-c(algorithm,testNumber,X))
colnames(collected_sum)<-c("Zbiór danych","Metoda","Precision","Recall","Accuracy","F-measure","Kappa Cohena","AUC")
collected_sum[,-c(1,2)]<-round(collected_sum[,-c(1,2)],5)

#parse for emphasis
formatted<-collected_sum
for(i in 1:nrow(format.collected)){
  for(j in 1:ncol(format.collected)){
    formatted[i,2+j]<-ifelse(format.collected[i,j], paste0("\\textbf{", formatted[i,2+j], "}"), formatted[i,2+j])
  }
}

result_table <- xtable(formatted,
                       digits=5,
                       caption = table.caption,
                       label = table.label)
align(result_table) <- "c|r|r||l|l|l|l|l|l|"

print(result_table,
      include.rownames=FALSE,
      sanitize.text.function = function(x) x,
      NA.string = "---",
      hline.after = c(-1,0,seq(3,nrow(result_table),3)))

