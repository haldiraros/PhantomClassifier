library(dplyr)
library(stats)
library(xtable)

path = "testResults/"
alg <- "nb"
file.names <- dir(path, pattern =".csv")
file.filter <- paste0(".*",alg,"\\.csv$")

table.caption <- paste0("Wyniki dla algorytmu ",alg,".")
table.label<- paste0("tbl_",alg)

wins.caption <- paste0("Wins dla algorytmu ",alg,".")
wins.label<- paste0("tbl_",alg,"_wins")

rank.caption <- paste0("rank dla algorytmu ",alg,".")
rank.label<- paste0("tbl_",alg,"_rank")

measure_list<-c("precision","recall","accuracy","Fmeasure","kappa","AUC")

filtered.files <-file.names[grepl(file.filter,file.names)]
data.collected <- data.frame()
format.collected<-data.frame()
wins.collected<-0
rank.collected<-0
for(file in filtered.files){
  print(file)
  data<-read.csv(paste0(path,file))
  #prepare for summary
  data.collected<-rbind(data.collected,data)
  #per file summary
  data_sum<-tbl_df(data) %>% group_by(datasetName,technique) %>% summarise_each(funs(mean),-c(algorithm,testNumber,X))
  data_format <- NULL
  wins_group <- NULL
  rank_group <-0
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

    #Test podsumowania
    wins_group <- cbind(wins_group,if(sum(res)>1){c(0,0,0,1)}else{c(res,0)})
    rank_group <- rank_group + rbind(
      if(res[1]){if(sum(res)>1){c(0,1,0)}else{c(1,0,0)}}else{c(0,0,1)},
      if(res[2]){if(sum(res)>1){c(0,1,0)}else{c(1,0,0)}}else{c(0,0,1)},
      if(res[3]){if(sum(res)>1){c(0,1,0)}else{c(1,0,0)}}else{c(0,0,1)}
    )

  }
  format.collected<-rbind(format.collected,data_format)

  wins.collected<-wins.collected+wins_group

  rank.collected<-rank.collected+rank_group
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


wins.df<-data.frame(cbind(c("base","Phantom","SMOTE","Remis"),wins.collected))
colnames(wins.df)<-c("Metoda","Precision","Recall","Accuracy","F-measure","Kappa Cohena","AUC")
wins_table <- xtable(wins.df,
                       caption = wins.caption,
                       label = wins.label)
align(wins_table) <- "c|r||l|l|l|l|l|l|"
print(wins_table,
      include.rownames=FALSE,
      NA.string = "---",
      hline.after = c(-1,0,seq(4,nrow(wins_table),4)))

## have rank tables here


rank.df<-data.frame(cbind(c(alg,alg,alg),c("base","Phantom","SMOTE"),rank.collected))
colnames(rank.df)<-c("Algortym","Metoda","Dominuje","Remis","Przegrywa")
rank_table <- xtable(rank.df,
                     caption = rank.caption,
                     label = rank.label)
align(rank_table) <- "c|r|r||l|l|l|"
print(rank_table,
      include.rownames=FALSE,
      NA.string = "---",
      hline.after = c(-1,0,seq(3,nrow(rank_table),3)))

## have rank tables here
