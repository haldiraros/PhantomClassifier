## Function creating phantoms for given classification errors for given dataset
# form   - a model formula
# data   - the original training set (with the unbalanced distribution)
# errors - dataset containing just wrongly classified objects from the original dataset
# k      - number of closest examples of same class to be used when creating phantoms
# alpha  - location of the newly created phantoms on 'error object'-neigbour line

metaPhantom <-function(form,data,errors,k,alpha){
  #load needed library
  library(FNN)
  
  # the column where the target variable is
  targetVar <- which(names(data) == as.character(form[[2]]))
  #make target value column be last column
  if (targetVar < ncol(data)) {
    cols <- 1:ncol(data)
    cols[c(targetVar,ncol(data))] <- cols[c(ncol(data),targetVar)]
    data <-  data[,cols]
    errors<- errors[,cols]
  }
  
  #get columns with nominal values
  nomCols <- which(sapply(sapply(data[,-ncol(data)],class),function(x) any(x %in% c('factor','character'))))
  if(length(nomCols)==0) nomCols<-NULL
  err_split <- split(errors,errors[,ncol(errors)])
  
  #init phantoms
  phantoms <- data[0,]
  
  for(cl in names(err_split)){
    err_sub <- err_split[[cl]]
    if(dim(err_sub)[1]>0){
      data_sub<-data[data[,ncol(data)]==cl,]
      #basic knn - works just for numeric data for now
      closeObj <- knnx.index(data_sub[,-c(nomCols,ncol(data_sub))],
                             err_sub[,-c(nomCols,ncol(err_sub))],k=k+1)
      closeObj <- closeObj[,-1] #remove error object - it is always closest
      
      for(x in 1:dim(err_sub)[1]){
        closeObj_ <- if(is.null(dim(closeObj))){ closeObj }else{ closeObj[x,] }
        pha <- createPhantoms(err_sub[x,],data_sub[closeObj_,],k,alpha,nomCols)
        phantoms <- rbind(phantoms,pha)
      }
    }
  }
  
  #reset data column order
  if (targetVar < ncol(data)) {
    newExs <- newExs[,cols]
    data <- data[,cols]
    errors<- errors[,cols]
  }
  
  #rename phantom rownames
  rownames(phantoms) <- NULL
  rownames(phantoms) <-paste0('pha_',rownames(phantoms))
  return(phantoms)
  
}

getNominalValues <- function(error,neighbours,nominals,n){
  t <- rbind(error,neighbours)[,nominals]
  counts<-lapply(nominals,function(x) plyr::count(t,x))
  nomValues<- lapply(counts,function(x) {
                            x[,2]<-prop.table(x[,2]) 
                            sample(x[,1],size=n,replace=TRUE,prob=x[,2])
                          })
  return( nomValues)
}

#calculation of a single phantom object
## !!! DEPRECATED !!! doing it with vector operations now
calculatePhantom <- function(a,object,alpha,nominal){
  #simple numeric objects
  res <- a
  if(!is.null(nominal)){
    for(i in (1:length(a))[-nominal])
      res[i] <-as.numeric(a[i])*alpha+as.numeric(object[i])*(1-alpha)
  }else{
    res<-a*alpha+as.numeric(object)*(1-alpha)
  }
  return(res)
}

##calculate phantom function
# x - misclassified object
# Y - closest objects of same class for object x
# alpha - how close to object should phantoms be located in [0;1]
createPhantoms <- function(x,Y,k=3,alpha=0.5,nominals=NULL){
  x_classless <- x[,-ncol(x)]
  Y_classless <- Y[,-ncol(Y)]
  #base version - just numeric
  phantoms <- Y[0,]
  
  if(!is.null(nominals)){
    nominalValues<-getNominalValues(x_classless,Y_classless,nominals,k)
  }

  #phantoms <- t(apply(Y_classless,1,calculatePhantom,x_classless,alpha,nominals)) ## Seems to work, keep as backup

  b <- suppressWarnings(x_classless*(1-alpha))
  phantoms <- suppressWarnings(Y_classless*alpha+b[rep(1,k),])

  phantoms <-data.frame(phantoms)
  if(!is.null(nominals)){
    for(i in 1:length(nominals)){
      phantoms[,nominals[i]]<-nominalValues[[i]]
      #phantoms[,nominals[i]]<-factor(nominalValues[i],levels=1:nlevels(nominalValues[i]),labels=levels(nominalValues[i]))
    }
  }
  phantoms <- cbind(phantoms, rep.int(x[,ncol(x)],dim(Y)[1]))
  names(phantoms)[[ncol(phantoms)]] <- names(x)[[ncol(x)]]
  return(phantoms)
}
