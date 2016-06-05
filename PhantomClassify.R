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
  }
  
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
      closeObj <- closeObj[,-1] #remove error object - it is always closest
      
      for(x in 1:dim(err_sub)[1]){
        closeObj_ <- if(is.null(dim(closeObj))){ closeObj }else{ closeObj[x,] }
        pha <- createPhantoms(err_sub[x,],data_sub[closeObj_,],alpha)
        phantoms <- rbind(phantoms,pha)
      }
    }
  }
  
  #reset data column order
  if (targetVar < ncol(data)) {
    newExs <- newExs[,cols]
    data <- data[,cols]
  }
  
  #rename phantom rownames
  rownames(phantoms) <- NULL
  rownames(phantoms) <-paste0('pha_',rownames(phantoms))
  return(phantoms)
  
}

#calculation of a single phantom object
calculatePhantom <- function(a,object,alpha){
  #simple numeric objects
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
