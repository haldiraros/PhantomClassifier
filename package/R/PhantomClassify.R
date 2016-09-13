## Function creating phantoms for given classification errors for given dataset
# form   - a model formula
# data   - the original training set (with the unbalanced distribution)
# errors - dataset containing just wrongly classified objects from the original dataset
# k      - number of closest examples of same class to be used when creating phantoms
# alpha  - location of the newly created phantoms on 'error object'-neigbour line

metaPhantom <-function(form, data, errors,
                       k=3,
                       alpha=0.5){

  if(is.null(errors) || nrow(errors)==0) {
    warning("Provided errors dataframe is empty. Returning an empty result")
    return(data[0,])
  }
  if(k<1){
    stop("k can not be smaller than 1")
  }
  if(alpha<=0 || alpha>=1){
    stop("Given alpha lies outside of (0,1) range")
  }

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

  #normalize for latter KNN
  numericalVals<-data[,-c(nomCols,ncol(data))]
  minVals <-apply(numericalVals,2,min)
  maxVals <-apply(numericalVals,2,max)
  ranges <- maxVals-minVals
  normalizedVals<-scale(numericalVals,minVals,ranges)

  nominalPenalty <- (stats::median(apply(normalizedVals,2,stats::sd)))^2
  #init phantoms
  phantoms <- data[0,]


  err_split <- split(errors,errors[,ncol(errors)])

  for(cl in names(err_split)){
    err_sub <- err_split[[cl]]
    if(dim(err_sub)[1]>0){
      data_sub<-data[data[,ncol(data)]==cl,]
      #basic knn - works just for numeric data for now
      if(is.null(nomCols)){
        closeObj <- FNN::knnx.index(data_sub[,-c(nomCols,ncol(data_sub))],
                               err_sub[,-c(nomCols,ncol(err_sub))],k=k+1)
        closeObj <- closeObj[,-1] #remove error object - it is always closest
      }
      for(subLoop in 1:dim(err_sub)[1]){
        closeObj_<-NULL
        if(is.null(nomCols)){
          closeObj_ <- if(is.null(dim(closeObj))){ closeObj }else{ closeObj[subLoop,] }
        }else{
          closeNumericals<-data_sub[,-c(nomCols,ncol(data_sub))]
          numericalErr<-as.numeric(err_sub[subLoop,-c(nomCols,ncol(err_sub))])
          d1<-scale(closeNumericals,numericalErr,ranges)
          dis_numeric <- drop(d1^2 %*% rep(1, ncol(d1)))

          nomDiff<-data_sub[,nomCols]!=err_sub[rep(1,nrow(data_sub)),nomCols]
          if(is.null(dim(nomDiff))){
            nomPenalties<-nomDiff
          }else{
            nomPenalties<-apply(nomDiff,1,sum)
          }
          dis_total<-dis_numeric+(nomPenalties*nominalPenalty)
          closeObj_<-order(dis_total)[2:(k+1)]
          # closeObj_<-data_sub[kNNs,]
        }
        pha <- createPhantoms(err_sub[subLoop,],data_sub[closeObj_,],k,alpha,nomCols)
        phantoms <- rbind(phantoms,pha)
      }
    }
  }

  #reset data column order
  if (targetVar < ncol(data)) {
    phantoms <- phantoms[,cols]
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
  counts<-lapply(1:length(nominals),function(x) plyr::count(t,x))
  nomValues<- lapply(counts,function(x) {
                            x[,2]<-prop.table(x[,2])
                            sample(x[,1],size=n,replace=TRUE,prob=x[,2])
                          })
  return( nomValues)
}

##calculate phantom function
# x - misclassified object
# Y - closest objects of same class for object x
# alpha - how close to object should phantoms be located in [0;1]
createPhantoms <- function(x,Y,k,alpha,nominals=NULL){
  x_classless <- x[,-ncol(x)]
  Y_classless <- Y[,-ncol(Y)]
  #base version - just numeric
  phantoms <- Y[0,]

  if(!is.null(nominals)){
    nominalValues<-getNominalValues(x_classless,Y_classless,nominals,k)
  }

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
