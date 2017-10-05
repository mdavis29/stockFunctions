#' @title  Find Outliers by quantile
#' @param mydata a data frame
#' @param r IQR range mulitplier
#' @verbose print debugging output
#' @description takes a data frame and returns an index with IRQ outliers
#' @return logical vector of outliers
findOutliersByQuantile<-function(mydata, r = 1.8, verbose = FALSE){
  nc<-ncol(mydata)
  nr<-nrow(mydata)
  cores<-parallel::detectCores()
  cl<-makeCluster(cores)
  registerDoParallel(cl)
  outputMatrix<-foreach( i  =  1:nc, .combine = 'cbind') %dopar%{
    tempOut<-rep(FALSE, nr)
    tempData<-mydata[,i]
    if(length(unique(tempData))>2){
      quant<-quantile(tempData, na.rm = TRUE) 
      iqr<-IQR(tempData, na.rm = TRUE)
      lows<-ifelse(tempData < quant[3] - iqr*r, TRUE, FALSE)
      highs<-ifelse(tempData > quant[3] + iqr*r, TRUE, FALSE)
      tempOut<-apply(cbind(lows,highs), c(1), any)
    }
  }
  stopCluster(cl)
  if(verbose)print(summary(outputMatrix))
  output<-apply(outputMatrix,  c(1), any)
  return(output)
}
