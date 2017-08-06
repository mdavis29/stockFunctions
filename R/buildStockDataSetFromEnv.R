#' @title Build Stock Data Predictors from Enviroment
#' @param e enviroment where the xts stock data is stored
#' @param stockNameList a list of stockNames
#' @description takes and enviroment with xts stock data, engineers features and merges it into one data frame 
#' @return a data frame of merge stock data
#' @export
#' 
buildStockDataSetFromEnv<-function(e, stockNameList = NULL){
  if(is.null(stockNameList)){stockNameList<-ls(e)}
  output<-data.frame()
  cores<-parallel::detectCores()
  cl<-makeCluster(cores)
  registerDoParallel(cl)
    output <- foreach ( i = 1:(length(stockNameList)), .combine = 'rbind') %dopar% {
    try(tempData<-get(stockNameList[i], envir = e))
    if(!is.xts(tempData)){print(paste('missing from envir : ', stockNameList[i]))}
    if(is.xts(tempData)){
      tempDf<-NULL
      tempDf<-try(data.frame(buildPredictors(OHLCV(tempData))))
      if(class(tempDf) == 'data.frame'){
        print(paste(stockNameList[i], 'success'))
        tempDf$Date<-as.Date(index(tempData))
        tempDf$month<-as.factor(months(tempDf$Date))
        tempDf$tradeFlag<-tradeFlag(OHLCV(tempData))
        tempDf$tradeInd<-T.ind(OHLCV(tempData))
        tempDf$stockName<-as.factor(stockNameList[i])
        tempDf
      }
    }
  }
  stopCluster(cl)
  return(output)
}

