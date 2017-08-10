#' @title Build Stock Data Predictors from Enviroment
#' @param e enviroment where the xts stock data is stored
#' @param stockNameList a list of stockNames
#' @description takes and enviroment with xts stock data, engineers features and merges it into one data frame 
#' @return a data frame of merge stock data
#' @export
#' 
buildStockDataSetFromEnv<-function(e, stockNameList = NULL, verbose = FALSE){
  if(is.null(stockNameList)){stockNameList<-ls(e)}
  if(verbose)print(stockNameList)
  cores<-parallel::detectCores()
  cl<-makeCluster(cores)
  registerDoParallel(cl)
    output <- foreach ( i = 1:(length(stockNameList)), 
                        .combine = 'rbind',
                        .packages = c('quantmod', 'stockTools')) %dopar% {
    tempName<-stockNameList[i]
    try(tempData<-get(tempName, envir = e))
    if(!is.xts(tempData)){print(paste('missing from envir : ', stockNameList[i]))}
    if(is.xts(tempData)){
      if(all(grepl(tempName ,colnames(tempData)))){
        colnames(tempData)<-stringr::str_replace(colnames(tempData), paste(tempName, '.', sep = ''), '')
      }
    }
      tempDf<-NULL
        print(paste(stockNameList[i], 'successully found'))
        tempDf<-data.frame(date = as.numeric(as.Date(index(tempData))))

        tempDf$ATR <- ATR(tempData)
        tempDf$SMI <- SMI(HLC(tempData))
        tempDf$ADX <- ADX(HLC(tempData))[,'ADX']
        tempDf$AROON <- aroon(HLC(tempData)[,2:3])$oscillator
        tempDf$BB <- BBands(HLC(tempData))[,'pctB']
        tempDf$SDELT <- Delt(chaikinVolatility(HLC(tempData)[,1:2]))[,1]
        tempDf$EMV <- EMV(HLC(tempData)[,2:3],tempData[,5])[,2]
        tempDf$MACD <- MACD(Cl(tempData))[,2]
        tempDf$SAR <- SAR(HLC(tempData)[,1:2])[,1]
        tempDf$OpenCloseDiffVol<-(tempData[,'Open'] - tempData[,'Close'])*tempData[,'Volume']
        tempDf$HighLowDiff<-tempData[,'High'] - tempData[,'Low']
        tempDf$SMAOpenClose<-SMA(tempData[,'Open'])-SMA(tempData[,'Close'])
        tempDf<-data.frame(tempDf, tempData)
        tempDf$tradeFlag<-T.flag(T.ind(OHLCV(tempData)))
        tempDf$tradeInd<-T.ind(OHLCV(tempData))
        tempDf<-as.matrix(tempDf)
        tempDf<-data.frame(stockName = tempName, tempDf)
        tempDf
          }
  stopCluster(cl)
  return(output)
}

