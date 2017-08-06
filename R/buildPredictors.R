#' @title Build Predictors
#' @param x An xts object containing stock quotes (OHLCV info) from quant mod
#' @param funList a list of functions for to build features from and xts object
#' @param funNames column names for the the output xts
#' @return and xts with the funNames and ouput from funList
#' @author Matthew Davis
#' @export
#' @description takes and xts object and uses TTR package to create non lagged formated set of predictors (typically used to predict the trade Flag T.flag())
#' @return xts object of predictors (not lagged) to be used to predict the trade signal
buildPredictors<-function(x, funList = NULL, funNames = NULL){
  if(is.null(funList)){
    funList = c(myATR,mySMI,myADX ,myAroon,myBB,myChaikinVol, myCLV,myEMV, myMACD ,myMFI,mySAR,myVolat )
  }
  if(is.null(funNames)){
    funNames<-c('ATR','SMI','ADX' ,'Aroon','BB','ChaikinVol', 'CLV','EMV',
              'MACD' ,'MFI','SAR','Volat')
  }
  output<-NULL
  x<-OHLCV(x)
  for( i in 1:(length(funList))){
    tempData<-NULL
    tempFun<-funList[[i]]
    tempData<-tempFun(x)
    output<-cbind(output, tempData)
  }
  colnames(output)<-funNames
  output<-merge.xts(x, output, join = 'left')
  return(output)
}
