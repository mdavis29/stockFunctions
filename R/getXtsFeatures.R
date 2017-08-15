#' @title Build Predictors
#' @param x An xts object containing stock quotes (OHLCV info) from quant mod
#' @param funList a list of functions for to build features from and xts object
#' @param funNames column names for the the output xts
#' @return and xts with the funNames and ouput from funList
#' @author Matthew Davis
#' @export
#' @description takes and xts object and uses TTR package to create non lagged formated set of predictors (typically used to predict the trade Flag T.flag())
#' @return xts object of predictors (not lagged) to be used to predict the trade signal
getXtsFeatures<-function(x, wid = 20,verbose = FALSE){
  output<-data.frame(
    sdevs<-zoo::rollapply(x,width = wid,FUN =  sd, fill = NA, align = 'right' ),
    means<-zoo::rollapply(x,width = wid,FUN =  mean, fill = NA, align = 'right' ),
    diffs<-x - sdevs
    )
  colnames(output)<-paste(rep(colnames(x),3), c(rep('sdevs',6), rep('means',6), rep('diffs',6)), sep = '.')
  output$ATR <- ATR(x, n=6)$atr
  output$SMI <- SMI(HLC(x),n = 1)
  output$ADX <- ADX(HLC(x))[,'ADX']
  output$AROON <- aroon(HLC(x)[,2:3])$oscillator
  output$BB <- BBands(HLC(x))[,'pctB']
  output$SDELT <- Delt(chaikinVolatility(HLC(x)[,1:2]))[,1]
  output$EMV <- EMV(HLC(x)[,2:3],x[,5])[,2]
  output$MACD <- MACD(Cl(x))[,2]
  output$SAR <- SAR(HLC(x)[,1:2])[,1]
  output$OpenCloseDiffVol<-(x[,'Open'] - x[,'Close'])*x[,'Volume']
  output$HighLowDiff<-x[,'High'] - x[,'Low']
  output$SMAOpenClose<-SMA(x[,'Open'])-SMA(x[,'Close'])
  output$tradeFlag<-T.flag(T.ind(OHLCV(x)))
  output$tradeInd<-T.ind(OHLCV(x))
  return(output)
  }
  
