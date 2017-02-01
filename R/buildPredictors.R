#' @title Build Predictors
#' @param x An xts object containing stock quotes (OHLCV info) from quant mod
#' @author Matthew Davis
#' @export
#' @description takes and xts object and uses TTR package to create non lagged formated set of predictors (typically used to predict the trade Flag T.flag())
#' @return xts object of predictors (not lagged) to be used to predict the trade signal
buildPredictors<-function(x){
  output<-cbind(
    ATR(x)[, 'tr'],
    SMI(HLC(x)) ,
    ADX(x)[,1:3],
    aroon(Cl(x)),
    BBands(Cl(x)),
    CLV(x),
    MACD(Cl(x)),
    MFI(HLC(x), volume = Vo(x)),
    SAR(HLC(x)),
    volatility(x))
  colnames(output)<-paste('p',make.names(colnames(output)), sep = '')
  return(output)
  }
