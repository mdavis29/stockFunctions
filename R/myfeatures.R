#' @title myATR 
#' @param x and xts object from quantmod
#' @return and xts object 
#' @description functions to build predictors varaibles from an xts quantmod object
#' @export
myATR <- function(x) ATR(HLC(x))[,'atr']
#' @title mySMI 
#' @param x and xts object from quantmod
#' @return and xts object 
#' @description functions to build predictors varaibles from an xts quantmod object
#' @export
mySMI <- function(x) SMI(HLC(x))[,'SMI']
#' @title myADX
#' @param x and xts object from quantmod
#' @return and xts object 
#' @description functions to build predictors varaibles from an xts quantmod object
#' @export
myADX <- function(x) ADX(HLC(x))[,'ADX']
#' @title myAroon
#' @param x and xts object from quantmod
#' @return and xts object 
#' @description functions to build predictors varaibles from an xts quantmod object
#' @export
myAroon <- function(x) aroon(HLC(x)[,2:3])$oscillator
#' @title myBB
#' @param x and xts object from quantmod
#' @return and xts object 
#' @description functions to build predictors varaibles from an xts quantmod object
#' @export
myBB <- function(x) BBands(HLC(x))[,'pctB']
#' @title myChaikinVol
#' @param x and xts object from quantmod
#' @return and xts object 
#' @description functions to build predictors varaibles from an xts quantmod object
#' @export
myChaikinVol <- function(x)Delt(chaikinVolatility(HLC(x)[,1:2]))[,1]
#' @title myCLV
#' @param x and xts object from quantmod
#' @return and xts object 
#' @description functions to build predictors varaibles from an xts quantmod object
#' @export
myCLV <- function(x) EMA(CLV(HLC(x)))[,1]
#' @title mEMV
#' @param x and xts object from quantmod
#' @return and xts object 
#' @description functions to build predictors varaibles from an xts quantmod object
#' @export
myEMV <- function(x) EMV(HLC(x)[,2:3],x[,5])[,2]
#' @title myMACD
#' @param x and xts object from quantmod
#' @return and xts object 
#' @description functions to build predictors varaibles from an xts quantmod object
#' @export
myMACD <- function(x) MACD(Cl(x))[,2]
#' @title myMFI
#' @param x and xts object from quantmod
#' @return and xts object 
#' @description functions to build predictors varaibles from an xts quantmod object
#' @export
myMFI <- function(x)MFI(HLC(x), x[,"Volume"])
#' @title mySAR
#' @param x and xts object from quantmod
#' @return and xts object 
#' @description functions to build predictors varaibles from an xts quantmod object
#' @export
mySAR <- function(x)SAR(HLC(x)[,1:2])[,1]
#' @title myVolat
#' @param x and xts object from quantmod
#' @return and xts object 
#' @description functions to build predictors varaibles from an xts quantmod object
#' @export
myVolat <- function(x)volatility(OHLC(x),calc="garman")[,1]
