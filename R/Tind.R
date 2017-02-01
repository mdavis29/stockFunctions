#' @title Trade Signal Detector
#' @param quotes An xts object containing stock quotes (build from getSymbols in quantmod)
#' @param tgt.margin percent increase or decrease in high or low price
#' @param n.days Number of days ahead
#' @author Matthew Davis
#' @export
#' @description creates a buy sell hold trade signal from stock quotes downloaded using quatmod
#' @return xts object of trade signals, buy when T.ind > 0.1, Sell < -0.1, else hold.
T.ind<-function(quotes, tgt.margin = 0.025, n.days =10){
      v<-apply(HLC(quotes),1,mean)
      r<-matrix(NA, ncol = n.days, nrow = nrow(quotes))
      for ( x in 1:n.days){r[,x]<-Next(Delt(v, k =x),x)}
      x<-apply(r, 1, function(x)sum(x[x > tgt.margin | x < -tgt.margin ]))
      if (is.xts(quotes))
      xts(x,time(quotes))
      else x }

