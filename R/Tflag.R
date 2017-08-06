#' @title Trade Signal Flag
#' @param x An xts object containing output from T.ind()
#' @param margin signal strength from T.ind() that will trigger the flag 
#' @export
#' @description flags when to buy sell and hold a stock, after inputing trade signal from T.ind()
#' @return xts object of trade signals,buy = 1, sell = -1, hold = 0
T.flag<-function(x, margin=0.025){
  o<-apply(x, 1,
           flag<-function(x, m=margin){
             if(!is.na(x)){
               if(x >= m) y <- 1
               if(x <= -m) y <- -1
               if( x < m & x > -m) y <-0}
             if(is.na(x))y<-NA
             return(y)})
  o<-as.xts(o)
  return(o)}

